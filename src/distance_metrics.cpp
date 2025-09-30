# define ARMA_WARN_LEVEL 0
# include <RcppArmadillo.h>
// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::plugins(openmp)]]


#ifdef _OPENMP
#include <omp.h>
#endif



// return data struct
//

struct return_data {

  arma::mat knn_idx;

  arma::mat knn_dist;
};



// kernelknn-class
//


class kernelKnn {

  private:

    arma::mat knn_indices;

    arma::mat knn_distances;

  public:

    kernelKnn() { }


    // inner loop [ no difference between threaded and non-threaded version for the 'inner_loop' method (parallelization) ]
    //

    arma::rowvec inner_loop(arma::mat& MATRIX_1st, arma::mat& MATRIX_2nd, int i, int ITERS, int k, std::string& method, arma::mat& cov_mat, double p, double eps = 1.0e-6) {


      arma::rowvec tmp_out = arma::zeros<arma::rowvec>(ITERS);


      for (int j = 0; j < ITERS; j++) {                                                                                                                                                        // http://scikit-learn.org/stable/modules/generated/sklearn.neighbors.DistanceMetric.html

        double tmp_idx;

        if (method == "euclidean") {

          tmp_idx = std::sqrt(arma::as_scalar(arma::accu(arma::square((MATRIX_1st.row(j) - MATRIX_2nd.row(i))))));
        }

        else if (method == "manhattan") {

          tmp_idx = arma::as_scalar(arma::accu(arma::abs((MATRIX_1st.row(j) - MATRIX_2nd.row(i)))));
        }

        else if (method == "chebyshev") {

          tmp_idx = arma::as_scalar(max(arma::abs((MATRIX_1st.row(j) - MATRIX_2nd.row(i)))));
        }

        else if (method == "canberra") {

          tmp_idx = arma::as_scalar(arma::accu(arma::abs((MATRIX_1st.row(j) - MATRIX_2nd.row(i)) + eps)/(arma::abs(MATRIX_1st.row(j)) + arma::abs(MATRIX_2nd.row(i)) + eps)));                 // added 1.0e-6 otherwise rstudio crashes
        }

        else if (method == "braycurtis") {

          tmp_idx = arma::as_scalar(arma::accu(arma::abs((MATRIX_1st.row(j) - MATRIX_2nd.row(i))))/(arma::accu(arma::abs(MATRIX_1st.row(j))) + arma::accu(arma::abs(MATRIX_2nd.row(i)))));
        }

        else if (method == "pearson_correlation") {

          tmp_idx = arma::as_scalar(1.0 - arma::cor(MATRIX_1st.row(j), MATRIX_2nd.row(i)));
        }

        else if (method == "simple_matching_coefficient") {

          double a = eps;
          double d = eps;

          for (unsigned int t = 0; t < MATRIX_1st.row(j).n_elem; t++) {

            if (MATRIX_1st.row(j)(t) == 1 && MATRIX_2nd.row(i)(t) == 1) {

              a += 1.0;}

            if (MATRIX_1st.row(j)(t) == 0 && MATRIX_2nd.row(i)(t) == 0) {

              d += 1.0;
            }
          }

          tmp_idx = 1.0 - ((a + d) / MATRIX_1st.row(j).n_elem);
        }

        else if (method == "minkowski") {                                                                                                       // by default the order of the minkowski parameter equals k

          tmp_idx = std::pow(arma::as_scalar(arma::accu(arma::pow(arma::abs((MATRIX_1st.row(j) - MATRIX_2nd.row(i))), p))), 1.0/p);
        }

        else if (method == "hamming") {                                                                                                          // for binary data

          tmp_idx = arma::as_scalar(accu(MATRIX_1st.row(j) != MATRIX_2nd.row(i))/(MATRIX_1st.row(j).n_elem * 1.0));
        }

        else if (method == "mahalanobis") {                                                                                                       // first create covariance matrix from data

          tmp_idx = arma::as_scalar(std::sqrt(arma::as_scalar(((MATRIX_1st.row(j) - MATRIX_2nd.row(i)) * cov_mat) * (MATRIX_1st.row(j) - MATRIX_2nd.row(i)).t())));
        }

        else if (method == "jaccard_coefficient") {                                                                                                // for binary data

          double a = eps;
          double b = eps;
          double c = eps;

          for (unsigned int t = 0; t < MATRIX_1st.row(j).n_elem; t++) {

            if (MATRIX_1st.row(j)(t) == 1 && MATRIX_2nd.row(i)(t) == 1) {

              a += 1.0;}

            if (MATRIX_1st.row(j)(t) == 1 && MATRIX_2nd.row(i)(t) == 0) {

              b += 1.0;}

            if (MATRIX_1st.row(j)(t) == 0 && MATRIX_2nd.row(i)(t) == 1) {

              c += 1.0;
            }
          }

          tmp_idx = 1.0 - (a / (a + b + c));
        }

        else if (method == "Rao_coefficient") {                                                                                     // for binary data

          double a = eps;

          for (unsigned int t = 0; t < MATRIX_1st.row(j).n_elem; t++) {

            if (MATRIX_1st.row(j)(t) == 1 && MATRIX_2nd.row(i)(t) == 1) {

              a += 1.0;
            }
          }

          tmp_idx = 1.0 - (a / MATRIX_1st.row(j).n_elem);
        }

        else {

          tmp_idx = 0;                                             // default = 0; create exceptions in R, so that tmp_idx is never 0;
        }

        if ( tmp_idx != tmp_idx ) {                                // handling of NAs, if NaN then distance 1.0 [  NaN will compare false to everything, including itself ], http://stackoverflow.com/questions/11569337/using-an-if-statement-to-switch-nan-values-in-an-array-to-0-0]

          tmp_out(j) = 1.0;
        }

        else {

          tmp_out(j) = tmp_idx;
        }
      }

      return tmp_out;
    }


    // calculate the 'inverse' AND in case of exception the 'Moore-Penrose pseudo-inverse' of the covariance matrix FOR the 'mahalanobis' distance
    // https://github.com/mlampros/KernelKnn/issues/1
    //

    arma::mat INV_EXC(arma::mat cov_data) {

      arma::mat inv_tmp;

      try {

        inv_tmp = arma::inv(arma::cov(cov_data));
      }

      catch(...) {

        Rcpp::warning("the input matrix seems singular. The Moore-Penrose pseudo-inverse of the covariance matrix will be calculated");
      }

      if (inv_tmp.empty()) {

        inv_tmp = arma::pinv(arma::cov(cov_data));
      }

      return inv_tmp;
    }


    // secondary function for the 'train_only' and 'test_only' methods [ due to ASAN errors ]
    //

    arma::field<arma::rowvec> inner_field_func(arma::mat& MATRIX, arma::mat& MATRIX1, int i, int ITERS, int k, std::string& method, arma::mat& cov_mat, double p, double eps = 1.0e-6) {

      arma::rowvec tmp_out = inner_loop(MATRIX, MATRIX1, i, ITERS, k, method, cov_mat, p, eps);

      arma::uvec index_out = arma::sort_index(tmp_out, "ascend");

      arma::field<arma::rowvec> F(2,1);

      F(0,0) = tmp_out;

      F(1,0) = arma::conv_to< arma::rowvec >::from(index_out);

      return F;
    }


    // train-data-input-only
    //

    void train_only(arma::mat& MATRIX, int k, std::string& method, int threads, double p, double eps = 1.0e-6) {

      #ifdef _OPENMP
      omp_set_num_threads(threads);
      #endif

      unsigned int COLS = MATRIX.n_cols;

      arma::mat cov_mat(COLS, COLS);

      if (method == "mahalanobis") {

        cov_mat = INV_EXC(MATRIX);
      }

      int ITERS = MATRIX.n_rows;

      knn_indices.set_size(ITERS, k);                                                                   // loop to calculate the distances for the TRAIN data [ MATRIX ]

      knn_distances.set_size(ITERS, k);

      int i,f;

      #ifdef _OPENMP
      #pragma omp parallel for schedule(static) shared(ITERS, p, eps, cov_mat, method, k, MATRIX) private(i, f)
      #endif
      for (i = 0; i < ITERS; i++) {

        arma::field<arma::rowvec> unl_field = inner_field_func(MATRIX, MATRIX, i, ITERS, k, method, cov_mat, p, eps);

        for (f = 1; f < k + 1; f++) {

          int IDX_subset_iter = unl_field(1,0)(f);

          #ifdef _OPENMP
          #pragma omp atomic write
          #endif
          knn_indices(i,f-1) = IDX_subset_iter + 1;                                                     // 'knn_indices', 'knn_distances' : class-members (not variables) are 'shared' by default when they aren't present in the clauses [ see comment https://stackoverflow.com/questions/5891641/data-members-in-an-openmp-loop ]

          #ifdef _OPENMP
          #pragma omp atomic write
          #endif
          knn_distances(i,f-1) = unl_field(0,0)(IDX_subset_iter);
        }
      }
    }


    // test-data-only
    //

    void test_only(arma::mat& MATRIX, arma::mat& TEST_DATA, int k, std::string& method, int threads, double p, double eps = 1.0e-6) {

      #ifdef _OPENMP
      omp_set_num_threads(threads);
      #endif

      unsigned int COLS = MATRIX.n_cols;

      arma::mat cov_mat(COLS, COLS);

      if (method == "mahalanobis") {

        cov_mat = INV_EXC(arma::join_vert(MATRIX, TEST_DATA));
      }

      int ITERS_TEST = TEST_DATA.n_rows;

      int ITERS_TRAIN = MATRIX.n_rows;

      knn_indices.set_size(ITERS_TEST, k);

      knn_distances.set_size(ITERS_TEST, k);

      int i,f;

      #ifdef _OPENMP
      #pragma omp parallel for schedule(static) shared(ITERS_TEST, p, eps, cov_mat, threads, method, k, ITERS_TRAIN, TEST_DATA, MATRIX) private(i, f)
      #endif
      for (i = 0; i < ITERS_TEST; i++) {

        arma::field<arma::rowvec> unl_field = inner_field_func(MATRIX, TEST_DATA, i, ITERS_TRAIN, k, method, cov_mat, p, eps);

        for (f = 0; f < k; f++) {

          int IDX_subset_iter = unl_field(1,0)(f);

          #ifdef _OPENMP
          #pragma omp atomic write
          #endif
          knn_indices(i,f) = IDX_subset_iter + 1;

          #ifdef _OPENMP
          #pragma omp atomic write
          #endif
          knn_distances(i,f) = unl_field(0,0)(IDX_subset_iter);
        }
      }
    }


    // secondary function for the 'input_dist_mat' [ due to ASAN errors ]
    //

    arma::field<arma::rowvec> inner_dist_field(arma::mat& DIST_MAT, bool idx_openmp_flag, arma::uvec& test_idx, double min_sort, unsigned int i, const char *asc_des) {

      arma::rowvec tmp_row = DIST_MAT.row(i);                                                             // take each row IF test-indices NOT Null AND IF minimize = true THEN fill the test-indices with A MAXIMUM VALUE (so that sort_index does not pick one of the test-indices) IF minimize = false THEN fill the test-indices with a MINIMUM VALUE

      if (idx_openmp_flag) {

        tmp_row(test_idx).fill(min_sort);                                                                  // another option would be to use erase for the 'test_idx' (remove the 'test_idx' from 'tmp_row') so that the for-loop can run faster, however I won't be able to calculate the sorted indices correctly (especially in case of cross-validation)
      }

      arma::uvec index_out = arma::sort_index(tmp_row, asc_des);

      arma::field<arma::rowvec> F(2,1);

      F(0,0) = tmp_row;

      F(1,0) = arma::conv_to< arma::rowvec >::from(index_out);

      return F;
    }


    // input data is a distance matrix
    //

    void input_dist_mat(arma::mat DIST_MAT_input, Rcpp::Nullable<Rcpp::NumericVector> TEST_IDX = R_NilValue, bool is_min = true, int k = 5, int threads = 1) {

      #ifdef _OPENMP
      omp_set_num_threads(threads);
      #endif

      arma::mat& DIST_MAT = DIST_MAT_input;                                    // copy initial data (by reference), because I'll modified it

      arma::uvec test_idx;

      bool idx_openmp_flag = false;

      if (TEST_IDX.isNotNull()) {

        idx_openmp_flag = true;

        test_idx = Rcpp::as<arma::uvec>(TEST_IDX);

        test_idx = test_idx - 1;                                               // adjust the indices to c++ indexing (which begins from 0)

        DIST_MAT = DIST_MAT.rows(test_idx);                                    // overwrite the DIST_MAT using the test-indices (in case that TEST_IDX not NULL)
      }

      unsigned int ROWS = DIST_MAT.n_rows;

      const char *asc_des = is_min ? "ascend" : "descend";

      double min_sort = arma::datum::inf;;

      if (idx_openmp_flag) {

        min_sort = is_min ? arma::datum::inf : (-arma::datum::inf);             // in case of minimization of distance (and TEST_IDX not NULL) assign the max. value to train-data (and the opposite if maximization). That way I "indirectly" omit the TEST_IDX data from sorted-distance-calculation
      }

      int start_idx = idx_openmp_flag ? 0 : 1;

      int end_idx = idx_openmp_flag ? k : (k + 1);

      knn_indices.set_size(ROWS, k);

      knn_distances.set_size(ROWS, k);

      unsigned int i;

      #ifdef _OPENMP
      #pragma omp parallel for schedule(static) shared(ROWS, DIST_MAT, idx_openmp_flag, min_sort, test_idx, asc_des, start_idx, end_idx) private(i)
      #endif
      for (i = 0; i < ROWS; i++) {

        arma::field<arma::rowvec> unl_field = inner_dist_field(DIST_MAT, idx_openmp_flag, test_idx, min_sort, i, asc_des);

        for (int f = start_idx; f < end_idx; f++) {

          int IDX_subset_iter = unl_field(1,0)(f);

          int app_idx = idx_openmp_flag ? f : f-1;

          #ifdef _OPENMP
          #pragma omp atomic write
          #endif
          knn_indices(i,app_idx) = IDX_subset_iter + 1;

          #ifdef _OPENMP
          #pragma omp atomic write
          #endif
          knn_distances(i,app_idx) = unl_field(0,0)(IDX_subset_iter);
        }
      }
    }


    // return data for either train or test
    //

    return_data return_train_test() {

      return { knn_indices, knn_distances };
    }


    ~kernelKnn() { }
};




//------------------------------------------Rcpp::export


// kernel-knn (input : raw data)
//------------------------------

// [[Rcpp::export]]
Rcpp::List knn_index_dist_rcpp(arma::mat& MATRIX, arma::mat& TEST_DATA, int k, std::string& method, int threads, double p, double eps = 1.0e-6) {

  kernelKnn kn;

  std::string name_idx;

  std::string name_dist;

  if (TEST_DATA.is_empty()) {

    kn.train_only( MATRIX, k, method, threads, p, eps );

    name_idx = "train_knn_idx";

    name_dist = "train_knn_dist";
  }

  if (!TEST_DATA.is_empty()) {

    kn.test_only( MATRIX, TEST_DATA, k, method, threads, p, eps );

    name_idx = "test_knn_idx";

    name_dist = "test_knn_dist";
  }

  return_data dat = kn.return_train_test();

  return Rcpp::List::create(Rcpp::Named(name_idx) = dat.knn_idx, Rcpp::Named(name_dist) = dat.knn_dist);
}




// kernel-knn (input : distance matrix)
//-------------------------------------

// [[Rcpp::export]]
Rcpp::List DIST_MATRIX_knn(arma::mat& DIST_MAT, Rcpp::Nullable<Rcpp::NumericVector> TEST_IDX = R_NilValue, bool is_min = true, int k = 5, int threads = 1, bool rcpp_list_names = false) {

  kernelKnn dist_knn;

  dist_knn.input_dist_mat(DIST_MAT, TEST_IDX, is_min, k, threads);

  return_data dat = dist_knn.return_train_test();

  std::string name_idx, name_dist;

  if (!rcpp_list_names) {                 // used in 'distMat.KernelKnn'

    name_idx = "knn_idx";
    name_dist = "knn_dist";}

  else {

    if (TEST_IDX.isNull()) {              // used in 'distMat.knn.index.dist'

      name_idx = "train_knn_idx";
      name_dist = "train_knn_dist";}

    else {

      name_idx = "test_knn_idx";
      name_dist = "test_knn_dist";
    }
  }

  return Rcpp::List::create(Rcpp::Named(name_idx) = dat.knn_idx, Rcpp::Named(name_dist) = dat.knn_dist);
}



