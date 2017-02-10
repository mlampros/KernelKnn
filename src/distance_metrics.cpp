#define ARMA_DONT_PRINT_ERRORS
# include <RcppArmadillo.h>
// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::plugins(cpp11)]]


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
    
    
    // inner loop
    //
    
    arma::rowvec inner_loop(arma::mat& MATRIX_1st, arma::mat& MATRIX_2nd, int i, int ITERS, int k, std::string method, int threads, arma::mat cov_mat, double eps = 1.0e-6) {
      
      #ifdef _OPENMP
      omp_set_num_threads(threads);
      #endif
      
      double tmp_idx;
      
      arma::rowvec tmp_out = arma::zeros<arma::rowvec>(ITERS);
      
      #ifdef _OPENMP
      #pragma omp parallel for schedule(static)
      #endif
      for (int j = 0; j < ITERS; j++) {                                                                       // http://scikit-learn.org/stable/modules/generated/sklearn.neighbors.DistanceMetric.html
        
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
        
        else if (method == "minkowski") {                                                                                     // by default the order of the minkowski parameter equals k
          
          tmp_idx = std::pow(arma::as_scalar(arma::accu(arma::pow(arma::abs((MATRIX_1st.row(j) - MATRIX_2nd.row(i))), k))), 1.0/k);
        }
        
        else if (method == "hamming") {                                                                                     // for binary data
          
          tmp_idx = arma::as_scalar(accu(MATRIX_1st.row(j) != MATRIX_2nd.row(i))/(MATRIX_1st.row(j).n_elem * 1.0));
        }
        
        else if (method == "mahalanobis") {                                                                                     // first create covariance matrix from data
          
          tmp_idx = arma::as_scalar(std::sqrt(arma::as_scalar(((MATRIX_1st.row(j) - MATRIX_2nd.row(i)) * cov_mat) * (MATRIX_1st.row(j) - MATRIX_2nd.row(i)).t())));
        }
        
        else if (method == "jaccard_coefficient") {                                                                                     // for binary data
          
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
          
          tmp_out(j) = 1.0;}
        
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
    
    
    // train-data-input-only
    //
    
    void train_only(arma::mat& MATRIX, int k, std::string method, int threads, double eps = 1.0e-6) {
      
      arma::mat cov_mat;
      
      if (method == "mahalanobis") {
        
        cov_mat = INV_EXC(arma::cov(MATRIX));
      }
      
      int ITERS = MATRIX.n_rows;
      
      knn_indices.set_size(ITERS, k);                                                           // loop to calculate the distances for the TRAIN data [MATRIX]
      
      knn_distances.set_size(ITERS, k);
      
      #ifdef _OPENMP
      #pragma omp parallel for schedule(static)
      #endif
      for (int i = 0; i < ITERS; i++) {
        
        arma::rowvec tmp_out = inner_loop(MATRIX, MATRIX, i, ITERS, k, method, threads, cov_mat, eps);
        
        arma::rowvec index_out = arma::conv_to< arma::rowvec >::from(arma::sort_index(tmp_out, "ascend")) + 1;
        
        knn_indices.row(i) = index_out.subvec(1,k);
        
        arma::rowvec tmp_sort = arma::conv_to< arma::rowvec >::from(arma::sort(tmp_out, "ascend"));
        
        knn_distances.row(i) = tmp_sort.subvec(1,k);
      }
    }
    
    
    
    // test-data-only
    //
    
    void test_only(arma::mat& MATRIX, arma::mat& TEST_DATA, int k, std::string method, int threads, double eps = 1.0e-6) {
      
      arma::mat cov_mat;
      
      if (method == "mahalanobis") {
        
        cov_mat = INV_EXC(arma::cov(arma::join_vert(MATRIX, TEST_DATA)));
      }
      
      int ITERS_TEST = TEST_DATA.n_rows;
      
      int ITERS_TRAIN = MATRIX.n_rows;
      
      knn_indices.set_size(ITERS_TEST, k);
      
      knn_distances.set_size(ITERS_TEST, k);
      
      #ifdef _OPENMP
      #pragma omp parallel for schedule(static)
      #endif
      for (int i = 0; i < ITERS_TEST; i++) {
        
        arma::rowvec tmp_out = inner_loop(MATRIX, TEST_DATA, i, ITERS_TRAIN, k, method, threads, cov_mat, eps);
        
        arma::rowvec index_out = arma::conv_to< arma::rowvec >::from(arma::sort_index(tmp_out, "ascend")) + 1;
        
        knn_indices.row(i) = index_out.subvec(0,k-1);
        
        arma::rowvec tmp_sort = arma::conv_to< arma::rowvec >::from(arma::sort(tmp_out, "ascend"));
        
        knn_distances.row(i) = tmp_sort.subvec(0,k-1);
      }
    }
    
    
    // return data for either train or test
    //
    
    return_data return_train_test() {
      
      return { knn_indices, knn_distances };
    }
    
    
    ~kernelKnn() { }
};




//------------------------------------------


// function to be exported
//

// [[Rcpp::export]]
Rcpp::List knn_index_dist_rcpp(arma::mat& MATRIX, arma::mat& TEST_DATA, int k, std::string method, int threads, double eps = 1.0e-6) {
  
  kernelKnn kn;
  
  std::string name_idx;
  
  std::string name_dist;
  
  if (TEST_DATA.is_empty()) {
    
    kn.train_only( MATRIX, k, method, threads, eps );
    
    name_idx = "train_knn_idx";
    
    name_dist = "train_knn_dist";
  }
  
  if (!TEST_DATA.is_empty()) {
    
    kn.test_only( MATRIX, TEST_DATA, k, method, threads, eps );
    
    name_idx = "test_knn_idx";
    
    name_dist = "test_knn_dist";
  }
  
  return_data dat = kn.return_train_test();
  
  return Rcpp::List::create(Rcpp::Named(name_idx) = dat.knn_idx, Rcpp::Named(name_dist) = dat.knn_dist);
}


