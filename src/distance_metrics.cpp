//========================================================================================================================================================================================

# include <RcppArmadillo.h>
// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::plugins(cpp11)]]

#ifdef _OPENMP
#include <omp.h>
#endif



// [[Rcpp::export]]
Rcpp::List knn_index_dist_rcpp(arma::mat& MATRIX, arma::mat& TEST_DATA, int k, std::string method, int threads, double eps = 1.0e-6) {

  #ifdef _OPENMP
  omp_set_num_threads(threads);
  #endif

  if (TEST_DATA.is_empty()) {

    arma::mat cov_mat;

    if (method == "mahalanobis") {

      cov_mat = arma::inv(arma::cov(MATRIX));
    }

    int ITERS = MATRIX.n_rows;

    arma::mat out(ITERS, k, arma::fill::zeros);                                                           // loop to calculate the distances for the TRAIN data [MATRIX]

    arma::mat sorted_train_dist(ITERS, k, arma::fill::zeros);
    
    #ifdef _OPENMP
    #pragma omp parallel for schedule(static)
    #endif
    for (int i = 0; i < ITERS; i++) {

      double tmp_idx;
      
      arma::rowvec tmp_out = arma::zeros<arma::rowvec>(ITERS);

      #ifdef _OPENMP
      #pragma omp parallel for schedule(static)
      #endif
      for (int j = 0; j < ITERS; j++) {                                                                       // http://scikit-learn.org/stable/modules/generated/sklearn.neighbors.DistanceMetric.html

        if (method == "euclidean") {

          tmp_idx = std::sqrt(arma::as_scalar(arma::accu(arma::square((MATRIX.row(j) - MATRIX.row(i))))));
        }

        else if (method == "manhattan") {

          tmp_idx = arma::as_scalar(arma::accu(arma::abs((MATRIX.row(j) - MATRIX.row(i)))));
        }

        else if (method == "chebyshev") {

          tmp_idx = arma::as_scalar(max(arma::abs((MATRIX.row(j) - MATRIX.row(i)))));
        }

        else if (method == "canberra") {

          tmp_idx = arma::as_scalar(arma::accu(arma::abs((MATRIX.row(j) - MATRIX.row(i)) + eps)/(arma::abs(MATRIX.row(j)) + arma::abs(MATRIX.row(i)) + eps)));                 // added 1.0e-6 otherwise rstudio crashes
        }

        else if (method == "braycurtis") {

          tmp_idx = arma::as_scalar(arma::accu(arma::abs((MATRIX.row(j) - MATRIX.row(i))))/(arma::accu(arma::abs(MATRIX.row(j))) + arma::accu(arma::abs(MATRIX.row(i)))));
        }

        else if (method == "pearson_correlation") {

          tmp_idx = arma::as_scalar(1.0 - arma::cor(MATRIX.row(j), MATRIX.row(i)));
        }

        else if (method == "simple_matching_coefficient") {
          
          double a = eps;
          double d = eps;
          
          for (int t = 0; t < MATRIX.row(j).n_elem; t++) {
            
            if (MATRIX.row(j)(t) == 1 && MATRIX.row(i)(t) == 1) {
              
              a += 1.0;}
            
            if (MATRIX.row(j)(t) == 0 && MATRIX.row(i)(t) == 0) {
              
              d += 1.0;
            }
          }
          
          tmp_idx = 1.0 - ((a + d) / MATRIX.row(j).n_elem);
        }

        else if (method == "minkowski") {                                                                                     // by default the order of the minkowski parameter equals k

          tmp_idx = std::pow(arma::as_scalar(arma::accu(arma::pow(arma::abs((MATRIX.row(j) - MATRIX.row(i))), k))), 1.0/k);
        }

        else if (method == "hamming") {                                                                                     // for binary data

          tmp_idx = arma::as_scalar(accu(MATRIX.row(j) != MATRIX.row(i))/(MATRIX.row(j).n_elem * 1.0));
        }

        else if (method == "mahalanobis") {                                                                                     // first create covariance matrix from data

          tmp_idx = arma::as_scalar(std::sqrt(arma::as_scalar(((MATRIX.row(j) - MATRIX.row(i)) * cov_mat) * (MATRIX.row(j) - MATRIX.row(i)).t())));
        }
        
        else if (method == "jaccard_coefficient") {                                                                                     // for binary data
          
          double a = eps;
          double b = eps;
          double c = eps;
          
          for (int t = 0; t < MATRIX.row(j).n_elem; t++) {
            
            if (MATRIX.row(j)(t) == 1 && MATRIX.row(i)(t) == 1) {
              
              a += 1.0;}
            
            if (MATRIX.row(j)(t) == 1 && MATRIX.row(i)(t) == 0) {
              
              b += 1.0;}
            
            if (MATRIX.row(j)(t) == 0 && MATRIX.row(i)(t) == 1) {
              
              c += 1.0;
            }
          }
          
          tmp_idx = 1.0 - (a / (a + b + c));
        }
        
        else if (method == "Rao_coefficient") {                                                                                     // for binary data
          
          double a = eps;
          
          for (int t = 0; t < MATRIX.row(j).n_elem; t++) {
            
            if (MATRIX.row(j)(t) == 1 && MATRIX.row(i)(t) == 1) {
              
              a += 1.0;
            }
          }
          
          tmp_idx = 1.0 - (a / MATRIX.row(j).n_elem);
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

      arma::rowvec index_out = arma::conv_to< arma::rowvec >::from(sort_index(tmp_out, "ascend")) + 1;

      out.row(i) = index_out.subvec(1,k);

      arma::rowvec tmp_sort = arma::conv_to< arma::rowvec >::from(arma::sort(tmp_out, "ascend"));

      sorted_train_dist.row(i) = tmp_sort.subvec(1,k);
    }

    return(Rcpp::List::create(Rcpp::Named("train_knn_idx") = out, Rcpp::Named("train_knn_dist") = sorted_train_dist));
  }

  if (!TEST_DATA.is_empty()) {                                                                                  // check if object is NOT empty OTHERWISE return only the train data

    arma::mat cov_mat;

    if (method == "mahalanobis") {

      cov_mat = arma::inv(arma::cov(arma::join_vert(MATRIX, TEST_DATA)));
    }

    int ITERS_TEST = TEST_DATA.n_rows;

    int ITERS_TRAIN = MATRIX.n_rows;

    arma::mat TEST_OUT(ITERS_TEST, k, arma::fill::zeros);

    arma::mat sorted_dist(ITERS_TEST, k, arma::fill::zeros);

    #ifdef _OPENMP
    #pragma omp parallel for schedule(static)
    #endif
    for (int i = 0; i < ITERS_TEST; i++) {
      
      double tmp_idx;

      arma::rowvec tmp_out = arma::zeros<arma::rowvec>(ITERS_TRAIN);

      #ifdef _OPENMP
      #pragma omp parallel for schedule(static)
      #endif
      for (int j = 0; j < ITERS_TRAIN; j++) {

        if (method == "euclidean") {

          tmp_idx = std::sqrt(arma::as_scalar(arma::accu(arma::square((MATRIX.row(j) - TEST_DATA.row(i))))));}

        else if (method == "manhattan") {

          tmp_idx = arma::as_scalar(arma::accu(arma::abs((MATRIX.row(j) - TEST_DATA.row(i)))));
        }

        else if (method == "chebyshev") {

          tmp_idx = arma::as_scalar(arma::max(arma::abs((MATRIX.row(j) - TEST_DATA.row(i)))));
        }

        else if (method == "canberra") {

          tmp_idx = arma::as_scalar(arma::accu(arma::abs((MATRIX.row(j) - TEST_DATA.row(i)) + eps)/(arma::abs(MATRIX.row(j)) + arma::abs(TEST_DATA.row(i)) + eps)));         // added 1.0e-6 otherwise rstudio crashes
        }

        else if (method == "braycurtis") {

          tmp_idx = arma::as_scalar(arma::accu(arma::abs((MATRIX.row(j) - TEST_DATA.row(i))))/(arma::accu(arma::abs(MATRIX.row(j))) + arma::accu(arma::abs(TEST_DATA.row(i)))));
        }

        else if (method == "pearson_correlation") {

          tmp_idx = arma::as_scalar(1.0 - arma::cor(MATRIX.row(j), TEST_DATA.row(i)));
        }

        else if (method == "simple_matching_coefficient") {                                                                                     // for binary data
          
          double a = eps;
          double d = eps;
          
          for (int t = 0; t < MATRIX.row(j).n_elem; t++) {
            
            if (MATRIX.row(j)(t) == 1 && TEST_DATA.row(i)(t) == 1) {
              
              a += 1.0;}
            
            if (MATRIX.row(j)(t) == 0 && TEST_DATA.row(i)(t) == 0) {
              
              d += 1.0;
            }
          }
          
          tmp_idx = 1.0 - ((a + d) / MATRIX.row(j).n_elem);
        }

        else if (method == "minkowski") {                                                                                     //  by default the order of the minkowski parameter equals k

          tmp_idx = std::pow(arma::as_scalar(arma::accu(arma::pow(arma::abs((MATRIX.row(j) - TEST_DATA.row(i))), k))), 1.0/k);
        }

        else if (method == "hamming") {                                                                                     // for binary data

          tmp_idx = arma::as_scalar(arma::accu(MATRIX.row(j) != TEST_DATA.row(i))/(MATRIX.row(j).n_elem * 1.0));
        }

        else if (method == "mahalanobis") {                                                                                     //  first create covariance matrix from data 

          tmp_idx = arma::as_scalar(std::sqrt(arma::as_scalar(((MATRIX.row(j) - TEST_DATA.row(i)) * cov_mat) * (MATRIX.row(j) - TEST_DATA.row(i)).t())));
        }
        
        else if (method == "jaccard_coefficient") {                                                                                     // for binary data
          
          double a = eps;
          double b = eps;
          double c = eps;
          
          for (int t = 0; t < MATRIX.row(j).n_elem; t++) {
            
            if (MATRIX.row(j)(t) == 1 && TEST_DATA.row(i)(t) == 1) {
              
              a += 1.0;}
            
            if (MATRIX.row(j)(t) == 1 && TEST_DATA.row(i)(t) == 0) {
              
              b += 1.0;}
            
            if (MATRIX.row(j)(t) == 0 && TEST_DATA.row(i)(t) == 1) {
              
              c += 1.0;
            }
          }
          
          tmp_idx = 1.0 - (a / (a + b + c));
        }
        
        else if (method == "Rao_coefficient") {                                                                                     // for binary data
          
          double a = eps;
          
          for (int t = 0; t < MATRIX.row(j).n_elem; t++) {
            
            if (MATRIX.row(j)(t) == 1 && TEST_DATA.row(i)(t) == 1) {
              
              a += 1.0;
            }
          }
          
          tmp_idx = 1.0 - (a / MATRIX.row(j).n_elem);
        }
        
        else {
          
          tmp_idx = 0;                                             // default = 0; create exceptions in R, so that tmp_idx is never 0;
        }
        
        if( tmp_idx != tmp_idx ) { 
          
          tmp_out(j) = 1.0;}
        
        else {
          
          tmp_out(j) = tmp_idx;
        }
      }

      arma::rowvec index_out = arma::conv_to< arma::rowvec >::from(arma::sort_index(tmp_out, "ascend")) + 1;

      TEST_OUT.row(i) = index_out.subvec(0,k-1);

      arma::rowvec tmp_sort = arma::conv_to< arma::rowvec >::from(arma::sort(tmp_out, "ascend"));

      sorted_dist.row(i) = tmp_sort.subvec(0,k-1);
    }

    return(Rcpp::List::create(Rcpp::Named("test_knn_idx") = TEST_OUT, Rcpp::Named("test_knn_dist") = sorted_dist));
  }
  
  return 0;
}

//========================================================================================================================================================================================
