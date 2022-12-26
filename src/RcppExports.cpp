// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// knn_index_dist_rcpp
Rcpp::List knn_index_dist_rcpp(arma::mat& MATRIX, arma::mat& TEST_DATA, int k, std::string& method, int threads, double p, double eps);
RcppExport SEXP _KernelKnn_knn_index_dist_rcpp(SEXP MATRIXSEXP, SEXP TEST_DATASEXP, SEXP kSEXP, SEXP methodSEXP, SEXP threadsSEXP, SEXP pSEXP, SEXP epsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type MATRIX(MATRIXSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type TEST_DATA(TEST_DATASEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< std::string& >::type method(methodSEXP);
    Rcpp::traits::input_parameter< int >::type threads(threadsSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    Rcpp::traits::input_parameter< double >::type eps(epsSEXP);
    rcpp_result_gen = Rcpp::wrap(knn_index_dist_rcpp(MATRIX, TEST_DATA, k, method, threads, p, eps));
    return rcpp_result_gen;
END_RCPP
}
// DIST_MATRIX_knn
Rcpp::List DIST_MATRIX_knn(arma::mat& DIST_MAT, Rcpp::Nullable<Rcpp::NumericVector> TEST_IDX, bool is_min, int k, int threads, bool rcpp_list_names);
RcppExport SEXP _KernelKnn_DIST_MATRIX_knn(SEXP DIST_MATSEXP, SEXP TEST_IDXSEXP, SEXP is_minSEXP, SEXP kSEXP, SEXP threadsSEXP, SEXP rcpp_list_namesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type DIST_MAT(DIST_MATSEXP);
    Rcpp::traits::input_parameter< Rcpp::Nullable<Rcpp::NumericVector> >::type TEST_IDX(TEST_IDXSEXP);
    Rcpp::traits::input_parameter< bool >::type is_min(is_minSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type threads(threadsSEXP);
    Rcpp::traits::input_parameter< bool >::type rcpp_list_names(rcpp_list_namesSEXP);
    rcpp_result_gen = Rcpp::wrap(DIST_MATRIX_knn(DIST_MAT, TEST_IDX, is_min, k, threads, rcpp_list_names));
    return rcpp_result_gen;
END_RCPP
}
