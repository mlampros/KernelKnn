#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _KernelKnn_DIST_MATRIX_knn(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _KernelKnn_knn_index_dist_rcpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_KernelKnn_DIST_MATRIX_knn",     (DL_FUNC) &_KernelKnn_DIST_MATRIX_knn,     6},
    {"_KernelKnn_knn_index_dist_rcpp", (DL_FUNC) &_KernelKnn_knn_index_dist_rcpp, 6},
    {NULL, NULL, 0}
};

void R_init_KernelKnn(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
