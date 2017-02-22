#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP KernelKnn_knn_index_dist_rcpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"KernelKnn_knn_index_dist_rcpp", (DL_FUNC) &KernelKnn_knn_index_dist_rcpp, 6},
    {NULL, NULL, 0}
};

void R_init_KernelKnn(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

