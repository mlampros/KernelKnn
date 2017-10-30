
#' indices and distances of k-nearest-neighbors using a distance matrix
#'
#' @param DIST_mat a distance matrix (square matrix) having a diagonal filled with either zero's (\emph{0}) or NA's (\emph{missing values})
#' @param TEST_indices a numeric vector specifying the indices of the test data in the distance matrix (row-wise or column-wise). If the parameter equals NULL then no test data is included in the distance matrix
#' @param k an integer specifying the k-nearest-neighbors
#' @param threads the number of cores to be used in parallel (openmp will be employed)
#' @param minimize either TRUE or FALSE. If TRUE then lower values will be considered as relevant for the k-nearest search, otherwise higher values.
#' @return a list of length 2. The first sublist returns the indices and the second the distances of the k nearest neighbors for each observation.
#' If TEST_indices is NULL the number of rows of each sublist equals the number of rows in the DIST_mat data. If TEST_indices is not NULL the number of rows of each sublist equals the length of the input TEST_indices.
#' @author Lampros Mouselimis
#' @details
#' This function takes a number of arguments and it returns the indices and distances of the k-nearest-neighbors for each observation. If TEST_indices is NULL then the indices-distances for the DIST_mat be returned, whereas if TEST_indices is not NULL then the indices-distances for the test data only will be returned.
#' @export
#' @examples
#'
#' data(Boston)
#' 
#' X = Boston[, -ncol(Boston)]
#' 
#' dist_obj = dist(X)
#' 
#' dist_mat = as.matrix(dist_obj)
#' 
#' out = distMat.knn.index.dist(dist_mat, TEST_indices = NULL, k = 5)
#' 


distMat.knn.index.dist = function(DIST_mat, TEST_indices = NULL, k = 5, threads = 1, minimize = T) {
  
  if (!is.matrix(DIST_mat)) stop("the 'DIST_mat' parameter should be of type matrix")
  if (nrow(DIST_mat) != ncol(DIST_mat)) stop("the input 'DIST_mat' should be a square matrix with number of rows equal to number of columns")
  DIAG = diag(DIST_mat)
  nas = all(is.na(DIAG))
  if (nas) { 
    diag(DIST_mat) = 0 }              # set diagonal to 0.0 if equal to NA
  else {
    if (sum(DIAG) != 0) {
      stop("the diagonal of the distance matrix must be a vector of zeros or NA's")
    }
  }
  if (!is.null(TEST_indices)) {
    if (!inherits(TEST_indices, c("numeric", "integer"))) {
      stop("the 'TEST_indices' parameter should be a numeric vector")
    }
  }
  if (!is.null(TEST_indices)) {
    if (max(TEST_indices) > nrow(DIST_mat)) {
      stop('the maximum number of the TEST_indices is greater than the rows of the input distance matrix')
    }
  }
  if (!is.numeric(k) || is.null(k) || (k >= nrow(DIST_mat)) || k < 1) stop('k must be of type integer, greater than 0 and less than nrow(DIST_mat)')
  if (abs(k - round(k)) > 0) {
    k = round(k)
    warning('k is float and will be rounded to : ', call. = F, expr = k)}
  if (any(is.na(DIST_mat))) stop('the DIST_mat includes missing values')
  if (!inherits(minimize, "logical")) stop("the 'minimize' parameter should be either TRUE or FALSE")
  
  res = DIST_MATRIX_knn(DIST_mat, TEST_indices, minimize, k, threads, T)
  
  return(res)
}


