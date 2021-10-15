
#' kernel k-nearest-neighbors using a distance matrix
#'
#' @param DIST_mat a distance matrix (square matrix) having a \emph{diagonal} filled with either zero's (\emph{0}) or NA's (\emph{missing values})
#' @param TEST_indices a numeric vector specifying the indices of the test data in the distance matrix (row-wise or column-wise). If the parameter equals NULL then no test data is included in the distance matrix
#' @param y a numeric vector (in classification the labels must be numeric from 1:Inf). It is assumed that if the \emph{TEST_indices} is not NULL then the length of \emph{y} equals to the rows of the train data \emph{( nrow(DIST_mat) - length(TEST_indices) )}, otherwise  \emph{length(y) == nrow(DIST_mat)}.
#' @param k an integer specifying the k-nearest-neighbors
#' @param h the bandwidth (applicable if the weights_function is not NULL, defaults to 1.0)
#' @param weights_function there are various ways of specifying the kernel function. See the details section.
#' @param regression a boolean (TRUE,FALSE) specifying if regression or classification should be performed
#' @param threads the number of cores to be used in parallel (openmp will be employed)
#' @param extrema if TRUE then the minimum and maximum values from the k-nearest-neighbors will be removed (can be thought as outlier removal)
#' @param Levels a numeric vector. In case of classification the unique levels of the response variable are necessary
#' @param minimize either TRUE or FALSE. If TRUE then lower values will be considered as relevant for the k-nearest search, otherwise higher values.
#' @return a vector (if regression is TRUE), or a data frame with class probabilities (if regression is FALSE)
#' @author Lampros Mouselimis
#' @details
#' This function takes a distance matrix (square matrix where the diagonal is filled with \emph{0} or \emph{NA}) as input. If the \emph{TEST_indices} parameter is NULL then the predictions for the train data will be returned, whereas if the \emph{TEST_indices} parameter is not NULL then the predictions for the test data will be returned.
#' There are three possible ways to specify the weights function, 1st option : if the weights_function is NULL then a simple k-nearest-neighbor is performed. 2nd option : the weights_function is one of 'uniform', 'triangular', 'epanechnikov', 'biweight', 'triweight', 'tricube', 'gaussian', 'cosine', 'logistic', 'gaussianSimple', 'silverman', 'inverse', 'exponential'. The 2nd option can be extended by combining kernels from the existing ones (adding or multiplying). For instance, I can multiply the tricube with the gaussian kernel by giving 'tricube_gaussian_MULT' or I can add the previously mentioned kernels by giving 'tricube_gaussian_ADD'. 3rd option : a user defined kernel function
#' @export
#' @importFrom stats dist
#' @examples
#'
#' data(Boston)
#'
#' X = Boston[, -ncol(Boston)]
#' y = Boston[, ncol(Boston)]
#'
#' dist_obj = dist(X)
#'
#' dist_mat = as.matrix(dist_obj)
#'
#' out = distMat.KernelKnn(dist_mat, TEST_indices = NULL, y, k = 5, regression = TRUE)
#'


distMat.KernelKnn = function(DIST_mat, TEST_indices = NULL, y, k = 5, h = 1.0, weights_function = NULL, regression = F, threads = 1, extrema = F, Levels = NULL, minimize = T) {

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
    if (!inherits(TEST_indices, c("numeric", "integer"))) stop("the 'TEST_indices' parameter should be a numeric vector")
    if (max(TEST_indices) > nrow(DIST_mat)) stop('the maximum number of the TEST_indices is greater than the rows of the input distance matrix')
    tr_idx = 1:nrow(DIST_mat)
    tr_idx = tr_idx[-TEST_indices]
    if (!(min(TEST_indices) > max(tr_idx))) stop("The minimum index of the 'TEST_indices' parameter is greater than the maximum index of the 'DIST_mat' data! Make sure that the 'TEST_indices' consist of the last indices of the 'DIST_mat' parameter!")
  }
  if (!is.numeric(k) || is.null(k) || (k >= nrow(DIST_mat)) || k < 1) stop('k must be of type integer, greater than 0 and less than nrow(DIST_mat)')
  if (abs(k - round(k)) > 0) {
    k = round(k)
    warning('k is float and will be rounded to : ', call. = F, expr = k)}
  if (h == 0) stop('h can be any number except for 0')
  if (is.null(y)) stop('the response variable should be numeric')
  if (is.integer(y)) y = as.numeric(y)
  if (!is.numeric(y)) stop('in both regression and classification the response variable should be numeric or integer and in classification it should start from 1')
  if (!regression && is.null(Levels)) stop('In classification give the unique values of y in form of a vector')
  if (!regression && any(unique(y) < 1)) stop('the response variable values should begin from 1')
  if (!regression) {
    if (!all(Levels %in% unique(y))) stop("The specified 'Levels' must match the unique 'y' labels!")
  }
  if (any(is.na(DIST_mat)) || any(is.na(y))) stop('the DIST_mat or the response variable includes missing values')
  if (is.null(TEST_indices)) {
    if (length(y) != nrow(DIST_mat)) {
      stop('the size of the DIST_mat and y differ')
    }
  }
  if (extrema && k < 4) stop('k must be greater than 3 if extrema = TRUE')
  if (!inherits(minimize, "logical")) stop("the 'minimize' parameter should be either TRUE or FALSE")

  if (extrema) {

    k = k + 2           # add two values (for min-max)
  }

  index_train = DIST_MATRIX_knn(DIST_mat, TEST_indices, minimize, k, threads, F)        # the last parameter is FALSE because it is only applicable to 'distMat.knn.index.dist' [ here I don't need two separate functions for train and test data, as the function returns only predictions and not indices ]

  if (extrema) {

    index_train$knn_idx = index_train$knn_idx[, -c(1,k)]           # remove min, max  (matrices already sorted)
    index_train$knn_dist = index_train$knn_dist[, -c(1,k)]         # remove min, max  (matrices already sorted)

    k = k - 2          # adjust k to previous value
  }

  out_train = matrix(y[index_train$knn_idx], ncol = k)

  if (!regression) {

    if (is.null(weights_function)) {

      out = func_tbl_dist(out_train, sort(Levels))
    }
    else if (is.function(weights_function)) {

      W = FUNCTION_weights(index_train$knn_dist, weights_function)

      out = func_tbl(out_train, W, sort(Levels))
    }
    else if (is.character(weights_function) && nchar(weights_function) > 1) {

      W = FUN_kernels(weights_function, index_train$knn_dist, h)

      out = func_tbl(out_train, W, sort(Levels))
    }
    else {

      stop('false input for the weights_function argument')
    }

    colnames(out) = paste0('class_', sort(Levels))
  }

  else {

    if (is.null(weights_function)) {

      out = rowMeans(out_train)
    }
    else if (is.function(weights_function)) {

      W = FUNCTION_weights(index_train$knn_dist, weights_function)

      out = rowSums(out_train * W)
    }
    else if (is.character(weights_function) && nchar(weights_function) > 1) {

      W = FUN_kernels(weights_function, index_train$knn_dist, h)

      out = rowSums(out_train * W)
    }
    else {

      stop('false input for the weights_function argument')
    }
  }

  return(out)
}


#================================================================================================================================================================================
