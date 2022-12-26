
#' indices and distances of k-nearest-neighbors
#'
#' This function returns the k nearest indices and distances of each observation
#'
#' @param data a data.frame or matrix
#' @param TEST_data a data.frame or matrix (it can be also NULL)
#' @param k an integer specifying the k-nearest-neighbors
#' @param method a string specifying the method. Valid methods are 'euclidean', 'manhattan', 'chebyshev', 'canberra', 'braycurtis', 'pearson_correlation', 'simple_matching_coefficient', 'minkowski' (by default the order 'p' of the minkowski parameter equals k), 'hamming', 'mahalanobis', 'jaccard_coefficient', 'Rao_coefficient'
#' @param transf_categ_cols a boolean (TRUE, FALSE) specifying if the categorical columns should be converted to numeric or to dummy variables
#' @param threads the number of cores to be used in parallel (openmp will be employed)
#' @param p a numeric value specifying the 'minkowski' order, i.e. if 'method' is set to 'minkowski'. This parameter defaults to 'k'
#' @return a list of length 2. The first sublist returns the indices and the second the distances of the k nearest neighbors for each observation.
#' If TEST_data is NULL the number of rows of each sublist equals the number of rows in the train data. If TEST_data is not NULL the number of rows of each sublist equals the number of rows in the TEST data.
#' @author Lampros Mouselimis
#' @details
#' This function takes a number of arguments and it returns the indices and distances of the k-nearest-neighbors for each observation. If TEST_data is NULL then the indices-distances for the train data will be returned, whereas if TEST_data is not NULL then the indices-distances for the TEST_data will be returned.
#' @export
#' @examples
#'
#' data(Boston)
#'
#' X = Boston[, -ncol(Boston)]
#'
#' out = knn.index.dist(X, TEST_data = NULL, k = 4, method = 'euclidean', threads = 1)
#'


knn.index.dist = function(data, TEST_data = NULL, k = 5, method = 'euclidean', transf_categ_cols = F, threads = 1, p = k) {

  categorical_data_present = sapply(data, function(x) is.factor(x) || is.character(x))

  if (sum(categorical_data_present) && !transf_categ_cols) stop('Categorical columns present in data. These should be either converted to numeric or the function should be run with transf_categ_cols = TRUE')
  if (!is.numeric(k) || is.null(k) || (k >= nrow(data)) || k < 1) stop('k must be of type integer, greater than 0 and less than nrow(train)')
  if (!is.character(method) || is.null(method) || !method %in% c('euclidean', 'manhattan', 'chebyshev', 'canberra', 'braycurtis', 'pearson_correlation', 'simple_matching_coefficient',
                                                                 'minkowski', 'hamming', 'mahalanobis', 'jaccard_coefficient', 'Rao_coefficient'))
    stop("method must be of type character and one of 'euclidean', 'manhattan', 'chebyshev', 'canberra', 'braycurtis', 'pearson_correlation', 'simple_matching_coefficient',
         'minkowski', 'hamming', 'mahalanobis', 'jaccard_coefficient', 'Rao_coefficient'")
  if (any(is.na(data))) stop('the data or the response variable includes missing values')
  if (!is.null(TEST_data) && any(is.na(TEST_data))) stop('the TEST_data includes missing values')
  if (method %in% c('simple_matching_coefficient', 'jaccard_coefficient', 'Rao_coefficient') && !all(unlist(lapply(1:ncol(data), function(x) sum(c(0,1) %in% unique(data[, x])) == 2))))
    stop("methods : 'simple_matching_coefficient', 'jaccard_coefficient' and 'Rao_coefficient' require the data to be in binary form e.g 0,1")
  if (!is.null(TEST_data) && method %in% c('simple_matching_coefficient', 'jaccard_coefficient', 'Rao_coefficient') && !all(unlist(lapply(1:ncol(TEST_data), function(x) sum(c(0,1) %in% unique(TEST_data[, x])) == 2))))
    stop("methods : 'simple_matching_coefficient', 'jaccard_coefficient' and 'Rao_coefficient' require the TEST_data to be in binary form e.g 0,1")

  if (!is.null(TEST_data) && ncol(data) != ncol(TEST_data)) stop('the number of columns in train and test data differ')

  #----------------------------------------------------------------------------------------------------

  # check if any of the variables is categorical, if TRUE then convert categorical predictors to either dummy variables or numeric variables [ depending on the number of levels ]

  if (transf_categ_cols) {

    if (is.null(TEST_data)) {

      data = func_categorical_preds(data)}

    else {

      tmp_dat = func_categorical_preds(rbind(data, TEST_data))

      data = tmp_dat[1:nrow(data), ]

      TEST_data = tmp_dat[(nrow(data) + 1):nrow(tmp_dat), ]
    }
  }
  #----------------------------------------------------------------------------------------------------

  if (is.null(TEST_data)) {

    mat = matrix(, nrow = 0, ncol = 0)

    if (!is.matrix(data)) data = as.matrix(data)

    res = knn_index_dist_rcpp(data, mat, k = k, method = method, threads = threads, p = p)
  }

  else {

    if (!is.matrix(data)) data = as.matrix(data)
    if (!is.matrix(TEST_data)) TEST_data = as.matrix(TEST_data)

    res = knn_index_dist_rcpp(data, TEST_data, k = k, method = method, threads = threads, p = p)
  }

  return(res)
}


