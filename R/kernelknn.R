
#' kernel k-nearest-neighbors
#'
#' This function utilizes kernel k nearest neighbors to predict new observations
#'
#' @param data a data frame or matrix
#' @param TEST_data a data frame or matrix (it can be also NULL)
#' @param y a numeric vector (in classification the labels must be numeric from 1:Inf)
#' @param k an integer specifying the k-nearest-neighbors
#' @param h the bandwidth (applicable if the weights_function is not NULL, defaults to 1.0)
#' @param method a string specifying the method. Valid methods are 'euclidean', 'manhattan', 'chebyshev', 'canberra', 'braycurtis', 'pearson_correlation', 'simple_matching_coefficient', 'minkowski' (by default the order 'p' of the minkowski parameter equals k), 'hamming', 'mahalanobis', 'jaccard_coefficient', 'Rao_coefficient'
#' @param weights_function there are various ways of specifying the kernel function. See the details section.
#' @param regression a boolean (TRUE,FALSE) specifying if regression or classification should be performed
#' @param transf_categ_cols a boolean (TRUE, FALSE) specifying if the categorical columns should be converted to numeric or to dummy variables
#' @param threads the number of cores to be used in parallel (openmp will be employed)
#' @param extrema if TRUE then the minimum and maximum values from the k-nearest-neighbors will be removed (can be thought as outlier removal)
#' @param Levels a numeric vector. In case of classification the unique levels of the response variable are necessary
#' @return a vector (if regression is TRUE), or a data frame with class probabilities (if regression is FALSE)
#' @author Lampros Mouselimis
#' @details
#' This function takes a number of arguments and it returns the predicted values. If TEST_data is NULL then the predictions for the train data will be returned, whereas if TEST_data is not NULL then the predictions for the TEST_data will be returned.
#' There are three possible ways to specify the weights function, 1st option : if the weights_function is NULL then a simple k-nearest-neighbor is performed. 2nd option : the weights_function is one of 'uniform', 'triangular', 'epanechnikov', 'biweight', 'triweight', 'tricube', 'gaussian', 'cosine', 'logistic', 'gaussianSimple', 'silverman', 'inverse', 'exponential'. The 2nd option can be extended by combining kernels from the existing ones (adding or multiplying). For instance, I can multiply the tricube with the gaussian kernel by giving 'tricube_gaussian_MULT' or I can add the previously mentioned kernels by giving 'tricube_gaussian_ADD'. 3rd option : a user defined kernel function
#' @export
#' @examples
#'
#' data(Boston)
#'
#' X = Boston[, -ncol(Boston)]
#' y = Boston[, ncol(Boston)]
#'
#' out = KernelKnn(X, TEST_data = NULL, y, k = 5, method = 'euclidean', regression = TRUE)
#'



KernelKnn = function(data, TEST_data = NULL, y, k = 5, h = 1.0, method = 'euclidean', weights_function = NULL, regression = F, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL) {

  categorical_data_present = sapply(data, function(x) is.factor(x) || is.character(x))

  if (sum(categorical_data_present) && !transf_categ_cols) stop('Categorical columns present in data. These should be either converted to numeric or the function should be run with transf_categ_cols = TRUE')
  if (!is.numeric(k) || is.null(k) || (k >= nrow(data)) || k < 1) stop('k must be of type integer, greater than 0 and less than nrow(train)')
  if (abs(k - round(k)) > 0) {
    k = round(k)
    warning('k is float and will be rounded to : ', call. = F, expr = k)}
  if (h == 0) stop('h can be any number except for 0')
  if (!is.character(method) || is.null(method) || !method %in% c('euclidean', 'manhattan', 'chebyshev', 'canberra', 'braycurtis', 'pearson_correlation', 'simple_matching_coefficient', 
                                                                 'minkowski', 'hamming', 'mahalanobis', 'jaccard_coefficient', 'Rao_coefficient'))
    stop("method must be of type character and one of 'euclidean', 'manhattan', 'chebyshev', 'canberra', 'braycurtis', 'pearson_correlation', 'simple_matching_coefficient',
         'minkowski', 'hamming', 'mahalanobis', 'jaccard_coefficient', 'Rao_coefficient'")
  if (is.null(y)) stop('the response variable should be numeric')
  if (is.integer(y)) y = as.numeric(y)
  if (!is.numeric(y)) stop('in both regression and classification the response variable should be numeric or integer and in classification it should start from 1')
  if (!regression && is.null(Levels)) stop('In classification give the unique values of y in form of a vector')
  if (!regression && any(unique(y) < 1)) stop('the response variable values should begin from 1')
  if (any(is.na(data)) || any(is.na(y))) stop('the data or the response variable includes missing values')
  if (!is.null(TEST_data) && any(is.na(TEST_data))) stop('the TEST_data includes missing values')
  if (length(y) != nrow(data)) stop('the size of the data and y differ')
  if (extrema && k < 4) stop('k must be greater than 3 if extrema = TRUE')
  if (method %in% c('simple_matching_coefficient', 'jaccard_coefficient', 'Rao_coefficient') && !sum(apply(data, 2, function(x) all(unique(x) %in% c(0,1)))) == ncol(data))
    stop("methods : 'simple_matching_coefficient', 'jaccard_coefficient' and 'Rao_coefficient' require the data to be in binary form e.g 0,1")
  if (!is.null(TEST_data) && method %in% c('simple_matching_coefficient', 'jaccard_coefficient', 'Rao_coefficient') && !sum(apply(TEST_data, 2, function(x) all(unique(x) %in% c(0,1)))) == ncol(TEST_data))
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

    if (extrema) {

      k = k + 2           # add two values (for min-max)
    }

    index_train = knn_index_dist_rcpp(data, mat, k = k, method = method, threads = threads)

    if (extrema) {

      index_train$train_knn_idx = index_train$train_knn_idx[, -c(1,k)]           # remove min, max  (matrices already sorted)
      index_train$train_knn_dist = index_train$train_knn_dist[, -c(1,k)]         # remove min, max  (matrices already sorted)

      k = k - 2          # adjust k to previous value
    }

    out_train = matrix(y[index_train$train_knn_idx], nrow = nrow(data), ncol = k)

    if (!regression) {

      if (is.null(weights_function)) {

        out = func_tbl_dist(out_train, sort(Levels))

        colnames(out) = paste0('class_', sort(Levels))}

      else if (is.function(weights_function)) {

        W = FUNCTION_weights(index_train$train_knn_dist, weights_function)

        out = func_tbl(out_train, W, sort(Levels))}

      else if (is.character(weights_function) && nchar(weights_function) > 1) {

        W = FUN_kernels(weights_function, index_train$train_knn_dist, h)

        out = func_tbl(out_train, W, sort(Levels))}

      else {

        stop('false input for the weights_function argument')
      }
    }

    else {

      if (is.null(weights_function)) {

        out = rowMeans(out_train)
      }

      else if (is.function(weights_function)) {

        W = FUNCTION_weights(index_train$train_knn_dist, weights_function)

        out = rowSums(out_train * W)
      }

      else if (is.character(weights_function) && nchar(weights_function) > 1) {

        W = FUN_kernels(weights_function, index_train$train_knn_dist, h)

        out = rowSums(out_train * W)
      }

      else {

        stop('false input for the weights_function argument')
      }
    }

    return(out)
  }

  else {

    if (!is.matrix(data)) data = as.matrix(data)
    if (!is.matrix(TEST_data)) TEST_data = as.matrix(TEST_data)

    if (extrema) {

      k = k + 2           # add two values (for min-max)
    }

    index = knn_index_dist_rcpp(data, TEST_data, k = k, method = method, threads = threads)

    if (extrema) {

      index$test_knn_idx = index$test_knn_idx[, -c(1,k)]             # remove min, max  (matrices already sorted)
      index$test_knn_dist = index$test_knn_dist[, -c(1,k)]           # remove min, max  (matrices already sorted)

      k = k - 2          # adjust k to previous value
    }

    out_test = matrix(y[index$test_knn_idx], ncol = k)

    if (!regression) {

      if (is.null(weights_function)) {

        out_te = func_tbl_dist(out_test, sort(Levels))

        colnames(out_te) = paste0('class_', sort(Levels))}

      else if (is.function(weights_function)) {

        W_te = FUNCTION_weights(index$test_knn_dist, weights_function)

        out_te = func_tbl(out_test, W_te, sort(Levels))}

      else if (is.character(weights_function) && nchar(weights_function) > 1) {

        W_te = FUN_kernels(weights_function, index$test_knn_dist, h)

        out_te = func_tbl(out_test, W_te, sort(Levels))}

      else {

        stop('false input for the weights_function argument')
      }
    }

    else {

      if (is.null(weights_function)) {

        out_te = rowMeans(out_test)
      }

      else if (is.function(weights_function)) {

        W_te = FUNCTION_weights(index$test_knn_dist, weights_function)

        out_te = rowSums(out_test * W_te)
      }

      else if (is.character(weights_function) && nchar(weights_function) > 1) {

        W_te = FUN_kernels(weights_function, index$test_knn_dist, h)

        out_te = rowSums(out_test * W_te)
      }

      else {

        stop('false input for the weights_function argument')
      }
    }

    return(out_te)
  }
}


#================================================================================================================================================================================
