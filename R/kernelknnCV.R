
#' kernel-k-nearest-neighbors using cross-validation
#'
#' This function performs kernel k nearest neighbors regression and classification using cross validation
#'
#' @param data a data frame or matrix
#' @param y a numeric vector (in classification the labels must be numeric from 1:Inf)
#' @param k an integer specifying the k-nearest-neighbors
#' @param folds the number of cross validation folds (must be greater than 1)
#' @param h the bandwidth (applicable if the weights_function is not NULL, defaults to 1.0)
#' @param method a string specifying the method. Valid methods are 'euclidean', 'manhattan', 'chebyshev', 'canberra', 'braycurtis', 'pearson_correlation', 'simple_matching_coefficient', 'minkowski' (by default the order 'p' of the minkowski parameter equals k), 'hamming', 'mahalanobis', 'jaccard_coefficient', 'Rao_coefficient'
#' @param weights_function there are various ways of specifying the kernel function. See the details section.
#' @param regression a boolean (TRUE,FALSE) specifying if regression or classification should be performed
#' @param transf_categ_cols a boolean (TRUE, FALSE) specifying if the categorical columns should be converted to numeric or to dummy variables
#' @param threads the number of cores to be used in parallel (openmp will be employed)
#' @param extrema if TRUE then the minimum and maximum values from the k-nearest-neighbors will be removed (can be thought as outlier removal)
#' @param Levels a numeric vector. In case of classification the unique levels of the response variable are necessary
#' @return a list of length 2. The first sublist is a list of predictions (the length of the list equals the number of the folds). The second sublist is a list with the indices for each fold.
#' @author Lampros Mouselimis
#' @details
#' This function takes a number of arguments (including the number of cross-validation-folds) and it returns predicted values and indices for each fold. 
#' There are three possible ways to specify the weights function, 1st option : if the weights_function is NULL then a simple k-nearest-neighbor is performed. 2nd option : the weights_function is one of 'uniform', 'triangular', 'epanechnikov', 'biweight', 'triweight', 'tricube', 'gaussian', 'cosine', 'logistic', 'gaussianSimple', 'silverman', 'inverse', 'exponential'. The 2nd option can be extended by combining kernels from the existing ones (adding or multiplying). For instance, I can multiply the tricube with the gaussian kernel by giving 'tricube_gaussian_MULT' or I can add the previously mentioned kernels by giving 'tricube_gaussian_ADD'. 3rd option : a user defined kernel function
#' @export
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @examples
#'
#' data(ionosphere)
#' 
#' X = ionosphere[, -c(2, ncol(ionosphere))]
#' y = as.numeric(ionosphere[, ncol(ionosphere)])
#'
#' out = KernelKnnCV(X, y, k = 5, folds = 3, regression = FALSE, Levels = unique(y))
#' 


KernelKnnCV = function(data, y, k = 5, folds = 5, h = 1.0, method = 'euclidean', weights_function = NULL, regression = F, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL) {
  
  if (length(y) != nrow(data)) stop('the size of the data and y differ')
  if (!is.logical(regression)) stop('the regression argument should be either TRUE or FALSE')
  if (any(is.na(data)) || any(is.na(y))) stop('the data or the response variable includes missing values')
  if (is.null(y)) stop('the response variable should be numeric')
  if (is.integer(y)) y = as.numeric(y)
  if (!is.numeric(y)) stop('in both regression and classification the response variable should be numeric or integer and in classification it should start from 1')
  if (!regression && any(unique(y) < 1)) stop('the response variable values should begin from 1')
  if (folds < 2) stop('the number of folds should be at least 2')
  
  start = Sys.time()
  
  if (regression) {
    
    set.seed(folds)
    n_folds = regr_folds(folds, y)}
  
  else {
    
    set.seed(folds)
    n_folds = class_folds(folds, as.factor(y))
  }
  
  if (!all(unlist(lapply(n_folds, length)) > 5)) stop('Each fold has less than 5 observations. Consider decreasing the number of folds or increasing the size of the data.')
  
  tmp_fit = list()
  
  cat('\n') ; cat('cross-validation starts ..', '\n')
  
  pb <- txtProgressBar(min = 1, max = folds, style = 3); cat('\n')
  
  for (i in 1:folds) {
    
    tmp_fit[[i]] = KernelKnn(data[unlist(n_folds[-i]), ], TEST_data = data[unlist(n_folds[i]), ], y[unlist(n_folds[-i])], k = k, h = h, method = method, 
                        
                        weights_function = weights_function, regression = regression, transf_categ_cols = transf_categ_cols, threads = threads, extrema = extrema, Levels = Levels)
    
    setTxtProgressBar(pb, i)
  }
  
  close(pb); cat('\n')
  
  end = Sys.time()
  
  t = end - start
  
  cat('time to complete :', t, attributes(t)$units, '\n'); cat('\n');
  
  return(list(preds = tmp_fit, folds = n_folds))
}


