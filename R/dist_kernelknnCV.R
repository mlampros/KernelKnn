
#' cross validated kernel-k-nearest-neighbors using a distance matrix
#'
#'
#' @param DIST_mat a distance matrix (square matrix) having a diagonal filled with either zero's (\emph{0}) or NA's (\emph{missing values})
#' @param y a numeric vector (in classification the labels must be numeric from 1:Inf)
#' @param k an integer specifying the k-nearest-neighbors
#' @param folds the number of cross validation folds (must be greater than 1)
#' @param h the bandwidth (applicable if the weights_function is not NULL, defaults to 1.0)
#' @param weights_function there are various ways of specifying the kernel function. See the details section.
#' @param regression a boolean (TRUE,FALSE) specifying if regression or classification should be performed
#' @param threads the number of cores to be used in parallel (openmp will be employed)
#' @param extrema if TRUE then the minimum and maximum values from the k-nearest-neighbors will be removed (can be thought as outlier removal)
#' @param Levels a numeric vector. In case of classification the unique levels of the response variable are necessary
#' @param minimize either TRUE or FALSE. If TRUE then lower values will be considered as relevant for the k-nearest search, otherwise higher values.
#' @param seed_num a numeric value specifying the seed of the random number generator
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
#' \dontrun{
#' data(ionosphere)
#' 
#' X = ionosphere[, -c(2, ncol(ionosphere))]
#' y = as.numeric(ionosphere[, ncol(ionosphere)])
#' 
#' dist_obj = dist(X)
#' 
#' dist_mat = as.matrix(dist_obj)
#'
#' out = distMat.KernelKnnCV(dist_mat, y, k = 5, folds = 3, Levels = unique(y))
#' }


distMat.KernelKnnCV = function(DIST_mat, y, k = 5, folds = 5, h = 1.0, weights_function = NULL, regression = F, threads = 1, extrema = F, Levels = NULL, minimize = T, seed_num = 1) {
  
  if (is.null(y)) stop('the response variable should be numeric')
  if (is.integer(y)) y = as.numeric(y)
  if (!is.numeric(y)) stop('in both regression and classification the response variable should be numeric or integer and in classification it should start from 1')
  if (length(y) != nrow(DIST_mat)) stop('the size of the distance matrix and y differ')
  if (!is.logical(regression)) stop('the regression argument should be either TRUE or FALSE')
  if (any(is.na(DIST_mat)) || any(is.na(y))) stop('the DIST_mat or the response variable includes missing values')
  if (!regression && any(unique(y) < 1)) stop('the response variable values should begin from 1')
  if (folds < 2) stop('the number of folds should be at least 2')
  
  start = Sys.time()
  
  if (regression) {
    
    set.seed(seed_num)
    n_folds = regr_folds(folds, y)}
  
  else {
    
    set.seed(seed_num)
    n_folds = class_folds(folds, as.factor(y))
  }
  
  if (!all(unlist(lapply(n_folds, length)) > 5)) stop('Each fold has less than 5 observations. Consider decreasing the number of folds or increasing the size of the data.')
  
  tmp_fit = list()
  
  cat('\n') ; cat('cross-validation starts ..', '\n')
  
  pb <- txtProgressBar(min = 0, max = folds, style = 3); cat('\n')
  
  for (i in 1:folds) {
    
    tmp_fit[[i]] = distMat.KernelKnn(DIST_mat, TEST_indices = unlist(n_folds[i]), y, k = k, h = h, weights_function = weights_function, regression = regression, 
                                     
                                     threads = threads, extrema = extrema, Levels = Levels, minimize = minimize)
    
    setTxtProgressBar(pb, i)
  }
  
  close(pb); cat('\n')
  
  end = Sys.time()
  
  t = end - start
  
  cat('time to complete :', t, attributes(t)$units, '\n'); cat('\n');
  
  return(list(preds = tmp_fit, folds = n_folds))
}


