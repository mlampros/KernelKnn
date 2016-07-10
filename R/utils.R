#==============================================================================================================================================

#' this function normalizes the data
#'
#' @keywords internal

normalized = function(x) {

  out = (x - min(x))/(max(x) - min(x))

  out
}



#' this function returns the probabilities in case of classification
#'
#' @keywords internal

func_tbl_dist = function(DF, Levels) {

  mat = matrix(rep(0, dim(DF)[1] * length(Levels)), ncol = length(Levels), nrow = dim(DF)[1])

  for (i in 1:dim(DF)[1]) {

    tmp_tbl = prop.table(table(DF[i, ]))

    mat[i, as.numeric(names(tmp_tbl))] = tmp_tbl
  }

  mat
}



#' this function returns a table of probabilities for each label
#'
#' @keywords internal

func_tbl = function(DF, W, labels) {

  tmp_W = matrix(rep(0, dim(DF)[1] * length(labels)), ncol = length(labels), nrow = dim(DF)[1])

  for (i in 1:length(labels)) {

    tmp_W[, i] <- rowSums(W * (DF == labels[i]))
  }

  tmp_W
}



#' this function is used as a kernel-function-identifier [ takes the distances and a weights-kernel (in form of a function) and returns weights ]
#'
#' @keywords internal

FUNCTION_weights = function(W_dist_matrix, weights_function, eps = 1.0e-6) {

  W_dist_matrix = t(apply(W_dist_matrix, 1, normalized))

  W_dist_matrix = W_dist_matrix - eps

  W = do.call(weights_function, list(W_dist_matrix))

  W <- W/rowSums(W)

  W
}



# secondary function used in 'FUN_kernels' function
#'
#' @keywords internal

switch_secondary = function(kernel, W, h, eps = 1.0e-6) {
  
  W = t(apply(W, 1, normalized))
  
  W = W - eps                                                 # add small value in case of 0's

  kernel <- match.arg(kernel, c('uniform', 'triangular', 'epanechnikov', 'biweight', 'triweight', 'tricube', 'gaussian', 'cosine', 'logistic', 'gaussianSimple',
                                
                                'silverman', 'inverse', 'exponential'), FALSE)
  
  switch(kernel,
         
         uniform = {W = (1/2)* abs(W/h)},
         
         triangular = {W = (1 - abs(W/h))},
         
         epanechnikov = {W = (3/4) * (1 - (W / h) ^ 2)},
         
         biweight = {W = (15/16) * ((1 - ((W / h) ^ 2)) ^ 2)},
         
         triweight = {W = (35/32) * ((1 - ((W / h) ^ 2)) ^ 3)},
         
         tricube = {W = (70/81) * ((1 - (abs(W) ^ 3)/(h ^ 3)) ^ 3)},
         
         gaussian = {W = (1/sqrt(2*pi)) * exp((-1/2) * ((W ^ 2)/(2*(h ^ 2))))},
         
         gaussianSimple = {W = exp(- (W / h) ^ 2)},
         
         cosine = {W = (pi/4) * cos((pi * abs(W - 0.5))/(2*h))},
         
         logistic = {W = (1/(exp(W/h) + 2 + exp(-W/h)))},
         
         silverman = {W = 1/2 * exp(-abs(W/h)/sqrt(2)) * sin((abs(W)/(2*h)) + (pi/4))},
         
         inverse = {W <- 1/abs(W/h)},
         
         exponential = {W = exp(- abs(W / h))},
  )
  
  W <- W/rowSums(W)	                                           # normalize weights
  
  return(W)
}


#' Arithmetic operations on lists
#' 
#' @keywords internal


switch.ops = function (LST, MODE = 'ADD') {
  
  if (class(LST) != "list")  stop("LST must be a list")
  
  if (!all(unlist(lapply(LST, class)) %in% c('data.frame', 'matrix'))) stop('the sublist objects must be either matrices or data frames')
  
  r = all(unlist(lapply(LST, nrow)) == unlist(lapply(LST, nrow))[1])
  
  c = all(unlist(lapply(LST, ncol)) == unlist(lapply(LST, ncol))[1])
  
  if (!all(c(r, c))) stop("the dimensions of the included data.frames or matrices differ")
  
  if (MODE == 'ADD') {
    
    init_df = data.frame(matrix(rep(0, dim(LST[[1]])[1] * dim(LST[[1]])[2]), nrow = dim(LST[[1]])[1], ncol = dim(LST[[1]])[2]))}
  
  else if (MODE == 'MULT') {
    
    init_df = data.frame(matrix(rep(1, dim(LST[[1]])[1] * dim(LST[[1]])[2]), nrow = dim(LST[[1]])[1], ncol = dim(LST[[1]])[2]))
  }
  
  else {
    
    stop('invalid MODE type')
  }
  
  for (i in 1:length(LST)) {
    
    if (MODE == 'ADD') {
      
      init_df = init_df + LST[[i]]}
    
    if (MODE == 'MULT') {
      
      init_df = init_df * LST[[i]]
    }
  }

  colnames(init_df) = colnames(LST[[1]])
  
  return(as.matrix(init_df))
}



#' performs kernel smoothing using a bandwidth. Besides using a kernel there is also the option to combine kernels 
#'
#' @keywords internal


FUN_kernels = function(kernel, W, h) {
  
  spl = strsplit(kernel, '_')[[1]]
  
  s_op = spl[length(spl)]
  
  s_kerns = spl[-length(spl)]
  
  if (length(spl) > 1) {                               # SEE, on combining_kernels : http://people.seas.harvard.edu/~dduvenaud/cookbook/
    
    if (length(s_kerns) < 2) stop('invalid kernel combination')
    
    kernels = c('uniform', 'triangular', 'epanechnikov', 'biweight', 'triweight', 'tricube', 'gaussian', 'cosine', 
                
                'logistic', 'gaussianSimple', 'silverman', 'inverse', 'exponential')
    
    if (sum(s_kerns %in% kernels) != length(s_kerns)) stop('invalid kernel combination')
    
    lap = lapply(s_kerns, function(x) switch_secondary(x, W, h))
    
    W = switch.ops(lap, MODE = s_op)
    
    W <- W/rowSums(W)
  }
  
  else {
    
    W = switch_secondary(kernel, W, h)
  }
  
  return(W)
}



#' OPTION to convert categorical features TO either numeric [ if levels more than 32] OR to dummy variables [ if levels less than 32 ]
#'
#' @keywords internal
#' @importFrom stats model.matrix

func_categorical_preds = function(prepr_categ) {

  less32 = sapply(prepr_categ, function(x) is.factor(x) && length(unique(x)) < 32)
  greater32 = sapply(prepr_categ, function(x) is.factor(x) && length(unique(x)) >= 32)

  if (sum(less32) == 1) {

    rem_predictors = names(which(less32))
    out_L = model.matrix(~. - 1, data = data.frame(prepr_categ[, rem_predictors]))
    colnames(out_L) = paste0(rem_predictors, 1:dim(out_L)[2])
  }

  if (sum(less32) > 1) {

    rem_predictors = names(which(less32))
    out_L = model.matrix(~. - 1, data = prepr_categ[, rem_predictors])
    colnames(out_L) = make.names(colnames(out_L))
  }

  if (sum(greater32) > 0) {

    fact_predictors = names(which(greater32))

    for (nams in fact_predictors) {

      prepr_categ[, nams] = as.numeric(prepr_categ[, nams])
    }
  }

  if (sum(less32) > 0) {

    return(cbind(prepr_categ[, -which(colnames(prepr_categ) %in% rem_predictors)], out_L))
  }

  else {

    return(prepr_categ)
  }
}


#' shuffle data
#'
#' this function shuffles the items of a vector
#' @keywords internal

func_shuffle = function(vec, times = 10) {
  
  for (i in 1:times) {
    
    out = sample(vec, length(vec))
  }
  out
}


#' stratified folds (in classification)                      [ detailed information about class_folds in the FeatureSelection package ]
#'
#' this function creates stratified folds in binary and multiclass classification
#' @keywords internal
#' @importFrom utils combn


class_folds = function(folds, RESP) {
  
  if (!is.factor(RESP)) {
    
    stop(simpleError("RESP must be a factor"))
  }
  
  clas = lapply(unique(RESP), function(x) which(RESP == x))
  
  len = lapply(clas, function(x) length(x))
  
  samp_vec = rep(1/folds, folds)
  
  prop = lapply(len, function(y) sapply(1:length(samp_vec), function(x) round(y * samp_vec[x])))
  
  repl = unlist(lapply(prop, function(x) sapply(1:length(x), function(y) rep(paste0('fold_', y), x[y]))))
  
  spl = suppressWarnings(split(1:length(RESP), repl))
  
  sort_names = paste0('fold_', 1:folds)
  
  spl = spl[sort_names]
  
  spl = lapply(spl, function(x) func_shuffle(x))           # the indices of the unique levels will be shuffled
  
  ind = t(combn(1:folds, 2))
  
  ind1 = apply(ind, 1, function(x) length(intersect(spl[x[1]], spl[x[2]])))
  
  if (sum(ind1) > 0) {
    
    stop(simpleError("there is an intersection between the resulted indexes of the folds"))
    
  }
  
  if (length(unlist(spl)) != length(RESP)) {
    
    stop(simpleError("the number of items in the folds are not equal with the response items"))
  }
  
  spl
}


#' create folds (in regression)                                           [ detailed information about class_folds in the FeatureSelection package ]
#'
#' this function creates both stratified and non-stratified folds in regression
#' @keywords internal


regr_folds = function(folds, RESP) {
  
  if (is.factor(RESP)) {
    
    stop(simpleError("this function is meant for regression for classification use the 'class_folds' function"))
  }
  
  samp_vec = rep(1/folds, folds)
  
  sort_names = paste0('fold_', 1:folds)
  
  prop = lapply(length(RESP), function(y) sapply(1:length(samp_vec), function(x) round(y * samp_vec[x])))
  
  repl = func_shuffle(unlist(lapply(prop, function(x) sapply(1:length(x), function(y) rep(paste0('fold_', y), x[y])))))
  
  spl = suppressWarnings(split(1:length(RESP), repl))
  
  spl = spl[sort_names]
  
  if (length(unlist(spl)) != length(RESP)) {
    
    stop(simpleError("the length of the splits are not equal with the length of the response"))
  }
  
  spl
}



#================================================================================================================================================================