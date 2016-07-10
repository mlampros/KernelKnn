#================================================================================================================================================================

context("Utils testing")


# 'normalized' function

testthat::test_that("normalize data takes a vector as input and returns a vector as output", {

  y = c(1:50)

  testthat::expect_true(is.vector(normalized(y)))
})

testthat::test_that("the length of the input vector equals the length of the output vector", {

  y = c(1:50)

  output = normalized(y)

  testthat::expect_true(length(y) == length(output))
})


# 'func_tbl_dist' function

testthat::test_that("the output of the func_tbl_dist function has : same columns as the number of the unique levels, same rows as the initial matrix and all rows sum to 1.0", {

  unq_levels = 1:3

  tmp = matrix(sample(unq_levels, 40, replace = T), 8, 5)

  res = func_tbl_dist(tmp, unq_levels)

  testthat::expect_true(ncol(res) == length(unq_levels) && nrow(res) == nrow(tmp) && mean(rowSums(res)) == 1.0)
})


# 'func_tbl' function

testthat::test_that("the output of the func_tbl function has : same columns as the number of the unique levels, same rows as the initial matrix and all rows sum to 1.0", {

  unq_levels = 1:3

  W = matrix(rep(0.2, 40), 8, 5)

  tmp = matrix(sample(unq_levels, 40, replace = T), 8, 5)

  res = func_tbl(tmp, W, unq_levels)

  testthat::expect_true(ncol(res) == length(unq_levels) && nrow(res) == nrow(tmp) && mean(rowSums(res)) == 1.0)
})


# 'FUNCTION_weights' function

testthat::test_that("the output of the FUNCTION_weights is the multiplication of a distance matrix with a kernel function. It returns a matrix with the same dimensions

                    as the distance matrix and the rows sum to 1.0", {

  func = function(x) { 0.25 * x }

  tmp = matrix(sample(1:10, 16, replace = T), 4, 4)

  res = FUNCTION_weights(tmp, func, eps = 0.0)

  testthat::expect_true(ncol(res) == ncol(tmp) && nrow(res) == nrow(tmp) && mean(rowSums(res)) == 1.0)
})



# 'switch_secondary' function

testthat::test_that("the output of the switch_secondary is the multiplication of a distance matrix with a known kernel such as 'uniform', 'triangular' etc.

                    It returns a non-empty matrix with the same dimensions as the distance matrix", {

  W = matrix(1:25, 5, 5)

  lap = lapply(c('uniform', 'triangular', 'epanechnikov', 'biweight', 'triweight', 'tricube', 'gaussian', 'cosine', 'logistic', 'gaussianSimple', 'silverman', 'inverse', 'exponential'),

               function(x) switch_secondary(x, W, 1.0))

  dims = unlist(lapply(lap, function(x) sum(dim(x)) == sum(dim(W))))

  testthat::expect_true(sum(dims) == length(lap))
})


# switch.ops function

testthat::test_that("returns an error if the sublists are not matrices or data frames", {
  
  tmp = list(vec1 = 1:10, vec2 = 11:20)
  
  testthat::expect_error( switch.ops(tmp, MODE = 'ADD') )
})


# 'FUN_kernels' function

testthat::test_that("returns an error if the combination of the kernels is incorrect", {
  
  testthat::expect_error( FUN_kernels('uniform_triangular', matrix(runif(100), 10, 10), h = 1.0) )
})

testthat::test_that("returns an error if the combination of the kernels includes an unknown kernel", {
  
  testthat::expect_error( FUN_kernels('uniform_invalid_ADD', matrix(runif(100), 10, 10), h = 1.0) )
})

testthat::test_that("the output of the FUN_kernels is the multiplication of a distance matrix with a known kernel such as 'uniform', 'triangular' etc. In addition here 
                    
                    ADD and MULTiply different kernels. It returns a non-empty matrix with the same dimensions as the distance matrix", {

  W = matrix(1:25, 5, 5)

  lap = lapply(c('uniform', 'triangular', 'epanechnikov', 'biweight', 'triweight', 'tricube', 'gaussian', 'cosine', 'logistic', 'gaussianSimple', 'silverman', 'inverse', 
                 
                 'exponential', 'uniform_triangular_ADD', 'biweight_triweight_MULT', 'tricube_gaussian_MULT', 'gaussianSimple_silverman_ADD'), 
               
               function(x) FUN_kernels(x, W, 1.0))

  dims = unlist(lapply(lap, function(x) sum(dim(x)) == sum(dim(W))))

  testthat::expect_true(sum(dims) == length(lap))
})


# 'func_categorical_preds' function

testthat::test_that("if TRUE the function builds dummy variables for factor predictors with less than 32 unique levels and converts to numeric factor predictors with more than 32 levels", {

  m = data.frame(a = as.factor(c(1:19, rep(20, 21))), b = 1:40, c = as.factor(1:40), d = 40:1)

  res = func_categorical_preds(m)

  testthat::expect_true(ncol(res) == 20 + 3)
})


# 'func_categorical_preds' function [ multiple factor variables ]

testthat::test_that("if TRUE the function builds dummy variables for factor predictors with less than 32 unique levels and converts to numeric factor predictors with more than 32 levels", {
  
  m = data.frame(a = as.factor(c(1:19, rep(20, 21))), s = as.factor(c(1:19, rep(20, 21))), k = as.factor(c(1:19, rep(20, 21))), b = 1:40, c = as.factor(1:40), d = 40:1)
  
  res = func_categorical_preds(m)
  
  testthat::expect_true(ncol(res) == 20 + 19 + 19 + 3)
})


# switch.ops - function


testthat::test_that("adding 3 matrices or data.frames of all 1's results in a a matrix of all 3's", {
  m1 = matrix(rep(1, 25), 5, 5)
  m2 = matrix(rep(1, 25), 5, 5)
  m3 = matrix(rep(1, 25), 5, 5)
  
  lst = list(m1, m2, m3)
  
  testthat::expect_equal(switch.ops(lst, MODE = 'ADD'), matrix(rep(3, 25), 5,5), check.attributes = FALSE)     # check.attributes = F otherwise due to dimnames error
})


testthat::test_that("multiplying 3 matrices or data.frames of all 2's results in a a matrix of all 8's", {
  m1 = matrix(rep(2, 25), 5, 5)
  m2 = matrix(rep(2, 25), 5, 5)
  m3 = matrix(rep(2, 25), 5, 5)
  
  lst = list(m1, m2, m3)
  
  testthat::expect_equal(switch.ops(lst, MODE = 'MULT'), matrix(rep(8, 25), 5,5), check.attributes = FALSE)     # check.attributes = F otherwise due to dimnames error
})


testthat::test_that("it throws an error if the sublist matrices are of unequal dimensions", {
  m1 = matrix(rep(2, 35), 5, 7)
  m2 = matrix(rep(2, 25), 5, 5)
  m3 = matrix(rep(2, 25), 5, 5)
  
  lst = list(m1, m2, m3)
  
  testthat::expect_error(switch.ops(lst, MODE = 'MULT'))     # check.attributes = F otherwise due to dimnames error
})


testthat::test_that("if LST is not a list it throws error", {
  m1 = matrix(rep(1, 25), 5, 5)
  
  testthat::expect_error( switch.ops(m1, MODE = 'MULT') ) 
})


testthat::test_that("if the MODE is an invalid string it throws an error", {  
  
  m1 = matrix(rep(2, 25), 5, 5)
  m2 = matrix(rep(2, 25), 5, 5)
  m3 = matrix(rep(2, 25), 5, 5)
  
  lst = list(m1, m2, m3)
  
  testthat::expect_error( switch.ops(lst, MODE = 'invalid') ) 
})

#================================================================================================================================================================
