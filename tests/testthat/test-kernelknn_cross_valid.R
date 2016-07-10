
# data sets

# regression
data(Boston, package = 'KernelKnn')
X = Boston[, -dim(Boston)[2]]
y = Boston[, dim(Boston)[2]]

# binary classification
data(ionosphere, package = 'KernelKnn')
ionosphere = ionosphere[, -2]                                                                             # remove second column which has a single unique value
X_class = ionosphere[, -dim(ionosphere)[2]]
y1_class = ionosphere[, dim(ionosphere)[2]]


context("Cross validate kernelknn")


# shuffle data

testthat::test_that("shuffle data takes a vector as input and returns a vector as output", {
  
  y = c(1:50)
  
  testthat::expect_true(is.vector(func_shuffle(y, times = 10)))
})

testthat::test_that("the length of the input vector equals the length of the output vector", {
  
  y = c(1:50)
  
  output = func_shuffle(y, times = 10)
  
  testthat::expect_true(length(y) == length(output))
})


# classification folds

testthat::test_that("throws an error if the RESP is not a factor", {
  
  y = c(1:10)
  
  testthat::expect_error(class_folds(5, y), "RESP must be a factor")
})


testthat::test_that("the number of folds equals the number of the resulted sublist indices", {
  
  y = as.factor(sample(1:5, 100, replace = T))
  
  testthat::expect_length(class_folds(5, y), 5)
})

# regression folds

testthat::test_that("throws an error if the RESP is not a factor", {
  
  y = as.factor(c(1:50))
  
  testthat::expect_error(regr_folds(5, y), "this function is meant for regression for classification use the 'class_folds' function")
})


testthat::test_that("the number of folds equals the number of the resulted sublist indices", {
  
  y = sample(1:5, 100, replace = T)
  
  testthat::expect_length(regr_folds(5, y), 5)
})


# KernelKnnCV function

testthat::test_that("it returns an error if y is NULL", {
  
  testthat::expect_error( KernelKnnCV(X, y = NULL, k = 5, folds = 5, h = 1.0, method = 'euclidean', weights_function = NULL, regression = T) )
})

testthat::test_that("it returns an error if y is not numeric", {
  
  testthat::expect_error(  KernelKnnCV(X, y = list(y), k = 5, folds = 5, h = 1.0, method = 'euclidean', weights_function = NULL, regression = T) )
})


testthat::test_that("it returns an error if missing values are present in the data", {
  
  tmp_dat = X
  tmp_dat$crim[sample(1:length(tmp_dat$crim), 10)] = NA
  
  testthat::expect_error( KernelKnnCV(tmp_dat, y = y, k = 5, folds = 5, h = 1.0, method = 'euclidean', weights_function = NULL, regression = T) )
})


testthat::test_that("it returns an error if missing values are present in the response variable", {
  
  tmp_dat = y
  tmp_dat[sample(1:length(tmp_dat), 10)] = NA
  
  testthat::expect_error( KernelKnnCV(X, y = tmp_dat, k = 5, folds = 5, h = 1.0, method = 'euclidean', weights_function = NULL, regression = T) )
})


testthat::test_that("it returns an error if the length of y is not equal to the number of rows of the train data", {
  
  testthat::expect_error( KernelKnnCV(X, y[1:(length(y)-10)], k = 5, folds = 5, h = 1.0, method = 'euclidean', weights_function = NULL, regression = T) )
})


testthat::test_that("it returns an error if regression = F and there are unique labels less than 1", {
  
  testthat::expect_error( KernelKnnCV(X_class, as.numeric(y1_class) - 1, k = 5, folds = 5, h = 1.0, method = 'euclidean', weights_function = NULL, regression = F) )
})


testthat::test_that("it returns an error if folds < 2", {
  
  testthat::expect_error( KernelKnnCV(X_class, as.numeric(y1_class), k = 5, folds = 1, h = 1.0, method = 'euclidean', weights_function = NULL, regression = F, Levels = unique(y1_class)) )
})


testthat::test_that("it returns an error if regression is not TRUE or FALSE", {
  
  testthat::expect_error( KernelKnnCV(X_class, as.numeric(y1_class), k = 5, folds = 3, h = 1.0, method = 'euclidean', weights_function = NULL, regression = 'F', Levels = unique(y1_class)) )
})


testthat::test_that("it returns an error if each fold has less than 5 observations", {
  
  testthat::expect_error( KernelKnnCV(X, y = y, k = 5, folds = 100, h = 1.0, method = 'euclidean', weights_function = NULL, regression = T) )
})


# test KernelKnnCV functionality

testthat::test_that("it returns a list of length 2 where the length of the unlisted sublists equal the length of the train data, if REGRESSION = TRUE", {
  
  res =  KernelKnnCV(X, y, k = 5, folds = 3, method = 'euclidean', weights_function = NULL, regression = T, Levels = NULL)
  
  lap_pr = sum(unlist(lapply(res$preds, length)))
  
  lap_idx = sum(unlist(lapply(res$folds, length)))
  
  testthat::expect_true( lap_pr == nrow(X) && lap_idx == nrow(X) )
})


testthat::test_that("it returns a list of length 2 where the length of the unlisted sublists equal the length of the train data, if REGRESSION = FALSE", {
  
  res =  KernelKnnCV(X_class, as.numeric(y1_class), k = 5, folds = 3, method = 'euclidean', weights_function = NULL, regression = F, Levels = unique(y1_class))
  
  lap_pr = sum(unlist(lapply(res$preds, nrow)))
  
  lap_idx = sum(unlist(lapply(res$folds, length)))
  
  testthat::expect_true( lap_pr == nrow(X_class) && lap_idx == nrow(X_class) )
})

