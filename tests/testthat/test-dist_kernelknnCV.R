

context("kernel knn cross-validation with distance matrix")

#=================
# Error handling
#=================


testthat::test_that("it returns an error if y is NULL", {
  
  testthat::expect_error( distMat.KernelKnnCV(DIST_mat, y = NULL, k = 5, folds = 5, h = 1.0, weights_function = NULL, regression = F, threads = 1, extrema = F, Levels = NULL, minimize = T, seed_num = 1) )
})

testthat::test_that("it returns an error if y is not numeric", {
  
  testthat::expect_error(  distMat.KernelKnnCV(DIST_mat, y = list(), k = 5, folds = 5, h = 1.0, weights_function = NULL, regression = F, threads = 1, extrema = F, Levels = NULL, minimize = T, seed_num = 1) )
})

testthat::test_that("it returns an error if the length of y is not equal to the number of rows of the distance matrix", {
  
  testthat::expect_error( distMat.KernelKnnCV(DIST_mat, y[1:100], k = 5, folds = 5, h = 1.0, weights_function = NULL, regression = F, threads = 1, extrema = F, Levels = NULL, minimize = T, seed_num = 1) )
})

testthat::test_that("it returns an error if missing values are present in the distance matrix", {
  
  tmp_dat = DIST_mat
  tmp_dat[2,1] = NA
  
  testthat::expect_error( distMat.KernelKnnCV(tmp_dat, y, k = 5, folds = 5, h = 1.0, weights_function = NULL, regression = F, threads = 1, extrema = F, Levels = NULL, minimize = T, seed_num = 1) )
})

testthat::test_that("it returns an error if missing values are present in the response variable", {

  tmp_dat = y
  tmp_dat[1] = NA
  
  testthat::expect_error( distMat.KernelKnnCV(DIST_mat, tmp_dat, k = 5, folds = 5, h = 1.0, weights_function = NULL, regression = F, threads = 1, extrema = F, Levels = NULL, minimize = T, seed_num = 1) )
})


testthat::test_that("it returns an error if folds < 2", {
  
  testthat::expect_error( distMat.KernelKnnCV(DIST_mat_class, as.numeric(y1_class_ext), k = 5, folds = 1, h = 1.0, weights_function = NULL, regression = F, threads = 1, extrema = F, Levels = unique(y1_class_ext), minimize = T, seed_num = 1) )
})


testthat::test_that("it returns an error if regression is not TRUE or FALSE", {
  
  testthat::expect_error( distMat.KernelKnnCV(DIST_mat_class, as.numeric(y1_class_ext), k = 5, folds = 5, h = 1.0, weights_function = NULL, regression = 'F', threads = 1, extrema = F, Levels = unique(y1_class_ext), minimize = T, seed_num = 1) )
})


testthat::test_that("it returns an error if each fold has less than 5 observations", {
  
  testthat::expect_error( distMat.KernelKnnCV(DIST_mat, y, k = 5, folds = 100, h = 1.0, weights_function = NULL, regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T, seed_num = 1) )
})


#===============================
# test KernelKnnCV functionality
#===============================


testthat::test_that("it returns a list of length 2 where the length of the unlisted sublists equal the length of the train data, if REGRESSION = TRUE", {
  
  res = distMat.KernelKnnCV(DIST_mat, y, k = 5, folds = 3, h = 1.0, weights_function = NULL, regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T, seed_num = 1)
  
  lap_pr = sum(unlist(lapply(res$preds, length)))
  
  lap_idx = sum(unlist(lapply(res$folds, length)))
  
  testthat::expect_true( lap_pr == nrow(X) && lap_idx == nrow(X) )
})


testthat::test_that("it returns a list of length 2 where the length of the unlisted sublists equal the length of the train data, if REGRESSION = FALSE", {
  
  res = distMat.KernelKnnCV(DIST_mat_class, as.numeric(y1_class_ext), k = 5, folds = 3, h = 1.0, weights_function = NULL, regression = F, threads = 1, extrema = F, Levels = unique(y1_class_ext), minimize = T, seed_num = 1)
  
  lap_pr = sum(unlist(lapply(res$preds, nrow)))
  
  lap_idx = sum(unlist(lapply(res$folds, length)))
  
  testthat::expect_true( lap_pr == nrow(X_class) && lap_idx == nrow(X_class) )
})


