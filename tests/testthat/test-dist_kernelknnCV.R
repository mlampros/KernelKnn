

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


testthat::test_that("both the 'distMat.KernelKnnCV()' and 'KernelKnnCV()' return the same results", {
  
  lst_out = list()
  
  count = 1
  for (k_iter in 3:6) {
    
    out = KernelKnnCV(X, y, k = k_iter, folds = 5, h = 1, method = "euclidean", weights_function = NULL, regression = TRUE, transf_categ_cols = F,
                      threads = 1, extrema = F, Levels = NULL, seed_num = 1)
  
    out_dist = distMat.KernelKnnCV(DIST_mat, y, k = k_iter, folds = 5, h = 1,
                                   weights_function = NULL, regression = T, threads = 1, extrema = F,
                                   Levels = NULL, minimize = T, seed_num = 1)
    
    res = all(unlist(lapply(1:length(out$preds), function(x) all(out$preds[[x]] == out_dist$preds[[x]]))))
    res_dist = all(unlist(lapply(1:length(out$folds), function(x) all(out$folds[[x]] == out_dist$folds[[x]]))))
    
    lst_out[[count]] = (res == res_dist)
    count = count + 1
  }

  testthat::expect_true( all(unlist(lst_out)) )
})

