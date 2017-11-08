

context("kernel knn with distance matrix")

#=================
# Error handling
#=================


testthat::test_that("it returns an error if the input distance object is not of type matrix", {
  
  tmp_df = as.data.frame(DIST_mat)
  
  testthat::expect_error(distMat.KernelKnn(tmp_df, TEST_indices = NULL, y, k = 5, h = 1.0, weights_function = NULL, regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T))
})


testthat::test_that("it returns an error if the input distance matrix is not square", {

  testthat::expect_error(distMat.KernelKnn(DIST_mat[, -ncol(DIST_mat)], TEST_indices = NULL, y, k = 5, h = 1.0, weights_function = NULL, regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T))
})


testthat::test_that("it returns an error if the diagonal of the distance matrix is other than 0's or NA's", {
  
  TMP_DIAG = DIST_mat
  
  diag(TMP_DIAG) = -1
  
  testthat::expect_error(distMat.KernelKnn(TMP_DIAG, TEST_indices = NULL, y, k = 5, h = 1.0, weights_function = NULL, regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T))
})


testthat::test_that("it returns an error if the TEST_indices parameter is not of type numeric or integer", {
  
  invalid_tst_idx = letters[1:100]
  
  testthat::expect_error(distMat.KernelKnn(DIST_mat, TEST_indices = invalid_tst_idx, y, k = 5, h = 1.0, weights_function = NULL, regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T))
})


testthat::test_that("it returns an error if the maximum index of the TEST_indices parameter is greater than the rows of the distance matrix", {
  
  invalid_tst_idx = 1:(nrow(DIST_mat) + 10)
  
  invalid_tst_idx = (nrow(DIST_mat)-100):length(invalid_tst_idx)
  
  testthat::expect_error(distMat.KernelKnn(DIST_mat, TEST_indices = invalid_tst_idx, y, k = 5, h = 1.0, weights_function = NULL, regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T))
})

testthat::test_that("it returns an error if k is NULL", {
  
  testthat::expect_error( distMat.KernelKnn(DIST_mat, TEST_indices = NULL, y, k = NULL, h = 1.0, weights_function = NULL, regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T) )
})


testthat::test_that("it returns an error if k is a character", {
  
  testthat::expect_error( distMat.KernelKnn(DIST_mat, TEST_indices = NULL, y, k = "invalid", h = 1.0, weights_function = NULL, regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T) )
})


testthat::test_that("it returns an error if k is greater or equal to the number of rows of the distance matrix", {
  
  testthat::expect_error( distMat.KernelKnn(DIST_mat, TEST_indices = NULL, y, k = nrow(DIST_mat), h = 1.0, weights_function = NULL, regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T) )
})


testthat::test_that("it returns an error if k is less than 1", {
  
  testthat::expect_error( distMat.KernelKnn(DIST_mat, TEST_indices = NULL, y, k = -1, h = 1.0, weights_function = NULL, regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T) )
})


testthat::test_that("it returns a warning if k is a float", {
  
  testthat::expect_warning( distMat.KernelKnn(DIST_mat, TEST_indices = NULL, y, k = 1.5, h = 1.0, weights_function = NULL, regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T) )
})


testthat::test_that("it returns an error if h = 0", {
  
  testthat::expect_error( distMat.KernelKnn(DIST_mat, TEST_indices = NULL, y, k = 5, h = 0.0, weights_function = NULL, regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T) )
})


testthat::test_that("it returns an error if y is NULL", {
  
  testthat::expect_error( distMat.KernelKnn(DIST_mat, TEST_indices = NULL, y = NULL, k = 5, h = 1.0, weights_function = NULL, regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T) )
})


testthat::test_that("it returns an error if y is not numeric", {
  
  testthat::expect_error( distMat.KernelKnn(DIST_mat, TEST_indices = NULL, y = list(y), k = 5, h = 1.0, weights_function = NULL, regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T)  )
})


testthat::test_that("it returns an error if regression = F and the Levels = NULL", {
  
  testthat::expect_error( distMat.KernelKnn(DIST_mat_class, TEST_indices = NULL, as.numeric(y1_class_ext), k = 5, h = 1.0, weights_function = NULL, regression = F, threads = 1, extrema = F, Levels = NULL, minimize = T) )
})


testthat::test_that("it returns an error if regression = F and there are unique labels less than 1", {
  
  testthat::expect_error( distMat.KernelKnn(DIST_mat_class, TEST_indices = NULL, as.numeric(y1_class_ext) - 1, k = 5, h = 1.0, weights_function = NULL, regression = F, threads = 1, extrema = F, Levels = unique(as.numeric(y1_class_ext)), minimize = T) )
})


testthat::test_that("it returns an error if missing values are present in the data", {
  
  tmp_dat = DIST_mat
  tmp_dat[2,1] = NA
  
  testthat::expect_error( distMat.KernelKnn(tmp_dat, TEST_indices = NULL, y, k = 5, h = 1.0, weights_function = NULL, regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T)  )
})


testthat::test_that("it returns an error if missing values are present in the response variable", {
  
  tmp_dat = y
  tmp_dat[1] = NA
  
  testthat::expect_error( distMat.KernelKnn(DIST_mat, TEST_indices = NULL, tmp_dat, k = 5, h = 1.0, weights_function = NULL, regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T) )
})


testthat::test_that("it returns an error if the length of y is not equal to the number of rows of the distance matrix", {
  
  testthat::expect_error( distMat.KernelKnn(DIST_mat, TEST_indices = NULL, y[1:100], k = 5, h = 1.0, weights_function = NULL, regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T) )
})


testthat::test_that("it returns an error if k is less than 3 and extrema = TRUE", {
  
  testthat::expect_error( distMat.KernelKnn(DIST_mat, TEST_indices = NULL, y, k = 3, h = 1.0, weights_function = NULL, regression = T, threads = 1, extrema = T, Levels = NULL, minimize = T) )
})


testthat::test_that("it returns an error if the minimize parameter is not a boolean", {
  
  testthat::expect_error( distMat.KernelKnn(DIST_mat, TEST_indices = NULL, y, k = 5, h = 1.0, weights_function = NULL, regression = T, threads = 1, extrema = F, Levels = NULL, minimize = 'T') )
})


testthat::test_that("it returns error if the weights function is invalid for regression = T, if TEST_indices is NULL", {
  
  testthat::expect_error( distMat.KernelKnn(DIST_mat, TEST_indices = NULL, y, k = 5, h = 1.0, weights_function = list(), regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T) )
})


testthat::test_that("it returns error if the weights function is invalid for regression = F, if TEST_indices is NULL", {
  
  testthat::expect_error( distMat.KernelKnn(DIST_mat, TEST_indices = NULL, y, k = 5, h = 1.0, weights_function = matrix(,0,0), regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T) )
})


testthat::test_that("it returns error if the weights function is invalid for regression = F, TEST_indices is NOT NULL", {
  
  testthat::expect_error(distMat.KernelKnn(DIST_mat_class, TEST_indices = 1:100, as.numeric(y1_class_ext), k = 5, h = 1.0, weights_function = data.frame(matrix(,0,0)), regression = F, threads = 1, extrema = F, Levels = unique(as.numeric(y1_class_ext)), minimize = T))
})


testthat::test_that("it returns error if the weights function is invalid for regression = T, TEST_indices is NOT NULL", {
  
  testthat::expect_error( distMat.KernelKnn(DIST_mat, TEST_indices = 1:100, y, k = 5, h = 1.0, weights_function = as.factor(1:10), regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T) )
})


#=====================
# testing of KernelKnn
#=====================


testthat::test_that("if TEST_indices = NULL, if regression = T the function returns the correct output", {
  
  res = distMat.KernelKnn(DIST_mat, TEST_indices = NULL, y, k = 5, h = 1.0, weights_function = NULL, regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T)

  testthat::expect_true(length(res) == nrow(DIST_mat))
})


testthat::test_that("if TEST_indices = NULL, if regression = T, if extrema = T, the function returns the correct output", {
  
  res = distMat.KernelKnn(DIST_mat, TEST_indices = NULL, y, k = 5, h = 1.0, weights_function = NULL, regression = T, threads = 1, extrema = T, Levels = NULL, minimize = T)
  
  testthat::expect_true(length(res) == nrow(DIST_mat))
})


testthat::test_that("if TEST_indices = NULL, if regression = T, if the diagonal of the distance matrix is filled with NA's the function returns the correct output", {
  
  tmp_dist = DIST_mat
  
  diag(tmp_dist) = NA
  
  res = distMat.KernelKnn(tmp_dist, TEST_indices = NULL, y, k = 5, h = 1.0, weights_function = NULL, regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T)
  
  testthat::expect_true(length(res) == nrow(tmp_dist))
})


testthat::test_that("if TEST_indices is not NULL, if regression = T the function returns the correct output", {
  
  tmp_idx = 1:100
  
  res = distMat.KernelKnn(DIST_mat, TEST_indices = tmp_idx, y, k = 5, h = 1.0, weights_function = NULL, regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T)
  
  testthat::expect_true(length(res) == length(tmp_idx))
})


testthat::test_that("using either a kernel or a user-defined-kernel-function (here in both cases a 'uniform' kernel), IF regression = T, returns the same result", {
  
  res_kernel = distMat.KernelKnn(DIST_mat, TEST_indices = NULL, y, k = 5, h = 1.0, weights_function = 'uniform', regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T)
  
  uniform = function(W) {
    
    W = (1/2) * abs(W)
    
    W = W / rowSums(W)
    
    return(W)
  }
  
  res_kernel_function = distMat.KernelKnn(DIST_mat, TEST_indices = NULL, y, k = 5, h = 1.0, weights_function = uniform, regression = T, threads = 1, extrema = F, Levels = NULL, minimize = T)
  
  df = data.frame(first_case = res_kernel, sec_case = res_kernel_function)
  
  df$difference_of_results = round(df$first_case - df$sec_case, 3)          # difference of results
  
  testthat::expect_true(sum(df$difference_of_results) == 0.0)
})



testthat::test_that("using either a kernel or a user-defined-kernel-function (here in both cases a 'uniform' kernel) returns the same result, IF regression = F, wenn TEST_indices is not NULL", {
  
  res_kernel = distMat.KernelKnn(DIST_mat_class, TEST_indices = 1:100, as.numeric(y1_class_ext), k = 5, h = 1.0, weights_function = 'uniform', regression = F, threads = 1, extrema = F, Levels = unique(as.numeric(y1_class_ext)), minimize = T)
  
  uniform = function(W) {
    
    W = (1/2) * abs(W)
    
    W = W / rowSums(W)
    
    return(W)
  }
  
  res_kernel_function = distMat.KernelKnn(DIST_mat_class, TEST_indices = 1:100, as.numeric(y1_class_ext), k = 5, h = 1.0, weights_function = uniform, regression = F, threads = 1, extrema = F, Levels = unique(as.numeric(y1_class_ext)), minimize = T)
  
  df_col1 = data.frame(first_case = res_kernel[, 1], sec_case = res_kernel_function[, 1])
  
  df_col2 = data.frame(first_case = res_kernel[, 2], sec_case = res_kernel_function[, 2])
  
  df_col1$difference_of_results = round(df_col1$first_case - df_col1$sec_case, 3)          # difference of results in 'col1'
  
  df_col2$difference_of_results = round(df_col2$first_case - df_col2$sec_case, 3)          # difference of results in 'col2'
  
  testthat::expect_true(sum(df_col1$difference_of_results) == 0.0 && sum(df_col2$difference_of_results) == 0.0)
})

