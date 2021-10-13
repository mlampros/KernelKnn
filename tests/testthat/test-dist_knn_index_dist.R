

context("kernel knn list of distances and indices using a distance matrix")


#=================
# Error handling
#=================


testthat::test_that("it returns an error if the input distance object is not of type matrix", {

  tmp_df = as.data.frame(DIST_mat)

  testthat::expect_error(distMat.knn.index.dist(tmp_df, TEST_indices = NULL, k = 5, threads = 1, minimize = T))
})


testthat::test_that("it returns an error if the input distance matrix is not square", {

  testthat::expect_error(distMat.knn.index.dist(DIST_mat[, -ncol(DIST_mat)], TEST_indices = NULL, k = 5, threads = 1, minimize = T))
})


testthat::test_that("it returns an error if the diagonal of the distance matrix is other than 0's or NA's", {

  TMP_DIAG = DIST_mat

  diag(TMP_DIAG) = -1

  testthat::expect_error(distMat.knn.index.dist(TMP_DIAG, TEST_indices = NULL, k = 5, threads = 1, minimize = T))
})


testthat::test_that("it returns an error if the TEST_indices parameter is not of type numeric or integer", {

  invalid_tst_idx = letters[1:100]

  testthat::expect_error(distMat.knn.index.dist(DIST_mat, TEST_indices = invalid_tst_idx, k = 5, threads = 1, minimize = T))
})


testthat::test_that("it returns an error if the maximum index of the TEST_indices parameter is greater than the rows of the distance matrix", {

  invalid_tst_idx = 1:(nrow(DIST_mat) + 10)

  invalid_tst_idx = (nrow(DIST_mat)-100):length(invalid_tst_idx)

  testthat::expect_error(distMat.knn.index.dist(DIST_mat, TEST_indices = invalid_tst_idx, k = 5, threads = 1, minimize = T))
})

testthat::test_that("it returns an error if k is NULL", {

  testthat::expect_error(distMat.knn.index.dist(DIST_mat, TEST_indices = NULL, k = NULL, threads = 1, minimize = T))
})


testthat::test_that("it returns an error if k is a character", {

  testthat::expect_error( distMat.knn.index.dist(DIST_mat, TEST_indices = NULL, k = "invalid", threads = 1, minimize = T) )
})


testthat::test_that("it returns an error if k is greater or equal to the number of rows of the distance matrix", {

  testthat::expect_error( distMat.knn.index.dist(DIST_mat, TEST_indices = NULL, k = nrow(DIST_mat), threads = 1, minimize = T) )
})


testthat::test_that("it returns an error if k is less than 1", {

  testthat::expect_error(distMat.knn.index.dist(DIST_mat, TEST_indices = NULL, k = -1, threads = 1, minimize = T) )
})


testthat::test_that("it returns a warning if k is a float", {

  testthat::expect_warning( distMat.knn.index.dist(DIST_mat, TEST_indices = NULL, k = 1.5, threads = 1, minimize = T) )
})


testthat::test_that("it returns an error if the minimize parameter is not a boolean", {

  testthat::expect_error( distMat.knn.index.dist(DIST_mat, TEST_indices = NULL, k = 5, threads = 1, minimize = 'T')  )
})



# testing of distMat.knn.index.dist
#----------------------------------


testthat::test_that("it returns the correct output if TEST_indices is NULL", {

  res = distMat.knn.index.dist(DIST_mat, TEST_indices = NULL, k = 5, threads = 1, minimize = T)

  testthat::expect_true( inherits(res, "list") && length(res) == 2 && mean(unlist(lapply(res, nrow))) == nrow(DIST_mat))
})


testthat::test_that("it returns the correct output if TEST_indices is not NULL", {

  idxs = (nrow(DIST_mat) - 100): nrow(DIST_mat)

  res = distMat.knn.index.dist(DIST_mat, TEST_indices = idxs, k = 5, threads = 1, minimize = T)

  testthat::expect_true( inherits(res, "list") && length(res) == 2 && mean(unlist(lapply(res, nrow))) == length(idxs) )
})


testthat::test_that("it returns the correct output if TEST_indices is NULL and the main diagonal is NA's", {

  tmp_dist = DIST_mat

  diag(tmp_dist) = NA

  res = distMat.knn.index.dist(tmp_dist, TEST_indices = NULL, k = 5, threads = 1, minimize = T)

  testthat::expect_true( inherits(res, "list") && length(res) == 2 && mean(unlist(lapply(res, nrow))) == nrow(tmp_dist))
})


