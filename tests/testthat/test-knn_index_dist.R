#==============================================================================================================================================================

context("Knn index distance")

#=================
# Error handling
#=================


testthat::test_that("it returns an error if a factor variable is present in the data and the transf_categ_cols = FALSE", {

  tmp_dat = xtr

  tmp_dat$rad = as.factor(tmp_dat$rad)

  testthat::expect_error(knn.index.dist(tmp_dat, TEST_data = xte, k = 5, method = 'euclidean', transf_categ_cols = F, threads = 1))
})

testthat::test_that("it returns an error if a character variable is present in the data and the transf_categ_cols = FALSE", {

  tmp_dat = xtr

  tmp_dat$rad = as.character(tmp_dat$rad)

  testthat::expect_error(knn.index.dist(tmp_dat, TEST_data = xte, k = 5, method = 'euclidean', transf_categ_cols = F, threads = 1))
})


testthat::test_that("'simple_matching_coefficient', 'jaccard_coefficient' and 'Rao_coefficient' can work only with binary data, case TEST_data is NULL", {

  testthat::expect_error(knn.index.dist(xtr, TEST_data = NULL, k = 5, method = 'simple_matching_coefficient', transf_categ_cols = F, threads = 1))
})


testthat::test_that("'simple_matching_coefficient', 'jaccard_coefficient' and 'Rao_coefficient' can work only with binary data, case TEST_data is NULL", {

  testthat::expect_error(knn.index.dist(xtr, TEST_data = NULL, k = 5, method = 'jaccard_coefficient', transf_categ_cols = F, threads = 1))
})


testthat::test_that("'simple_matching_coefficient', 'jaccard_coefficient' and 'Rao_coefficient' can work only with binary data, case TEST_data is NULL", {

  testthat::expect_error(knn.index.dist(xtr, TEST_data = NULL, k = 5, method = 'Rao_coefficient', transf_categ_cols = F, threads = 1))
})


testthat::test_that("'simple_matching_coefficient', 'jaccard_coefficient' and 'Rao_coefficient' can work only with binary data, case TEST_data is not NULL", {

  testthat::expect_error(knn.index.dist(xtr, TEST_data = xte, k = 5, method = 'simple_matching_coefficient', transf_categ_cols = F, threads = 1))
})


testthat::test_that("'simple_matching_coefficient', 'jaccard_coefficient' and 'Rao_coefficient' can work only with binary data, case TEST_data is not NULL", {

  testthat::expect_error(knn.index.dist(xtr, TEST_data = xte, k = 5, method = 'jaccard_coefficient', transf_categ_cols = F, threads = 1))
})


testthat::test_that("'simple_matching_coefficient', 'jaccard_coefficient' and 'Rao_coefficient' can work only with binary data, case TEST_data is not NULL", {

  testthat::expect_error(knn.index.dist(xtr, TEST_data = xte, k = 5, method = 'Rao_coefficient', transf_categ_cols = F, threads = 1))
})


testthat::test_that("it returns an error if k is NULL", {

  testthat::expect_error( knn.index.dist(xtr, TEST_data = xte, k = NULL, method = 'euclidean', transf_categ_cols = F, threads = 1))
})


testthat::test_that("it returns an error if k is a character", {

  testthat::expect_error( knn.index.dist(xtr, TEST_data = xte, k = 'invalid', method = 'euclidean', transf_categ_cols = F, threads = 1) )
})


testthat::test_that("it returns an error if k is greater or equal to the number of rows of the train data", {

  testthat::expect_error( knn.index.dist(xtr, TEST_data = xte, k = nrow(xtr), method = 'euclidean', transf_categ_cols = F, threads = 1) )
})


testthat::test_that("it returns an error if k is less than 1", {

  testthat::expect_error( knn.index.dist(xtr, TEST_data = xte, k = -1, method = 'euclidean', transf_categ_cols = F, threads = 1) )
})


testthat::test_that("it returns an error if the method is NULL", {

  testthat::expect_error( knn.index.dist(xtr, TEST_data = xte, k = 5, method = NULL, transf_categ_cols = F, threads = 1) )
})


testthat::test_that("it returns an error if the method is not a character", {

  testthat::expect_error( knn.index.dist(xtr, TEST_data = xte, k = 5 , method = 1, transf_categ_cols = F, threads = 1) )
})


testthat::test_that("it returns an error if the method is a character, but not one of the valid names", {

  testthat::expect_error( knn.index.dist(xtr, TEST_data = xte, k = 5 , method = 'invalid', transf_categ_cols = F, threads = 1) )
})


testthat::test_that("it returns an error if missing values are present in the data", {

  tmp_dat = xtr
  tmp_dat$crim[sample(1:length(tmp_dat$crim), 10)] = NA

  testthat::expect_error( knn.index.dist(tmp_dat, TEST_data = xte, k = 5 , method = 'euclidean', transf_categ_cols = F, threads = 1) )
})


testthat::test_that("it returns an error if missing values are present in the TEST data", {

  tmp_dat = xte
  tmp_dat$crim[sample(1:length(tmp_dat$crim), 10)] = NA

  testthat::expect_error( knn.index.dist(xtr, TEST_data = tmp_dat, k = 5 , method = 'euclidean', transf_categ_cols = F, threads = 1) )
})


testthat::test_that("if the number of columns in train and test data differ it returns an error", {

  tmp_xte = xte[, -ncol(xte)]

  testthat::expect_error( knn.index.dist(xtr, TEST_data = tmp_xte, k = 4 , method = 'euclidean', transf_categ_cols = F, threads = 1) )
})


# testing of knn.index.dist


testthat::test_that("if transf_categ_cols = TRUE and TEST_data = NULL the knn.index.dist returns a list with number of rows in each sublist equal to the number of rows in train data", {

  tmp_xtr = xtr
  tmp_xtr$rad = as.factor(tmp_xtr$rad)

  res = knn.index.dist(tmp_xtr, TEST_data = NULL, k = 5 , method = 'euclidean', transf_categ_cols = T, threads = 1)

  testthat::expect_true(length(res) == 2 && mean(unlist(lapply(res, nrow))) == nrow(xtr))
})


testthat::test_that("if transf_categ_cols = TRUE and TEST_data is NOT NULL the knn.index.dist returns a list with number of rows in each sublsit equal to the number of rows in the TEST data", {

  tmp_xtr = xtr
  tmp_xtr$rad = as.factor(tmp_xtr$rad)

  tmp_xte = xte
  tmp_xte$rad = as.factor(tmp_xte$rad)

  res = knn.index.dist(tmp_xtr, TEST_data = tmp_xte, k = 5 , method = 'euclidean', transf_categ_cols = T, threads = 1)

  testthat::expect_true(length(res) == 2 && mean(unlist(lapply(res, nrow))) == nrow(xte))
})



testthat::test_that("if the TEST data is NULL for all posible combinations [ when transf_categ_cols = T ] the knn.index.dist returns a list of length 2 with

                    number of rows in each sublist equal to the number of rows in the train data", {

  tmp_xtr = xtr
  tmp_xtr$rad = as.factor(tmp_xtr$rad)

  lst = list()

  for (k in 4:6) {

    for (metric in c('euclidean', 'manhattan', 'chebyshev')) {

      tmp_res = knn.index.dist(tmp_xtr, TEST_data = NULL, k = k , method = metric, transf_categ_cols = T, threads = 1)

      lst = lappend(lst, length(tmp_res) == 2 && mean(unlist(lapply(tmp_res, nrow))) == nrow(xtr))
    }
  }

  testthat::expect_true(all(unlist(lst)))
})



testthat::test_that("if the TEST data is NULL for all posible combinations [ when transf_categ_cols = F ] the knn.index.dist returns a list of length 2 with

                    number of rows in each sublist equal to the number of rows in the train data", {

  lst = list()

  for (k in 4:6) {

    for (metric in c('euclidean', 'manhattan', 'chebyshev')) {

      tmp_res = knn.index.dist(xtr, TEST_data = NULL, k = k , method = metric, transf_categ_cols = F, threads = 1)

      lst = lappend(lst, length(tmp_res) == 2 && mean(unlist(lapply(tmp_res, nrow))) == nrow(xtr))
    }
  }

  testthat::expect_true(all(unlist(lst)))
})



testthat::test_that("if the TEST data is NOT NULL for all posible combinations [ when transf_categ_cols = T ] the knn.index.dist returns a list of length 2 with

                    number of rows in each sublist equal to the number of rows in the TEST data", {

  tmp_xtr = xtr
  tmp_xtr$rad = as.factor(tmp_xtr$rad)

  tmp_xte = xte
  tmp_xte$rad = as.factor(tmp_xte$rad)

  lst = list()

  for (k in 4:6) {

    for (metric in c('euclidean', 'manhattan', 'chebyshev')) {

      tmp_res = knn.index.dist(tmp_xtr, TEST_data = tmp_xte, k = k , method = metric, transf_categ_cols = T, threads = 1)

      lst = lappend(lst, length(tmp_res) == 2 && mean(unlist(lapply(tmp_res, nrow))) == nrow(tmp_xte))
    }
  }

  testthat::expect_true(all(unlist(lst)))
})



testthat::test_that("if the TEST data is NULL for all posible combinations [ when transf_categ_cols = F ] the knn.index.dist returns a list of length 2 with

                    number of rows in each sublist equal to the number of rows in the train data", {

  lst = list()

  for (k in 4:6) {

    for (metric in c('euclidean', 'manhattan', 'chebyshev')) {

      tmp_res = knn.index.dist(xtr, TEST_data = xte, k = k , method = metric, transf_categ_cols = F, threads = 1)

      lst = lappend(lst, length(tmp_res) == 2 && mean(unlist(lapply(tmp_res, nrow))) == nrow(xte))
    }
  }

  testthat::expect_true(all(unlist(lst)))
})



testthat::test_that("the similarity measures 'simple_matching_coefficient', 'jaccard_coefficient', 'Rao_coefficient' and 'pearson_correlation' return correct output

                    in case of binary data when TEST data is not NULL", {


  dat = do.call(cbind, lapply(1:10, function(x) sample(0:1, 100, replace = T)))
  TES = do.call(cbind, lapply(1:10, function(x) sample(0:1, 50, replace = T)))

  lst = count = list()

  for (k in 4:6) {

    for (metric in c('simple_matching_coefficient', 'jaccard_coefficient', 'Rao_coefficient', 'pearson_correlation')) {

      tmp_lst = knn.index.dist(dat, TEST_data = TES, k = k , method = metric, transf_categ_cols = F, threads = 1)

      lst = lappend(lst, tmp_lst)

      count = lappend(count, ncol(tmp_lst$test_knn_idx) == k && ncol(tmp_lst$test_knn_dist) == k)
    }
  }

  res = unlist(lapply(lst, function(x) nrow(x$test_knn_idx) == nrow(TES) && nrow(x$test_knn_dist) == nrow(TES)))

  testthat::expect_true(all(res) && all(unlist(count)))
})


testthat::test_that("the 'p' parameter when method is 'minkowski' returns the expected output", {

  k = 2
  res_wo = knn.index.dist(data = X, k = k , method = 'minkowski')            # without specifying the 'p' parameter
  res_w = knn.index.dist(data = X, k = k , method = 'minkowski', p = k)      # by specifying the 'p' parameter
  res_dif = knn.index.dist(data = X, k = k , method = 'minkowski', p = 1)    # 'p' is set to 1

  is_identical = identical(res_wo, res_w)
  is_not_identical = identical(res_wo, res_dif)

  testthat::expect_true(is_identical & (!is_not_identical))
})


#==============================================================================================================================================================
