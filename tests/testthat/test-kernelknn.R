#==============================================================================================================================================================

context("Kernel knn")


#=================
# Error handling
#=================


testthat::test_that("it returns an error if a factor variable is present in the data and the transf_categ_cols = FALSE", {

  tmp_dat = xtr

  tmp_dat$rad = as.factor(tmp_dat$rad)

  testthat::expect_error(KernelKnn(tmp_dat, TEST_data = xte, y1, k = 5, 1.0, method = 'euclidean', weights_function = NULL, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL))
})

testthat::test_that("it returns an error if a character variable is present in the data and the transf_categ_cols = FALSE", {

  tmp_dat = xtr

  tmp_dat$rad = as.character(tmp_dat$rad)

  testthat::expect_error(KernelKnn(tmp_dat, TEST_data = xte, y1, k = 5, 1.0, method = 'euclidean', weights_function = NULL, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL))
})


testthat::test_that("'simple_matching_coefficient', 'jaccard_coefficient' and 'Rao_coefficient' can work only with binary data, case TEST_data is NULL", {

  testthat::expect_error(KernelKnn(xtr, TEST_data = NULL, y1, k = 5, 1.0, method = 'simple_matching_coefficient', weights_function = NULL, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL))
})


testthat::test_that("'simple_matching_coefficient', 'jaccard_coefficient' and 'Rao_coefficient' can work only with binary data, case TEST_data is NULL", {

  testthat::expect_error(KernelKnn(xtr, TEST_data = NULL, y1, k = 5, 1.0, method = 'jaccard_coefficient', weights_function = NULL, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL))
})


testthat::test_that("'simple_matching_coefficient', 'jaccard_coefficient' and 'Rao_coefficient' can work only with binary data, case TEST_data is NULL", {

  testthat::expect_error(KernelKnn(xtr, TEST_data = NULL, y1, k = 5, 1.0, method = 'Rao_coefficient', weights_function = NULL, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL))
})


testthat::test_that("'simple_matching_coefficient', 'jaccard_coefficient' and 'Rao_coefficient' can work only with binary data, case TEST_data is not NULL", {

  testthat::expect_error(KernelKnn(xtr, TEST_data = xte, y1, k = 5, 1.0, method = 'simple_matching_coefficient', weights_function = NULL, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL))
})


testthat::test_that("'simple_matching_coefficient', 'jaccard_coefficient' and 'Rao_coefficient' can work only with binary data, case TEST_data is not NULL", {

  testthat::expect_error(KernelKnn(xtr, TEST_data = xte, y1, k = 5, 1.0, method = 'jaccard_coefficient', weights_function = NULL, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL))
})


testthat::test_that("'simple_matching_coefficient', 'jaccard_coefficient' and 'Rao_coefficient' can work only with binary data, case TEST_data is not NULL", {

  testthat::expect_error(KernelKnn(xtr, TEST_data = xte, y1, k = 5, 1.0, method = 'Rao_coefficient', weights_function = NULL, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL))
})


testthat::test_that("it returns an error if k is NULL", {

  testthat::expect_error( KernelKnn(xtr, TEST_data = xte, y1, k = NULL, 1.0, method = 'euclidean', weights_function = NULL, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL) )
})


testthat::test_that("it returns an error if k is a character", {

  testthat::expect_error( KernelKnn(xtr, TEST_data = xte, y1, k = 'invalid', 1.0, method = 'euclidean', weights_function = NULL, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL) )
})


testthat::test_that("it returns an error if k is greater or equal to the number of rows of the train data", {

  testthat::expect_error( KernelKnn(xtr, TEST_data = xte, y1, k = nrow(xtr) , 1.0, method = 'euclidean', weights_function = NULL, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL) )
})


testthat::test_that("it returns an error if k is less than 1", {

  testthat::expect_error( KernelKnn(xtr, TEST_data = xte, y1, k = -1 , 1.0, method = 'euclidean', weights_function = NULL, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL) )
})


testthat::test_that("it returns a warning if k is a float", {

  testthat::expect_warning( KernelKnn(xtr, TEST_data = xte, y1, k = 1.5 , 1.0, method = 'euclidean', weights_function = NULL, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL) )
})


testthat::test_that("it returns an error if h = 0", {

  testthat::expect_error( KernelKnn(xtr, TEST_data = xte, y1, k = 4 , 0.0, method = 'euclidean', weights_function = NULL, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL) )
})

testthat::test_that("it returns an error if the method is NULL", {

  testthat::expect_error( KernelKnn(xtr, TEST_data = xte, y1, k = 5 , 1.0, method = NULL, weights_function = NULL, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL) )
})


testthat::test_that("it returns an error if the method is not a character", {

  testthat::expect_error( KernelKnn(xtr, TEST_data = xte, y1, k = 5 , 1.0, method = 1, weights_function = NULL, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL) )
})


testthat::test_that("it returns an error if the method is a character, but not one of the valid names", {

  testthat::expect_error( KernelKnn(xtr, TEST_data = xte, y1, k = 5 , 1.0, method = 'invalid', weights_function = NULL, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL) )
})


testthat::test_that("it returns an error if y is NULL", {

  testthat::expect_error( KernelKnn(xtr, TEST_data = xte, NULL, k = 5 , 1.0, method = 'euclidean', weights_function = NULL, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL) )
})


testthat::test_that("it returns an error if y is not numeric", {

  testthat::expect_error( KernelKnn(xtr, TEST_data = xte, list(y1), k = 5 , 1.0, method = 'euclidean', weights_function = NULL, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL) )
})


testthat::test_that("it returns an error if regression = F and the Levels = NULL", {

  testthat::expect_error( KernelKnn(xtr, TEST_data = xte, as.numeric(y1_class), k = 5 , 1.0, method = 'euclidean', weights_function = NULL, regression = F, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL) )
})


testthat::test_that("it returns an error if regression = F and there are unique labels less than 1", {

  testthat::expect_error( KernelKnn(xtr, TEST_data = xte, as.numeric(y1_class) - 1, k = 5 , 1.0, method = 'euclidean', weights_function = NULL, regression = F, transf_categ_cols = F, threads = 1, extrema = F, Levels = unique(as.numeric(y1_class))) )
})


testthat::test_that("it returns an error if missing values are present in the data", {

  tmp_dat = xtr
  tmp_dat$crim[sample(1:length(tmp_dat$crim), 10)] = NA

  testthat::expect_error( KernelKnn(tmp_dat, TEST_data = xte, y1, k = 5 , 1.0, method = 'euclidean', weights_function = NULL, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL) )
})


testthat::test_that("it returns an error if missing values are present in the response variable", {

  tmp_dat = y1
  tmp_dat[sample(1:length(tmp_dat), 10)] = NA

  testthat::expect_error( KernelKnn(xtr, TEST_data = xte, tmp_dat, k = 5 , 1.0, method = 'euclidean', weights_function = NULL, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL) )
})


testthat::test_that("it returns an error if missing values are present in the TEST data", {

  tmp_dat = xte
  tmp_dat$crim[sample(1:length(tmp_dat$crim), 10)] = NA

  testthat::expect_error( KernelKnn(xtr, TEST_data = tmp_dat, y1, k = 5 , 1.0, method = 'euclidean', weights_function = NULL, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL) )
})


testthat::test_that("it returns an error if the length of y is not equal to the number of rows of the train data", {

  testthat::expect_error( KernelKnn(xtr, TEST_data = xte, y1[1:(length(y1)-10)], k = 5 , 1.0, method = 'euclidean', weights_function = NULL, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL) )
})


testthat::test_that("it returns an error if k is less than 3 and extrema = TRUE", {

  testthat::expect_error( KernelKnn(xtr, TEST_data = xte, y1, k = 3 , 1.0, method = 'euclidean', weights_function = NULL, regression = T, transf_categ_cols = F, threads = 1, extrema = T, Levels = NULL) )
})


testthat::test_that("if the number of columns in train and test data differ it returns an error", {

  tmp_xte = xte[, -ncol(xte)]

  testthat::expect_error( KernelKnn(xtr, TEST_data = tmp_xte, y1, k = 4 , 1.0, method = 'euclidean', weights_function = NULL, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL) )
})


testthat::test_that("it returns error if the weights function is invalid for regression = T, if TEST_data is NULL", {

  testthat::expect_error(KernelKnn(xtr, TEST_data = NULL, y1, k = 5 , h = 1.0, method = 'euclidean', weights_function = list(), regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL))
})


testthat::test_that("it returns error if the weights function is invalid for regression = F, if TEST_data is NULL", {

  testthat::expect_error(KernelKnn(xtr_class, TEST_data = NULL, as.numeric(y1_class), k = 5 , h = 1.0, method = 'euclidean', weights_function = matrix(,0,0), regression = F, transf_categ_cols = F, threads = 1, extrema = F, Levels = unique(y1_class)))
})


testthat::test_that("it returns error if the weights function is invalid for regression = F, TEST_data is NOT NULL", {

  testthat::expect_error(KernelKnn(xtr_class, TEST_data = xte_class, as.numeric(y1_class), k = 5 , h = 1.0, method = 'euclidean', weights_function = data.frame(matrix(,0,0)), regression = F, transf_categ_cols = F, threads = 1, extrema = F, Levels = unique(y1_class)))
})


testthat::test_that("it returns error if the weights function is invalid for regression = T, TEST_data is NOT NULL", {

  testthat::expect_error(KernelKnn(xtr, TEST_data = xte, y1, k = 5 , h = 1.0, method = 'euclidean', weights_function = as.factor(1:10), regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL))
})


testthat::test_that("it returns a warning if the input matrix seems singular in case of the 'mahalanobis' distance", {

  tmp_x = singular_mat[, -ncol(singular_mat)]

  tmp_y = as.numeric(singular_mat[, ncol(singular_mat)])

  testthat::expect_warning(KernelKnn(tmp_x, TEST_data = NULL, tmp_y, k = 5 , h = 1.0, method = 'mahalanobis', weights_function = NULL, regression = F, transf_categ_cols = F, threads = 1, extrema = F, Levels = unique(tmp_y)))
})


# testing of KernelKnn


testthat::test_that("if transf_categ_cols = TRUE and TEST_data = NULL the KernelKnn returns output equal to the number of rows in the train data", {

  tmp_xtr = xtr
  tmp_xtr$rad = as.factor(tmp_xtr$rad)

  res = KernelKnn(tmp_xtr, TEST_data = NULL, y1, k = 5 , 1.0, method = 'euclidean', weights_function = NULL, regression = T, transf_categ_cols = T, threads = 1, extrema = F, Levels = NULL)

  testthat::expect_true(length(res) == nrow(xtr))
})


testthat::test_that("if transf_categ_cols = TRUE and TEST_data is NOT NULL the KernelKnn returns output equal to the number of rows in the TEST data", {

  tmp_xtr = xtr
  tmp_xtr$rad = as.factor(tmp_xtr$rad)

  tmp_xte = xte
  tmp_xte$rad = as.factor(tmp_xte$rad)

  res = KernelKnn(tmp_xtr, TEST_data = tmp_xte, y1, k = 5 , 1.0, method = 'euclidean', weights_function = NULL, regression = T, transf_categ_cols = T, threads = 1, extrema = F, Levels = NULL)

  testthat::expect_true(length(res) == nrow(tmp_xte))
})



testthat::test_that("using either a kernel or a user-defined-kernel-function (here in both cases a 'uniform' kernel) returns the same result, when both train and test sets are used", {

  res_kernel = KernelKnn(xtr, TEST_data = xte, y1, k = 5 , 1.0, method = 'euclidean', weights_function = 'uniform', regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL)

  uniform = function(W) {

    W = (1/2) * abs(W)

    W = W / rowSums(W)

    return(W)
  }

  res_kernel_function = KernelKnn(xtr, TEST_data = xte, y1, k = 5 , 1.0, method = 'euclidean', weights_function = uniform, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL)

  df = data.frame(first_case = res_kernel, sec_case = res_kernel_function)

  df$difference_of_results = round(df$first_case - df$sec_case, 3)          # difference of results

  testthat::expect_true(sum(df$difference_of_results) == 0.0)
})


testthat::test_that("using either a kernel or a user-defined-kernel-function (here in both cases a 'uniform' kernel) returns the same result, when ONLY train data is used", {

  res_kernel = KernelKnn(xtr, TEST_data = NULL, y1, k = 5 , 1.0, method = 'euclidean', weights_function = 'uniform', regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL)

  uniform = function(W) {

    W = (1/2) * abs(W)

    W = W / rowSums(W)

    return(W)
  }

  res_kernel_function = KernelKnn(xtr, TEST_data = NULL, y1, k = 5 , 1.0, method = 'euclidean', weights_function = uniform, regression = T, transf_categ_cols = F, threads = 1, extrema = F, Levels = NULL)

  df = data.frame(first_case = res_kernel, sec_case = res_kernel_function)

  df$difference_of_results = round(df$first_case - df$sec_case, 3)          # difference of results

  testthat::expect_true(sum(df$difference_of_results) == 0.0)
})


testthat::test_that("if the TEST data is NULL for all posible combinations [ WHEN the weights_function is a character, when regression = T, when transf_categ_cols = T] the length of the output

                    matches the number of rows of the input", {

  tmp_xtr = xtr
  tmp_xtr$rad = as.factor(tmp_xtr$rad)

  lst = count = list()

  for (k in 4:6) {

    for (h in c(0.1, 0.5, 1.0)) {

      for (metric in c('euclidean', 'manhattan', 'chebyshev')) {

        for (w_func in c('uniform', 'triangular', 'epanechnikov')) {

          for (extr in c(T,F)) {

            lst = lappend(lst, KernelKnn(tmp_xtr, TEST_data = NULL, y1, k = k , h = h, method = metric, weights_function = w_func, regression = T, transf_categ_cols = T, threads = 1, extrema = extr, Levels = NULL))

            count = lappend(count, 1)
          }
        }
      }
    }
  }

  testthat::expect_true(nrow(do.call(cbind, lst)) == nrow(tmp_xtr) && ncol(do.call(cbind, lst)) == length(unlist(count)))
})


testthat::test_that("if the TEST data is NULL for all posible combinations [ WHEN the weights_function is a function, when regression = T, when transf_categ_cols = T] the length of the output

                    matches the number of rows of the input", {

  tmp_xtr = xtr
  tmp_xtr$rad = as.factor(tmp_xtr$rad)

  epanechnikov = function(W) {

    W = (3/4) * (1 - W ^ 2)

    W = W / rowSums(W)

    return(W)
  }

  lst = count = list()

  for (k in 4:6) {

    for (h in c(0.1, 0.5, 1.0)) {

      for (metric in c('canberra', 'braycurtis', 'minkowski')) {

        for (extr in c(T,F)) {

          lst = lappend(lst, KernelKnn(tmp_xtr, TEST_data = NULL, y1, k = k , h = h, method = metric, weights_function = epanechnikov, regression = T, transf_categ_cols = T, threads = 1, extrema = extr, Levels = NULL))

          count = lappend(count, 1)
        }
      }
    }
  }

  testthat::expect_true(nrow(do.call(cbind, lst)) == nrow(tmp_xtr) && ncol(do.call(cbind, lst)) == length(unlist(count)))
})


testthat::test_that("if the TEST data is NULL for all posible combinations [ WHEN the weights_function is a character, when regression = F, when transf_categ_cols = F] the length of the output

                    matches the number of rows of the input", {

  lst = count = list()

  for (k in 4:6) {

    for (h in c(0.1, 0.5, 1.0)) {

      for (metric in c('euclidean', 'manhattan', 'chebyshev')) {

        for (w_func in c('uniform', 'triangular', 'epanechnikov')) {

          for (extr in c(T,F)) {

            lst = lappend(lst, KernelKnn(xtr_class, TEST_data = NULL, as.numeric(y1_class), k = k , h = h, method = metric, weights_function = w_func, regression = F, transf_categ_cols = F, threads = 1, extrema = extr, Levels = as.numeric(unique(y1_class))))

            count = lappend(count, 1)
          }
        }
      }
    }
  }

  NCOL = mean(unlist(lapply(lst, ncol)))
  NROW = mean(unlist(lapply(lst, nrow)))

  testthat::expect_true(NROW == nrow(xtr_class) && NCOL == length(unique(y1_class)))
})


testthat::test_that("if the TEST data is NULL for all posible combinations [ WHEN the weights_function is a function, when regression = F, when transf_categ_cols = F] the length of the output

                    matches the number of rows of the input", {

  logistic = function(W) {

    W = (1/(exp(W) + 2 + exp(-W)))

    W = W / rowSums(W)

    return(W)
  }

  lst = count = list()

  for (k in 4:6) {

    for (h in c(0.1, 0.5, 1.0)) {

      for (metric in c('canberra', 'braycurtis', 'minkowski')) {

        for (extr in c(T,F)) {

          lst = lappend(lst, KernelKnn(xtr_class, TEST_data = NULL, as.numeric(y1_class), k = k , h = h, method = metric, weights_function = logistic, regression = F, transf_categ_cols = F, threads = 1, extrema = extr, Levels = as.numeric(unique(y1_class))))

          count = lappend(count, 1)
        }
      }
    }
  }

  NCOL = mean(unlist(lapply(lst, ncol)))
  NROW = mean(unlist(lapply(lst, nrow)))

  testthat::expect_true(NROW == nrow(xtr_class) && NCOL == length(unique(y1_class)))
})



testthat::test_that("if the TEST data is NOT NULL for all posible combinations [ WHEN the weights_function is a character, when regression = T, when transf_categ_cols = T] the length of the output

                    matches the number of rows of the input", {

  tmp_xtr = xtr
  tmp_xtr$rad = as.factor(tmp_xtr$rad)

  tmp_xte = xte
  tmp_xte$rad = as.factor(tmp_xte$rad)

  lst = count = list()

  for (k in 4:6) {

    for (h in c(0.1, 0.5, 1.0)) {

      for (metric in c('canberra', 'braycurtis', 'minkowski')) {

        for (w_func in c('uniform', 'triangular', 'epanechnikov')) {

          for (extr in c(T,F)) {

            lst = lappend(lst, KernelKnn(tmp_xtr, TEST_data = tmp_xte, y1, k = k , h = h, method = metric, weights_function = w_func, regression = T, transf_categ_cols = T, threads = 1, extrema = extr, Levels = NULL))

            count = lappend(count, 1)
          }
        }
      }
    }
  }

  testthat::expect_true(nrow(do.call(cbind, lst)) == nrow(tmp_xte) && ncol(do.call(cbind, lst)) == length(unlist(count)))
})


testthat::test_that("if the TEST data is NOT NULL for all posible combinations [ WHEN the weights_function is a function, when regression = T, when transf_categ_cols = T] the length of the output

                    matches the number of rows of the input", {

  tmp_xtr = xtr
  tmp_xtr$rad = as.factor(tmp_xtr$rad)

  tmp_xte = xte
  tmp_xte$rad = as.factor(tmp_xte$rad)

  epanechnikov = function(W) {

    W = (3/4) * (1 - W ^ 2)

    W = W / rowSums(W)

    return(W)
  }

  lst = count = list()

  for (k in 4:6) {

    for (h in c(0.1, 0.5, 1.0)) {

      for (metric in c('canberra', 'hamming', 'mahalanobis')) {

        for (extr in c(T,F)) {

          lst = lappend(lst, KernelKnn(tmp_xtr, TEST_data = tmp_xte, y1, k = k , h = h, method = metric, weights_function = epanechnikov, regression = T, transf_categ_cols = T, threads = 1, extrema = extr, Levels = NULL))

          count = lappend(count, 1)
        }
      }
    }
  }

  testthat::expect_true(nrow(do.call(cbind, lst)) == nrow(tmp_xte) && ncol(do.call(cbind, lst)) == length(unlist(count)))
})


testthat::test_that("if the TEST data is NOT NULL for all posible combinations [ WHEN the weights_function is a character, when regression = F, when transf_categ_cols = F] the length of the output

                    matches the number of rows of the input", {

  lst = count = list()

  for (k in 4:6) {

    for (h in c(0.1, 0.5, 1.0)) {

      for (metric in c('euclidean', 'manhattan', 'chebyshev')) {

        for (w_func in c('uniform', 'triangular', 'epanechnikov')) {

          for (extr in c(T,F)) {

            lst = lappend(lst, KernelKnn(xtr_class, TEST_data = xte_class, as.numeric(y1_class), k = k , h = h, method = metric, weights_function = w_func, regression = F, transf_categ_cols = F, threads = 1, extrema = extr, Levels = as.numeric(unique(y1_class))))

            count = lappend(count, 1)
          }
        }
      }
    }
  }

  NCOL = mean(unlist(lapply(lst, ncol)))
  NROW = mean(unlist(lapply(lst, nrow)))

  testthat::expect_true(NROW == nrow(xte_class) && NCOL == length(unique(y1_class)))
})


testthat::test_that("if the TEST data is NOT NULL for all posible combinations [ WHEN the weights_function is a function, when regression = F, when transf_categ_cols = F] the length of the output

                    matches the number of rows of the input", {

  logistic = function(W) {

    W = (1/(exp(W) + 2 + exp(-W)))

    W = W / rowSums(W)

    return(W)
  }

  lst = count = list()

  for (k in 4:6) {

    for (h in c(0.1, 0.5, 1.0)) {

      for (metric in c('canberra', 'braycurtis', 'minkowski')) {

        for (extr in c(T,F)) {

          lst = lappend(lst, KernelKnn(xtr_class, TEST_data = xte_class, as.numeric(y1_class), k = k , h = h, method = metric, weights_function = logistic, regression = F, transf_categ_cols = F, threads = 1, extrema = extr, Levels = as.numeric(unique(y1_class))))

          count = lappend(count, 1)
        }
      }
    }
  }

  NCOL = mean(unlist(lapply(lst, ncol)))
  NROW = mean(unlist(lapply(lst, nrow)))

  testthat::expect_true(NROW == nrow(xte_class) && NCOL == length(unique(y1_class)))
})


testthat::test_that("if the TEST data is NULL for all posible combinations [ WHEN the weights_function is NULL, when regression = T, when transf_categ_cols = T] the length of the output

                    matches the number of rows of the input", {

  tmp_xtr = xtr
  tmp_xtr$rad = as.factor(tmp_xtr$rad)

  lst = count = list()

  for (k in 4:6) {

    for (h in c(0.1, 0.5, 1.0)) {

      for (metric in c('euclidean', 'manhattan', 'chebyshev')) {

        for (extr in c(T,F)) {

          lst = lappend(lst, KernelKnn(tmp_xtr, TEST_data = NULL, y1, k = k , h = h, method = metric, weights_function = NULL, regression = T, transf_categ_cols = T, threads = 1, extrema = extr, Levels = NULL))

          count = lappend(count, 1)
        }
      }
    }
  }

  testthat::expect_true(nrow(do.call(cbind, lst)) == nrow(tmp_xtr) && ncol(do.call(cbind, lst)) == length(unlist(count)))
})



testthat::test_that("if the TEST data is NULL for all posible combinations [ WHEN the weights_function is NULL, when regression = F, when transf_categ_cols = F] the length of the output

                    matches the number of rows of the input", {

  lst = count = list()

  for (k in 4:6) {

    for (h in c(0.1, 0.5, 1.0)) {

      for (metric in c('euclidean', 'manhattan', 'chebyshev')) {

        for (extr in c(T,F)) {

          lst = lappend(lst, KernelKnn(xtr_class, TEST_data = NULL, as.numeric(y1_class), k = k , h = h, method = metric, weights_function = NULL, regression = F, transf_categ_cols = F, threads = 1, extrema = extr, Levels = as.numeric(unique(y1_class))))

          count = lappend(count, 1)
        }
      }
    }
  }

  NCOL = mean(unlist(lapply(lst, ncol)))
  NROW = mean(unlist(lapply(lst, nrow)))

  testthat::expect_true(NROW == nrow(xtr_class) && NCOL == length(unique(y1_class)))
})



testthat::test_that("if the TEST data is NULL for all posible combinations [ WHEN the weights_function is NULL, when regression = T, when transf_categ_cols = T] the length of the output

                    matches the number of rows of the input", {

  tmp_xtr = xtr
  tmp_xtr$rad = as.factor(tmp_xtr$rad)

  tmp_xte = xte
  tmp_xte$rad = as.factor(tmp_xte$rad)

  lst = count = list()

  for (k in 4:6) {

    for (h in c(0.1, 0.5, 1.0)) {

      for (metric in c('euclidean', 'manhattan', 'chebyshev')) {

        for (extr in c(T,F)) {

          lst = lappend(lst, KernelKnn(tmp_xtr, TEST_data = tmp_xte, y1, k = k , h = h, method = metric, weights_function = NULL, regression = T, transf_categ_cols = T, threads = 1, extrema = extr, Levels = NULL))

          count = lappend(count, 1)
        }
      }
    }
  }

  testthat::expect_true(nrow(do.call(cbind, lst)) == nrow(tmp_xte) && ncol(do.call(cbind, lst)) == length(unlist(count)))
})



testthat::test_that("if the TEST data is NULL for all posible combinations [ WHEN the weights_function is NULL, when regression = F, when transf_categ_cols = F] the length of the output

                    matches the number of rows of the input", {

  lst = count = list()

  for (k in 4:6) {

    for (h in c(0.1, 0.5, 1.0)) {

      for (metric in c('euclidean', 'manhattan', 'chebyshev')) {

        for (extr in c(T,F)) {

          lst = lappend(lst, KernelKnn(xtr_class, TEST_data = xte_class, as.numeric(y1_class), k = k , h = h, method = metric, weights_function = NULL, regression = F, transf_categ_cols = F, threads = 1, extrema = extr, Levels = as.numeric(unique(y1_class))))

          count = lappend(count, 1)
        }
      }
    }
  }

  NCOL = mean(unlist(lapply(lst, ncol)))
  NROW = mean(unlist(lapply(lst, nrow)))

  testthat::expect_true(NROW == nrow(xte_class) && NCOL == length(unique(y1_class)))
})




testthat::test_that("the similarity measures 'simple_matching_coefficient', 'jaccard_coefficient', 'Rao_coefficient' and 'pearson_correlation' return correct output

                    in case of binary data", {


  dat = do.call(cbind, lapply(1:10, function(x) sample(0:1, 100, replace = T)))
  TES = do.call(cbind, lapply(1:10, function(x) sample(0:1, 50, replace = T)))
  y = sample(1:2, 100, replace = T)

  lst = count = list()

  for (k in 4:6) {

    for (h in c(0.1, 0.5, 1.0)) {

      for (metric in c('simple_matching_coefficient', 'jaccard_coefficient', 'Rao_coefficient', 'pearson_correlation')) {

        for (extr in c(T,F)) {

          lst = lappend(lst, KernelKnn(dat, TEST_data = TES, y, k = k , h = h, method = metric, weights_function = NULL, regression = F, transf_categ_cols = F, threads = 1, extrema = extr, Levels = unique(y)))

          count = lappend(count, 1)
        }
      }
    }
  }

  NCOL = mean(unlist(lapply(lst, ncol)))
  NROW = mean(unlist(lapply(lst, nrow)))

  testthat::expect_true(NROW == nrow(TES) && NCOL == length(unique(y)))
})



testthat::test_that("the 'p' parameter when method is 'minkowski' returns the expected output", {

  k = 2
  res_wo = KernelKnn(data = X, y = y, k = k , h = 1.0, method = 'minkowski', regression = T, threads = 1)            # without specifying the 'p' parameter
  res_w = KernelKnn(data = X, y = y, k = k , h = 1.0, method = 'minkowski', regression = T, threads = 1, p = k)      # by specifying the 'p' parameter
  res_dif = KernelKnn(data = X, y = y, k = k , h = 1.0, method = 'minkowski', regression = T, threads = 1, p = 1)    # 'p' is set to 1

  is_identical = identical(res_wo, res_w)
  is_not_identical = identical(res_wo, res_dif)

  testthat::expect_true(is_identical & (!is_not_identical))
})


#==============================================================================================================================================================
