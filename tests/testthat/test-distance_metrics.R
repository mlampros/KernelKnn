#==============================================================================================================================================================

context('Distance metrics rcpp')

# TRAIN data

testthat::test_that("in case that the TEST data is an empty matrix (is.null) : the result for each metric is a list, the length of the output is 2 [meaning 2 sublists] and the nrows in total for the two

                    sublists [ indices and distances ] are 2 times the TRAIN data", {

  test_dat = matrix(, nrow = 0, ncol = 0)

  lap_regr = lapply(c('euclidean', 'manhattan', 'chebyshev', 'canberra', 'braycurtis', 'pearson_correlation', 'simple_matching_coefficient', 
                      'minkowski', 'hamming', 'mahalanobis', 'jaccard_coefficient', 'Rao_coefficient'), function(x)  knn_index_dist_rcpp(as.matrix(xtr), test_dat, 5, x, 1, eps = 1.0e-6))

  lap_class = lapply(c('euclidean', 'manhattan', 'chebyshev', 'canberra', 'braycurtis', 'pearson_correlation', 'simple_matching_coefficient', 
                       'minkowski', 'hamming', 'mahalanobis', 'jaccard_coefficient', 'Rao_coefficient'), function(x)  knn_index_dist_rcpp(as.matrix(xtr_class), test_dat, 5, x, 1, eps = 1.0e-6))

  out1 = sum(unlist(lapply(lap_regr, is.list))) == length(lap_regr) && sum(unlist(lapply(lap_class, is.list))) == length(lap_class)         # sublists are lists

  out2 = mean(unlist(lapply(lap_regr, length))) == 2 && mean(unlist(lapply(lap_class, length))) == 2                                        # length of sublists are 2

  out3 = mean(unlist(lapply(lap_regr, function(x) nrow(x$train_knn_dist) + nrow(x$train_knn_idx)))) == nrow(xtr) * 2 &&

    unlist(lapply(lap_class, function(x) nrow(x$train_knn_dist) + nrow(x$train_knn_idx))) == nrow(xtr_class) * 2                            # output nrow for idx and distance is nrow(train_data) * 2 [ as I sum both sublists ]

  testthat::expect_true(sum(c(out1, out2, out3)) == 3)
})



# TEST data

testthat::test_that("in case that the TEST data is not an empty matrix : the result for each metric is a list, the length of the output is 2 [meaning 2 sublists] and the nrows in total for the two

                    sublists [ indices and distances ] are 2 times the TEST data", {

  test_dat = matrix(, nrow = 0, ncol = 0)

  lap_regr = lapply(c('euclidean', 'manhattan', 'chebyshev', 'canberra', 'braycurtis', 'pearson_correlation', 'simple_matching_coefficient', 
                      'minkowski', 'hamming', 'mahalanobis', 'jaccard_coefficient', 'Rao_coefficient'), function(x)  knn_index_dist_rcpp(as.matrix(xtr), as.matrix(xte), 5, x, 1, eps = 1.0e-6))

  lap_class = lapply(c('euclidean', 'manhattan', 'chebyshev', 'canberra', 'braycurtis', 'pearson_correlation', 'simple_matching_coefficient', 
                       'minkowski', 'hamming', 'mahalanobis', 'jaccard_coefficient', 'Rao_coefficient'), function(x)  knn_index_dist_rcpp(as.matrix(xtr_class), as.matrix(xte_class), 5, x, 1, eps = 1.0e-6))

  out1 = sum(unlist(lapply(lap_regr, is.list))) == length(lap_regr) && sum(unlist(lapply(lap_class, is.list))) == length(lap_class)         # sublists are lists

  out2 = mean(unlist(lapply(lap_regr, length))) == 2 && mean(unlist(lapply(lap_class, length))) == 2                                        # length of sublists are 2

  out3 = mean(unlist(lapply(lap_regr, function(x) nrow(x$test_knn_dist) + nrow(x$test_knn_idx)))) == nrow(xte) * 2 &&

    unlist(lapply(lap_class, function(x) nrow(x$test_knn_dist) + nrow(x$test_knn_idx))) == nrow(xte_class) * 2                            # output nrow for idx and distance is nrow(train_data) * 2 [ as I sum both sublists ]

  testthat::expect_true(sum(c(out1, out2, out3)) == 3)
})


#==============================================================================================================================================================
