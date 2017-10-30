#===================================================================================================================================

# REGRESSION
#-----------

data(Boston, package = 'KernelKnn')

X = Boston[, -dim(Boston)[2]]
xtr = X[1:350, ]
xte = X[351:nrow(X), ]
y1 = Boston[1:350, dim(Boston)[2]]
y = Boston[, dim(Boston)[2]]


# CLASSIFICATION
#---------------

data(ionosphere, package = 'KernelKnn')

singular_mat = ionosphere
ionosphere1 = ionosphere[, -2]  
ionosphere = ionosphere[, -c(2, ncol(ionosphere))]                    # remove second column which has a single unique value
xtr_class = ionosphere[1:200, ]
xte_class = ionosphere[201:nrow(ionosphere), ]

X_class = ionosphere1[, -dim(ionosphere1)[2]]
xtr_class = X_class[1:200, ]
xte_class = X_class[201:nrow(X_class), ]
y1_class = ionosphere1[1:200, dim(ionosphere1)[2]]
y1_class_ext = ionosphere1[, dim(ionosphere1)[2]]


# DISTANCE MATRIX (REGRESSION)

DIST_obj = stats::dist(X, method = "euclidean")
DIST_mat = as.matrix(DIST_obj)

DIST_obj_class = stats::dist(ionosphere, method = "euclidean")
DIST_mat_class = as.matrix(DIST_obj_class)


# utility function ( lappend )
#-----------------------------

lappend <- function(lst, ...){                                       # lappend() function to append vector to list
  
  lst <- c(lst, list(...))
  
  return(lst)
}

#===================================================================================================================================