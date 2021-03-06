% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dist_kernelknnCV.R
\name{distMat.KernelKnnCV}
\alias{distMat.KernelKnnCV}
\title{cross validated kernel-k-nearest-neighbors using a distance matrix}
\usage{
distMat.KernelKnnCV(
  DIST_mat,
  y,
  k = 5,
  folds = 5,
  h = 1,
  weights_function = NULL,
  regression = F,
  threads = 1,
  extrema = F,
  Levels = NULL,
  minimize = T,
  seed_num = 1
)
}
\arguments{
\item{DIST_mat}{a distance matrix (square matrix) having a diagonal filled with either zero's (\emph{0}) or NA's (\emph{missing values})}

\item{y}{a numeric vector (in classification the labels must be numeric from 1:Inf)}

\item{k}{an integer specifying the k-nearest-neighbors}

\item{folds}{the number of cross validation folds (must be greater than 1)}

\item{h}{the bandwidth (applicable if the weights_function is not NULL, defaults to 1.0)}

\item{weights_function}{there are various ways of specifying the kernel function. See the details section.}

\item{regression}{a boolean (TRUE,FALSE) specifying if regression or classification should be performed}

\item{threads}{the number of cores to be used in parallel (openmp will be employed)}

\item{extrema}{if TRUE then the minimum and maximum values from the k-nearest-neighbors will be removed (can be thought as outlier removal)}

\item{Levels}{a numeric vector. In case of classification the unique levels of the response variable are necessary}

\item{minimize}{either TRUE or FALSE. If TRUE then lower values will be considered as relevant for the k-nearest search, otherwise higher values.}

\item{seed_num}{a numeric value specifying the seed of the random number generator}
}
\value{
a list of length 2. The first sublist is a list of predictions (the length of the list equals the number of the folds). The second sublist is a list with the indices for each fold.
}
\description{
cross validated kernel-k-nearest-neighbors using a distance matrix
}
\details{
This function takes a number of arguments (including the number of cross-validation-folds) and it returns predicted values and indices for each fold. 
There are three possible ways to specify the weights function, 1st option : if the weights_function is NULL then a simple k-nearest-neighbor is performed. 2nd option : the weights_function is one of 'uniform', 'triangular', 'epanechnikov', 'biweight', 'triweight', 'tricube', 'gaussian', 'cosine', 'logistic', 'gaussianSimple', 'silverman', 'inverse', 'exponential'. The 2nd option can be extended by combining kernels from the existing ones (adding or multiplying). For instance, I can multiply the tricube with the gaussian kernel by giving 'tricube_gaussian_MULT' or I can add the previously mentioned kernels by giving 'tricube_gaussian_ADD'. 3rd option : a user defined kernel function
}
\examples{

\dontrun{
data(ionosphere)

X = ionosphere[, -c(2, ncol(ionosphere))]
y = as.numeric(ionosphere[, ncol(ionosphere)])

dist_obj = dist(X)

dist_mat = as.matrix(dist_obj)

out = distMat.KernelKnnCV(dist_mat, y, k = 5, folds = 3, Levels = unique(y))
}
}
\author{
Lampros Mouselimis
}
