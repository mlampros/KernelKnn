\name{Boston}
\alias{Boston}
\docType{data}
\title{
Boston Housing Data    (Regression)
}
\description{
housing values in suburbs of Boston
}
\usage{data(Boston)}
\format{
  A data frame with 506 Instances and 14 attributes (including the class attribute, "medv")
  \describe{
    \item{\code{crim}}{per capita crime rate by town}
    \item{\code{zn}}{proportion of residential land zoned for lots over 25,000 sq.ft.}
    \item{\code{indus}}{proportion of non-retail business acres per town}
    \item{\code{chas}}{Charles River dummy variable (= 1 if tract bounds)}
    \item{\code{nox}}{nitric oxides concentration (parts per 10 million)}
    \item{\code{rm}}{average number of rooms per dwelling}
    \item{\code{age}}{proportion of owner-occupied units built prior to 1940}
    \item{\code{dis}}{weighted distances to five Boston employment centres}
    \item{\code{rad}}{index of accessibility to radial highways}
    \item{\code{tax}}{full-value property-tax rate per $10,000}
    \item{\code{ptratio}}{pupil-teacher ratio by town}
    \item{\code{black}}{1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town}
    \item{\code{lstat}}{percentage of lower status of the population}
    \item{\code{medv}}{Median value of owner-occupied homes in $1000's}
  }
}
\source{
This dataset was taken from the StatLib library which is maintained at Carnegie Mellon University.

Creator:  Harrison, D. and Rubinfeld, D.L. 'Hedonic prices and the demand for clean air', J. Environ. Economics & Management, vol.5, 81-102, 1978.
}
\references{
https://archive.ics.uci.edu/ml/datasets/Housing
}
\examples{

data(Boston)

X = Boston[, -ncol(Boston)]

y = Boston[, ncol(Boston)]
}
\keyword{datasets}
