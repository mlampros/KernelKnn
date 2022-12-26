
## KernelKnn 1.1.5

* I added the order *'p'* of the "minkowski" method as a new parameter to the *'KernelKnn()'*, *'KernelKnnCV()'* and *'knn.index.dist()'* functions. It defaults to 'k' (see https://github.com/mlampros/KernelKnn/issues/9)
* I added test cases for the *'KernelKnn()'* and *'knn.index.dist()'* functions


## KernelKnn 1.1.4

* The pull request 7 fixed a bug in the checking of the Levels argument (see https://github.com/mlampros/KernelKnn/pull/7)
* I fixed an omission of the column names in case of classification in the *KernelKnn()* and *distMat.KernelKnn()* functions (see https://github.com/mlampros/KernelKnn/issues/8)


## KernelKnn 1.1.3

* I updated the References section of the *switch.ops()* function in the *utils.R* file which explain how the combination of the kernels work
* I added an error case in all functions that make usage of the 'Levels' parameter if the 'Levels' do not match the unique 'y' labels
* I removed the *distMat.KernelKnnCV()* function (and the *tests/test-dist_kernelknnCV.R* file) because based on the current implementation of the *distMat.KernelKnn()* function the *TEST_indices* parameter *must* consist of the *last indices* of the input *DIST_mat* distance matrix and this is not the case if we run cross-validation (see [issue 5](https://github.com/mlampros/KernelKnn/issues/5))


## KernelKnn 1.1.2

* I've fixed an error in the *CITATION* file


## KernelKnn 1.1.1

* I've added the *CITATION* file in the *inst* directory


## KernelKnn 1.1.0

* I fixed the *"failure: the condition has length > 1"* CRAN error which appeared mainly due to the misuse of the base *class()* function in multiple code snippets in the package (for more info on this matter see: https://developer.r-project.org/Blog/public/2019/11/09/when-you-think-class.-think-again/index.html)


## KernelKnn 1.0.9

I added a test case to check equality of the results between *KernelKnnCV* and *distMat.KernelKnnCV* functions


## KernelKnn 1.0.8

I added the *DARMA_64BIT_WORD* flag in the Makevars file to allow the package processing big datasets


## KernelKnn 1.0.7

I modified the *input_dist_mat* function of the *distance_metrics.cpp* file due to a bug.
I modified  the *distMat.KernelKnn* function so that it does not return an error if the rows of the *DIST_mat* distance matrix is not equal to the length of *y* (added comments in the function documentation).


## KernelKnn 1.0.6

In this version the following functions/parameters were added:

* *seed_num* : parameter in *KernelKnnCV* and *distMat.KernelKnnCV* cross-validation functions, which specifies the seed of R's random number generator 
* *distMat.KernelKnn* : this function performs kernel k-nearest-neighbor search by using a *distance matrix* as input
* *distMat.knn.index.dist* : this function returns the indices and distances for k-nearest neighbors using a distance matrix
* *distMat.KernelKnnCV* : this function performs cross-validated kernel k-nearest-neighbor search using a distance matrix as input

I also modified the *OpenMP* clauses of the .cpp file to address the ASAN errors.


## KernelKnn 1.0.5

I removed *OpenImageR* and *irlba* as package dependencies. I also added an *init.c* file in the *src* folder due to a change in CRAN submissions for compiled code [  *references* : http://stackoverflow.com/questions/42313373/r-cmd-check-note-found-no-calls-to-r-registerroutines-r-usedynamicsymbols, https://github.com/RcppCore/Rcpp/issues/636  ]


## KernelKnn 1.0.4

I added a try-catch Rcpp function to make possible the calculation of singular covariance matrices as sugggested in https://github.com/mlampros/KernelKnn/issues/1


## KernelKnn 1.0.3

Reimplementation of the Rcpp function due to ASAN-memory-errors


## KernelKnn 1.0.2

I updated the Description file with a URL and a BugReports web-address.


## KernelKnn 1.0.1

Currently, Software platforms like OSX do not support openMP, thus I've made openMP optional for all cpp functions.


## KernelKnn 1.0.0

