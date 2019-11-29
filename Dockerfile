FROM rocker/rstudio:devel 

 
LABEL maintainer='Lampros Mouselimis' 

 
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update && \ 
 apt-get install -y git-core pandoc pandoc-citeproc libssl-dev libcurl4-openssl-dev && \ 
 apt-get install -y sudo && \ 
 apt-get install -y libarmadillo-dev && \ 
 apt-get install -y libblas-dev && \ 
 apt-get install -y liblapack-dev && \ 
 apt-get install -y libarpack++2-dev && \ 
 apt-get install -y gfortran  && \ 
 apt-get install -y libxml2-dev && \ 
 apt-get install -y libssh2-1-dev && \ 
 apt-get install -y zlib1g-dev && \ 
 R -e "install.packages('devtools', dependencies = TRUE, repos = 'https://cloud.r-project.org/')" && \ 
 R -e "install.packages(c( 'Rcpp', 'RcppArmadillo', 'testthat', 'covr', 'knitr', 'rmarkdown', 'remotes' ), repos =  'https://cloud.r-project.org/' )" && \ 
 R -e "remotes::install_github('mlampros/KernelKnn', upgrade = 'always', dependencies = TRUE, repos = 'https://cloud.r-project.org/')" && \ 
 apt-get autoremove -y && \ 
 apt-get clean 

 
ENV USER rstudio 

 
