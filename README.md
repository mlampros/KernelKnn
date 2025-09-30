
[![R-CMD-check](https://github.com/mlampros/KernelKnn/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mlampros/KernelKnn/actions/workflows/R-CMD-check.yaml)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/KernelKnn)](http://cran.r-project.org/package=KernelKnn)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/KernelKnn?color=blue)](http://www.r-pkg.org/pkg/KernelKnn)
[![](https://img.shields.io/docker/automated/mlampros/kernelknn.svg)](https://hub.docker.com/r/mlampros/kernelknn)
[![status](https://tinyverse.netlify.app/badge/KernelKnn)](https://CRAN.R-project.org/package=KernelKnn)


## KernelKnn
<br>

The KernelKnn package extends the simple k-nearest neighbors algorithm by incorporating numerous kernel functions and a variety of distance metrics. The package takes advantage of 'RcppArmadillo' to speed up the calculation of distances between observations. More details on the functionality of KernelKnn can be found in the [blog-post](http://mlampros.github.io/2016/07/10/KernelKnn/) and in the package Vignettes ( *scroll down for information on how to use the* **docker image** ).
<br><br>

To install the package from CRAN use, 

```R

install.packages("KernelKnn")


```
<br>

or download the latest version from Github using the *pak* package,

<br>

```R

pak::pak('mlampros/KernelKnn')

```
<br>

Use the following link to report bugs/issues,
<br><br>

[https://github.com/mlampros/KernelKnn/issues](https://github.com/mlampros/KernelKnn/issues)


<br>

**UPDATE 29-11-2019**

<br>

**Docker images** of the *KernelKnn* package are available to download from my [dockerhub](https://hub.docker.com/r/mlampros/kernelknn) account. The images come with *Rstudio* and the *R-development* version (latest) installed. The whole process was tested on Ubuntu 18.04. To **pull** & **run** the image do the following,

<br>

```R

docker pull mlampros/kernelknn:rstudiodev

docker run -d --name rstudio_dev -e USER=rstudio -e PASSWORD=give_here_your_password --rm -p 8787:8787 mlampros/kernelknn:rstudiodev

```

<br>

The user can also **bind** a home directory / folder to the image to use its files by specifying the **-v** command,

<br>

```R

docker run -d --name rstudio_dev -e USER=rstudio -e PASSWORD=give_here_your_password --rm -p 8787:8787 -v /home/YOUR_DIR:/home/rstudio/YOUR_DIR mlampros/kernelknn:rstudiodev


```

<br>

In the latter case you might have first give permission privileges for write access to **YOUR_DIR** directory (not necessarily) using,

<br>

```R

chmod -R 777 /home/YOUR_DIR


```

<br>

The **USER** defaults to *rstudio* but you have to give your **PASSWORD** of preference (see [https://rocker-project.org](https://rocker-project.org/) for more information).

<br>

Open your web-browser and depending where the docker image was *build / run* give, 

<br>

**1st. Option** on your personal computer,

<br>

```R
http://0.0.0.0:8787 

```

<br>

**2nd. Option** on a cloud instance, 

<br>

```R
http://Public DNS:8787

```

<br>

to access the Rstudio console in order to give your username and password.

<br>

### **Citation:**

If you use the **KernelKnn** R package in your paper or research please cite [https://CRAN.R-project.org/package=KernelKnn/citation.html](https://CRAN.R-project.org/package=KernelKnn/citation.html):

<br>

```R
@Manual{,
  title = {{KernelKnn}: Kernel k Nearest Neighbors},
  author = {Lampros Mouselimis},
  year = {2025},
  note = {R package version 1.1.6},
  url = {https://CRAN.R-project.org/package=KernelKnn},
}
```

<br>

