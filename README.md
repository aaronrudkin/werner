
<!-- README.md is generated from README.Rmd. Please edit that file -->

werner: Improve R package quality by mapping interdependence between functions.
===============================================================================

[![Build Status](https://travis-ci.org/aaronrudkin/werner.svg?branch=master)](https://travis-ci.org/aaronrudkin/werner) [![Coverage Status](https://coveralls.io/repos/github/aaronrudkin/werner/badge.svg?branch=master)](https://coveralls.io/github/aaronrudkin/werner?branch=master)

### Installing werner

To install the latest development release of **werner**, please ensure that you are running version 3.3 or later of R and run the following code:

``` r
install.packages("devtools")
devtools::install_github("aaronrudkin/werner")
```

### Getting started

**werner** contains two main functions.

`explore_package` returns a list of connections between functions:

``` r
library(werner)
explore_package("werner")
```

`adjacency_matrix` returns an adjacency matrix (by default a sparse matrix from the [**Matrix**](https://cran.r-project.org/web/packages/Matrix/index.html) package, optionally a base R `matrix` object.)

``` r
library(werner)
adjacency_matrix("werner") # Sparse matrix
adjacency_matrix("werner", coerce_to_matrix=TRUE) # Base R matrix.
```

These adjacency matrices can be fed to [**igraph**](http://igraph.org/r/) or other graphing packages in R.

### Code Style

Code should adhere to the [tidyverse style guide](http://style.tidyverse.org/), with the exception of assignment operators, which should be the traditional "=" rather than the "&lt;-" assignment. You can check code style by opening the package Rproj file and running:

``` r
library(goodpractice)
gp(checks = setdiff(all_checks(), "lintr_assignment_linter"))
```
