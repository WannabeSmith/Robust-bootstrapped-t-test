# rbtt (Robust bootstrapped t-test) 

[![CRAN version](https://www.r-pkg.org/badges/version/rbtt)](https://www.r-pkg.org/badges/version/rbtt)

[![Travis-CI Build Status](http://travis-ci.org/WannabeSmith/rbtt.svg?branch=master)](http://travis-ci.org/WannabeSmith/rbtt)

Tu & Zhou (1999) <doi:10.1002/(SICI)1097-0258(19991030)18:20%3C2749::AID-SIM195%3E3.0.CO;2-C> showed that comparing the means of populations whose data-generating distributions are non-negative with excess zero observations is a problem of great importance in the analysis of medical cost data. In the same study, Tu & Zhou discuss that it can be difficult to control type-I error rates of general-purpose statistical tests for comparing the means of these particular data sets. This package allows users to perform a modified bootstrap-based t-test that aims to better control type-I error rates in these situations.

## Install

To download and use the current version of this package:

```
install.packages("rbtt")
```

## Install development version

To download and use the development version of this package:

```r
install.packages("devtools") # Unless you already have it
library(devtools)
devtools::install_github("WannabeSmith/rbtt")
```
