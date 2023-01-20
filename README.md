
<!-- README.md is generated from README.Rmd. Please edit that file -->

# adbcdrivermanager

<!-- badges: start -->

[![R-CMD-check](https://github.com/paleolimbot/adbcdrivermanager/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/paleolimbot/adbcdrivermanager/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of adbcdrivermanager is to …

## Installation

You can install the development version of adbcdrivermanager from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("paleolimbot/adbcdrivermanager")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(adbcdrivermanager)
db <- adbcdrivermanager:::adbc_database_init(adbcdrivermanager:::adbc_driver_void())
adbcdrivermanager:::adbc_database_release(db)
```
