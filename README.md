
<!-- README.md is generated from README.Rmd. Please edit that file -->

# radbc

<!-- badges: start -->
<!-- badges: end -->

The goal of radbc is to …

## Installation

You can install the development version of radbc from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("paleolimbot/radbc")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(radbc)
db <- radbc:::radbc_database_init(radbc:::radbc_driver_void())
radbc:::radbc_database_release(db)
```
