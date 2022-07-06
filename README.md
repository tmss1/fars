
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fars

<!-- badges: start -->

[![Build
Status](https://app.travis-ci.com/tmss1/fars.svg?branch=main)](https://app.travis-ci.com/tmss1/fars)
<!-- badges: end -->

The goal of fars is to analyse data from the US National Highway Traffic
Safety Administration’s Fatality Analysis Reporting System.

## Installation

You can install the development version of fars from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tmss1/fars")
```

## Example

This is a basic example which shows you how to make a file name based on
the year of FARS data interested:

``` r
library(fars)
fname <- make_filename(2015)
fname
#> [1] "accident_2015.csv.bz2"
```
