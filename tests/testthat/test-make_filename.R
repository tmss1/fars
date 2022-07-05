library(testthat)


# Test make_filename
testthat::expect_equal(make_filename(2015),"accident_2015.csv.bz2")

