library(testthat)
expect_that(make_filename(2015), is_identical_to("accident_2015.csv.bz2"))
