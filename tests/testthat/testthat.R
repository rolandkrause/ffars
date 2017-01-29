context("FARS data test")
library(testthat)
library(ffars)

## A bit difficult to write the test when test data cannot be checked in

test_that("make filename",{
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
})

