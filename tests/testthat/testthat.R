context("FARS data test")
library(testthat)
library(ffars)

test_that("true is true", {
expect_equal(TRUE, TRUE)
  expect_equal(TRUE, TRUE)
  expect_equal(TRUE, TRUE)
  expect_equal(FALSE, FALSE)
})

test_that("make filename",{
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
})

