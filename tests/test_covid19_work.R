source("../covid19_functions.R", chdir = TRUE)
library(testthat)

# test data
wa_df <- read.csv("./wa.csv")

setwd("/tmp")

test_that("state_pop_txt", {
  expect_equal(state_pop_txt("Washington", wa_df), "Washington State (pop=7,614,893)")
})

test_that("pop_format", {
  expect_equal(pop_format(.8), "80,000")
})

test_that("init", {
  expect_equal(onetime(), 0)
  expect_equal(newday(), 0)
  expect_equal(vax_data(), 0)
})

population <- get_population()
test_that("pop_from_jhu", {
  expect_equal(dim(population), c(4195,12))
})


#  expect_equal(get_population(), 0)
#  expect_is(get_population(), 0)

#test_that("vectors", #{
#  expect_equal(increment(c(0,1)), c(1,2))
#})
#
#test_that("empty vector", {
#  expect_equal(increment(c()), c())
#})
#
#test_that("test NA", {
#  expect_true(is.na(increment(NA)))
#})
