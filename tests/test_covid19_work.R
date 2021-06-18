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

test_that("get_pop", {
  expect_equal(get_pop("Maryland", "Baltimore City"), 5.9349, tolerance = 1)
  expect_equal(get_pop("Washington", "Island"), 0.85141, tolerance = 1)
  expect_equal(get_pop("Washington", "island"), 0.85141, tolerance = 1)
  expect_equal(get_pop("Washington"), 76.14893, tolerance = 1)
  expect_equal(get_pop("Washington", "Total"), 76.14893, tolerance = 1)
  expect_equal(get_pop("Alabama"), 49.03185, tolerance = 1)
  expect_equal(get_pop("District Of Columbia"), 7.05749, tolerance = 1)
  expect_equal(get_pop("District of Columbia"), 7.05749, tolerance = 1)
  expect_equal(get_pop("district of columbia"), 7.05749, tolerance = 1)
  expect_equal(get_pop("Puerto Rico"), 31.93694, tolerance = 1)
  expect_equal(get_pop("american samoa"), 0.55641, tolerance = 1)
  expect_equal(get_pop(country = "india"), 13800.04, tolerance = 1)
  expect_equal(get_pop(country = "Canada"), 378.557, tolerance = 1)
  expect_equal(get_pop(state = "Washington", county = "Columbia"), 0.03985, tolerance = .01)
  expect_equal(get_pop("Diamond Princess"), 0)
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
