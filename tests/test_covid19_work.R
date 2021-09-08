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
  expect_equal(get_vax_data(), 0)
})

population <- get_population()
test_that("pop", {
  expect_true(dim(population)[1] >= 4196)
  expect_true(dim(population)[2] >= 12)
})

# really don't need all of these and they're likely to break as pops get adjusted
test_that("get_pop", {
  expect_equal(get_pop(admin1="Maryland", admin2="Baltimore City"), 5.9349, tolerance = 1)
  expect_equal(get_pop(admin1="Washington", "Island"), 0.85141, tolerance = 1)
  expect_equal(get_pop(admin1="Washington", "island"), 0.85141, tolerance = 1)
  expect_equal(get_pop(admin1="Washington"), 76.14893, tolerance = 1)
  expect_equal(get_pop(admin1="Alabama"), 49.03185, tolerance = 1)
  expect_equal(get_pop(admin1="District Of Columbia"), 7.05749, tolerance = 1)
  expect_equal(get_pop(country="US", admin1="District of Columbia"), 7.05749, tolerance = 1)
  expect_equal(get_pop(admin1="district of columbia"), 7.05749, tolerance = 1)
  expect_equal(get_pop(admin1="Puerto Rico"), 31.93694, tolerance = 1)
  expect_equal(get_pop(admin1="american samoa"), 0.55641, tolerance = 1)
  expect_equal(get_pop(country = "india"), 13800.04, tolerance = 1)
  expect_equal(get_pop(country = "Canada"), 378.557, tolerance = 1)
  expect_equal(get_pop(admin1 = "Washington", admin2 = "Columbia"), 0.03985, tolerance = .01)
  expect_equal(get_pop(admin1="Diamond Princess"), 0)
})


#p <-  make_plot(
#    df = wa_df,
#    loc_txt = "bongo",
#    cases_per_hundy = TRUE,
#    cases = FALSE,
#    daily_cases = FALSE)
# how to test ggplot?

usa_df <- build_all_states()
test_that("build usa all", {
  expect_true(dim(usa_df)[1] >= 512)
})

