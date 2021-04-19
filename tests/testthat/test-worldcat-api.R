context("checking functions in worldcat-api.R")

# can't really test these functions much

# worldcat_api_classify_by_oclc
test_that("worldcat_api_classify_by_oclc() works", {
  expect_equal(worldcat_api_classify_by_oclc(NA), NA)
  expect_error(worldcat_api_classify_by_oclc(c("877749545", "939766505")),
               "only accepts one standard number at a time")
})

# worldcat_api_classify_by_isbn
test_that("worldcat_api_classify_by_isbn() works", {
  expect_equal(worldcat_api_classify_by_isbn(NA), NA)
  expect_error(worldcat_api_classify_by_isbn(c("978039333712", "978039333712")),
               "only accepts one standard number at a time")
})

# worldcat_api_classify_by_issn
test_that("worldcat_api_classify_by_issn() works", {
  expect_equal(worldcat_api_classify_by_issn(NA), NA)
  expect_error(worldcat_api_classify_by_issn(c("12341234", "12341234")),
               "only accepts one standard number at a time")
})

