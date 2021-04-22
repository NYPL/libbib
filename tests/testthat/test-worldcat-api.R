context("checking functions in worldcat-api.R")


options("libbib.wskey"="test")


# can't really test these functions much

# worldcat_api_classify_by_oclc
test_that("worldcat_api_classify_by_oclc() works", {
  expect_null(worldcat_api_classify_by_oclc(NA))
  expect_error(worldcat_api_classify_by_oclc(c("877749545", "939766505")),
               "only accepts one standard number at a time")
})

# worldcat_api_classify_by_isbn
test_that("worldcat_api_classify_by_isbn() works", {
  expect_null(worldcat_api_classify_by_isbn(NA))
  expect_error(worldcat_api_classify_by_isbn(c("978039333712", "978039333712")),
               "only accepts one standard number at a time")
})

# worldcat_api_classify_by_issn
test_that("worldcat_api_classify_by_issn() works", {
  expect_null(worldcat_api_classify_by_issn(NA))
  expect_error(worldcat_api_classify_by_issn(c("12341234", "12341234")),
               "only accepts one standard number at a time")
})




# worldcat_api_bib_read_info_by_oclc
test_that("worldcat_api_bib_read_info_by_oclc() works", {
  expect_null(worldcat_api_bib_read_info_by_oclc(NA))
  expect_error(worldcat_api_bib_read_info_by_oclc(c("12341234", "12341234")),
               "only accepts one standard number at a time")
})

# worldcat_api_bib_read_info_by_isbn
test_that("worldcat_api_bib_read_info_by_isbn() works", {
  expect_null(worldcat_api_bib_read_info_by_isbn(NA))
  expect_error(worldcat_api_bib_read_info_by_isbn(c("12341234", "12341234")),
               "only accepts one standard number at a time")
})

# worldcat_api_bib_read_info_by_issn
test_that("worldcat_api_bib_read_info_by_issn() works", {
  expect_null(worldcat_api_bib_read_info_by_issn(NA))
  expect_error(worldcat_api_bib_read_info_by_issn(c("12341234", "12341234")),
               "only accepts one standard number at a time")
})




# construct_wcapiloc_url
test_that("construct_wcapiloc_url() works", {
  expect_equal(construct_wcapiloc_url("877749545"),
               "http://www.worldcat.org/webservices/catalog/content/libraries/877749545?location=10032&maximumLibraries=100&servicelevel=full&frbrGrouping=on&startLibrary=1&wskey=test")
  expect_equal(construct_wcapiloc_url("9781788393720", type_std_num="isbn"),
               "http://www.worldcat.org/webservices/catalog/content/libraries/isbn/9781788393720?location=10032&maximumLibraries=100&servicelevel=full&frbrGrouping=on&startLibrary=1&wskey=test")
  expect_equal(construct_wcapiloc_url("877749545", location="10463"),
               "http://www.worldcat.org/webservices/catalog/content/libraries/877749545?location=10463&maximumLibraries=100&servicelevel=full&frbrGrouping=on&startLibrary=1&wskey=test")
  expect_equal(construct_wcapiloc_url("877749545", max_libraries=2),
               "http://www.worldcat.org/webservices/catalog/content/libraries/877749545?location=10032&maximumLibraries=2&servicelevel=full&frbrGrouping=on&startLibrary=1&wskey=test")
  expect_equal(construct_wcapiloc_url("877749545", servicelevel="default"),
               "http://www.worldcat.org/webservices/catalog/content/libraries/877749545?location=10032&maximumLibraries=100&servicelevel=default&frbrGrouping=on&startLibrary=1&wskey=test")
  expect_equal(construct_wcapiloc_url("857542062", frbrGrouping="off"),
               "http://www.worldcat.org/webservices/catalog/content/libraries/857542062?location=10032&maximumLibraries=100&servicelevel=full&frbrGrouping=off&startLibrary=1&wskey=test")
  expect_equal(construct_wcapiloc_url("877749545", libtype="government"),
               "http://www.worldcat.org/webservices/catalog/content/libraries/877749545?location=10032&maximumLibraries=100&servicelevel=full&frbrGrouping=on&libtype=3&startLibrary=1&wskey=test")
  expect_equal(construct_wcapiloc_url("877749545", start_at=10),
               "http://www.worldcat.org/webservices/catalog/content/libraries/877749545?location=10032&maximumLibraries=100&servicelevel=full&frbrGrouping=on&startLibrary=10&wskey=test")

  expect_error(construct_wcapiloc_url(877749545),
               "standard number must be a string")
  expect_error(construct_wcapiloc_url(),
               'argument "stdnum" is missing, with no default')
  expect_error(construct_wcapiloc_url("877749545", type_std_num="lccn"),
               'type of standard number must be "oclc", "isbn", or "issn"')
  expect_error(construct_wcapiloc_url("877749545", max_libraries="a billion"),
               "max_libraries must be a number between 1 and 100")
  expect_error(construct_wcapiloc_url("877749545", max_libraries=1000),
               "max_libraries must be a number between 1 and 100")
  expect_error(construct_wcapiloc_url("877749545", servicelevel="all"),
               'service level must be "full" or "default"')
  expect_error(construct_wcapiloc_url("877749545", frbrGrouping="all"),
               'frfbGrouping must be "on" or "off"')
  expect_error(construct_wcapiloc_url("877749545", libtype="all"),
               'libtype must be either NULL, "academic", "public", "government", or "other"')
  expect_error(construct_wcapiloc_url("877749545", start_at="all"),
               'start_at must be a number')

})




# worldcat_api_locations_by_oclc
test_that("worldcat_api_locations_by_oclc() works", {
  expect_null(worldcat_api_locations_by_oclc(NA))
  expect_error(worldcat_api_locations_by_oclc(c("12341234", "12341234")),
               "only accepts one standard number at a time")
})

# worldcat_api_locations_by_isbn
test_that("worldcat_api_locations_by_isbn() works", {
  expect_null(worldcat_api_locations_by_isbn(NA))
  expect_error(worldcat_api_locations_by_isbn(c("12341234", "12341234")),
               "only accepts one standard number at a time")
})

# worldcat_api_locations_by_issn
test_that("worldcat_api_locations_by_issn() works", {
  expect_null(worldcat_api_locations_by_issn(NA))
  expect_error(worldcat_api_locations_by_issn(c("12341234", "12341234")),
               "only accepts one standard number at a time")
})


