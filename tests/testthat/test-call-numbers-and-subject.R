context("checking functions in call-numbers-and-subject.R")


data("books_and_serials_sample")

# get_lc_call_subject_classification
test_that("lc_call_subject_classification() works", {
  expect_equal(get_lc_call_subject_classification("ND 237.S18 $b S87 1997"),
               "Fine Arts")
  expect_equal(get_lc_call_subject_classification("ND 237.S18 $b S87 1997", subclassification=TRUE),
               "Painting")
  expect_equal(get_lc_call_subject_classification("PQ2246.M3", already.parsed=TRUE),
               NA_character_)
  expect_equal(get_lc_call_subject_classification(c("ND 237",
                                                    "\\\\$a ND 2",
                                                    NA,
                                                    "PQ2246.M3"),
                                                  subclassification=TRUE),
               c("Painting", NA, NA,
                 "French, Italian, Spanish, and Portuguese literature"))
  expect_equal(get_lc_call_subject_classification(books_and_serials_sample$lccall),
               books_and_serials_sample$lc_subject_class)
  expect_equal(get_lc_call_subject_classification(books_and_serials_sample$lccall,
                                                  subclassification=TRUE),
               books_and_serials_sample$lc_subject_subclass)
})

# is_valid_lc_call
test_that("is_valid_lc_call() works", {
  expect_equal(is_valid_lc_call(c("Q 172.5", "AF172", "PR6023.A93")),
               c(TRUE, FALSE, TRUE))
})

# get_lc_call_first_letter
test_that("get_lc_call_first_letter() works", {
  expect_equal(get_lc_call_first_letter(c("Q 172.5", "AF172", NA, "PR6023.A93")),
               c("Q", NA, NA, "P"))
})

# get_all_lc_call_subject_letters
test_that("get_all_lc_call_subject_letters() works", {
  expect_equal(get_all_lc_call_subject_letters(c("Q 172.5", "AF172", NA,
                                                 "PR6023.A93")),
               c("Q", NA, NA, "PR"))
})


# get_dewey_decimal_subject_class
test_that("get_dewey_decimal_subject_class() works", {
  expect_equal(get_dewey_decimal_subject_class(books_and_serials_sample$dewey),
               books_and_serials_sample$dewey_subject_class)
  expect_equal(get_dewey_decimal_subject_class(c("709.05", "invalid", NA, "823.912")),
               c("Arts", NA, NA, "Literature (Belles-lettres) and rhetoric"))
})

# get_dewey_decimal_subject_division
test_that("get_dewey_decimal_subject_division() works", {
  expect_equal(get_dewey_decimal_subject_division(books_and_serials_sample$dewey),
               books_and_serials_sample$dewey_subject_division)
  expect_equal(get_dewey_decimal_subject_division(c("709.05", "invalid", NA, "823.912")),
               c("Arts", NA, NA, "English and Old English literatures"))
})

# get_dewey_decimal_subject_section
test_that("get_dewey_decimal_subject_section() works", {
  expect_equal(get_dewey_decimal_subject_section(books_and_serials_sample$dewey),
               books_and_serials_sample$dewey_subject_section)
  expect_equal(get_dewey_decimal_subject_section(c("709.05", "invalid", NA, "823.912")),
               c("History, geographic treatment, biography", NA, NA, "English fiction"))
})





