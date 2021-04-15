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
  expect_equal(get_lc_call_subject_classification("H29.S75 A38"),
               "Social Sciences")
  expect_equal(get_lc_call_subject_classification("H29.S75 A38", subclassification=TRUE),
               "Social Sciences (General)")
  expect_equal(get_lc_call_subject_classification("Pk3798.K74 C33 1963"),
               "Language and Literature")
  expect_equal(get_lc_call_subject_classification("Pk3798.K74 C33 1963", subclassification=TRUE),
               "Indo-Iranian languages and literatures")
  expect_equal(get_lc_call_subject_classification("KKT5674.18 .P75 1986"),
               "Law")
  expect_equal(get_lc_call_subject_classification("KKT5674.18 .P75 1986", subclassification=TRUE),
               "Spain")
  expect_equal(get_lc_call_subject_classification("KZD1006 .J68", subclassification=TRUE),
               "Space law. Law of outer space")
  expect_equal(get_lc_call_subject_classification("KLP13 1919", subclassification=TRUE),
               "Ukraine (1919-1991) and Zakavkazskaia Sotsialisticheskaia Federativnaia Sovetskaia Respublika (to 1936)")
  expect_equal(get_lc_call_subject_classification("KDC296.A75", subclassification=TRUE),
               "Scotland")
  expect_equal(get_lc_call_subject_classification("KDC", subclassification=TRUE),
               "Scotland")
  expect_equal(get_lc_call_subject_classification("KDC", allow.bare=FALSE, subclassification=TRUE),
               NA_character_)
  expect_equal(get_lc_call_subject_classification("K"), "Law")
  expect_equal(get_lc_call_subject_classification("K", allow.bare=FALSE), NA_character_)
  expect_equal(get_lc_call_subject_classification("C13.2:R 31/7"),
               "Auxiliary Sciences of History")
  expect_equal(get_lc_call_subject_classification("C13.2:R 31/7", subclassification=TRUE),
               "Auxiliary Sciences of History (General)")
  expect_equal(get_lc_call_subject_classification("A11.K5"),
               "General Works")
  expect_equal(get_lc_call_subject_classification("A11.K5", subclassification=TRUE),
               "General Works")
})

# is_valid_lc_call
test_that("is_valid_lc_call() works", {
  expect_equal(is_valid_lc_call(c("Q 172.5", "AF172", "PR6023.A93")),
               c(TRUE, FALSE, TRUE))
  expect_equal(is_valid_lc_call("H29.S75 A38"), TRUE)
  expect_equal(is_valid_lc_call("Pk3798.K74 C33 1963"), TRUE)
  expect_equal(is_valid_lc_call("Rs164 .K894818 1966"), TRUE)
  expect_equal(is_valid_lc_call("KKT5674.18 .P75 1986"), TRUE)
  expect_equal(is_valid_lc_call("KDC"), FALSE)
  expect_equal(is_valid_lc_call("KDC", allow.bare=TRUE), TRUE)
  expect_equal(is_valid_lc_call("K"), FALSE)
  expect_equal(is_valid_lc_call("K", allow.bare=TRUE), TRUE)
})

# get_lc_call_first_letter
test_that("get_lc_call_first_letter() works", {
  expect_equal(get_lc_call_first_letter(c("Q 172.5", "AF172", NA, "PR6023.A93")),
               c("Q", NA, NA, "P"))
  expect_equal(get_lc_call_first_letter("H29.S75 A38"), "H")
  expect_equal(get_lc_call_first_letter("Pk3798.K74 C33 1963"), "P")
  expect_equal(get_lc_call_first_letter("KKT5674.18 .P75 1986"), "K")
  expect_equal(get_lc_call_first_letter("KDC"), NA)
  expect_equal(get_lc_call_first_letter("KDC", allow.bare=TRUE), "K")
  expect_equal(get_lc_call_first_letter("K"), NA)
  expect_equal(get_lc_call_first_letter("K", allow.bare=TRUE), "K")
})

# get_all_lc_call_subject_letters
test_that("get_all_lc_call_subject_letters() works", {
  expect_equal(get_all_lc_call_subject_letters(c("Q 172.5", "AF172", NA,
                                                 "PR6023.A93")),
               c("Q", NA, NA, "PR"))
  expect_equal(get_all_lc_call_subject_letters("Pk3798.K74 C33 1963"), "PK")
  expect_equal(get_all_lc_call_subject_letters("KKT5674.18 .P75 1986"), "KKT")
  expect_equal(get_all_lc_call_subject_letters("KDC"), NA)
  expect_equal(get_all_lc_call_subject_letters("KDC", allow.bare=TRUE), "KDC")
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



