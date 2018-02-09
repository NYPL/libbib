context("checking functions in bibcodes.R")


EX.valid.10.digit.isbns <- c("012491540X", "9004037810", "0256016054")
EX.invalid.10.digit.isbns <- c("0124915401", "9004037811", "0256016051")
EX.9.digit.isbns <- c("012491540", "900403781", "025601605")


# get_isbn_10_check_digit succeeds properly
test_that("get_isbn_10_check_digit() succeeds properly", {
  expect_equal(get_isbn_10_check_digit(EX.valid.10.digit.isbns[1]), "X")
  expect_equal(get_isbn_10_check_digit(EX.valid.10.digit.isbns), c("X", "0", "4"))
  expect_equal(get_isbn_10_check_digit(EX.9.digit.isbns), c("X", "0", "4"))
})

# get_isbn_10_check_digit fails properly
test_that("get_isbn_10_check_digit() fails properly", {
  expect_equal(get_isbn_10_check_digit(c(EX.valid.10.digit.isbns, NA)),
                                       c("X", "0", "4", NA))
  expect_error(get_isbn_10_check_digit(123456789),
               "Input must be a character string")
  expect_error(get_isbn_10_check_digit("123456"),
               "Input must be either 9 or 10 characters")
})


# check_isbn_10_check_digit succeeds properly
test_that("get_isbn_10_check_digit() succeeds properly", {
  expect_equal(check_isbn_10_check_digit(EX.valid.10.digit.isbns[1]),
               TRUE)
  expect_equal(check_isbn_10_check_digit(EX.valid.10.digit.isbns),
               c(TRUE, TRUE, TRUE))
  expect_equal(check_isbn_10_check_digit(EX.invalid.10.digit.isbns),
               c(FALSE, FALSE, FALSE))
  expect_equal(check_isbn_10_check_digit(c(EX.valid.10.digit.isbns,
                                           EX.invalid.10.digit.isbns)),
               c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(check_isbn_10_check_digit(c(12, 12), error.is.false = TRUE),
               c(FALSE, FALSE))
  expect_equal(check_isbn_10_check_digit(c(EX.valid.10.digit.isbns,
                                           "hubo un tiempo"),
                                         error.is.false = TRUE),
               c(TRUE, TRUE, TRUE, FALSE))
})

# check_isbn_10_check_digit fails properly
test_that("check_isbn_10_check_digit() fails properly", {
  expect_equal(check_isbn_10_check_digit(c(EX.valid.10.digit.isbns, NA)),
               c(TRUE, TRUE, TRUE, NA))
  expect_error(check_isbn_10_check_digit(0123456789),
               "Input must be a character string")
  expect_error(check_isbn_10_check_digit("123456789"),
               "Input must be 10 characters")
})





# is_valid_isbn_10
test_that("is_valid_isbn_10() succeeds properly", {
  expect_equal(is_valid_isbn_10(EX.valid.10.digit.isbns[1]),
               TRUE)
  expect_equal(is_valid_isbn_10(EX.valid.10.digit.isbns),
               c(TRUE, TRUE, TRUE))
  expect_equal(is_valid_isbn_10(c(EX.valid.10.digit.isbns, "012491540x")),
               c(TRUE, TRUE, TRUE, TRUE))
  expect_equal(is_valid_isbn_10(c(EX.valid.10.digit.isbns, "012491540x"),
                                lower.x.allowed=FALSE),
               c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(is_valid_isbn_10(EX.invalid.10.digit.isbns),
               c(FALSE, FALSE, FALSE))
  expect_equal(is_valid_isbn_10(c(EX.valid.10.digit.isbns,
                                           EX.invalid.10.digit.isbns)),
               c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(is_valid_isbn_10(c(EX.valid.10.digit.isbns, "hubo un tiempo")),
               c(TRUE, TRUE, TRUE, FALSE))
})

test_that("is_valid_isbn_10() fails properly", {
  expect_equal(is_valid_isbn_10(c(EX.valid.10.digit.isbns, NA)),
               c(TRUE, TRUE, TRUE, NA))
  expect_error(is_valid_isbn_10(0123456789),
               "Input must be a character string")
})




