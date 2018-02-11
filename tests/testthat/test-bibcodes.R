context("checking functions in bibcodes.R")


##############################################
###               SETUP                    ###
##############################################

EX.valid.10.digit.isbns <- c("012491540X", "9004037810", "0256016054")
EX.invalid.10.digit.isbns <- c("0124915401", "9004037811", "0256016051", "0256X01605")
EX.9.digit.isbns <- c("012491540", "900403781", "025601605")

EX.hairy.isbn.10s <- c("01249-^@^1540X",         # has nonsense but good
                       "01249-^@^15402",         # has nonsense and bad
                       "9004037810",             # good
                       "898477250",              # good if x added
                       "900403781",              # too short and bad
                       "1249-^@^1540X",          # good if leading 0 is added
                       # OMG BOTH SIDES ARE DIFFERENT
                       "95700683539570068361",   # good if divided in half
                       "95700683539570068361",   # good if divided in half
                       NA,                       # NA (obviously)
                       "90040X3781"              # no
)

EX.valid.13.digit.isbns <- c("9780306406157", "9783161484100")
EX.12.digit.isbns <- c("978030640615", "978316148410")
EX.invalid.13.digit.isbns <- c("978316__84101", "9780306406151")




##############################################
###               ISBN 10                  ###
##############################################

# get_isbn_10_check_digit succeeds properly
test_that("get_isbn_10_check_digit() succeeds properly", {
  expect_equal(get_isbn_10_check_digit(EX.valid.10.digit.isbns[1]), "X")
  expect_equal(get_isbn_10_check_digit(EX.valid.10.digit.isbns), c("X", "0", "4"))
  expect_equal(get_isbn_10_check_digit(EX.9.digit.isbns), c("X", "0", "4"))
  expect_equal(get_isbn_10_check_digit(c(EX.valid.10.digit.isbns, NA)),
               c("X", "0", "4", NA))
  expect_equal(get_isbn_10_check_digit(EX.invalid.10.digit.isbns, errors.as.nas=TRUE),
               c("X", "0", "4", NA))
  expect_equal(get_isbn_10_check_digit("0-124-91540-X", allow.hyphens=TRUE), "X")
})

# get_isbn_10_check_digit fails properly
test_that("get_isbn_10_check_digit() fails properly", {
  expect_error(get_isbn_10_check_digit(123456789),
               "Input must be a character string")
  expect_error(get_isbn_10_check_digit("123456"),
               "Input must be either 9 or 10 characters")
  expect_error(get_isbn_10_check_digit(EX.invalid.10.digit.isbns),
               "Illegal input")
})


# check_isbn_10_check_digit succeeds properly
test_that("check_isbn_10_check_digit() succeeds properly", {
  expect_equal(check_isbn_10_check_digit(EX.valid.10.digit.isbns[1]),
               TRUE)
  expect_equal(check_isbn_10_check_digit(EX.valid.10.digit.isbns),
               c(TRUE, TRUE, TRUE))
  expect_equal(check_isbn_10_check_digit(EX.invalid.10.digit.isbns),
               c(FALSE, FALSE, FALSE, FALSE))
  expect_equal(check_isbn_10_check_digit(c(EX.valid.10.digit.isbns,
                                           EX.invalid.10.digit.isbns)),
               c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(check_isbn_10_check_digit(c(12, 12), errors.as.false = TRUE),
               c(FALSE, FALSE))
  expect_equal(check_isbn_10_check_digit(c(EX.valid.10.digit.isbns,
                                           "hubo un tiempo"),
                                         errors.as.false = TRUE),
               c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(check_isbn_10_check_digit("0-124-91540-X"), TRUE)
  expect_equal(check_isbn_10_check_digit("0-124-91540-X", allow.hyphens=FALSE),
               FALSE)
})

# check_isbn_10_check_digit fails properly
test_that("check_isbn_10_check_digit() fails properly", {
  expect_equal(check_isbn_10_check_digit(c(EX.valid.10.digit.isbns, NA)),
               c(TRUE, TRUE, TRUE, NA))
  expect_error(check_isbn_10_check_digit(0123456789, errors.as.false=FALSE),
               "Input must be a character string")
  expect_error(check_isbn_10_check_digit("123456789", errors.as.false=FALSE),
               "Illegal input")
  expect_error(check_isbn_10_check_digit("0-124-91540-X", allow.hyphens=FALSE,
                                         errors.as.false=FALSE),
               "Illegal input")
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
               c(FALSE, FALSE, FALSE, FALSE))
  expect_equal(is_valid_isbn_10(c(EX.valid.10.digit.isbns,
                                           EX.invalid.10.digit.isbns)),
               c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(is_valid_isbn_10(c(EX.valid.10.digit.isbns, "hubo un tiempo")),
               c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(is_valid_isbn_10("0-124-91540-X"), TRUE)
  expect_equal(is_valid_isbn_10("0-124-91540-1"), FALSE)
  expect_equal(is_valid_isbn_10("0-124-91540-X", allow.hyphens=FALSE), FALSE)
})

test_that("is_valid_isbn_10() fails properly", {
  expect_equal(is_valid_isbn_10(c(EX.valid.10.digit.isbns, NA)),
               c(TRUE, TRUE, TRUE, NA))
  expect_error(is_valid_isbn_10(0123456789),
               "Input must be a character string")
})


# normalize ISBN 10
test_that("normalize_isbn_10() succeeds properly", {
  expect_equal(normalize_isbn_10(EX.hairy.isbn.10s),
               c("012491540X", NA, "9004037810", "898477250X",
                 NA, "012491540X", "9570068353", "9570068353", NA, NA))
  expect_equal(normalize_isbn_10(EX.hairy.isbn.10s, aggresive=FALSE),
               c("012491540X", NA, "9004037810", NA,
                 NA, NA, NA, NA, NA, NA))
})

# this can't really fail
# test_that("normalize_isbn_10 fails properly", {
#
# })

# ------------------------------------------ #

##############################################
###               ISBN 13                  ###
##############################################

# get_isbn_13_check_digit succeeds properly
test_that("get_isbn_13_check_digit() succeeds properly", {
  expect_equal(get_isbn_13_check_digit(EX.valid.13.digit.isbns[1]), "7")
  expect_equal(get_isbn_13_check_digit(EX.valid.13.digit.isbns), c("7", "0"))
  expect_equal(get_isbn_13_check_digit(EX.12.digit.isbns), c("7", "0"))
  expect_equal(get_isbn_13_check_digit(c(EX.valid.13.digit.isbns, NA)),
               c("7", "0", NA))
  expect_equal(get_isbn_13_check_digit(EX.invalid.13.digit.isbns, errors.as.nas=TRUE),
               c(NA, "7"))
  expect_equal(get_isbn_13_check_digit("978-0-306-40615-7", allow.hyphens=TRUE), "7")
})

# get_isbn_13_check_digit fails properly
test_that("get_isbn_13_check_digit() fails properly", {
 expect_error(get_isbn_13_check_digit(1234567891234),
              "Input must be a character string")
 expect_error(get_isbn_13_check_digit("123456"),
              "Input must be either 12 or 13 characters")
 expect_error(get_isbn_13_check_digit(EX.invalid.13.digit.isbns),
              "Illegal input")
 expect_error(get_isbn_13_check_digit("978-0-306-40615-7"),
              "Input must be either 12 or 13 characters")
})


# check_isbn_13_check_digit succeeds properly
test_that("get_isbn_13_check_digit() succeeds properly", {
  expect_equal(check_isbn_13_check_digit(EX.valid.13.digit.isbns[1]),
               TRUE)
  expect_equal(check_isbn_13_check_digit(EX.valid.13.digit.isbns),
               c(TRUE, TRUE))
  expect_equal(check_isbn_13_check_digit(EX.invalid.13.digit.isbns),
               c(FALSE, FALSE))
  expect_equal(check_isbn_13_check_digit(c(EX.valid.13.digit.isbns,
                                           EX.invalid.13.digit.isbns)),
               c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(check_isbn_13_check_digit(c(12, 12), errors.as.false = TRUE),
               c(FALSE, FALSE))
  expect_equal(check_isbn_13_check_digit(c(EX.valid.13.digit.isbns,
                                           "hubo un tiempo"),
                                         errors.as.false = TRUE),
               c(TRUE, TRUE, FALSE))
  expect_equal(check_isbn_13_check_digit("978-0-306-40615-7"), TRUE)
  expect_equal(check_isbn_13_check_digit("978-0-306-40615-7", allow.hyphens=FALSE),
               FALSE)
})

# check_isbn_13_check_digit fails properly
test_that("check_isbn_13_check_digit() fails properly", {
  expect_equal(check_isbn_13_check_digit(c(EX.valid.13.digit.isbns, NA)),
               c(TRUE, TRUE, NA))
  expect_error(check_isbn_13_check_digit(0123456789, errors.as.false=FALSE),
               "Input must be a character string")
  expect_error(check_isbn_13_check_digit("978-0-306-40615-", errors.as.false=FALSE),
               "Illegal input")
  expect_error(check_isbn_13_check_digit("978-0-306-40615-7", allow.hyphens=FALSE,
                                         errors.as.false=FALSE),
               "Illegal input")
})

# is_valid_isbn_13
test_that("is_valid_isbn_13() succeeds properly", {
  expect_equal(is_valid_isbn_13(EX.valid.13.digit.isbns[1]),
               TRUE)
  expect_equal(is_valid_isbn_13(EX.valid.13.digit.isbns),
               c(TRUE, TRUE))
  expect_equal(is_valid_isbn_13(EX.invalid.13.digit.isbns),
               c(FALSE, FALSE))
  expect_equal(is_valid_isbn_13(c(EX.valid.13.digit.isbns,
                                  EX.invalid.13.digit.isbns)),
               c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(is_valid_isbn_13(c(EX.valid.13.digit.isbns, "hubo un tiempo")),
               c(TRUE, TRUE, FALSE))
  expect_equal(is_valid_isbn_13("978-0-306-40615-7"), TRUE)
  expect_equal(is_valid_isbn_13("978-0-306-40615-1"), FALSE)
  expect_equal(is_valid_isbn_13("978-0-306-40615-7", allow.hyphens=FALSE), FALSE)
})

test_that("is_valid_isbn_13() fails properly", {
  expect_equal(is_valid_isbn_13(c(EX.valid.13.digit.isbns, NA)),
               c(TRUE, TRUE, NA))
  expect_error(is_valid_isbn_13(0123456789),
               "Input must be a character string")
})


# ------------------------------------------ #





##############################################
###                 ISSN                   ###
##############################################

# get_issn_check_digit succeeds properly
test_that("get_issn_check_digit() succeeds properly", {
  expect_equal(get_issn_check_digit("03785955"), "5")
  expect_equal(get_issn_check_digit(c("0378595", "2434561X", NA)),
               c("5", "X", NA))
  expect_equal(get_issn_check_digit(c("0378595", "2434-561X", NA),
                                    allow.hyphens=TRUE),
               c("5", "X", NA))
})

# get_issn_check_digit fails properly
test_that("get_issn_check_digit() fails properly", {
  expect_error(get_issn_check_digit(03785955),
               "Input must be a character string")
  expect_error(get_issn_check_digit("03785955432"),
               "Input must be either 7 or 8 characters")
})



# check_issn_check_digit succeeds properly
test_that("get_issn_check_digit() succeeds properly", {
  expect_equal(check_issn_check_digit(c("03785955", "2434561X", NA)),
               c(TRUE, TRUE, NA))
  expect_equal(check_issn_check_digit(c("03785954", "2434-561X", NA)),
               c(FALSE, TRUE, NA))
  expect_equal(check_issn_check_digit(c("03785954", "________", NA),
                                      errors.as.false=TRUE),
               c(FALSE, FALSE, NA))
})

# check_issn_check_digit fails properly
test_that("get_issn_check_digit() fails properly", {
  expect_error(check_issn_check_digit(c(12345678)),
               "Input must be a character string")
  expect_error(check_issn_check_digit(c("03785954", "2434-561X", NA),
                                      allow.hyphens=FALSE),
               "Illegal input")
})

# ------------------------------------------ #


