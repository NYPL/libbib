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
                       "95700683539570068361",   # good if divided in half
                       "95700683539570068361",   # good if divided in half
                       NA,                       # NA (obviously)
                       "90040X3781",             # no
                       "32132012491540X23464"    # ok but hidden
)

EX.valid.13.digit.isbns <- c("9780306406157", "9783161484100")
EX.12.digit.isbns <- c("978030640615", "978316148410")
EX.invalid.13.digit.isbns <- c("978316__84101", "9780306406151")

EX.hairy.isbn.13s <- c("9789668197918",
                       "978966819^*@!)X7918",
                       NA,
                       "9770800783197708007912",
                       "97815724115799781572411579",
                       "",
                       "9789668197911")



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
  expect_equal(get_isbn_10_check_digit(NA), as.character(NA))
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
  expect_equal(check_isbn_10_check_digit(NA, errors.as.false = FALSE), as.character(NA))
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
  expect_equal(is_valid_isbn_10(NA), as.character(NA))
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
                 NA, "012491540X", "9570068353", "9570068353", NA, NA,
                 "012491540X"))
  expect_equal(normalize_isbn_10(EX.hairy.isbn.10s, aggressive=FALSE),
               c("012491540X", NA, "9004037810", NA,
                 NA, NA, NA, NA, NA, NA, NA))
  expect_equal(normalize_isbn_10("012491540x"),
               "012491540X")
  expect_equal(normalize_isbn_10("012491540x xe32ea"),
               "012491540X")
  expect_equal(normalize_isbn_10("012491540x", convert.to.isbn.13=TRUE),
               "9780124915404")
  expect_equal(normalize_isbn_10(c("513213012491540x", "012491540x", NA)),
               c("012491540X", "012491540X", NA))
  expect_equal(normalize_isbn_10(c("513213012491540x", "012491540x", NA), aggressive=FALSE),
               c(NA, "012491540X", NA))
  expect_equal(normalize_isbn_10(NA), as.character(NA))
})

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
  expect_equal(get_isbn_13_check_digit(NA), as.character(NA))
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
test_that("check_isbn_13_check_digit() succeeds properly", {
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
  expect_equal(check_isbn_13_check_digit(NA, errors.as.false = FALSE), as.character(NA))
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
  expect_equal(is_valid_isbn_13(NA), as.character(NA))
})

test_that("is_valid_isbn_13() fails properly", {
  expect_equal(is_valid_isbn_13(c(EX.valid.13.digit.isbns, NA)),
               c(TRUE, TRUE, NA))
  expect_error(is_valid_isbn_13(0123456789),
               "Input must be a character string")
})


# normalize ISBN 13
test_that("normalize_isbn_13() succeeds properly", {
  expect_equal(normalize_isbn_13(EX.hairy.isbn.13s),
               c("9789668197918", "9789668197918", NA, NA, "9781572411579", NA, NA))
  expect_equal(normalize_isbn_13(EX.hairy.isbn.13s, aggressive=FALSE),
               c("9789668197918", "9789668197918", NA, NA, NA, NA, NA))
  expect_equal(normalize_isbn_13(NA), as.character(NA))
})



# convert_to_isbn_13
test_that("convert_to_isbn_13() succeeds properly", {
  expect_equal(convert_to_isbn_13(c("012491540X", "9004037810")),
                                  c("9780124915404", "9789004037816"))
  expect_equal(convert_to_isbn_13(c("012491540X", "9004037810", NA)),
               c("9780124915404", "9789004037816", NA))
  expect_equal(convert_to_isbn_13("0124915401", errors.as.nas=TRUE),
               c(NA))
  expect_equal(convert_to_isbn_13(NA, errors.as.nas=TRUE),
               as.character(c(NA)))
  expect_equal(convert_to_isbn_13("9780124915404", skip.validity.check=TRUE),
               "9789780124915")
  expect_equal(convert_to_isbn_13(NA, skip.validity.check=TRUE),
               as.character(c(NA)))

})

test_that("convert_to_isbn_13() fails properly", {
  expect_error(convert_to_isbn_13("0124915401"),
               "Invalid ISBN 10 detected")
  expect_error(convert_to_isbn_13(123),
               "Input must be a character string")
})

# ------------------------------------------ #



# ------------------------------------------ #


##############################################
###             GENERAL ISBN               ###
##############################################


# normalize ISBN (general)
test_that("normalize_isbn() succeeds properly", {
  expect_equal(normalize_isbn(c(EX.hairy.isbn.10s, EX.hairy.isbn.13s)),
               c(c("012491540X", NA, "9004037810", "898477250X",
                   NA, "012491540X", "9570068353", "9570068353", NA, NA,
                   "012491540X"),
                 c("9789668197918", "9789668197918", NA, "9770800783",
                   "9781572411579", NA, NA)))
  expect_equal(normalize_isbn(NA), as.character(NA))
})






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
  expect_equal(get_issn_check_digit(NA), as.character(NA))
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
  expect_equal(check_issn_check_digit(NA), as.character(NA))
})

# check_issn_check_digit fails properly
test_that("get_issn_check_digit() fails properly", {
  expect_error(check_issn_check_digit(c(12345678)),
               "Input must be a character string")
  expect_error(check_issn_check_digit(c("03785954", "2434-561X", NA),
                                      allow.hyphens=FALSE),
               "Illegal input")
})

# is_valid_issn
test_that("is_valid_issn() works", {
  expect_equal(is_valid_issn("2434561X"), TRUE)
  expect_equal(is_valid_issn("2434-561X"), TRUE)
  expect_equal(is_valid_issn(c("2434-561X", "2434-5611", "0378-5955", NA)),
               c(TRUE, FALSE, TRUE, NA))
})

# normalize_issn
test_that("normalize_issn() works", {
  expect_equal(normalize_issn(c("__2434__561X", NA, "2434561",
                                "21335212434561X")),
               c("2434561X", NA, "2434561X", "2434561X"))
  expect_equal(normalize_issn(3785955), "03785955")
})

# ------------------------------------------ #


##############################################
###                 LCCN                   ###
##############################################

test_that("normalize_lccn() succeeds properly", {
  expect_equal(normalize_lccn("n 78890351 "), "n78890351")
  expect_equal(normalize_lccn(" 85000002 "), "85000002")
  expect_equal(normalize_lccn(" 79139101 /AC/r932"), "79139101")
  expect_equal(normalize_lccn("1‡a   2014356397"), NA_character_)
  expect_equal(normalize_lccn("A   2014356397"), NA_character_)
  expect_equal(normalize_lccn("98114143 /MN"), "98114143")
  expect_equal(normalize_lccn("sa 65001662"), "sa65001662")
  expect_equal(normalize_lccn("2003306761"), "2003306761")
  expect_equal(normalize_lccn("###78890351#"), "78890351")
  expect_equal(normalize_lccn("^^^78890351^"), "78890351")
  expect_equal(normalize_lccn("i was happy for a day in 1975"), NA_character_)

  # hyphen fails
  expect_equal(normalize_lccn("n78-890351"), "n78890351")
  expect_equal(normalize_lccn("n78-89035"), "n78089035")
  expect_equal(normalize_lccn("85-2 "), "85000002")
  expect_equal(normalize_lccn("2001-000002"), "2001000002")
  expect_equal(normalize_lccn("75-425165//r75"), "75425165")
  expect_equal(normalize_lccn("75-425165//r75", allow.hyphens = FALSE),
               NA_character_)

  expect_error(normalize_lccn(8675309), "Input must be a character string")
  expect_equal(normalize_lccn(NA), NA_character_)

  # test vectorized
  expect_equal(normalize_lccn(c("1‡a   2014356397", "n 78890351 ")),
               c(NA_character_, "n78890351"))
  expect_equal(normalize_lccn(c("85-2 ", " 79139101 /AC/r932", "n 78890351 ")),
               c("85000002", "79139101", "n78890351"))
  expect_equal(normalize_lccn(c("85-2 ", " 79139101 /AC/r932", "n 78890351 "),
                              allow.hyphens=FALSE),
               c(NA_character_, "79139101", "n78890351"))
  expect_equal(normalize_lccn(c("85-8675309 ", " 79139101 /AC/r932", "n 78890351 ")),
               c(NA_character_, "79139101", "n78890351"))

})


