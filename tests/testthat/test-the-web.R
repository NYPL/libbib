context("checking functions in the-web.R")



# loc_permalink_from_lccn
test_that("loc_permalink_from_lccn() works", {
  expect_equal(loc_permalink_from_lccn("n78-890351"),
               "https://lccn.loc.gov/n78890351")
  expect_equal(loc_permalink_from_lccn("85-2 "),
               "https://lccn.loc.gov/85000002")
  expect_equal(loc_permalink_from_lccn(c("n78-890351", "85-2 ")),
               c("https://lccn.loc.gov/n78890351",
                 "https://lccn.loc.gov/85000002"))
  expect_equal(loc_permalink_from_lccn(c("###78890351#", NA, "n78-890351")),
               c("https://lccn.loc.gov/78890351", NA,
                 "https://lccn.loc.gov/n78890351"))

  expect_error(loc_permalink_from_lccn(123456789),
               "Input must be a character string")

  expect_equal(loc_permalink_from_lccn("73167510", format="marcxml"),
               "https://lccn.loc.gov/73167510/marcxml")
  expect_equal(loc_permalink_from_lccn("73167510", format="mods"),
               "https://lccn.loc.gov/73167510/mods")
  expect_equal(loc_permalink_from_lccn("73167510", format="mads"),
               "https://lccn.loc.gov/73167510/mads")
  expect_equal(loc_permalink_from_lccn("73167510", format="dublin"),
               "https://lccn.loc.gov/73167510/dc")
  expect_error(loc_permalink_from_lccn("73167510", format="nonsense"),
               'format must be one of "", "marcxml", "mods", "mads", or "dublin"')
})


# worldcat_permalink_from_issn
test_that("worldcat_permalink_from_issn() works", {
  expect_equal(worldcat_permalink_from_issn(c("0968-1221", NA, "2434-561X")),
               c("http://www.worldcat.org/issn/09681221", NA,
                 "http://www.worldcat.org/issn/2434561X"))
  expect_equal(worldcat_permalink_from_issn(c("0968-1221", NA, "2434-561X"),
                                            normalize=FALSE),
               c("http://www.worldcat.org/issn/0968-1221", NA,
                 "http://www.worldcat.org/issn/2434-561X"))

  expect_error(worldcat_permalink_from_issn(123456789),
               "Input must be a character string")
})


# worldcat_permalink_from_isbn
test_that("worldcat_permalink_from_isbn() works", {
  expect_equal(worldcat_permalink_from_isbn(c("1788393724", NA,
                                              "0-124-91540-X")),
               c("http://www.worldcat.org/isbn/1788393724", NA,
                 "http://www.worldcat.org/isbn/012491540X"))
  expect_equal(worldcat_permalink_from_isbn(c("1788393724", NA,
                                              "0-124-91540-X"),
                                            normalize=FALSE),
               c("http://www.worldcat.org/isbn/1788393724", NA,
                 "http://www.worldcat.org/isbn/0-124-91540-X"))

  expect_error(worldcat_permalink_from_isbn(123456789),
               "Input must be a character string")
})


# worldcat_permalink_from_oclc_number
test_that("worldcat_permalink_from_oclc_number() works", {

  expect_equal(worldcat_permalink_from_oclc_number(c("1049727704", NA,
                                                     "1005106045")),
               c("http://www.worldcat.org/oclc/1049727704", NA,
                 "http://www.worldcat.org/oclc/1005106045"))
  expect_equal(worldcat_permalink_from_oclc_number("733889
  7"), "http://www.worldcat.org/oclc/7338897")
  expect_error(worldcat_permalink_from_oclc_number(123456789),
               "Input must be a character string")
})


# oclc_classify_link_from_standard_num
test_that("oclc_classify_link_from_standard_num() works", {
  expect_equal(oclc_classify_link_from_standard_num("629725006"),
               "http://classify.oclc.org/classify2/ClassifyDemo?search-standnum-txt=629725006&startRec=0")
  expect_equal(oclc_classify_link_from_standard_num(c("039333712X", NA, "629725006")),
               c("http://classify.oclc.org/classify2/ClassifyDemo?search-standnum-txt=039333712X&startRec=0",
                 NA, "http://classify.oclc.org/classify2/ClassifyDemo?search-standnum-txt=629725006&startRec=0"))
  expect_equal(oclc_classify_link_from_standard_num("733889
  7"), "http://classify.oclc.org/classify2/ClassifyDemo?search-standnum-txt=7338897&startRec=0")
  expect_equal(oclc_classify_link_from_standard_num(NA),
               NA_character_)
  expect_error(oclc_classify_link_from_standard_num(1),
               "Input must be a character string")
})

