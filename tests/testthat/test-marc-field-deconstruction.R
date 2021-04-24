context("checking functions in marc-field-deconstruction.R")


data("books_serials_etc_sample")

# marc_leader_get_info
test_that("marc_leader_get_info() works", {
  expect_equal(books_serials_etc_sample[, marc_leader_get_info(marc_leader)][,record_type],
               books_serials_etc_sample[,record_type])
  expect_equal(books_serials_etc_sample[, marc_leader_get_info(marc_leader)][,bib_level],
               books_serials_etc_sample[,bib_level])
})

# marc_008_get_info
test_that("marc_008_get_info() works", {
  expect_equal(books_serials_etc_sample[, marc_008_get_info(marc_008, original.pub.date=TRUE)][,pub_date],
               books_serials_etc_sample[,pubyear2])
  expect_equal(books_serials_etc_sample[, marc_008_get_info(marc_008)][,lang_code],
               books_serials_etc_sample[,language])
})

