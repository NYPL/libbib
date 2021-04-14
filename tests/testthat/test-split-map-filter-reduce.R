context("checking functions in split-map-filter-reduce.R")


##############################################
###               SETUP                    ###
##############################################


ejemplo <- c("9782711875177;garbage-isbn;2711875172;2844268900",
             "1861897952; 978-1-86189-795-4",
             "1861897952     978-1-86189-795-4")

ejemplo2 <- c("9782711875177;garbage-isbn;2711875172;2844268900",
              NA, "")


# --------------------------------------------------------------- #


test_that("car() works", {
  expect_equal(car(c(8, 6, 7, 5, 3, 0, 9)), 8)
  # expect_equal(car(c(NA, 6, 7, 5, 3, 0, 9)), NA)
  expect_equal(car(NA), NA)
  expect_equal(car(""), "")
})


test_that("remove_duplicates_and_nas() works", {
  expect_equal(remove_duplicates_and_nas(c(8, 6, 7, 5, 3, 0, 9, 6, NA, 3)),
               c(8, 6, 7, 5, 3, 0, 9))
  expect_equal(remove_duplicates_and_nas(1), 1)
  expect_equal(remove_duplicates_and_nas(c(NA, NA, NA)), NA)
})


test_that("recombine_with_sep_closure() works", {
  expect_equal(recombine_with_sep_closure()(c(8,6,7)), "8;6;7")
  expect_equal(recombine_with_sep_closure()(c()), "")
  expect_equal(recombine_with_sep_closure()(NA), NA)
  expect_equal(recombine_with_sep_closure(sep=" ")(c("this", "that", NA,"the-other")),
               "this that NA the-other")
})


test_that("split_map_filter_reduce() works", {
  expect_equal(split_map_filter_reduce(ejemplo),
               c("9782711875177", "1861897952",
                 "1861897952     978-1-86189-795-4"))

  expect_equal(split_map_filter_reduce(ejemplo, sep="\\s+"),
               c("9782711875177;garbage-isbn;2711875172;2844268900",
                 "1861897952; 978-1-86189-795-4",
                 "1861897952     978-1-86189-795-4"))

  expect_equal(split_map_filter_reduce(ejemplo, sep="\\s+", fixed=FALSE),
               c("9782711875177;garbage-isbn;2711875172;2844268900",
                 "1861897952;", "1861897952"))

  expect_equal(split_map_filter_reduce(ejemplo, sep="\\s+", fixed=FALSE,
                          reduxfun=recombine_with_sep_closure(" ")),
               c("9782711875177;garbage-isbn;2711875172;2844268900",
                 "1861897952; 978-1-86189-795-4", "1861897952 978-1-86189-795-4"))

  expect_equal(split_map_filter_reduce(ejemplo,
                          mapfun=function(x){normalize_isbn(x, convert.to.isbn.13=TRUE)},
                          reduxfun=recombine_with_sep_closure(" ")),
               c("9782711875177 NA 9782711875177 9782844268907",
                 "9781861897954 9781861897954", "9781861897954"))

  expect_equal(split_map_filter_reduce(ejemplo,
                          mapfun=function(x){normalize_isbn(x, convert.to.isbn.13=TRUE)},
                          filterfun=remove_duplicates_and_nas,
                          reduxfun=recombine_with_sep_closure(" ")),
               c("9782711875177 9782844268907", "9781861897954", "9781861897954"))

  expect_equal(split_map_filter_reduce(ejemplo2,
                                       mapfun=function(x){normalize_isbn(x, convert.to.isbn.13=TRUE)},
                                       filterfun=remove_duplicates_and_nas,
                                       reduxfun=recombine_with_sep_closure(" ")),
               c("9782711875177 9782844268907", NA, NA))
})




