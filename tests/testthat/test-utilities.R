context("checking functions in utilities.R")


##############################################
###               SETUP                    ###
##############################################

mt <- as.data.table(mtcars)
iris_dt <- as.data.table(iris)
mt_some_na <- copy(mt)
mt_some_na[mpg<16, mpg:=NA]

ejemplo <- as.data.table(iris)
setnames(ejemplo, c("Sepal Length", "Sepal@Width", "Petal Length",
                    "Petal\\nWidth", "Spêcies"))

# --------------------------------------------------------------- #


test_that("dt_del_cols() works", {
  expect_error(dt_del_cols(mtcars, "cyl"),
    regexp="DT must be a data.table object",
    fixed=TRUE)
})

test_that("dt_keep_cols() works", {
  expect_error(dt_keep_cols(mtcars, "cyl"),
    regexp="DT must be a data.table object",
    fixed=TRUE)
})

test_that("dt_counts_and_percents() works", {
  expect_error(dt_counts_and_percents(iris, "Species"),
    regexp="DT must be a data.table object",
    fixed=TRUE)
})


test_that("get_clean_names() works", {
  expect_equal(get_clean_names(ejemplo),
               c("Sepal_Length", "Sepal_Width", "Petal_Length",
                 "Petal_nWidth", "Sp_cies"))
})

test_that("dt_set_clean_names() works", {
  expect_error(dt_set_clean_names(iris),
    regexp="DT must be a data.table object",
    fixed=TRUE)
})


test_that("dt_percent_not_na() works", {
  # expect_equal(dt_percent_not_na(mt, "mpg"), 68.75)
  expect_error(dt_percent_not_na(iris, "Species"),
    regexp="DT must be a data.table object",
    fixed=TRUE)
})


