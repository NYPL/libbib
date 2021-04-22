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


test_that("dt_add_to_col_names() works", {
  expect_equal(names(dt_add_to_col_names(data.table(iris), "_post")),
               c("Sepal.Length_post", "Sepal.Width_post", "Petal.Length_post",
                 "Petal.Width_post", "Species_post"))
  expect_equal(names(dt_add_to_col_names(data.table(iris), "pre_", prefix=TRUE)),
               c("pre_Sepal.Length", "pre_Sepal.Width", "pre_Petal.Length",
                 "pre_Petal.Width", "pre_Species"))
  expect_equal(names(dt_add_to_col_names(data.table(iris), "pre_", prefix=TRUE,
                                         exclude=c("Species", "Sepal.Length"))),
               c("Sepal.Length", "pre_Sepal.Width", "pre_Petal.Length",
                 "pre_Petal.Width", "Species"))
  expect_equal(names(dt_add_to_col_names(data.table(iris), "pre_", prefix=TRUE,
                                         include=c("Sepal.Width", "Petal.Length",
                                                   "Petal.Width"))),
               c("Sepal.Length", "pre_Sepal.Width", "pre_Petal.Length",
                 "pre_Petal.Width", "Species"))
  expect_error(dt_add_to_col_names(iris, "hi"),
               "DT must be a data.table")
  expect_warning(dt_add_to_col_names(data.table(iris), "hi", include=c("people", "streets")),
               '') #'Columns ("people", "streets") are missing from supplied data.table')
  expect_warning(dt_add_to_col_names(data.table(iris), "hi", exclude=c("people", "streets")),
               '') #'Columns ("people", "streets") are missing from supplied data.table')
  expect_warning(dt_add_to_col_names(data.table(iris), "hi", exclude="people"),
               '') #'Columns ("people") are missing from supplied data.table')
  expect_error(dt_add_to_col_names(data.table(iris), exclude="bs", include="gs"),
               "cannot have both 'include' and 'exclude' parameters at the same time")
})


