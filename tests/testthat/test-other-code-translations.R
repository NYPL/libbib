context("checking functions in other-code-translations.R")



# get_language_from_code
test_that("get_language_from_code() works properly", {
  expect_equal(get_language_from_code("yor"), "Yoruba")
  expect_equal(get_language_from_code(c("yor", " SPA")),
               c("Yoruba", "Spanish"))
  expect_equal(get_language_from_code(c("yor", " SPA", "not-a-language", "nah", NA)),
               c("Yoruba", "Spanish", NA, "Nahuatl", NA))
  expect_equal(get_language_from_code(""), NA_character_)
  expect_equal(get_language_from_code(NA), NA_character_)
})

# get_country_from_code
test_that("get_country_from_code() works properly", {
  expect_equal(get_country_from_code("ck"), "Colombia")
  expect_equal(get_country_from_code(c(" PE", "nyu")), c("Peru", "New York (State)"))
  expect_equal(get_country_from_code(c(" PE", NA, "uv", "not-a-country", "nyu")),
               c("Peru", NA, "Burkina Faso", NA, "New York (State)"))
  expect_equal(get_country_from_code(""), NA_character_)
  expect_equal(get_country_from_code(NA), NA_character_)
})

