# Tests for valid outputs ----

# Output should be a list with two data frames
test_that("data frame output format", {
  ex <-
    GIFT_species_lookup(genus = "Fagus", epithet = "sylvatica")
  
  expect_s3_class(ex, "data.frame")
  expect_identical(ncol(ex), c(19L))
})

# Tests for invalid inputs ----
test_that("invalid inputs", {
  expect_error(
    GIFT_species_lookup(genus = 1),
    "genus must be a character string indicating genus to look for.",
    fixed = TRUE)
  
  expect_error(
    GIFT_species_lookup(api = 1),
    "api must be a character string indicating which API to use.",
    fixed = TRUE)
  
  expect_error(
    GIFT_species_lookup(GIFT_version = 1),
    "'GIFT_version' must be a character string stating what version
of GIFT you want to use. Available options are 'latest' and the different
versions.",
    fixed = TRUE)
})
