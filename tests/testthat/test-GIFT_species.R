# Tests for valid outputs ----

# Output should be a data frame with 5 columns
# test_that("data frame output format", {
#   ex <- GIFT_species()
#   
#   expect_s3_class(ex, "data.frame")
#   expect_identical(ncol(ex), c(5L))
#   
# })

# Tests for invalid inputs ----
test_that("invalid inputs", {
  # Error message when ref_ID and list_ID are missing
  expect_error(
    GIFT_species(api = NA),
    "api must be a character string indicating which API to use.",
    fixed = TRUE)
  
  expect_error(
    GIFT_species(GIFT_version = NA),
    "'GIFT_version' must be a character string stating what version
of GIFT you want to use. Available options are 'latest', 'beta' and the 
different named stable versions of GIFT.",
    fixed = TRUE)
})
