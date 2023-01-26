# Tests for valid outputs ----

# Output should be a dataframe with 5 columns
test_that("data frame output format", {
  ex <- GIFT_traits(trait_IDs = "1.10.2", agreement = 0.66,
                    bias_ref = FALSE, bias_deriv = FALSE)
  
  expect_s3_class(ex, "data.frame")
  expect_identical(ncol(ex), c(7L))
  
})

# Tests for invalid inputs ----
test_that("invalid inputs", {
  expect_error(
    GIFT_traits(bias_deriv = NA),
    "bias_deriv must be a logical.", fixed = TRUE)
  
  expect_error(
    GIFT_traits(bias_ref = NA),
    "bias_ref must be a logical.", fixed = TRUE)
  
  # Error message when ref_ID and list_ID are missing
  expect_error(
    GIFT_traits(trait_IDs = "1.1.1", api = NA),
    "api must be a character string indicating which API to use.",
    fixed = TRUE)
  
  expect_error(
    GIFT_traits(trait_IDs = "1.1.1", GIFT_version = NA),
    "'GIFT_version' must be a character string stating what version
of GIFT you want to use. Available options are 'latest' and the different
versions.", fixed = TRUE)
  
  expect_message(
    GIFT_traits(trait_IDs = "1.10.2", GIFT_version = "beta"),
    "You are asking for the beta-version of GIFT which is subject to
updates and edits. Consider using 'latest' for the latest stable
version.")
})
