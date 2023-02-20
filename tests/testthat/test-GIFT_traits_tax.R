# Tests for valid outputs ----

# Output should be a dataframe with 5 columns
test_that("data frame output format", {
  ex <- GIFT_traits_tax(trait_IDs = "1.4.1", bias_ref = FALSE,
                        bias_deriv = FALSE)
  
  expect_s3_class(ex, "data.frame")
  expect_identical(ncol(ex), c(6L))
  
})

# Tests for invalid inputs ----
test_that("invalid inputs", {
  expect_error(
    GIFT_traits_tax(trait_IDs = NA),
    "trait_IDs must be a character string indicating which trait you want
         to retrieve.", fixed = TRUE)
  
  expect_error(
    GIFT_traits_tax(bias_deriv = NA),
    "bias_deriv must be a logical.", fixed = TRUE)
  
  expect_error(
    GIFT_traits_tax(bias_ref = NA),
    "bias_ref must be a logical.", fixed = TRUE)
  
  # Error message when ref_ID and list_ID are missing
  expect_error(
    GIFT_traits_tax(trait_IDs = "1.1.1", api = NA),
    "api must be a character string indicating which API to use.",
    fixed = TRUE)
  
  expect_error(
    GIFT_traits_tax(trait_IDs = "1.1.1", GIFT_version = NA),
    "'GIFT_version' must be a character string stating what version
of GIFT you want to use. Available options are 'latest', 'beta' and the 
different named stable versions of GIFT.", fixed = TRUE)
  
  expect_message(
    GIFT_traits_tax(trait_IDs = "1.1.1", GIFT_version = "beta"),
    "You are asking for the beta-version of GIFT which is subject to
updates and edits. Consider using 'latest' for the latest stable
version.")
})
