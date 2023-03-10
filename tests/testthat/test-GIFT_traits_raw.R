# Tests for valid outputs ----

# Output should be a data frame with 28 columns
# test_that("data frame output format", {
#   
#   expect_message(
#     ex <- GIFT_traits_raw(trait_IDs = c("4.10.1", "4.16.1"),
#                           GIFT_version = "beta"),
#     "You are asking for the beta-version of GIFT which is subject to
# updates and edits. Consider using 'latest' for the latest stable
# version.")
#   expect_s3_class(ex, "data.frame")
#   expect_identical(ncol(ex), c(28L))
#   
# })

# Tests for invalid inputs ----
test_that("invalid inputs", {
  expect_error(
    GIFT_traits_raw(bias_deriv = NA),
    "bias_deriv must be a logical.",
    fixed = TRUE)
  
  expect_error(
    GIFT_traits_raw(bias_ref = NA),
    "bias_ref must be a logical.",
    fixed = TRUE)
  
  expect_error(
    GIFT_traits_raw(trait_IDs = "4.10.1", api = NA),
    "api must be a character string indicating which API to use.",
    fixed = TRUE)
  
  expect_error(
    GIFT_traits_raw(trait_IDs = "4.10.1", GIFT_version = NA),
    "'GIFT_version' must be a character string stating what version
of GIFT you want to use. Available options are 'latest', 'beta' and the 
different named stable versions of GIFT.",
    fixed = TRUE)
  
})
