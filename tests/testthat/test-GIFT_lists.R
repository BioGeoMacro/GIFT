# Tests for valid outputs ----

# Output should be a data frame with 15 columns
test_that("data frame output format", {
  expect_message(ex <- GIFT_lists(GIFT_version = "beta"),
                 "You are asking for the beta-version of GIFT which is subject to
updates and edits. Consider using 'latest' for the latest stable
version.")

  expect_s3_class(ex, "data.frame")
  expect_identical(ncol(ex), c(15L))

})

# Tests for invalid inputs ----
test_that("invalid inputs", {
  # Error message when ref_ID and list_ID are missing
  expect_error(
    GIFT_lists(api = NA),
    "api must be a character string indicating which API to use.", fixed = TRUE)
  
  expect_error(
    GIFT_lists(GIFT_version = NA),
    "'GIFT_version' must be a character string stating what version
of GIFT you want to use. Available options are 'latest', 'beta' and the 
different named stable versions of GIFT.",
    fixed = TRUE)
  
})
