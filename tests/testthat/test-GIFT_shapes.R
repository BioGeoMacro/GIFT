# Tests for valid outputs ----

# Output should be a data frame with 13 columns
test_that("data frame output format", {
  
  expect_message(
    ex <- GIFT_shapes(entity_ID = c(677, 200),
                      GIFT_version = "beta"),
    "You are asking for the beta-version of GIFT which is subject to
updates and edits. Consider using 'latest' for the latest stable
version.")
  
  expect_identical(ncol(ex), c(13L))
  
})

# Tests for invalid inputs ----
test_that("invalid inputs", {
  # Error message when ref_ID and list_ID are missing
  expect_error(
    GIFT_shapes(entity_ID = c(677, 200), api = NA),
    "api must be a character string indicating which API to use.", fixed = TRUE)
  
  expect_error(
    GIFT_shapes(entity_ID = c(677, 200), GIFT_version = NA),
    "'GIFT_version' must be a character string stating what version
of GIFT you want to use. Available options are 'latest', 'beta' and the 
different named stable versions of GIFT.",
    fixed = TRUE)
  
})
