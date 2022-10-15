# Tests for valid outputs ----

# Output should be a dataframe with 5 columns
test_that("data frame output format", {
  ex <- GIFT_shape(entity_ID = c(677, 200))
  
  expect_identical(ncol(ex), c(21L))
  
})

# Tests for invalid inputs ----

# Error message when ref_ID and list_ID are missing
expect_error(
  GIFT_shape(api = NA),
  "api must be a character string indicating which API to use.", fixed = TRUE)

expect_error(
  GIFT_shape(GIFT_version = NA),
  "'GIFT_version' must be a character string stating what version
    of GIFT you want to use. Available options are 'latest' and the different
           versions.", fixed = TRUE)

expect_message(GIFT_shape(GIFT_version = "beta"),
               "You are asking for the beta-version of GIFT which is subject to
            updates and edits. Consider using 'latest' for the latest stable
            version.")
