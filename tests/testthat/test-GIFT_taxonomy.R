# Tests for valid outputs ----

# Output should be a dataframe with 6 columns
test_that("data frame output format", {
  ex <- GIFT_taxonomy()
  
  expect_s3_class(ex, "data.frame")
  expect_identical(ncol(ex), c(6L))
  
})

# Tests for invalid inputs ----

# Error message when ref_ID and list_ID are missing
expect_error(
  GIFT_taxonomy(api = 1),
  "api must be a character string indicating which API to use.", fixed = TRUE)
