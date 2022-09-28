# Tests for valid outputs ----

# Output should be a dataframe with 5 columns
test_that("data frame output format", {
  ex <- GIFT_traits_meta()
  
  expect_s3_class(ex, "data.frame")
  expect_identical(ncol(ex), c(10L))
  
})

# Tests for invalid inputs ----

# Error message when ref_ID and list_ID are missing
expect_error(
  GIFT_traits_meta(api = NA),
  "api must be a character string indicating which API to use.", fixed = TRUE)

expect_error(
  GIFT_traits_meta(GIFT_version = NA),
  "'GIFT_version' must be a character string stating what version
    of GIFT you want to use. Available options are 'latest' and the different
           versions.", fixed = TRUE)
