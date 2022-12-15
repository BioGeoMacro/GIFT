# Tests for valid outputs ----

# Output should be a dataframe with 5 columns
test_that("data frame output format", {
  ex <- GIFT_overlap()
  
  expect_s3_class(ex, "data.frame")
  expect_identical(ncol(ex), c(4L))
  
})

# Tests for invalid inputs ----
test_that("invalid inputs", {
expect_error(
  GIFT_overlap(resource = NA),
  "resource must be a character string indicating from which external
         resource you want to calculate the spatial overlap. Available options
         are 'glonaf' or 'gmba'.", fixed = TRUE)

expect_error(
  GIFT_overlap(api = NA),
  "api must be a character string indicating which API to use.", fixed = TRUE)

expect_error(
  GIFT_overlap(GIFT_version = NA),
  "'GIFT_version' must be a character string stating what version
of GIFT you want to use. Available options are 'latest' and the different
versions.",
  fixed = TRUE)
})
