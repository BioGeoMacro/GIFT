# Tests for valid outputs ----

# Output should be a dataframe with 5 columns
test_that("data frame output format", {
  ex <- GIFT_version()
  
  expect_s3_class(ex, "data.frame")
  expect_identical(ncol(ex), c(3L))
  
})
