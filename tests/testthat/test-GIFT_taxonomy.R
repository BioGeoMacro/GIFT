# Tests for valid outputs ----

# Output should be a dataframe with 6 columns
test_that("data frame output format", {
  ex <- GIFT_taxonomy()
  
  expect_s3_class(ex, "data.frame")
  expect_identical(ncol(ex), c(6L))
  
})
