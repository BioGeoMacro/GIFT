# Tests for valid outputs ----

# Output should be a data frame with 3 columns
test_that("data frame output format", {
  ex <- GIFT_versions()
  
  expect_s3_class(ex, "data.frame")
  expect_identical(ncol(ex), c(6L))
  
})
