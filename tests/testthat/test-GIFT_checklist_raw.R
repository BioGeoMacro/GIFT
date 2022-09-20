# Tests for valid outputs ----

# Output should be a dataframe with 14 columns when namesmatched = FALSE
test_that("data frame output format", {
  ex <- GIFT_checklist_raw(list_ID = c(5), namesmatched = FALSE)
  
  expect_s3_class(ex, "data.frame")
  expect_identical(ncol(ex), c(14L))
  
})

# Output should be a dataframe with 24 columns when namesmatched = TRUE
test_that("data frame output format 2", {
  ex <- GIFT_checklist_raw(list_ID = c(5), namesmatched = TRUE)
  
  expect_s3_class(ex, "data.frame")
  expect_identical(ncol(ex), c(24L))
  
})


# Tests for invalid inputs ----

# Error message when ref_ID and list_ID are missing
expect_error(
  GIFT_checklist_raw(ref_ID = NULL, list_ID = NULL),
  "Please provide the ID numbers of the references and/or lists you want 
         to load.", fixed = TRUE
)

