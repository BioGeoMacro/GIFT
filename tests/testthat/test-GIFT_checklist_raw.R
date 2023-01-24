# Tests for valid outputs ----

# Output should be a dataframe with 14 columns when namesmatched = FALSE
test_that("data frame output format", {
  ex <- GIFT_checklist_raw(list_ID = c(5), namesmatched = FALSE)
  
  expect_s3_class(ex, "data.frame")
  expect_identical(ncol(ex), c(16L))
  
})

# Output should be a dataframe with 24 columns when namesmatched = TRUE
test_that("data frame output format 2", {
  ex <- GIFT_checklist_raw(list_ID = c(5), namesmatched = TRUE)
  
  expect_s3_class(ex, "data.frame")
  expect_identical(ncol(ex), c(33L))
  
})

# Tests for invalid inputs ----
test_that("invalid inputs", {
  # Error message when ref_ID and list_ID are missing
  expect_error(
    GIFT_checklist_raw(ref_ID = NULL, list_ID = NULL),
    "Please provide the ID numbers of the references and/or lists you want 
         to load.",
    fixed = TRUE)
  
  expect_error(
    GIFT_checklist_raw(list_ID = 5, api = 1),
    "api must be a character string indicating which API to use.",
    fixed = TRUE)
  
  expect_error(
    GIFT_checklist_raw(list_ID = 5, GIFT_version = 1),
    "'GIFT_version' must be a character string stating what version
of GIFT you want to use. Available options are 'latest' and the different
versions.",
    fixed = TRUE)
  
  expect_error(
    GIFT_checklist_raw(list_ID = 5, taxon_name =  NA),
    "'taxon_name' is incorrect. It must be a character string among one of
         the taxonomic groups available in GIFT. To check them all, run
         'GIFT_taxonomy()'.", fixed = TRUE)
})
