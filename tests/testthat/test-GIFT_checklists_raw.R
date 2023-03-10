# Tests for valid outputs ----

# Output should be a data frame with 33 columns when namesmatched = TRUE
# test_that("data frame output format 2", {
#   ex <- GIFT_checklists_raw(list_ID = c(5), namesmatched = TRUE)
#   
#   expect_s3_class(ex, "data.frame")
#   expect_identical(ncol(ex), c(33L)) # 16 cols if namesmatched = FALSE
#   
# })

# Tests for invalid inputs ----
test_that("invalid inputs", {
  # Error message when ref_ID and list_ID are missing
  expect_error(
    GIFT_checklists_raw(ref_ID = NULL, list_ID = NULL),
    "Please provide the ID numbers of the references and/or lists you want 
         to load.",
    fixed = TRUE)
  
  expect_error(
    GIFT_checklists_raw(list_ID = 5, api = 1),
    "api must be a character string indicating which API to use.",
    fixed = TRUE)
  
  expect_error(
    GIFT_checklists_raw(list_ID = 5, GIFT_version = 1),
    "'GIFT_version' must be a character string stating what version
of GIFT you want to use. Available options are 'latest', 'beta' and the 
different named stable versions of GIFT.",
    fixed = TRUE)
  
  expect_error(
    GIFT_checklists_raw(list_ID = 5, taxon_name =  NA),
    "'taxon_name' is incorrect. It must be a character string among one of
         the taxonomic groups available in GIFT. To check them all, run
         'GIFT_taxonomy()'.", fixed = TRUE)
})
