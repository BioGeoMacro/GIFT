# Tests for valid outputs ----

# Output should be a dataframe with 5 columns
test_that("data frame output format", {
  ex <- GIFT_coverage()
  
  expect_s3_class(ex, "data.frame")
  expect_identical(ncol(ex), c(9L))
  
})

# Tests for invalid inputs ----
test_that("invalid inputs", {
  # Error message when ref_ID and list_ID are missing
  expect_error(
    GIFT_coverage(what = NA),
    "'what' is incorrect. It must be a character string equal to either 
         'taxonomic_coverage' or 'trait_coverage'.", fixed = TRUE)
  
  expect_error(
    GIFT_coverage(taxon_name = NA),
    "'taxon_name' is incorrect. It must be a character string among one of
         the taxonomic groups available in GIFT. To check them all, run
         'GIFT_taxonomy()'.", fixed = TRUE)
  
  expect_error(
    GIFT_coverage(trait_ID = NA),
    "'trait_ID' is incorrect. It must be a character string of the
    identification number of a trait. To check these IDs, run
         'GIFT_traits_meta()'.", fixed = TRUE)
  
  expect_error(
    GIFT_coverage(trait_ID = c("1.1.1", "1.2.1")),
    "Please provide one trait_ID only.", fixed = TRUE)
  
  expect_error(
    GIFT_coverage(api = NA),
    "api must be a character string indicating which API to use.",
    fixed = TRUE)
  
  expect_error(
    GIFT_coverage(GIFT_version = NA),
    "'GIFT_version' must be a character string stating what version
of GIFT you want to use. Available options are 'latest', 'beta' and the 
different named stable versions of GIFT.",
    fixed = TRUE)
  
  expect_message(GIFT_coverage(GIFT_version = "beta"),
                 "You are asking for the beta-version of GIFT which is subject to
updates and edits. Consider using 'latest' for the latest stable
version.")
})
