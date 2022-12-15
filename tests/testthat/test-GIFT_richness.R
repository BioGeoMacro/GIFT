# Tests for valid outputs ----

# Output should be a dataframe with 5 columns
test_that("data frame output format", {
  ex <- GIFT_richness()
  
  expect_s3_class(ex, "data.frame")
  expect_identical(ncol(ex), c(5L))
  
})

# Tests for invalid inputs ----
test_that("invalid inputs", {
  # Error message when ref_ID and list_ID are missing
  expect_error(
    GIFT_richness(what = NA),
    "'what' is incorrect. It must be a character string equal either to
         'species_richness' or 'trait_coverage'.", fixed = TRUE)
  
  expect_error(
    GIFT_richness(taxon_name = NA),
    "'taxon_name' is incorrect. It must be a character string among one of
         the taxonomic groups available in GIFT. To check them all, run
         'GIFT_taxonomy()'.", fixed = TRUE)
  
  expect_error(
    GIFT_richness(trait_ID = NA),
    "'trait_ID' is incorrect. It must be a character string of the
    identification number of a trait. To check these IDs, run
         'GIFT_traits_meta()'.", fixed = TRUE)
  
  expect_error(
    GIFT_richness(api = NA),
    "api must be a character string indicating which API to use.",
    fixed = TRUE)
  
  expect_error(
    GIFT_richness(GIFT_version = NA),
    "'GIFT_version' must be a character string stating what version
of GIFT you want to use. Available options are 'latest' and the different
versions.",
    fixed = TRUE)
  
  expect_message(GIFT_richness(GIFT_version = "beta"),
                 "You are asking for the beta-version of GIFT which is subject to
updates and edits. Consider using 'latest' for the latest stable
version.")
})
