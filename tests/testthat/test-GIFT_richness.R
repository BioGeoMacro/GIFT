# Tests for valid outputs ----

# Output should be a data frame with 5 columns
# test_that("data frame output format", {
#   
#   expect_message(ex <- GIFT_richness(GIFT_version = "beta"),
#                  "You are asking for the beta-version of GIFT which is subject to
# updates and edits. Consider using 'latest' for the latest stable
# version.")
#   expect_s3_class(ex, "data.frame")
#   expect_identical(ncol(ex), c(5L))
#   
# })

# Tests for invalid inputs ----
test_that("invalid inputs", {
  expect_error(
    GIFT_richness(taxon_name = NA),
    "'taxon_name' is incorrect. It must be a character string among one of
         the taxonomic groups available in GIFT. To check them all, run
         'GIFT_taxonomy()'.", fixed = TRUE)
  
  expect_error(
    GIFT_richness(api = NA),
    "api must be a character string indicating which API to use.",
    fixed = TRUE)
  
  expect_error(
    GIFT_richness(GIFT_version = NA),
    "'GIFT_version' must be a character string stating what version
of GIFT you want to use. Available options are 'latest', 'beta' and the 
different named stable versions of GIFT.",
    fixed = TRUE)
  
})
