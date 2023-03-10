# Tests for valid outputs ----

# Output should be a data frame with 5 columns
# test_that("data frame output format", {
#   ex <- GIFT_taxgroup(work_ID = c(1:5), taxon_lvl = "family")
#   
#   expect_identical(class(ex), "character")
#   expect_identical(length(ex), c(5L))
#   
# })

# Tests for invalid inputs ----
test_that("invalid inputs", {
  # Error message when ref_ID and list_ID are missing
  expect_error(
    GIFT_taxgroup(work_ID = c(1:5), api = NA),
    "api must be a character string indicating which API to use.",
    fixed = TRUE)
  
  expect_error(
    GIFT_taxgroup(work_ID = c(1:5), GIFT_version = NA),
    "'GIFT_version' must be a character string stating what version
of GIFT you want to use. Available options are 'latest', 'beta' and the 
different named stable versions of GIFT.",
    fixed = TRUE)

})
