# Tests for valid outputs ----

# Output should be a list with two data frames
test_that("data frame output format", {
  ex <- GIFT_phylogeny(taxon_name = "Tracheophyta", as_newick = FALSE,
                       GIFT_version = "beta")
  
  expect_identical(class(ex), "data.frame")
  expect_identical(length(ex), c(5L))
  
})

# Tests for invalid inputs ----
test_that("invalid inputs", {
  expect_error(
    GIFT_phylogeny(taxon_name =  NA, GIFT_version = "beta"),
    "'taxon_name' is incorrect. It must be a character string among one of
         the taxonomic groups available in GIFT. To check them all, run
         'GIFT_taxonomy()'.", fixed = TRUE)
  
  expect_error(
    GIFT_phylogeny(api = 1, GIFT_version = "beta"),
    "api must be a character string indicating which API to use.",
    fixed = TRUE)
  
  expect_error(
    GIFT_phylogeny(GIFT_version = 1),
    "'GIFT_version' must be a character string stating what version
of GIFT you want to use. Available options are 'latest' and the different
versions.",
    fixed = TRUE)
  
  expect_error(
    GIFT_phylogeny(as_newick = "zz", GIFT_version = "beta"),
    "'as_newick' must be a boolean stating whether you want to retrieve
         the phylogeny in a Newick format.", fixed = TRUE)

})
