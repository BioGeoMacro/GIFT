# Tests for valid outputs ----

# Output should be a list with two data frames
# test_that("data frame output format", {
#   ex <- GIFT_phylogeny(clade = "Tracheophyta", as_tree = FALSE,
#                        GIFT_version = "beta")
#   
#   expect_identical(class(ex), "data.frame")
#   expect_identical(length(ex), c(5L))
#   
# })

# Tests for invalid inputs ----
test_that("invalid inputs", {
  expect_error(
    GIFT_phylogeny(clade =  NA, GIFT_version = "beta"),
    "'clade' must be a character string corresponding to the node 
         labels in the phylogeny. Not all major taxonomic groups are 
         labelled.", fixed = TRUE)
  
  expect_error(
    GIFT_phylogeny(api = 1, GIFT_version = "beta"),
    "api must be a character string indicating which API to use.",
    fixed = TRUE)
  
  expect_error(
    GIFT_phylogeny(GIFT_version = 1),
    "'GIFT_version' must be a character string stating what version
of GIFT you want to use. Available options are 'latest', 'beta' and the 
different named stable versions of GIFT.",
    fixed = TRUE)
  
  expect_error(
    GIFT_phylogeny(as_tree = "zz", GIFT_version = "beta"),
    "'as_tree' must be a logical stating whether you want to retrieve
         the phylogeny as a tree object.", fixed = TRUE)
  
  expect_error(
    GIFT_phylogeny(return_work_ID = "zz", GIFT_version = "beta"),
    "'return_work_ID' must be a logical stating whether you want to
    get the species names or their work_ID as tip labels.", fixed = TRUE)
  
  expect_error(
    GIFT_phylogeny(work_ID_subset = "zz", GIFT_version = "beta"),
    "work_ID_subset must be a numeric vector listing the work_ID you
           want to use to prune the phylogenetic tree.", fixed = TRUE)
  
})
