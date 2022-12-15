# Tests for valid outputs ----

# Output should be a vector
test_that("length of format inferior or equal to length of output", {
            ex <- GIFT_no_overlap(entity_IDs = c(1, 2))
            expect_identical(length(ex), c(2L))
            
            ex2 <- GIFT_no_overlap(entity_IDs = c(10071, 12078))
            expect_identical(length(ex2), c(1L))
            
})

# Tests for invalid inputs ----
test_that("invalid inputs", {
  # Error message when ref_ID and list_ID are missing
  expect_error(
    GIFT_no_overlap(entity_IDs = NULL),
    "Please provide the ID numbers of the regions you want to check the
         overlap of.", fixed = TRUE)
  
  expect_error(
    GIFT_no_overlap(entity_IDs = c(1, 2), area_th_island = NULL),
    "'area_th_island' is a surface in km^2 indicating from which
    surface the smallest overlapping polygon is kept.", fixed = TRUE)
  
  expect_error(
    GIFT_no_overlap(entity_IDs = c(1, 2), area_th_mainland = NULL),
    "'area_th_mainland' is a surface in km^2 indicating from which
    surface the smallest overlapping polygon is kept.", fixed = TRUE)
  
  expect_error(
    GIFT_no_overlap(entity_IDs = c(1, 2), overlap_th = NULL),
    "'overlap_th' is a number ranging from 0 to 1, indicating at what 
         percentage of overlap, partially overlapping polygons should be
         kept.", fixed = TRUE)
  
  expect_error(
    GIFT_no_overlap(entity_IDs = c(1, 2), api = NA),
    "api must be a character string indicating which API to use.",
    fixed = TRUE)
  
  expect_error(
    GIFT_no_overlap(entity_IDs = c(1, 2), GIFT_version = NA),
    "'GIFT_version' must be a character string stating what version
of GIFT you want to use. Available options are 'latest' and the different
versions.",
    fixed = TRUE)
})
