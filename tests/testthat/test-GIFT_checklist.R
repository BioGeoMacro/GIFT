# Tests for valid outputs ----

# Output should be a list with two data frames
test_that("data frame output format", {
  data("western_mediterranean")
  ex <- GIFT_checklist(shp = western_mediterranean,
                       overlap = "centroid_inside",
                       taxon_name = "Angiospermae")
  
  expect_identical(class(ex), "list")
  expect_identical(length(ex), c(2L))
  
})

# Tests for invalid inputs ----
test_that("invalid inputs", {
  expect_error(
    GIFT_checklist(taxon_name =  NA),
    "'taxon_name' is incorrect. It must be a character string among one of
         the taxonomic groups available in GIFT. To check them all, run
         'GIFT_taxonomy()'.", fixed = TRUE)
  
  expect_error(
    GIFT_checklist(complete_taxon = NA),
    "'complete_taxon' must be a boolean stating whether you want to
    retrieve checklists that only contain the exhaustive list of the
    'taxon_name' argument or as well incomplete lists.", fixed = TRUE)
  
  expect_error(
    GIFT_checklist(floristic_group = NA),
    "'floristic_group' must be a character string. Available options are
    'all', 'native', 'endemic' and 'naturalized'.", fixed = TRUE)
  
  expect_error(
    GIFT_checklist(complete_floristic = NA),
    "'complete_floristic' must be a boolean stating whether you want to
    retrieve checklists that only contain the exhaustive list of the
    'floristic_group' argument or as well incomplete lists.", fixed = TRUE)
  
  expect_error(
    GIFT_checklist(geo_type = NA),
    "'geo_type' must be a character string stating what geographic
    type you want to retrieve. Available options are 'Mainland', 'Island' or
    'Mainland, Island').", fixed = TRUE)
  
  expect_error(
    GIFT_checklist(overlap = NA),
    "overlap is a character string indicating whether you want to use
         centroid or extent of GIFT polygons to overlap with your shapefile.\n
         It has to be 'centroid_inside', 'shape_inside', 'shape_intersect' or
         'extent_intersect'.", fixed = TRUE)
  
  expect_error(
    GIFT_checklist(ref_excluded = NA),
    "'ref_excluded' must be a character string or a numeric stating the
         identification numbers of the references (ref_ID) that shall be 
         ignored.", fixed = TRUE)
  
  expect_error(
    GIFT_checklist(remove_overlap = NA),
    "'remove_overlap' must be a boolean stating whether you want to
    retrieve checklists that overlap or not.", fixed = TRUE)
  
  expect_error(
    GIFT_checklist(suit_geo = NA),
    "'suit_geo' must be a boolean stating whether you want to
    retrieve only suitable polygons or not.", fixed = TRUE)
  
  expect_error(
    GIFT_checklist(api = 1),
    "api must be a character string indicating which API to use.",
    fixed = TRUE)
  
  expect_error(
    GIFT_checklist(GIFT_version = 1),
    "'GIFT_version' must be a character string stating what version
of GIFT you want to use. Available options are 'latest' and the different
versions.",
    fixed = TRUE)

  expect_error(
    GIFT_checklist(remove_overlap = TRUE, area_threshold_island = NULL),
    "'area_threshold_island' is a surface in km^2 indicating from which
    surface the smallest overlapping polygon is kept.", fixed = TRUE)
  
  expect_error(
    GIFT_checklist(remove_overlap = TRUE, area_threshold_mainland = NULL),
    "'area_threshold_mainland' is a surface in km^2 indicating from which
    surface the smallest overlapping polygon is kept.", fixed = TRUE)
  
  expect_error(
    GIFT_checklist(remove_overlap = TRUE, overlap_threshold = NULL),
    "'overlap_threshold' is a number ranging from 0 to 1, indicating at
    what percentage of overlap, partially overlapping polygons should be
         kept.", fixed = TRUE)
  
  expect_error(
    GIFT_checklist(by_ref_ID = NA),
    "'by_ref_ID' must be a boolean stating whether indicating whether the
         removal of overlapping regions shall be applied only at the
         reference level.", fixed = TRUE)
  
})
