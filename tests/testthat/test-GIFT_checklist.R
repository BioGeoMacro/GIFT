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
    "'complete_taxon' must be a logical stating whether you want to
    retrieve checklists for regions for which the 'taxon_name' argument is 
    entirely covered or also regions for which only a subset of the 
    the 'taxon_name' is covered.", fixed = TRUE)
  
  expect_error(
    GIFT_checklist(floristic_group = NA),
    "'floristic_group' must be a character string. Available options are
    'all', 'native', 'endemic' and 'naturalized'.", fixed = TRUE)
  
  expect_error(
    GIFT_checklist(complete_floristic = NA),
    "'complete_floristic' must be a logical stating whether you want to
    retrieve checklists only for regions for which the 'floristic_group' is 
    entirely covered or also regions for which only a subset of the 
    the 'floristic_group' is covered.", fixed = TRUE)
  
  expect_error(
    GIFT_checklist(geo_type = NA),
    "'geo_type' must be a character string stating for what geographic
    type of regions you want to retrieve checklists. Available options are 
    'Mainland', 'Island' and 'Mainland, Island').", fixed = TRUE)
  
  expect_error(
    GIFT_checklist(overlap = NA),
    "'overlap' must be a character string indicating whether you want to 
    use the centroid or extent of the GIFT polygons to overlap with your 
    shapefile or coordinates.\n 
    It has to be 'centroid_inside', 'shape_inside', 'shape_intersect' or
    'extent_intersect'.", fixed = TRUE)
  
  expect_error(
    GIFT_checklist(ref_excluded = NA),
    "'ref_excluded' must be a character string or a numeric giving the
         identification numbers of references (ref_ID) that shall be 
         ignored.", fixed = TRUE)
  
  expect_error(
    GIFT_checklist(remove_overlap = NA),
    "'remove_overlap' must be a logical stating whether you want to
    remove overlapping regions and their checklists from the output.",
    fixed = TRUE)
  
  expect_error(
    GIFT_checklist(suit_geo = NA),
    "'suit_geo' must be a logical stating whether only regions classified 
    as suit_geo should be considered.", fixed = TRUE)
  
  expect_error(
    GIFT_checklist(api = 1),
    "api must be a character string indicating which API to use.",
    fixed = TRUE)
  
  expect_error(
    GIFT_checklist(GIFT_version = 1),
    "'GIFT_version' must be a character string stating what version
of GIFT you want to use. Available options are 'latest', 'beta' and the 
different named stable versions of GIFT.",
    fixed = TRUE)

  expect_error(
    GIFT_checklist(remove_overlap = TRUE, area_threshold_island = NULL),
    "'area_threshold_island' must be a numeric value indicating the 
    area threshold in km^2 above which smaller islands are preferred over 
    larger islands.", fixed = TRUE)
  
  expect_error(
    GIFT_checklist(remove_overlap = TRUE, area_threshold_mainland = NULL),
    "'area_threshold_mainland' must be a numeric value indicating the 
    area threshold in km^2 above which smaller regions are preferred over 
    larger regions.", fixed = TRUE)
  
  expect_error(
    GIFT_checklist(remove_overlap = TRUE, overlap_threshold = NULL),
    "'overlap_threshold' is a number ranging from 0 to 1, indicating above
    what proportion of overlap, only one of the partially overlapping polygons 
    should be kept.", fixed = TRUE)
  
  expect_error(
    GIFT_checklist(by_ref_ID = NA),
    "'by_ref_ID' must be a boolean indicating whether the
         removal of overlapping regions shall be applied only at the
         reference level (i.e. within references).", fixed = TRUE)
  
  expect_error(
    GIFT_checklist(taxonomic_group = NA),
    "'taxonomic_group' must be a logical. When set to TRUE, two additional
    columns ('family' and 'tax_group') will be available in the checklists.",
    fixed = TRUE)
  
  expect_error(
    GIFT_checklist(namesmatched = NA),
    "'namesmatched' must be a logical stating whether you only want the
    standardized species names or if you also want to retrieve original species 
    names and information on the name matching.",
    fixed = TRUE)
  
  expect_error(
    GIFT_checklist(list_set_only = NA),
    "'list_set_only' must be a logical stating whether you only want 
    metadata of the checklists or if you also want to retrieve the species 
    lists.",
    fixed = TRUE)
  
  expect_error(
    GIFT_checklist(shp = NA),
    "'shp' must be an object of classes 'sf' and 'data.frame', with a CRS
         set to WGS84 (EPSG: 4326).",
    fixed = TRUE)
})
