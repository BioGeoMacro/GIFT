# Tests for valid outputs ----
# Western Mediterranean basin shape
data("western_mediterranean")

# Output should be a list with two data frames
# test_that("data frame output format", {
#   
#   expect_message(
#     ex <- GIFT_spatial(shp = western_mediterranean,
#                        overlap = "centroid_inside",
#                        GIFT_version = "beta"),
#     "You are asking for the beta-version of GIFT which is subject to
# updates and edits. Consider using 'latest' for the latest stable
# version.")
#   
#   expect_s3_class(ex, "data.frame")
#   expect_identical(ncol(ex), c(3L))
#   
# })

# Tests for invalid inputs ----
test_that("data frame output format", {
  expect_error(
    GIFT_spatial(shp = NULL, coordinates = NULL),
    "Please provide a shapefile or a set of XY coordinates.",
    fixed = TRUE)
  
  expect_error(
    GIFT_spatial(shp = western_mediterranean, overlap = "error"),
    "'overlap' must be a character string indicating whether you want to 
    use the centroid or extent of the GIFT polygons to overlap with your 
    shapefile or coordinates.\n 
    It has to be 'centroid_inside', 'shape_inside', 'shape_intersect' or
    'extent_intersect'.",
    fixed = TRUE)
  
  expect_error(
    GIFT_spatial(shp = western_mediterranean, api = NA),
    "api must be a character string indicating which API to use.",
    fixed = TRUE)
  
  expect_error(
    GIFT_spatial(shp = western_mediterranean, GIFT_version = NA),
    "'GIFT_version' must be a character string stating what version
of GIFT you want to use. Available options are 'latest', 'beta' and the 
different named stable versions of GIFT.",
    fixed = TRUE)
  
})
