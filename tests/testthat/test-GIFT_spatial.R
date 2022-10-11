# Tests for valid outputs ----

# Output should be a list with two data frames
test_that("data frame output format",
          {
            data("med")
            ex <-
              GIFT_spatial(shp = med, overlap = "centroid_inside")
            
            expect_s3_class(ex, "data.frame")
            expect_identical(ncol(ex), c(3L))
            
          })

# Tests for invalid inputs ----

expect_error(
  GIFT_spatial(shp = NULL, coordinates = NULL),
  "Please provide a shapefile or a set of XY coordinates.", fixed = TRUE)
