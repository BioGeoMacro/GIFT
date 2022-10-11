# Tests for valid outputs ----

# Output should be a list with two data frames
test_that("data frame output format",
          {
            data("med")
            ex <-
              GIFT_checklist(shp = med, overlap = "centroid_inside",
                             taxon_name = "Angiospermae")
            
            expect_s3_class(ex, "list")
            expect_identical(length(ex), c(2L))
            
          })

# Tests for invalid inputs ----

expect_error(
  GIFT_checklist(taxon_name =  NA),
  "'taxon_name' is incorrect. It must be a character string among one of
         the taxonomic groups available in GIFT. To check them all, run
         'GIFT_taxonomy()'.", fixed = TRUE)
