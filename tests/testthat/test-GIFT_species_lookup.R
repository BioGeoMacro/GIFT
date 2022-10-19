# Tests for valid outputs ----

# Output should be a list with two data frames
test_that("data frame output format",
          {
            ex <-
              GIFT_species_lookup(genus = "Fagus", epithet = "sylvatica")
            
            expect_s3_class(ex, "data.frame")
            expect_identical(ncol(ex), c(24L))
            
          })

# Tests for invalid inputs ----

expect_error(
  GIFT_species_lookup(genus = 1),
  "genus must be a character string indicating genus to look for.",
  fixed = TRUE)
