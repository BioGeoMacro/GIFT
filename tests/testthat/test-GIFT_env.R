# Tests for valid outputs ----

# Output should be a dataframe with 5 columns
test_that("data frame output format", {
  ex <- GIFT_env(miscellaneous = "perimeter")
  
  expect_s3_class(ex, "data.frame")
  expect_identical(ncol(ex), c(3L))
  
})

# Tests for invalid inputs ----
test_that("invalid inputs", {
  # Error message when ref_ID and list_ID are missing
  expect_error(
    GIFT_env(api = NA),
    "api must be a character string indicating which API to use.",
    fixed = TRUE)
  
  expect_error(
    GIFT_env(sumstat = NA),
    'sumstat needs to be a character vector including one or more of the 
         following items: c("min", "q05", "q10", "q20", "q25", "q30", "q40", 
         "med", "q60", "q70", "q75", "q80", "q90", "q95", "max", "mean", "sd", 
         "modal", "unique_n", "H", "n")',
    fixed = TRUE)
  
  expect_error(
    GIFT_env(GIFT_version = NA),
    "'GIFT_version' must be a character string stating what version
of GIFT you want to use. Available options are 'latest' and the different
versions.",
    fixed = TRUE)
  
  expect_message(GIFT_env(GIFT_version = "beta"),
                 "You are asking for the beta-version of GIFT which is subject to
updates and edits. Consider using 'latest' for the latest stable
version.")
  
  expect_error(
    GIFT_env(miscellaneous = "XX"),
    "'miscellaneous' must be a character string stating what
           miscellaneous variable(s) you want to retrieve. Run
           GIFT_env_meta_misc() to see available options.",
    fixed = TRUE)
  
  expect_error(
    GIFT_env(rasterlayer = "XX"),
    "'rasterlayer' must be a character string stating what
           raster layer(s) you want to retrieve data from. Run
           GIFT_env_meta_raster() to see available options.",
    fixed = TRUE)
})
