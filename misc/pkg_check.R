devtools::load_all()

# To generate the documentation of functions and NAMESPACE
roxygen2::roxygenise()

# Build the website
# usethis::use_pkgdown() # Run once to configure your package to use pkgdown
pkgdown::build_site()

# Misspellings
devtools::spell_check()

# Practice improvements
goodpractice::gp()

# Tests # https://r-pkgs.org/testing-basics.html
# usethis::use_testthat() # setting up the tests structure
# Open a new test file
usethis::use_test("name_of_the_function_to_test")

# Test coverage of the package
covr::package_coverage()
devtools::test()

# library(checkmate)
# ?build_site

# Checking the package
devtools::check()
