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

# Tests # https://r-pkgs.org/tests.html
# usethis::use_testthat() # setting up the tests structure

# Test coverage of the package
covr::package_coverage()

# library(checkmate)
# ?build_site

