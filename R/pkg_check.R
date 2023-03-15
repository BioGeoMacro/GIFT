# devtools::load_all()

# To generate the documentation of functions and NAMESPACE
# roxygen2::roxygenise()

# Create Rmd README
# usethis::use_readme_rmd()
# Render it to README .md
# devtools::build_readme()

# Build the website
# usethis::use_pkgdown() # Run once to configure your package to use pkgdown
# pkgdown::build_site()

# Misspellings
# devtools::spell_check()
# spelling::spell_check_package()

# Practice improvements
# goodpractice::gp()

# Tests # https://r-pkgs.org/testing-basics.html
# usethis::use_testthat() # setting up the tests structure
# Open a new test file
# usethis::use_test("name_of_the_function_to_test")

# Test coverage of the package
# covr::package_coverage()
# devtools::test()

# library(checkmate)
# ?build_site

# Checking the package
# devtools::check()
# devtools::check(args = "--as-cran")

# CRAN specific requirements
# rhub::check_for_cran()
# devtools::check_rhub()

# Check win-builder
# devtools::check_win_devel()

# Update your manuals
# devtools::document()

# Create tar.gz
# devtools::build()

# Markdown in the development of the pages
# usethis::use_roxygen_md()

# Hexagon logo
# hexSticker::sticker("man/figures/GIFT.png",
#                     package = "", p_size = 20,
#                     s_x = 1, s_y = 1, s_width = 0.8,
#                     h_fill = "white",
#                     filename = "man/figures/GIFT_hexlogo.png")

# Favicon for the website
# pkgdown::build_favicons(pkg = ".", overwrite = FALSE)

# When resubmitting:
# \donttest in the function headers and not \dontrun
# resetting graphical parameters e.g. par_overlap_shp <- par(mfrow = c(1, 1))
# par(par_overlap_shp)
# overall check time ~10mins
