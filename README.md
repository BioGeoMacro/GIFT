
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GIFT <img src="man/figures/GIFT_hexlogo.png" align="right" alt="" width="200" />

<!-- badges: start -->

[![licence](https://img.shields.io/badge/Licence-GPL--3-blue.svg)](https://www.r-project.org/Licenses/GPL-3)
[![R build
status](https://github.com/BioGeoMacro/GIFT/workflows/R-CMD-check/badge.svg)](https://github.com/BioGeoMacro/GIFT/actions)
<!-- badges: end -->

This **R package** includes several functions to extract data from the
Global Inventory of Floras and Traits (**GIFT**) database. <br> **GIFT**
is a global database of plant checklists, covering several taxonomic
groups and providing information about the floristic status and
functional traits of plants as well as environmental information for
each checklist. <br> More details about the content of this database are
available in the publication of [Weigelt et al.,
(2021)](https://doi.org/10.1111/jbi.13623).

# :arrow_double_down: Installation

The package is not on CRAN yet and is still under active development.
You can install the development version from the GitHub repository with
the following command:

``` r
# install.packages("devtools")
devtools::install_github("https://github.com/BioGeoMacro/GIFT")
library("GIFT")
```

# :scroll: Vignettes

We wrote several vignettes that will help you using the **GIFT R
package**. So far, three vignettes are available: <br>

- **[Main
  tutorial](https://biogeomacro.github.io/GIFT/articles/GIFT.html)**  
- **[For advanced
  users](https://biogeomacro.github.io/GIFT/articles/GIFT_advanced_users.html)**  
- **[API](https://biogeomacro.github.io/GIFT/articles/GIFT_API.html)**

Alternatively, if you prefer to view the vignettes in R, you could
install the package with `build_vignettes = TRUE`. But be aware that
some vignettes are very slow to generate (the main one especially).

``` r
remotes::install_github("https://github.com/BioGeoMacro/GIFT", 
                        dependencies = TRUE, upgrade = "ask", 
                        build_vignettes = TRUE)

vignette("GIFT")
```

# :desktop_computer: Functions

An overview of all functions and data is given
**[here](https://biogeomacro.github.io/GIFT/reference/index.html)**.

# :bug: Find a bug?

Thank you for finding it. Head over to the GitHub Issues tab and let us
know about it. Alternatively, you can also send us an e-mail. We will
try to get to it as soon as we can!

# References and dependencies

`GIFT` depends on `dplyr`, `jsonlite`, `purrr`, `sf`, `stats`, `tidyr`
and `utils`.

Weigelt, P., König, C. & Kreft, H. (2020) GIFT – A Global Inventory of
Floras and Traits for macroecology and biogeography. *Journal of
Biogeography*, <https://doi.org/10.1111/jbi.13623>.
