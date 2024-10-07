# GIFT

# GIFT 1.3.3
## Minor changes

* *Anemonoides nemorosa* instead of *Anemone nemorosa* in the vignette
* Better explanation for the parameter *coordinates* in functions
GIFT_spatial and GIFT_checklists
* Publication records section added on the Website/Vignettes
* Section added for specific region's phylogeny

# GIFT 1.3.2
## Minor changes

* Better description of entity_ID argument in GIFT_spatial
* Example added in GIFT_spatial
* Main vignette: detail added regarding the version of GIFT used for the
species distribution, packages edited in the beginning
* Function check_query() added in the check_functions and implemented in
GITT_env_meta_raster() and GIFT_env_meta_misc()

# GIFT 1.3.1
## Minor changes

* Better message in GIFT_env with missing raster/misc
* Citation updated in all functions and Readme

# GIFT 1.3.0

* Better handling of functions when the Internet or the GIFT server are down

## Minor changes

* Example changed in `GIFT_phylogeny()`

# GIFT 1.2.0

* GIFT_version = "latest" now corresponds to GIFT 3.0
* `GIFT_spatial()`: provided shapefiles are re-projected in case they have a
missing CRS or if it does not correspond to WGS84
* `GIFT_phylogeny()`: error message if GIFT_version < 3.0
* All vignettes enhanced and improved

## Minor changes

* Documentation improved for `GIFT_coverage()`, `GIFT_env()`

# GIFT 1.1.0

* Check for internet connection, misspecified APIs and GIFT server response in
each function
* `GIFT_traits()` fixed. Previously, when querying multiple traits in a
different order than the one used in our metadata table, the number of
downloads could have been incorrect.
* All vignettes enhanced and improved

## Minor changes

* New options in `GIFT_overlap()`
* Outputs of `GIFT_lists()` and `GIFT_regions()` numeric
* Edits in the help of functions

# GIFT 1.0.0

* First CRAN release
* Added a `NEWS.md` file to track changes to the package
