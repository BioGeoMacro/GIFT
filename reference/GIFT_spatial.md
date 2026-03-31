# Spatial selection of GIFT checklists

Retrieve checklists overlapping with a shape file or a set of
coordinates.

## Usage

``` r
GIFT_spatial(
  shp = NULL,
  coordinates = NULL,
  overlap = "centroid_inside",
  entity_ID = NULL,
  GIFT_version = "latest",
  api = "https://gift.uni-goettingen.de/api/extended/"
)
```

## Arguments

- shp:

  Shapefile provided by the user. Its Coordinate Reference System (CRS)
  must be set to WGS84 (EPSG code 4326).

- coordinates:

  Custom set of coordinates. The format is a two columns data.frame, the
  first one being longitudes and the second being latitudes of the
  vertices of a polygon. If the data.frame only includes two rows, the
  function assumes that the values are the four limits (min and max.
  longitude and latitude) of a bounding box.

- overlap:

  A character string defining the criteria to use in order to retrieve
  checklists. Available options are `centroid_inside`,
  `extent_intersect`, `shape_intersect` and `shape_inside`. For example,
  `extent_intersect` means that every polygon from GIFT for which the
  extent intersects the provided shape/coordinates will be retrieved.

- entity_ID:

  Constrain the list of regions to be received by a predefined set of
  entity_IDs. E.g. this list could come from
  GIFT_checklists_conditional().

- GIFT_version:

  character string defining the version of the GIFT database to use. The
  function retrieves by default the `latest` stable version. If set to
  `beta`, the most up-to-date version which is still subject to changes
  and edits is used.

- api:

  character string defining from which API the data will be retrieved.

## Value

A data frame with 3 columns: *entity_ID* the identification number of a
polygon, *geo_entity_ref* its name, and *coverage* which indicates the
percentage of overlap between the provided shape and the different
polygons of GIFT.

## References

     Denelle, P., Weigelt, P., & Kreft, H. (2023). GIFT—An R package to
     access the Global Inventory of Floras and Traits. Methods in Ecology
     and Evolution, 14, 2738-2748.
     https://doi.org/10.1111/2041-210X.14213

     Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
     Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
     https://doi.org/10.1111/jbi.13623

## See also

[`GIFT_checklists()`](https://biogeomacro.github.io/GIFT/reference/GIFT_checklists.md)

## Examples

``` r
# \donttest{
# With a shapefile
data("western_mediterranean")
ex <- GIFT_spatial(shp = western_mediterranean, overlap = "centroid_inside")
#> You are asking for the latest stable version of GIFT which is 3.2.

# With a shapefile coming from GIFT
spain <- GIFT_shapes(entity_ID = 10071)
#> You are asking for the latest stable version of GIFT which is 3.2.
#> ================================================================================
ex_spain <- GIFT_spatial(shp = spain)
#> You are asking for the latest stable version of GIFT which is 3.2.

# With a point 
custom_point <- cbind(9.9, 51)
ex2 <- GIFT_spatial(coordinates = custom_point,
overlap = "extent_intersect")
#> You are asking for the latest stable version of GIFT which is 3.2.

# With an extent
custom_extent <- cbind(c(-13, -18), c(27.5, 29.3))
ex3 <- GIFT_spatial(coordinates = custom_extent,
overlap = "extent_intersect")
#> You are asking for the latest stable version of GIFT which is 3.2.
#> 4 coordinates provided: an extent box was drawn, assuming that
#>             minimum X and Y are on row 1, and maximum X and Y on row 2.

# With a custom polygon
custom_polygon <- cbind(c(-18, -16.9, -13, -13, -18, -18),
c(29.3, 33, 29.3, 27.5, 27.5, 29.3))
ex4 <- GIFT_spatial(coordinates = custom_polygon,
overlap = "extent_intersect")
#> You are asking for the latest stable version of GIFT which is 3.2.

#With a linestring
custom_linestring <- rbind(c(9.9, 51), c(2.35, 48.9))
custom_linestring <- sf::st_as_sf(as.data.frame(custom_linestring),
coords = c("V1", "V2"))
custom_linestring <- dplyr::summarise(custom_linestring,
geometry = sf::st_combine(geometry))
sf::st_crs(custom_linestring) <- sf::st_crs(western_mediterranean)
ex5 <- GIFT_spatial(shp = custom_linestring, overlap = "extent_intersect")
#> You are asking for the latest stable version of GIFT which is 3.2.

# }
```
