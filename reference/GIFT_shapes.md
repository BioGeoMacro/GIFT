# Shape files of GIFT regions

Get shapefile of GIFT regions for selected regions.

## Usage

``` r
GIFT_shapes(
  entity_ID = NULL,
  api = "https://gift.uni-goettingen.de/api/extended/",
  GIFT_version = "latest"
)
```

## Arguments

- entity_ID:

  A vector defining the IDs of the regions.

- api:

  character string defining from which API the data will be retrieved.

- GIFT_version:

  character string defining the version of the GIFT database to use. The
  function retrieves by default the `latest` stable version. If set to
  `beta`, the most up-to-date version which is still subject to changes
  and edits is used.

## Value

A spatial data.frame with 13 columns.

## Details

Here is the detail of each column:

*entity_ID* - Identification number of the polygon  
*geo_entity* - Name of the polygon  
*point_x* - Longitude of the centroid of the polygon  
*point_y* - Latitude of the centroid of the polygon  
*area* - Area in km2 of the polygon  
*x_min* - Minimum longitude of the polygon  
*x_max* - Maximum longitude of the polygon  
*y_min* - Minimum latitude of the polygon  
*y_max* - Maximum latitude of the polygon  
*entity_class* - Class of the polygon  
*entity_type* - Type of the entity  
*polygon_source* - Source of the polygon  
*geometry* - Geometry column from sf

## References

     Denelle, P., Weigelt, P., & Kreft, H. (2023). GIFT—An R package to
     access the Global Inventory of Floras and Traits. Methods in Ecology
     and Evolution, 14, 2738-2748.
     https://doi.org/10.1111/2041-210X.14213

     Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
     Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
     https://doi.org/10.1111/jbi.13623

## See also

[`GIFT_env()`](https://biogeomacro.github.io/GIFT/reference/GIFT_env.md)

## Examples

``` r
# \donttest{
ex <- GIFT_shapes(entity_ID = c(677, 200))
#> You are asking for the latest stable version of GIFT which is 3.2.
#> ================================================================================
plot(sf::st_geometry(ex), col = ex$entity_ID)

# }
```
