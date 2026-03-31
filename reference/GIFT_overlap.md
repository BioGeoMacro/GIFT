# Spatial overlap between GIFT polygons and external polygons

Calculate the spatial overlap between GIFT polygons and shapefiles
coming from other resources

## Usage

``` r
GIFT_overlap(
  resource = "glonaf",
  GIFT_version = "latest",
  api = "https://gift.uni-goettingen.de/api/extended/"
)
```

## Arguments

- resource:

  A character string indicating from which resource the spatial overlap
  is calculated. Available options are `glonaf` and `gmba`. glonaf
  stands for Global Naturalized Alien Flora and gmba for Global Mountain
  Biodiversity Assessment.

- GIFT_version:

  character string defining the version of the GIFT database to use. The
  function retrieves by default the `latest` stable version. If set to
  `beta`, the most up-to-date version which is still subject to changes
  and edits is used.

- api:

  character string defining from which API the data will be retrieved.

## Value

A data frame with the spatial overlap.

## Details

The columns of the data.frame are the following:

*entity_ID* - Identification number of the GIFT polygon  
*glonaf_ID* (or *gmba_ID*) - Identification number of the polygon from
the other resource  
*overlap12* - Spatial overlap in percentage between GIFT polygon and the
external polygon  
*overlap21* - The other way around

## References

     Denelle, P., Weigelt, P., & Kreft, H. (2023). GIFT—An R package to
     access the Global Inventory of Floras and Traits. Methods in Ecology
     and Evolution, 14, 2738-2748.
     https://doi.org/10.1111/2041-210X.14213

     Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
     Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
     https://doi.org/10.1111/jbi.13623

## See also

[`GIFT_lists()`](https://biogeomacro.github.io/GIFT/reference/GIFT_lists.md)

## Examples

``` r
# \donttest{
glonaf <- GIFT_overlap(resource = "glonaf")
#> You are asking for the latest stable version of GIFT which is 3.2.
gmba <- GIFT_overlap(resource = "gmba")
#> You are asking for the latest stable version of GIFT which is 3.2.
# }
```
