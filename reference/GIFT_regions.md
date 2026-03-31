# Metadata for GIFT regions

Retrieves miscellaneous information for GIFT regions.

## Usage

``` r
GIFT_regions(
  api = "https://gift.uni-goettingen.de/api/extended/",
  GIFT_version = "latest"
)
```

## Arguments

- api:

  character string defining from which API the data will be retrieved.

- GIFT_version:

  character string defining the version of the GIFT database to use. The
  function retrieves by default the `latest` stable version. If set to
  `beta`, the most up-to-date version which is still subject to changes
  and edits is used.

## Value

A data frame with 7 columns.

## Details

Here is the detail of each column:

*entity_ID* - Identification number of GIFT polygons  
*geo_entity* - Name of GIFT polygons  
*suit_geo* - Whether the polygon is suitable  
*entity_class* - Class of the polygon  
*entity_type* - Type of the polygon  
*TDWG_lvl3_ID* - Whether the polygon is a TDWG region (see
https://www.tdwg.org/)  
*country* - Whether the polygon is a country

## References

     Denelle, P., Weigelt, P., & Kreft, H. (2023). GIFT—An R package to
     access the Global Inventory of Floras and Traits. Methods in Ecology
     and Evolution, 14, 2738-2748.
     https://doi.org/10.1111/2041-210X.14213

     Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
     Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
     https://doi.org/10.1111/jbi.13623

## See also

[`GIFT_env_meta_misc()`](https://biogeomacro.github.io/GIFT/reference/GIFT_env_meta_misc.md)

## Examples

``` r
# \donttest{
ex <- GIFT_regions()
#> You are asking for the latest stable version of GIFT which is 3.2.
# }
```
