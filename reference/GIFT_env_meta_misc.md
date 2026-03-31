# Metadata for the environmental miscellaneous variables in GIFT

Retrieve the metadata of all miscellaneous environmental layers
accessible in GIFT.

## Usage

``` r
GIFT_env_meta_misc(
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

A data frame with 6 columns.

## Details

Here is what each column refers to:

*dataset* - Name of the source dataset.  
*variable* - Name of the environmental layer.  
*description*- Description.  
*unit* - Unit.  
*num* - Whether the environmental layer is numeric or not.  
*ref_long* - Full reference to cite when using an environmental layer.

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
ex <- GIFT_env_meta_misc()
#> You are asking for the latest stable version of GIFT which is 3.2.
# }
```
