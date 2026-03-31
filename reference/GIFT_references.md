# Metadata for references available in GIFT

Retrieve the metadata of every reference accessible in GIFT.

## Usage

``` r
GIFT_references(
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

A data frame with 14 columns.

## Details

Here is what each column refers to:

*ref_ID* - Identification number of the reference  
*ref_long* - Full reference for the reference  
*geo_entity_ref* - Name of the location  
*type* - What type the source is  
*subset* - What information regarding the status of species is
available  
*taxon_ID* - Identification number of the group of taxa available  
*taxon_name* - Name of the group of taxa available  
*checklist* - Is the source a checklist  
*native_indicated* - Whether native status of species is available in
the source  
*natural_indicated* - Whether naturalized status of species is available
in the source  
*end_ref* - Whether endemism information is available in the source  
*traits* - Whether trait information is available in the source  
*restricted* - Whether the access to this reference is restricted  
*proc_date* - When the source was processed

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
ex <- GIFT_references()
#> You are asking for the latest stable version of GIFT which is 3.2.
# }
```
