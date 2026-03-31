# Species list in GIFT

Retrieve the whole set of plant species available in GIFT.

## Usage

``` r
GIFT_species(
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

A data frame with 5 columns.

## Details

Here is what each column refers to:

*work_ID* - Identification number of the species  
*genus_ID* - Identification number of the genus  
*work_genus* - Genus name after taxonomic harmonization  
*work_species* - Species name after taxonomic harmonization  
*work_author* - Author who described the species (after taxonomic
harmonization)

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
ex <- GIFT_species()
#> You are asking for the latest stable version of GIFT which is 3.2.
# }
```
