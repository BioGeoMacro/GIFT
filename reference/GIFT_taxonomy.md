# Taxonomy of GIFT

Retrieves the taxonomy of GIFT.

## Usage

``` r
GIFT_taxonomy(
  GIFT_version = "latest",
  api = "https://gift.uni-goettingen.de/api/extended/"
)
```

## Arguments

- GIFT_version:

  character string defining the version of the GIFT database to use. The
  function retrieves by default the `latest` stable version. If set to
  `beta`, the most up-to-date version which is still subject to changes
  and edits is used.

- api:

  character string defining from which API the data will be retrieved.

## Value

A data frame with 6 columns.

## Details

Here is what each column refers to:

*taxon_ID* - the identification number of each taxonomic entry.  
*taxon_name* - names describing taxa.  
*taxon_author* - author name for a given taxon.  
*taxon_lvl* - splits every taxon in genus, family, order or superior
orders. Taxonomy is a linear sequence of left and right borders for each
taxon. This is nested, for example left and right borders of a genus
would fall between the left and right borders of the corresponding
family.  
*lft* - left border of one taxon in the taxonomic sequence.  
*rgt* - right border of one taxon in the taxonomic sequence.  

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
ex <- GIFT_taxonomy()
#> You are asking for the latest stable version of GIFT which is 3.2.
# }
```
