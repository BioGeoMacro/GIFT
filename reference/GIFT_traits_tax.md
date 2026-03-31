# Traits at the taxonomic level

Retrieve specific trait values at a high taxonomic level.

## Usage

``` r
GIFT_traits_tax(
  trait_IDs = "",
  agreement = 0.66,
  bias_ref = TRUE,
  bias_deriv = TRUE,
  api = "https://gift.uni-goettingen.de/api/extended/",
  GIFT_version = "latest"
)
```

## Arguments

- trait_IDs:

  a character string indicating which trait you want to retrieve. Traits
  must belong to the available list of traits.

- agreement:

  Percentage of resources that agree on an aggregated trait value,
  entries below this threshold will be omitted.

- bias_ref:

  When `FALSE`, exclude entries that are only based on a resource that
  potentially introduces a bias (e.g. a resource only including trees).

- bias_deriv:

  When `FALSE`, exclude entries that are only based on a derivation that
  potentially introduces a bias (e.g. all phanerophytes being woody but
  some life forms being ambiguous).

- api:

  character string defining from which API the data will be retrieved.

- GIFT_version:

  character string defining the version of the GIFT database to use. The
  function retrieves by default the `latest` stable version. If set to
  `beta`, the most up-to-date version which is still subject to changes
  and edits is used.

## Value

A long-format data frame with 7 columns: `taxon_ID`, `taxon_name`,
`trait_value`, `agreement`, `references` and `negative`.

## Details

Here is the detail of each column:

*taxon_ID* - Identification number of the taxon  
*taxon_name* - Name of the taxon  
*agreement* - Agreement score between the different sources for that
trait value, only for categorical traits  
*references* - Source of the trait values (`ref_ID`)  
*negative* - Does the record indicate the absence of trait value in
taxon_ID  

and then one column per trait with the respective trait values

## References

     Denelle, P., Weigelt, P., & Kreft, H. (2023). GIFT—An R package to
     access the Global Inventory of Floras and Traits. Methods in Ecology
     and Evolution, 14, 2738-2748.
     https://doi.org/10.1111/2041-210X.14213

     Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
     Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
     https://doi.org/10.1111/jbi.13623

## See also

[`GIFT_traits_meta()`](https://biogeomacro.github.io/GIFT/reference/GIFT_traits_meta.md)

## Examples

``` r
# \donttest{
ex <- GIFT_traits_tax(trait_IDs = c("1.2.1", "1.4.1"),
bias_ref = FALSE, bias_deriv = FALSE)
#> You are asking for the latest stable version of GIFT which is 3.2.
#> 
#> Retrieving species' names.
#> Preparing the download of trait data (2 traits asked).
#> ================================================================================
#> 
#> The following traits were not available at the taxonomic level: 1.2.1 
# }
```
