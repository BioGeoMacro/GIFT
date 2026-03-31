# Trait values at the species level

Retrieve specific trait values.

## Usage

``` r
GIFT_traits(
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

  When FALSE, exclude entries that are only based on a resource that
  potentially introduces a bias (e.g. a resource only including trees).

- bias_deriv:

  When FALSE, exclude entries that are only based on a derivation that
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

A long-format data frame with 6 columns: `trait_ID`, `work_ID`,
`species`, `trait_value`, `agreement` and `references`.

## Details

Here is the detail of each column:

*trait_ID* - Identification number of the trait  
*work_ID* - Identification number of the taxonomically harmonized
species  
*species* - Species name  
*trait_value* - Value of the trait  
*agreement* - Agreement score between the different sources for that
trait value, only for categorical traits  
*cv* - Coefficient of variation for the different sources for that trait
value, only for numeric traits  
*n* - Number of sources leading to the trait value  
*references* - ref_ID from which we got the trait information

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
self_fertilization <- GIFT_traits(trait_IDs = "3.1.1", agreement = 0.66,
bias_ref = FALSE, bias_deriv = FALSE)
#> 
#> Retrieving species' names.
#> Preparing the download of trait data for 1 trait(s).
#> ================================================================================
#> 
# }
```
