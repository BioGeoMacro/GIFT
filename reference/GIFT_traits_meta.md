# Trait metadata

Retrieve metadata of the functional traits coming from GIFT.

## Usage

``` r
GIFT_traits_meta(
  api = "https://gift.uni-goettingen.de/api/extended/",
  GIFT_version = "latest"
)
```

## Arguments

- api:

  Character string corresponding to the API.

- GIFT_version:

  character string defining the version of the GIFT database to use. The
  function retrieves by default the most up-to-date version.

## Value

A data frame with 10 columns.

## Details

Here is what each column refers to:

*Lvl1* - First level of the trait classification  
*Category* - Name of the first level of classification  
*Lvl2* - Second level of the trait classification  
*Trait1* - Name of the second level of classification  
*Lvl3* - Identification number of the trait  
*Trait2* - Trait name  
*Units* - Trait unit  
*type* - Trait type  
*comment* - Comment  
*count* - How many entries for that traits are in the database

## References

     Denelle, P., Weigelt, P., & Kreft, H. (2023). GIFT—An R package to
     access the Global Inventory of Floras and Traits. Methods in Ecology
     and Evolution, 14, 2738-2748.
     https://doi.org/10.1111/2041-210X.14213

     Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
     Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
     https://doi.org/10.1111/jbi.13623

## See also

[`GIFT_traits()`](https://biogeomacro.github.io/GIFT/reference/GIFT_traits.md)

## Examples

``` r
# \donttest{
ex <- GIFT_traits_meta()
#> You are asking for the latest stable version of GIFT which is 3.2.
# }
```
