# Versions of GIFT available

Returns a table with information on the different versions of the
database

## Usage

``` r
GIFT_versions()
```

## Value

A data frame with 4 columns.

## Details

Here is what each column refers to:

*ID* - Identification number of the version  
*version* - Version number  
*description* - What were the major updates about

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
ex <- GIFT_versions()
# }
```
