# Species richness per geographic region and taxonomic group in GIFT

Retrieve species richness of all species, native species, naturalized
species and endemic species per taxonomic group and geographic region
combination.

## Usage

``` r
GIFT_richness(
  taxon_name = "Embryophyta",
  GIFT_version = "latest",
  api = "https://gift.uni-goettingen.de/api/extended/"
)
```

## Arguments

- taxon_name:

  Taxonomic group to retrieve species richness for.

- GIFT_version:

  character string defining the version of the GIFT database to use. The
  function retrieves by default the `latest` stable version. If set to
  `beta`, the most up-to-date version which is still subject to changes
  and edits is used.

- api:

  character string defining from which API the data will be retrieved.

## Value

A data frame with species richness values for different floristic
subsets per geographic region in GIFT.

## Details

The output has 5 columns:

*entity_ID* - Identification number of the geographic region  
*total* - total species richness  
*native* - number of native species  
*naturalized* - number of naturalized species  
*endemic_min* - number of endemic species

The number of endemic species is a conservative count not counting
occurrences of species which go back to infraspecific taxa.

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
ex <- GIFT_richness(taxon_name = "Angiospermae")
#> You are asking for the latest stable version of GIFT which is 3.2.
# }
```
