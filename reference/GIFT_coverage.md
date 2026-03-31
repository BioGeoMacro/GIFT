# Taxonomic and trait coverage per geographic region and taxonomic group in GIFT

Retrieve taxonomic or trait coverage (for a given trait) of all species,
native species, naturalized species and endemic species per taxonomic
group and geographic region combination. This function works with
taxonomic groups above the genus level.

## Usage

``` r
GIFT_coverage(
  what = "taxonomic_coverage",
  taxon_name = "Embryophyta",
  trait_ID = "1.1.1",
  GIFT_version = "latest",
  api = "https://gift.uni-goettingen.de/api/extended/"
)
```

## Arguments

- what:

  character indicating whether `taxonomic_coverage` or `trait_coverage`
  shall be retrieved.

- taxon_name:

  Name of the taxonomic group you want to retrieve coverage for. See
  [`GIFT_taxonomy()`](https://biogeomacro.github.io/GIFT/reference/GIFT_taxonomy.md)
  for options. The function accepts family names and higher taxonomic
  groups.

- trait_ID:

  Identification number of the trait you want to retrieve coverage for.
  See
  [`GIFT_traits_meta()`](https://biogeomacro.github.io/GIFT/reference/GIFT_traits_meta.md)
  for details.

- GIFT_version:

  character string defining the version of the GIFT database to use. The
  function retrieves by default the `latest` stable version. If set to
  `beta`, the most up-to-date version which is still subject to changes
  and edits is used.

- api:

  character string defining from which API the data will be retrieved.

## Value

A data frame with either taxonomic or trait coverage per GIFT polygon.

## Details

The output has 9 columns:

*entity_ID* - Identification number of GIFT polygons  
*total* - taxonomic or trait coverage for all species  
*total_rst* - taxonomic or coverage for all species considering
restricted resources  
*native* - taxonomic or trait coverage for native species  
*native_rst* - taxonomic or trait coverage for native species
considering restricted resources  
*naturalized* - taxonomic or trait coverage for naturalized species  
*naturalized_rst* - taxonomic or trait coverage for naturalized species
considering restricted resources  
*endemic_min* - taxonomic or trait coverage for endemic species  
*endemic_min_rst* - taxonomic or trait coverage for endemic species
considering restricted resources

In the case of taxonomic coverage, a '1' means that species composition
data is available for the given combination of taxonomic group and
geographic region while 'NA' means that no data is available. This can
differ depending on whether restricted data in GIFT is considered or not
(columns with or without \_rst at the end).

In the case of trait coverage, the proportion of species of a given
taxonomic group with information on the defined trait is reported per
geographic region.

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
ex <- GIFT_coverage(what = "taxonomic_coverage", taxon_name = "Angiospermae")
#> You are asking for the latest stable version of GIFT which is 3.2.
ex2 <- GIFT_coverage(what = "trait_coverage", taxon_name = "Angiospermae",
trait_ID = "1.2.1")
#> You are asking for the latest stable version of GIFT which is 3.2.
# }
```
