# Taxonomic group of species

Assign taxonomic groups of various hierarchical level to species from
GIFT (`work_ID`).

## Usage

``` r
GIFT_taxgroup(
  work_ID = NULL,
  taxon_lvl = c("family", "order", "higher_lvl")[1],
  return_ID = FALSE,
  GIFT_version = "latest",
  api = "https://gift.uni-goettingen.de/api/extended/",
  taxonomy = NULL,
  species = NULL
)
```

## Arguments

- work_ID:

  A vector defining the IDs of the species to retrieve taxonomic groups
  for. `NULL` by default.

- taxon_lvl:

  taxonomic level to retrieve names for. `family` by default. Check
  [`GIFT_taxonomy()`](https://biogeomacro.github.io/GIFT/reference/GIFT_taxonomy.md)
  for available levels. In addition to the available levels one can put
  `higher_lvl` to retrieve the higher level groups "Anthocerotophyta",
  "Marchantiophyta", "Bryophyta", "Lycopodiophyta", "Monilophyta",
  "Gymnospermae", and "Angiospermae".

- return_ID:

  logical indicating whether to give back taxon_IDs instead of names.

- GIFT_version:

  character string defining the version of the GIFT database to use. The
  function retrieves by default the `latest` stable version. If set to
  `beta`, the most up-to-date version which is still subject to changes
  and edits is used.

- api:

  character string defining from which API the data will be retrieved.

- taxonomy:

  option to supply taxonomy object here if loaded already to avoid
  double loading. For internal use within GIFT functions. If `NULL`
  (default) taxonomy will be loaded within this function.

- species:

  option to supply species names object here if loaded already to avoid
  double loading. For internal use within GIFT functions. If `NULL`
  (default) species will be loaded within this function.

## Value

A vector with the taxonomic group of the species used as input.

## References

     Denelle, P., Weigelt, P., & Kreft, H. (2023). GIFT—An R package to
     access the Global Inventory of Floras and Traits. Methods in Ecology
     and Evolution, 14, 2738-2748.
     https://doi.org/10.1111/2041-210X.14213

     Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
     Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
     https://doi.org/10.1111/jbi.13623

## See also

[`GIFT_taxonomy()`](https://biogeomacro.github.io/GIFT/reference/GIFT_taxonomy.md)

## Examples

``` r
# \donttest{
ex <- GIFT_taxgroup(work_ID = c(1, 4, 7, 8), taxon_lvl = "family")
#> You are asking for the latest stable version of GIFT which is 3.2.
# }
```
