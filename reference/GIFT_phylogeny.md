# Phylogeny of the species in GIFT

Retrieve a phylogeny of the plant species available in GIFT. The
phylogeny table is not available for GIFT_version 1.0, 2.0, 2.1 and 2.2.

## Usage

``` r
GIFT_phylogeny(
  clade = "Tracheophyta",
  as_tree = TRUE,
  return_work_ID = FALSE,
  work_ID_subset = NULL,
  api = "https://gift.uni-goettingen.de/api/extended/",
  GIFT_version = "latest"
)
```

## Arguments

- clade:

  Character string indicating the taxonomic group of interest
  corresponding to the node labels in the phylogeny.

- as_tree:

  Logical, whether you want the phylogeny to be returned as a
  phylogenetic tree (`TRUE`) or in a table (`FALSE`). `TRUE` by default.

- return_work_ID:

  Logical, whether you want to retrieve the species' names or their
  identification number (work_ID) in the GIFT database. `FALSE` by
  default.

- work_ID_subset:

  A vector of work_ID to prune the phylogenetic tree. `NULL` by default.

- api:

  character string defining from which API the data will be retrieved.

- GIFT_version:

  character string defining the version of the GIFT database to use. The
  function retrieves by default the `latest` stable version. If set to
  `beta`, the most up-to-date version which is still subject to changes
  and edits is used.

## Value

A data frame with 5 columns or a tree object.

## Details

Here is what each column refers to:

*taxon_label* - Name of the taxonomic group  
*work_ID* - Standardized species name IDs for the species at the tips of
the tree  
*edge_length* - Edge length  
*lft* - Left border of a given taxon in the Newick sequence  
*rgt* - Right border of a given taxon in the Newick sequence  

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
ex <- GIFT_phylogeny(clade = "Tracheophyta", as_tree = FALSE)
#> You are asking for the latest stable version of GIFT which is 3.2.
# }
```
