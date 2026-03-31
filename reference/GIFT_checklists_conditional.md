# GIFT checklists meta data

Retrieve meta data of GIFT checklists for regions that are covered by
checklists jointly fulfilling specific criteria.

## Usage

``` r
GIFT_checklists_conditional(
  taxon_name = "Tracheophyta",
  floristic_scope = c("all", "native", "native and naturalized",
    "native and historically introduced", "endangered", "endemic", "naturalized",
    "other subset")[1:4],
  ref_excluded = NULL,
  type_ref = c("Account", "Catalogue", "Checklist", "Flora", "Herbarium collection",
    "Key", "Red list", "Report", "Species Database", "Survey"),
  entity_class = c("Island", "Island/Mainland", "Mainland", "Island Group",
    "Island Part"),
  native_indicated = FALSE,
  natural_indicated = FALSE,
  end_ref = FALSE,
  end_list = FALSE,
  suit_geo = FALSE,
  complete_taxon = TRUE,
  GIFT_version = "latest",
  api = "https://gift.uni-goettingen.de/api/extended/",
  list_set = NULL,
  taxonomy = NULL
)
```

## Arguments

- taxon_name:

  Character string corresponding to the taxonomic group of interest.

- floristic_scope:

  A vector listing floristic scopes of the references to be considered.
  Options are: `all`, `native`, `native and naturalized`,
  `native and historically introduced`, `endangered`, `endemic`,
  `naturalized`, `other subset`.

- ref_excluded:

  A vector listing potential ref_IDs that shall be ignored when
  assembling the set of regions and checklists fulfilling the given
  criteria. Checklists from these references will not be returned.
  `NULL` by default.

- type_ref:

  Character, options are `Account`, `Catalogue`, `Checklist`, `Flora`,
  `Herbarium collection`, `Key`, `Red list`, `Report`,
  `Species Database`, `Survey`.

- entity_class:

  Character, options are `Island`, `Island/Mainland`, `Mainland`,
  `Island Group`, `Island Part`.

- native_indicated:

  Logical, whether only lists where native status is available should be
  retrieved.

- natural_indicated:

  Logical, whether only lists where natural status is available should
  be retrieved.

- end_ref:

  Logical, whether only lists where endemism at the reference level is
  available should be retrieved.

- end_list:

  Logical, whether only lists where endemism at the list level is
  available should be retrieved.

- suit_geo:

  logical indicating whether only regions classified as suit_geo should
  be considered (see details).

- complete_taxon:

  Logical, default `TRUE`.

- GIFT_version:

  character string defining the version of the GIFT database to use. The
  function retrieves by default the `latest` stable version. If set to
  `beta`, the most up-to-date version which is still subject to changes
  and edits is used.

- api:

  character string defining from which API the data will be retrieved.

- list_set:

  list_set `NULL` by default. If not, it has to be the list table (see
  [`GIFT_lists()`](https://biogeomacro.github.io/GIFT/reference/GIFT_lists.md)).
  Used internally in
  [`GIFT_checklists()`](https://biogeomacro.github.io/GIFT/reference/GIFT_checklists.md)
  to avoid downloading the table of lists many times.

- taxonomy:

  default `NULL`. If not, it has to be the taxonomy table (see
  [`GIFT_taxonomy()`](https://biogeomacro.github.io/GIFT/reference/GIFT_taxonomy.md)).

## Value

A data frame with 16 columns.

## Details

Here is what each column refers to:

*ref_ID* - Identification number of each reference.  
*type* - What type the source is.  
*subset* - What information regarding the status of species is
available.  
*native_indicated* - Whether native status of species is available in
the source.  
*natural_indicated* - Whether naturalized status of species is available
in the source.  
*end_ref* - Whether endemism information is available in the source.  
*restricted* - Whether the access to this reference is restricted.  
*taxon_ID* - Identification number of species.  
*list_ID* - Identification number of each list.  
*end_list* - Whether endemism information is available in the list.  
*entity_ID* - Identification number of the polygon of the list.  
*geo_entity* - Name of the location.  
*suit_geo* - Is the polygon suitable.  
*entity_class* - Type of polygon.  
*entity_type* - Name of the location.  
*taxon_name* - Name of the group of taxa available.

## References

     Denelle, P., Weigelt, P., & Kreft, H. (2023). GIFT—An R package to
     access the Global Inventory of Floras and Traits. Methods in Ecology
     and Evolution, 14, 2738-2748.
     https://doi.org/10.1111/2041-210X.14213

     Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
     Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
     https://doi.org/10.1111/jbi.13623

## See also

[`GIFT_checklists_raw()`](https://biogeomacro.github.io/GIFT/reference/GIFT_checklists_raw.md)

## Examples

``` r
# \donttest{
ex <- GIFT_checklists_conditional(taxon_name = "Embryophyta", 
floristic_scope = c("all", "native", "native and naturalized",
"native and historically introduced", "endangered",
"endemic", "naturalized", "other subset")[7],
type_ref = c("Account", "Catalogue", "Checklist","Flora",
"Herbarium collection", "Key", "Red list", "Report", "Species Database",
 "Survey"),
 entity_class = c("Island", "Island/Mainland", "Mainland", "Island Group",
 "Island Part"),
 native_indicated = FALSE, natural_indicated = FALSE, end_ref = FALSE,
 end_list = FALSE, suit_geo = TRUE, complete_taxon = TRUE,
 list_set = NULL, taxonomy = NULL)
#> You are asking for the latest stable version of GIFT which is 3.2.

# }
```
