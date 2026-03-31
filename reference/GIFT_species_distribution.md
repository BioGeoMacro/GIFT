# GIFT species distribution

Retrieve the distribution of one species from GIFT checklists.

## Usage

``` r
GIFT_species_distribution(
  genus = "Fagus",
  epithet = "sylvatica",
  namesmatched = FALSE,
  remove_overlap = FALSE,
  area_th_island = 0,
  area_th_mainland = 100,
  overlap_th = 0.1,
  by_ref_ID = FALSE,
  aggregation = FALSE,
  GIFT_version = "latest",
  api = "https://gift.uni-goettingen.de/api/extended/"
)
```

## Arguments

- genus:

  Character string corresponding to the genus of the species of
  interest.

- epithet:

  Character string corresponding to the epithet of the species of
  interest.

- namesmatched:

  Logical `FALSE` by default, set to TRUE if you want to look for the
  species not only in the standardized species names but also in the
  original species names as they came in the original resources.

- remove_overlap:

  a logical stating whether you want to retrieve checklists that overlap
  or not. `FALSE` by default.

- area_th_island:

  A number stating from which surface the smallest overlapping polygon
  is kept. By default set to 0 square kilometer (meaning that by default
  the smallest islands will be conserved).

- area_th_mainland:

  When two polygons overlap, the smallest or the biggest one can be
  kept. When the surface of the smallest polygon exceeds this number,
  the smallest polygon is kept. Otherwise, we keep the bigger one. Set
  by default 100 square-kilometers.

- overlap_th:

  A number ranging from 0 to 1, indicating at what percentage of
  overlap, partially overlapping polygons should be kept.

- by_ref_ID:

  logical indicating whether the removal of overlapping regions shall be
  applied by ref_ID only. Note that regions overlapping with other
  regions from the same resource will be removed even if there are other
  references available for those regions.

- aggregation:

  A logical stating whether you want to aggregate in a simpler way the
  floristic status of species per entity_ID. For example, two lists
  associated to the same entity_ID could describe a species both as
  native and non-native. In that case, the aggregation would consider
  the species to be native. Reverse for naturalized and alien.

- GIFT_version:

  character string defining the version of the GIFT database to use. The
  function retrieves by default the `latest` stable version. If set to
  `beta`, the most up-to-date version which is still subject to changes
  and edits is used.

- api:

  character string defining from which API the data will be retrieved.

## Value

A data frame with 33 columns.

## Details

Here is the detail of each data.frame and their columns: *ref_ID* -
Identification number of the reference  
*list_ID* - Identification number of the list  
*entity_ID* - Identification number of the polygon  
*name_ID* - Identification number of the genus before taxonomic
harmonization  
*cf_genus* - Whether the genus name is uncertain  
*cf_species* - Whether the species' epithet is uncertain  
*aff_species* - Species' epithet uncertain  
*questionable* - Whether the species name is questionable  
*native* - Is the species native  
*quest_native* - Is the native status questionable  
*naturalized* - Is the species naturalized  
*endemic_ref* - Is the species endemic at the reference level  
*quest_end_ref* - Is the endemic_ref status questionable  
*endemic_list* - Is the species endemic at the list level  
*quest_end_list* - Is the endemic_list status questionable  
*genus* - Genus name before taxonomic harmonization  
*species_epithet* - Epithet before taxonomic harmonization  
*subtaxon* - Subtaxon name before taxonomic harmonization  
*author* - Author who described the species before taxonomic
harmonization  
*matched* - Is the species name matched in the taxonomic backbone  
*epithetscore* - Matching score for the epithet  
*overallscore* - Overall matching score for the species  
*resolved* - Is the species name resolved in the taxonomic backbone  
*synonym* -Is the species a synonym in the taxonomic backbone  
*matched_subtaxon* -Is the sub-species name matched in the taxonomic
backbone  
*accepted* - Is the species name accepted in the taxonomic backbone  
*service* - Service use for the taxonomic harmonization  
*work_ID* -Identification number of the species after taxonomic
harmonization  
*taxon_ID* -Identification number of the taxonomic group  
*work_genus* - Identification number of the genus after taxonomic
harmonization  
*work_species_epithet* - Identification number of the species epithet
after taxonomic harmonization  
*work_species* - Species name (after taxonomic harmonization)  
*work_author* - Author who described the species (after taxonomic
harmonization)

## References

     Denelle, P., Weigelt, P., & Kreft, H. (2023). GIFT—An R package to
     access the Global Inventory of Floras and Traits. Methods in Ecology
     and Evolution, 14, 2738-2748.
     https://doi.org/10.1111/2041-210X.14213

     Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
     Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
     https://doi.org/10.1111/jbi.13623

## See also

[`GIFT_species_lookup()`](https://biogeomacro.github.io/GIFT/reference/GIFT_species_lookup.md)

## Examples

``` r
# \donttest{
ex <- GIFT_species_distribution()
#> You are asking for the latest stable version of GIFT which is 3.2.
# }
```
