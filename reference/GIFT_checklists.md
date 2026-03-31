# GIFT checklists

Retrieve GIFT checklists that fulfill specific criteria.

## Usage

``` r
GIFT_checklists(
  taxon_name = "Tracheophyta",
  complete_taxon = TRUE,
  floristic_group = c("all", "native", "endemic", "naturalized")[2],
  complete_floristic = TRUE,
  geo_type = c("All", "Mainland", "Island")[1],
  ref_excluded = NULL,
  suit_geo = FALSE,
  shp = NULL,
  coordinates = NULL,
  overlap = "centroid_inside",
  remove_overlap = FALSE,
  area_threshold_island = 0,
  area_threshold_mainland = 100,
  overlap_threshold = 0.1,
  by_ref_ID = FALSE,
  taxonomic_group = TRUE,
  namesmatched = FALSE,
  list_set_only = FALSE,
  GIFT_version = "latest",
  api = "https://gift.uni-goettingen.de/api/extended/"
)
```

## Arguments

- taxon_name:

  Character string corresponding to the taxonomic group of interest.

- complete_taxon:

  logical stating you want to retrieve checklists that only contain the
  exhaustive list of the `taxon_name` argument or as well incomplete
  lists.

- floristic_group:

  Character among the following options: `all`, `native`, `endemic`,
  `naturalized`.

- complete_floristic:

  logical stating you want to retrieve checklists that only contain the
  exhaustive list of the `floristic_group` argument or as well
  incomplete lists.

- geo_type:

  Character string, either `Mainland`, `Island` or `All`. `Island` gets
  you to Island, Island Group & Island Part. `Mainland` gets you to
  Mainland & Island/Mainland. `All` gets you all types.

- ref_excluded:

  A vector listing potential ref_IDs that shall be ignored when
  assembling the set of regions and checklists fulfilling the given
  criteria. Checklists from these references will not be returned. NULL
  by default.

- suit_geo:

  logical indicating whether only regions classified as suit_geo should
  be considered (see details).

- shp:

  Shapefile provided by the user.

- coordinates:

  Custom set of coordinates. The format is a two columns data.frame, the
  first one being longitudes and the second being latitudes of the
  vertices of a polygon. If the data.frame only includes two rows, the
  function assumes that the values are the four limits (min and max.
  longitude and latitude) of a bounding box.

- overlap:

  A character string defining the criteria to use in order to retrieve
  checklists. Available options are `centroid_inside`,
  `extent_intersect`, `shape_intersect` and `shape_inside`. For example,
  `extent_intersect` means that every polygon from GIFT for which the
  extent intersects the provided shape/coordinates will be retrieved.

- remove_overlap:

  a logical stating whether you want to retrieve checklists that overlap
  or not.

- area_threshold_island:

  A number stating from which surface the smallest overlapping polygon
  is kept. By default set to 0 square kilometer (meaning that by default
  the smallest islands will be conserved).

- area_threshold_mainland:

  When two polygons overlap, the smallest or the biggest one can be
  kept. When the surface of the smallest polygon exceeds this number,
  the smallest polygon is kept. Otherwise, we keep the bigger one. Set
  by default 100 square-kilometers.

- overlap_threshold:

  A number ranging from 0 to 1, indicating at what percentage of
  overlap, partially overlapping polygons should be kept.

- by_ref_ID:

  logical indicating whether the removal of overlapping regions shall be
  applied by *ref_ID* only. Note that regions overlapping with other
  regions from the same resource will be removed even if there are other
  references available for those regions.

- taxonomic_group:

  logical. When set to `TRUE`, two additional columns (*family* and
  *tax_group*) are available in the checklists.

- namesmatched:

  logical. `FALSE` by default, set to `TRUE` if you want the original
  species name as they came in the references as well as details on the
  taxonomic harmonization.

- list_set_only:

  logical stating whether you only want the metadata or if you also want
  to retrieve the species lists.

- GIFT_version:

  character string defining the version of the GIFT database to use. The
  function retrieves by default the `latest` stable version. If set to
  `beta`, the most up-to-date version which is still subject to changes
  and edits is used.

- api:

  character string defining from which API the data will be retrieved.

## Value

List with two data frames: the checklist with species and the list of
ID.

## Details

Here is the detail of each data.frame and their columns:

*ref_ID* - Identification number of each reference.  
*type*- What type the source is.  
*subset*- What information regarding the status of species is
available.  
*native_indicated*- Whether native status of species is available in the
source.  
*natural_indicated* - Whether naturalized status of species is available
in the source.  
*end_ref* - Whether endemism information is available in the source.  
*restricted* - Whether the access to this reference is restricted.  
*taxon_ID*- Identification number of species.  
*list_ID* - Identification number of each list.  
*end_list* - Whether endemism information is available in the list.  
*entity_ID*- Identification number of the polygon of the list.  
*geo_entity* - Name of the location.  
*suit_geo* - Is the polygon suitable.  
*entity_class* - Type of polygon.  
*entity_type* - Name of the location.  
*taxon_name* - Name of the group of taxa available.

For the second data frame with the species, each column refers to:

*ref_ID* - Identification number of each reference.  
*list_ID* - Identification number of each list  
*work_ID* - Identification number of each species name, after taxonomic
harmonization.  
*genus_ID* - Identification number of each genus, after taxonomic
harmonization.  
*species* - Species name, after taxonomic harmonization.  
*questionable* - Whether the species occurrence is questionable.  
*native* - Whether the species is native.  
*quest_native* - Whether the native information is questionable.  
*naturalized* - Whether the species is naturalized.  
*endemic_ref* - Whether the species is endemic within the reference.  
*quest_end_ref* - Whether the endemic_ref information is questionable.  
*endemic_list* - Whether the species is endemic within the list.  
*quest_end_list* - Whether the endemic_list information is
questionable.  
*cons_status* - Conservation status of the species.  
*family* - Family of the species.  
*tax_group* - Taxonomic group of the species.

While the arguments `taxon_name` in combination with
`complete_taxon = TRUE` and `floristic_group` in combination with
`complete_floristic = TRUE` make sure to only get back checklists for
regions for which GIFT has lists aiming at covering both the entire
taxonomic group and floristic subset (for example native vascular
plants), it does not mean that the checklists are complete (include all
species). We therefore flagged regions in GIFT for which the combination
of all checklists is obviously incomplete as `suit_geo = 0`. This has
however only been done only for native angiosperms and the assessment
has been subjective. Set `suit_geo = TRUE` if you only want to consider
regions classified as `suit_geo`.

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
data("western_mediterranean")
ex <- GIFT_checklists(shp = western_mediterranean,
overlap = "centroid_inside", taxon_name = "Angiospermae",
list_set_only = TRUE) # set to FALSE to get species composition
#> You are asking for the latest stable version of GIFT which is 3.2.
#> 
#> Metadata for lists retrieved.
#> GIFT taxonomy downloaded.
# }
```
