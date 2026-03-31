# Metadata for checklists available in GIFT

Retrieves the metadata of each checklist within GIFT.

## Usage

``` r
GIFT_lists(
  api = "https://gift.uni-goettingen.de/api/extended/",
  GIFT_version = "latest"
)
```

## Arguments

- api:

  character string defining from which API the data will be retrieved.

- GIFT_version:

  character string defining the version of the GIFT database to use. The
  function retrieves by default the `latest` stable version. If set to
  `beta`, the most up-to-date version which is still subject to changes
  and edits is used.

## Value

A data frame with 15 columns.

## Details

Here is what each column refers to:

*ref_ID* - Identification number of each reference.  
Columns *type* and *subset* indicate what information can be found in
each reference. Similarly, *native_indicated*, *natural_indicated* and
*end_ref* indicate respectively whether native, naturalized and endemic
species were stated in the reference. *restricted* refers to the
availability of the reference, *taxon_ID* to the taxonomic group
available in a reference. *list_ID* is the identification number of a
checklist within a reference, *entity_ID* of the associated polygon.
*geo_entity* associates a name to this identification number. *suit_geo*
indicates whether the checklist is suitable for use, *entity_class* and
*entity_unit* give additional details about the polygon.

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
ex <- GIFT_lists()
#> You are asking for the latest stable version of GIFT which is 3.2.
# }
```
