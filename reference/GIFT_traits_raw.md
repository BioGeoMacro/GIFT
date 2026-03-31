# Raw trait values

Retrieve non-aggregated trait values at the level of the bibliographic
references and un-standardized species names in GIFT.

## Usage

``` r
GIFT_traits_raw(
  trait_IDs = "",
  derived = TRUE,
  bias_ref = TRUE,
  bias_deriv = TRUE,
  api = "https://gift.uni-goettingen.de/api/extended/",
  GIFT_version = "latest"
)
```

## Arguments

- trait_IDs:

  a character string indicating which traits you want to retrieve.
  Traits must belong to the available list of traits. See
  [`GIFT_traits_meta()`](https://biogeomacro.github.io/GIFT/reference/GIFT_traits_meta.md).

- derived:

  include logically derived traits.

- bias_ref:

  When `FALSE`, exclude entries that are only based on a resource that
  potentially introduces a bias (e.g. a resource only including trees).

- bias_deriv:

  When `FALSE`, exclude entries that are only based on a derivation that
  potentially introduces a bias (e.g. all phanerophytes being woody but
  some life forms being ambiguous).

- api:

  character string defining from which API the data will be retrieved.

- GIFT_version:

  character string defining the version of the GIFT database to use. The
  function retrieves by default the `latest` stable version. If set to
  `beta`, the most up-to-date version which is still subject to changes
  and edits is used.

## Value

A data.frame with 28 columns.

## Details

Here is the detail of each column:

*trait_derived_ID* - Identification number of the trait record in the
database  
*ref_ID* - Identification number of the reference  
*orig_ID* - Identification number of the species, as it came in the
source  
*trait_ID* - Identification number of the trait  
*trait_value* - Value of the trait (coded as character, even for
continuous trait)  
*derived* - Is the trait value derived from another information (e.g.
phanerophytes are woody)  
*bias_deriv* - Is the derivation potentially introducing a bias  
*bias_ref* - Is the resource potentially introducing a bias  
*name_ID* - Identification number of the species before being resolved  
*cf_genus* - Whether the genus name is uncertain  
*genus* - Genus of the species  
*cf_species* - Whether the species' epithet is uncertain  
*aff_species* - Species' epithet uncertain  
*species_epithet* - Epithet of the species  
*subtaxon* - Sub-taxon name  
*author* - Author who described the species  
*matched* - Was the species name matched in the taxonomic backbone  
*epithetscore* - Matching score for the epithet  
*overallscore* - Overall matching score  
*resolved* - Was the species name resolved in the taxonomic backbone  
*service* - Taxonomic backbone used for taxonomic harmonization  
*work_ID* - Identification number of the taxonomically harmonized
species  
*genus_ID* - Identification number of the taxonomically harmonized
genus  
*work_genus* - Genus name (after taxonomic harmonization)  
*work_species* - Species name (after taxonomic harmonization)  
*work_author* - Name of the author who described the species  
*geo_entity* \_ref - Name of the region of the reference  
*ref_long* - Full reference to cite

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
and
[`GIFT_traits()`](https://biogeomacro.github.io/GIFT/reference/GIFT_traits.md)

## Examples

``` r
# \donttest{
succulence <- GIFT_traits_raw(trait_IDs = c("4.10.1"))
#> ================================================================================
# }
```
