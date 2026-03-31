# Species list in GIFT

Retrieve all name matching information for one taxonomic name. All
results are returned, where the name is either found in the
unstandardized or taxonomically standardized names.

## Usage

``` r
GIFT_species_lookup(
  genus = "",
  epithet = "",
  namesmatched = FALSE,
  api = "https://gift.uni-goettingen.de/api/extended/",
  GIFT_version = "latest"
)
```

## Arguments

- genus:

  character string defining the genus name to be looked for.

- epithet:

  character string defining the specific epithet to be looked for.

- namesmatched:

  Logical `FALSE` by default, set to `TRUE` if you want to look for the
  species not only in the standardized species names but also in the
  original species names as they came in the original resources.

- api:

  character string defining from which API the data will be retrieved.

- GIFT_version:

  character string defining the version of the GIFT database to use. The
  function retrieves by default the `latest` stable version. If set to
  `beta`, the most up-to-date version which is still subject to changes
  and edits is used.

## Value

A data frame with 19 columns (or 24 if namesmatched = TRUE).

## Details

Here is what each column refers to: *orig_ID* - Identification number of
the species before taxonomic harmonization  
*orig_genus* - Genus before taxonomic harmonization  
*name_ID* - Identification number of the genus before taxonomic
harmonization  
*cf_genus*- Whether the genus name is uncertain  
*genus*- Genus before taxonomic harmonization  
*cf_species*- Whether the species' epithet is uncertain  
*aff_species*- Species' epithet uncertain  
*species_epithet*- Epithet of the species before taxonomic
harmonization  
*subtaxon*- Subtaxon of the species before taxonomic harmonization  
*author*- Author who described the species (before taxonomic
harmonization)  
*matched*- Is the species matched in the taxonomic backbone  
*epithetscore*- Matching score for the epithet  
*overallscore*- Overall matching score for the species  
*resolved*- Is the species name resolved in the taxonomic backbone  
*synonym*- Is the species name a synonym in the taxonomic backbone  
*matched_subtaxon*- Is the subtaxon matched in the taxonomic backbone  
*accepted*- Is the species name accepted in the taxonomic backbone  
*service*- Service use for the taxonomic harmonization  
*work_ID*- Identification number of the species after taxonomic
harmonization  
*taxon_ID*- Identification number of the taxonomic group  
*work_genus*- Identification number of the genus after taxonomic
harmonization  
*work_species_epithet*- Identification number of the species epithet
after taxonomic harmonization  
*work_species* - Species name (after taxonomic harmonization)  
*work_author*- Author who described the species (after taxonomic
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

[`GIFT_checklists()`](https://biogeomacro.github.io/GIFT/reference/GIFT_checklists.md)

## Examples

``` r
# \donttest{
ex <- GIFT_species_lookup(genus = "Fagus", epithet = "sylvatica")
#> You are asking for the latest stable version of GIFT which is 3.2.
# }
```
