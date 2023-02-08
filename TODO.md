# GIFT

# Table of Contents
1. [Functions](#functions)
2. [Vignette](#vignette)
3. [Database](#Database)

# 0. General

* ~~all calls of GIFT functions inside of other functions need the version
  and api forwarded!!!~~
* We need to control for empty spaces in arguments passed on to the api
* Harmonization of argument's names across functions (and no cryptic argument names)
* allow for a comparison of restricted vs non restricted meta data without having the restricted api


# 1. Details
* 1.1. GIFT_checklists_raw()
    
* 1.2. GIFT_checklists_conditional()
    - leveling-up (if complete fern and seed plant lists => you have a complete Tracheophyta list): count unique genera in all available groups and compare to unique genera included in desired group

* 1.3. GIFT_spatial()

* 1.4. GIFT_taxonomy()  

* 1.5. GIFT_tax_group()

* 1.6. GIFT_no_overlap()
    - Check: what happens to regions that do exist but don't have spatial data? kept or removed
    - Failed to parse example for topic 'GIFT_no_overlap'
    
* 1.7. GIFT_checklist()

* 2.1. GIFT_traits_meta()

* 2.2. GIFT_traits()
    - make numeric traits numeric?
    - Give back trait IDs that are not valid?

* 2.3. GIFT_traits_raw()
    - write into help that trait values are character values (even if numbers)
    - make orig_names/names matched info optional? Add the three new matching columns
    - Give back trait IDs that are not valid?

* 3.1. GIFT_env()

* 3.2. GIFT_env_meta_misc()    
    
* 3.3. GIFT_env_meta_raster() 
    - add argument available_only to allow for seeing unavailable layers

* 4.1. GIFT_species()

* 4.2. GIFT_ranges()
    - translate range_finder? # not for now

* 5.1. GIFT_shape()

* 6.1. western_mediterranean()

* 6.2. GIFT_references()

* GIFT_species_lookup()
    - get rid of orig ID to be faster? Make simple version at least. (Patrick)

* GIFT_species_distribution()
    - use simpler version of GIFT_species() for matchednames (Patrick)
    - something is wrong when aggregated = TRUE and matchednames TRUE, Columns missing

* GIFT_regions()

* GIFT_overlap()

* GIFT_no_overlap()

* GIFT_phylogeny()
    - additional columns needed?
    - Give information on phylogeny and cite it below
    - rename arguments? taxon_name -> clade, as_newick -> as_tree
    - make option for having work_IDs at tips
    - make option to prune for set of species

* versions of GIFT:
    - ~~restore old polygons for versions < 2.2~~
    
* GIFT_version()
    - so far, not useful for a random user => put it as a not exported function
    - later versions would give back a table with information on the different versions

* &nbsp;&nbsp;&nbsp;&nbsp; 6.3. GIFT_geology()? generic queries?  

* &nbsp;&nbsp;&nbsp;&nbsp; GIFT_traits_taxo() => retrieve trait at the taxono
  mic level and/or include this in GIFT_traits() as an option (add a column
  taxonomically_derived (and rename the current one derived trait_derived))

## 1.2. Controls
* Make a clear error message in case of wrong API or missing internet connection (httr, httr2 or rcurl)
* Put controls for all arguments in not exported functions whenever possible (like for api and GIFT_version)

# 2. Vignette
* API vignette: properly check the API arguments in the query table

# 3. Queries
* In queries.php (WinSCP) => put as a comment in which R function queries are used

# 4. GIFT admin/database
* ~~Traits: aggregation of categorical traits (agreement to revise (for ex. shrub and herb/shrub is NA so far))~~
* ~~Traits: continuous traits: add a column coeff. of var (sd/mean) in traits_final~~
* Traits: Agreement free text traits? Does not make sense. How is it handled => remove empty values for free text traits
* Traits: Scrutinize new traits workflow
* ~~Phylogeny: U.Phylo.maker => update *phylogeny* table~~
* ~~Phylogeny: prune phylogeny for species in GIFT before upload and add Tracheophyta label~~
* ~~Phylogeny: Wait for new WCVP phylogeny in U.Phylo.maker~~
* Phylogeny: Check phylogeny by Carta et al. 2022. NPH or see upcoming Kew phylogeny
* ~~exactextract~~
* unrealistic elevation ranges (+ flora of Gabon in feet)
* missing trait values in traits_final (see 4.11.1 for example) 
* ~~Ref_ID 10173 hat native == 1 und naturalized == 1 ohne das questionable oder quest_native angezeigt ist~~
* Heights Helena sent (Patrick)
* check whats restricted once again
* Add a bit more info to version table: beta, tpl-wcvp, numbers
* Trait links Matthias; Need to be update for new Austraits version
* Check type_ref = 0
* 4.15.1 combinations possible
* Check regions with multiple polygon resources (Patrick)
* ~~add environmental layers in GIFT:~~
* Check Julians Polygons
* Run counts again
* cleanup_names_work_unique() # I think we rather keep the old IDs
* ~~Lembrechts 2022 Global maps of soil temperature~~
* ~~Chelsa 2.1~~
* ~~Cai et al.~~
* ~~Zomer et al.~~
* Update citations Lembrechts, Brun, Zomer and Cai
* convert GIFT_version from beta into 3.0

https://developers.google.com/earth-engine/datasets/catalog/NASA_MEASURES_GFCC_TC_v3
https://glad.umd.edu/dataset/gedi
https://www.hydrosheds.org/products/gloric
https://www.worldwildlife.org/pages/global-lakes-and-wetlands-database

# 5. Extra-stuff
* tell bRacatus people to use this R package and not the API?
* allow comparison restricted/extended (GIFT_references() _richness(), _lists())
* GIFT website: link to package; Alumni, update, Pierre, Julian, etc.
