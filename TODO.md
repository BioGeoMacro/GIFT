# GIFT

**<span style="color:blue"><font size="4">TO DO list</span></font>**

# Table of Contents
1. [Functions](#functions)
2. [Vignette](#vignette)
3. [Database](#Database)

# 1. Functions

*New format*
    - GIFT_checklist()  
    - GIFT_checklist_raw()  
    - GIFT_checklist_conditional()  
    - med()  
    - GIFT_env()  
    - GIFT_spatial()  
    - GIFT_traits_meta()  
    - GIFT_references()  
    - GIFT_traits()  
    - GIFT_env_meta_misc()  
    - GIFT_env_meta_raster()  
    - GIFT_species()  
    - GIFT_taxonomy()  
    - GIFT_lists()  
  
*To update from GIFT_admin*
    - remove_overlap() => to be called GIFT_no_overlap()
    - plotting => to be called GIFT_plot()  
    - assign_higher_taxa => internal function for GIFT_species() (argument family=TRUE/FALSE)  
    - references/citation  => to be called GIFT_references
    - range_finder => 

## 1.1. Details
* GIFT_spatial
    - Check whether function works with POINT, MULTILINESTRING, etc
    - Make the function work with single point coordinate
    - Have a smaller shapefile than med
    - have option to download geojson based on a list of entity_ID

* GIFT_env_meta
    - Update/combine GIFT_env_meta_misc.R and GIFT_env_meta_raster.R (two arguments in that function)
        Create GIFT_env() => two arguments 

* GIFT_env
    - highlight GIFT_env_meta for getting overview and citations
    - Controls for arguments
    - GIFT_env() => message if selected NAs included
    
* GIFT_checklists_raw
    - ~~input: list of list_ID~~
    - ~~output: associated list~~

* GIFT_checklists_conditional
    - input: criteria
    - output: lists matching the criteria
    - To be improved: * simplify the filtering in the wrapper
                      * leveling-up (if complete fern and seed plant lists => you have a complete Tracheophyta list)
    - "translate" arguments for naive user
    - some parameters come as characters instead of numeric (nativeness 1 0)
    - Make the subsets more straightforward for users: for ex. nativeness, subset of $subset %in% native, native and naturalized etc AND $native_indicated==1 has to be done in the BACKGROUND

* GIFT_checklist : wrapper for GIFT_checklist_conditional() and _spatial()
    - needs to be simple!
    - add entity_ID to checklist table output

* GIFT_no_overlap() (last remove_overlapping_regions):
    - to do

* GIFT_species()
    - input: user's species name
    - output: table with user's input, whether the species are in GIFT, if yes corrected name

* GIFT_traits()
    - arguments: constraining output by species name (GIFT_species() either comes first in the workflow or we run it internally) or taxonomic group
    - we have to do it as it is now plus:
    - we need the possibility to have trait values down at the ref/orig species level (e.g. elevational range)
    - intraspecific variation (trait values from all references/species)
    - species names

* GIFT_species_distribution()
    - input: only one species at the moment
    - plotting argument => polygons where the sp is native/invasive/naturalized/absent
    - GIFT_species() run internally
    - output like on POWO website: lists of polygons where the focal species is native/alien/naturalized
    - following logic of bRacatus R package (https://cran.r-project.org/web/packages/bRacatus/vignettes/Using_bRacatus.html)
    
* get_checklists_spatial
    - include biome layer, country layer etc to choose from
    - submit own coordinates or polygons or lines
    - input: shape and criteria (calling get_checklists_conditional)
    - output: lists falling within the area required

* GIFT_plot() => 4 (or 5 options): species_richness, trait coverage, envt variable, species distribution, own variable (example for the vignette: proportion of Orchidaceae across polygons)
    - works with from GIFT_checklist() or GIFT_checklist_conditional() (faster because there is no need to retrieve the species occurrences) => species richness map
    - trait coverage (needs GIFT_checklist_conditional() to get taxonomic group/biogeography status and the table)

* GIFT_shape()
    - input: list of entity_ID
    - output: sf polygons of these IDs

* GIFT_tax_group(): update of assign_higher_taxa()  
    - input: species list (work_ID) and level of grouping  
    - output: a vector (not a species table with an extra column (level of grouping) because it is an internal function)
    
* GIFT_references(): before references/citation

* range_finder

* versions of GIFT: different options are list of entity_ID/list_ID trait_ID at a given time + upon request message OR restore the whole database at a given time
    - one API per version of the database
    - in R functions: argument 'version', if not specified, by default the function uses the most up-to-date version
    - if version is specified => API changed in the R function (correspondence version number versus API)

# 2. Vignette


# 3. Database
* spatial Database?

# 4. Queries
* Make a dependency graph
* In queries.php (WinSCP) => put as a comment in which R function queries are used

# 5. GIFT admin/database
* Traits: aggregation of categorical traits (agreement to revise (for ex. shrub and herb/shrub is NA so far))
* Traits: continuous traits: add a column sd/var in traits_final (or we put it in agreement column which is only for categorical traits so far)

# 6. Extra-stuff
* tell bRacatus people to use this R package and not the API?
