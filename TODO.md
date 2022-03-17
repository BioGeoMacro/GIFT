# GIFT

**<span style="color:blue"><font size="4">TO DO list</span></font>**

# Table of Contents
1. [Functions](#functions)
2. [Vignette](#vignette)
3. [Database](#Database)

# 1. Functions

*Finished*
    - GIFT_checklist()
    - GIFT_checklist_raw()
    - GIFT_checklist_conditional()
    - med()
    - GIFT_env()
    - GIFT_spatial()
    
*To update*
    - GIFT_references()
    - GIFT_traits()
    - GIFT_env_meta_misc()
    - GIFT_env_meta_raster()
    - GIFT_lists()
    - GIFT_species()
    - GIFT_taxonomy()
    - remove_overlap()

## 1.1. Details
* GIFT_spatial
    - Check whether function works with POINT, MULTILINESTRING, etc
    - Make the function work with single point coordinate
    - Have a smaller shapefile than med

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

* GIFT_species_distribution()
    - GIFT_species() either comes first in the workflow or we run it internally
    - output like on POWO website: lists of polygons where the focal species is native/alien/naturalized
    - following logic of bRacatus R package (https://cran.r-project.org/web/packages/bRacatus/vignettes/Using_bRacatus.html)
    
* get_checklists_spatial
    - include biome layer, country layer etc to choose from
    - submit own coordinates or polygons or lines
    - input: shape and criteria (calling get_checklists_conditional)
    - output: lists falling within the area required

* plotting
* assign_higher_taxa
* references/citation
* env_raster
* env_misc

* range_finder

* versions of GIFT => list of entity_ID/list_ID trait_ID at a given time + upon request message OR restore the whole database at a given time

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
