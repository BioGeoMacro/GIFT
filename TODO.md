# GIFT

**<span style="color:blue"><font size="4">TO DO list</span></font>**

# Table of Contents
1. [Functions](#functions)
2. [Vignette](#vignette)
3. [Database](#Database)

## 1. Functions

* GIFT_env_meta
    - Update/combine GIFT_env_meta_misc.R and GIFT_env_meta_raster.R (two arguments in that function)
        Create GIFT_env() => two arguments 

* GIFT_env
    - highlight GIFT_env_meta for getting overview and citations
    - Controls for arguments
    - GIFT_env() => message if selected NAs included
    
* get_checklists_raw
    - ~~input: list of list_ID~~
    - ~~output: associated list~~

* get_checklists_conditional
    - input: criteria
    - output: lists matching the criteria
    - To be improved: * simplify the filtering in the wrapper
                      * leveling-up (if complete fern and seed plant lists => you have a complete Tracheophyta list)

* GIFT_checklist : wrapper for GIFT_checklist_conditional() and _spatial()
    - needs to be simple!

* get_checklists_spatial
    - include biome layer, country layer etc to choose from
    - submit own coordinates or polygons or lines
    - input: shape and criteria (calling get_checklists_conditional)
    - output: lists falling within the area required

* remove_overlapping_regions
* plotting
* assign_higher_taxa
* references/citation
* env_raster
* env_misc

* range_finder

* versions of GIFT => list of entity_ID/list_ID trait_ID at a given time + upon request message OR restore the whole database at a given time

## 2. Vignette


## 3. Database
* spatial Database?
