# GIFT

**<span style="color:blue"><font size="4">TO DO list</span></font>**

# Table of Contents
1. [Functions](#functions)
2. [Vignette](#vignette)
3. [Database](#Database)

# 1. Functions

*Final check*  
1. Checklists  <br>
&nbsp;&nbsp;&nbsp;&nbsp; <span style="color:green">1.1. GIFT_checklist_raw()</span>  
&nbsp;&nbsp;&nbsp;&nbsp; <span style="color:green">1.2. GIFT_checklist_conditional()</span>   
&nbsp;&nbsp;&nbsp;&nbsp; 1.3. GIFT_spatial()  
&nbsp;&nbsp;&nbsp;&nbsp; 1.4. GIFT_taxonomy()   
&nbsp;&nbsp;&nbsp;&nbsp; 1.5. GIFT_taxgroup()  
&nbsp;&nbsp;&nbsp;&nbsp; 1.6. GIFT_no_overlap()  
&nbsp;&nbsp;&nbsp;&nbsp; 1.7. GIFT_checklist()  
  
2. Trait functions <br>
&nbsp;&nbsp;&nbsp;&nbsp; 2.1. GIFT_traits_meta()  
&nbsp;&nbsp;&nbsp;&nbsp; 2.2. GIFT_traits()  
&nbsp;&nbsp;&nbsp;&nbsp; 2.3. GIFT_traits_raw()  

3. Environment  
&nbsp;&nbsp;&nbsp;&nbsp; 3.1. GIFT_env()  
&nbsp;&nbsp;&nbsp;&nbsp; 3.2. GIFT_env_meta_misc()  
&nbsp;&nbsp;&nbsp;&nbsp; 3.3. GIFT_env_meta_raster()  

4. Species distribution  
&nbsp;&nbsp;&nbsp;&nbsp; 4.1. GIFT_species()  
&nbsp;&nbsp;&nbsp;&nbsp; 4.2. GIFT_ranges()  

5. Plotting functions  
&nbsp;&nbsp;&nbsp;&nbsp; 5.1. GIFT_shape()  
&nbsp;&nbsp;&nbsp;&nbsp; 5.2. GIFT_plot() 

6. Miscellaneous  
&nbsp;&nbsp;&nbsp;&nbsp; 6.1. med()  
&nbsp;&nbsp;&nbsp;&nbsp; 6.2. GIFT_references()  
&nbsp;&nbsp;&nbsp;&nbsp; 6.3. GIFT_geology()? generic queries?  

&nbsp;&nbsp;&nbsp;&nbsp; GIFT_lists() => useless function to be removed? 
  
*To update from GIFT_admin*<br>
    - plotting => to be called GIFT_plot()  <br>
    - assign_higher_taxa => internal function for GIFT_species() (argument family=TRUE/FALSE)  <br>
    - range_finder => <br>

## 1.1. Details
* 1.1. GIFT_checklists_raw (Pierre)  
    - add NA author column for old versions via api and remove according if statement (Patrick)
    - add NA columns for three new names matching columns and simplify as numeric (Patrick L 272) 
    
* 1.2. GIFT_checklists_conditional (Pierre)
    - To be improved: * leveling-up (if complete fern and seed plant lists => you have a complete Tracheophyta list): count unique genera in all available groups and compare to unique genera included in desired group
    - finish header (Pierre)
    - add geo_entity_ref column behind geoentity column (Patrick)

* 1.3. GIFT_spatial

* 1.4. GIFT_taxonomy()  

* 1.5. GIFT_tax_group()

* 1.6. GIFT_no_overlap() (last remove_overlapping_regions):
    - controls and output (Pierre)

* 1.7. GIFT_checklist() : wrapper for GIFT_checklist_conditional() and _spatial()
    - needs to be simple!
    - add entity_ID to checklist table output
    - add an argument to retrieve metadata only (like in GIFT_checklist_conditional())
    - check output when floristic_group argument has a length of two (e.g. endemic and native)
    - ~~add orig ID for link to traits_raw (Patrick)~~
    - ~~add additional match-up infos and author names (Patrick)~~
    - add message on taxonomic status for original names (Pierre)
    - naming: work_species

* 2.1. ~~GIFT_traits_meta() (Pierre)~~
    - ~~make work for version 1.0 (Patrick)~~

* 2.2. GIFT_traits()
    - arguments: constraining output by species name (GIFT_species() either comes first in the workflow or we run it internally) or taxonomic group
    - we have to do it as it is now plus:
    - we need the possibility to have trait values down at the ref/orig species level (e.g. elevational range)
    - intraspecific variation (trait values from all references/species)
    - species names
    - ~~needs to be updated for old GIFT versions without bias and restricted columns (Patrick)~~
    - ~~agreement and references per trait! long table?~~
    - message for version 1.0: no bias columns <- OR add NA columns?
    - add author names (Patrick)

* 2.3. GIFT_traits_raw()
    - ~~check error (only for restricted IP?) (Patrick)~~
    - ~~needs to be updated for old GIFT versions without bias column (Patrick)~~
    - ~~used tidyselect::starts_with() <- replace?~~
    - message: bias_ref and bias_deriv do not exist in version 1.0
    - write into help that trait values are character values
    - make orig_names optional?
    - progressbar for loop?

* 3.1. GIFT_env()
    - highlight GIFT_env_meta for getting overview and citations
    - Controls for arguments
    - GIFT_env() => message if selected NAs included
    - control for layer names and variables in env_raster_meta and env_misc_neta. Using the right api this will driectly control for restricted layers

* 3.2. GIFT_env_meta_misc()    
    - ~~restricted veriables (Patrick)~~
    
* 3.3. GIFT_env_meta_raster()  
    - ~~restricted raster layers (Patrick)~~

* 4.1. GIFT_species()
    - input: user's species name
    - output: table with user's input, whether the species are in GIFT, if yes corrected name
    - Is the current version of this function/query used at the moment?
    - Add author names? Would also add author names to GIFT_traits()
    
* 4.2. GIFT_ranges()

* 5.1. GIFT_shape()
    - give back warning if not all entity_IDs have polygons?
    - Put old polygons of old versions into respective folders and paste version in the R function
    - Add control for GIFT_version argument
    
* 5.2. GIFT_plot() => 4 (or 5 options): species_richness, trait coverage, envt variable, species distribution, own variable (example for the vignette: proportion of Orchidaceae across polygons)
    - works with from GIFT_checklist() or GIFT_checklist_conditional() (faster because there is no need to retrieve the species occurrences) => species richness map
    - trait coverage (needs GIFT_checklist_conditional() to get taxonomic group/biogeography status and the table)

* 6.1. med()

* 6.2. ~~GIFT_references(): before references/citation (Pierre)~~  
    - ~~remove comments, acknowledgements (Patrick)~~
    - ~~processed == 1 (Patrick)~~

* GIFT_species_lookup() (Patrick)
    - input: user's species name
    - output: table with user's input, whether the species are in GIFT, if yes corrected name
    - get rid of orig ID to be faster?

* GIFT_species_distribution() (Patrick)
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

* range_finder

* species numbers, trait coverage

* versions of GIFT: different options are list of entity_ID/list_ID trait_ID at a given time + upon request message OR restore the whole database at a given time
    - ~~one API per version of the database (Patrick)~~
    - restore old polygons for versions < 2.2 (Patrick)
    - account for old polygons of old versions in functions (Pierre)
    - in R functions: argument 'version', if not specified, by default the function uses the most up-to-date version
    - if version is specified => API changed in the R function (correspondence version number versus API)
    - versions of the shapefile for each database version has to be available

## 1.2. Controls
Make a clear error message in case of wrong API (have a clear error message for version)

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
* Restrict some checklists
