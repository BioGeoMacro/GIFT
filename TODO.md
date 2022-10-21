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

## 0. General

    - all calls of GIFT functions inside of other functions need the version
  and api forwarded!!!


## 1. Details
* 1.1. GIFT_checklists_raw
    - add message on taxonomic status for original names (Pierre)  
    
* 1.2. GIFT_checklists_conditional
    - To be improved: * leveling-up (if complete fern and seed plant lists => you have a complete Tracheophyta list): count unique genera in all available groups and compare to unique genera included in desired group
    - add geo_entity_ref column behind geoentity column (Patrick)

* 1.3. GIFT_spatial

* 1.4. GIFT_taxonomy()  

* 1.5. GIFT_tax_group()

* 1.6. GIFT_no_overlap() (last remove_overlapping_regions):

* 1.7. GIFT_checklist() : wrapper for GIFT_checklist_conditional() and _spatial()
    - check output when floristic_group argument has a length of two (e.g. endemic and native) Should not be possible

* 2.1. GIFT_traits_meta()

* 2.2. GIFT_traits()
    - message for version 1.0: no bias information considered. 

* 2.3. GIFT_traits_raw()
    - message: bias_ref and bias_deriv do not exist in version 1.0
    - write into help that trait values are character values (even if numbers)
    - make orig_names optional?

* 3.1. GIFT_env()
    - highlight GIFT_env_meta for getting overview and citations
    - GIFT_env() => message if selected NAs included
    - control for sumstats argument

* 3.2. GIFT_env_meta_misc()    

* 3.3. GIFT_env_meta_raster()  

* 4.1. GIFT_species()

* 4.2. GIFT_ranges()
    - translate range_finder?

* 5.1. GIFT_shape()
    - give back warning if not all entity_IDs have polygons?
    - Put old polygons of old versions into respective folders
    
* 5.2. GIFT_plot() => 4 (or 5 options): species_richness, trait coverage, envt variable, species distribution, own variable (example for the vignette: proportion of Orchidaceae across polygons)
    - works with from GIFT_checklist() or GIFT_checklist_conditional() (faster because there is no need to retrieve the species occurrences) => species richness map
    - trait coverage (needs GIFT_checklist_conditional() to get taxonomic group/biogeography status and the table)

* 6.1. med()

* 6.2. GIFT_references():
    - restricted data has extra columns: OK for controls and tests?

* GIFT_species_lookup() (Patrick)
    - get rid of orig ID to be faster? Make simple version at least.

* GIFT_species_distribution() (Patrick)
    -  make simpler version of GIFT_species() for matchednames

* GIFT_region()
     - R function (Pierre) => returns a dataframe with the following columns:

* get_checklists_spatial
    - include biome layer, country layer etc to choose from

* GloNAF overlap function (Patrick)
    - make query, then simple function


* versions of GIFT:
    - restore old polygons for versions < 2.2 (Patrick)

## 1.2. Controls
Make a clear error message in case of wrong API (have a clear error message for version)

# 2. Vignette

# 4. Queries
* Make a dependency graph
* In queries.php (WinSCP) => put as a comment in which R function queries are used

# 5. GIFT admin/database
* Traits: aggregation of categorical traits (agreement to revise (for ex. shrub and herb/shrub is NA so far))
* Traits: continuous traits: add a column sd/var in traits_final (or we put it in agreement column which is only for categorical traits so far)

# 6. Extra-stuff
* tell bRacatus people to use this R package and not the API?
