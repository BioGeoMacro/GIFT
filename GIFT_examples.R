############################################
### Examples
rm(list=ls())

### Load functions to connect to GIFT and retrieve data
source("GIFT_export_tools.R")

### Path to data on our network drive
wd_path <- ifelse(.Platform$OS.type == "windows", "M:/joint_projects/Checklist DB/","/media/Macroecology/joint_projects/Checklist DB/")

### Define user credentials and connection details. !!! Run this line seperately and not as part of a block of code !!!
get_credentials()

### Connect to GIFT-DB
conn <- DB_connect()


### Get information about content
dbGetQuery(conn, "SHOW TABLES")
dbGetQuery(conn, "DESCRIBE env_misc")


### Download metadata

# Meta information on miscellaneous environmental information in GIFT
env_misc <- dbGetQuery(conn, "SELECT * FROM env_misc")
head(env_misc)

# Meta information on environmental information from raster layers in GIFT
env_raster <- dbGetQuery(conn, "SELECT * FROM env_raster")

# Meta information on traits in GIFT
traits_meta <- dbGetQuery(conn, "SELECT * FROM traits_meta")

# Meta information on references in GIFT (<- )example for more complicated query; references needs to be put int ``)
references <- dbGetQuery(conn, "SELECT `references`.ref_ID, reference_citavi.ref_short, `references`.geo_entity_ref, reference_type.type, 
                         reference_included.subset,  taxonomy.taxon_name, `references`.native_indicated, `references`.end_ref, 
                         `references`.traits, `references`.proc_date
                         FROM (reference_type INNER JOIN (reference_included INNER JOIN ((reference_tax INNER JOIN `references` ON reference_tax.ref_ID = `references`.ref_ID) 
                         INNER JOIN reference_citavi ON `references`.citavi_seq_no = reference_citavi.citavi_seq_no) ON reference_included.ID = `references`.included) 
                         ON reference_type.ID = `references`.type_ref) INNER JOIN taxonomy ON reference_tax.taxon_ID = taxonomy.taxon_ID
                         WHERE (((`references`.checklist)=1) AND ((`references`.processed)=1) AND ((`references`.restricted)=0))")


### Download richness and checklist data

# check the tables taxonomy and ref_included for the settings below
dbGetQuery(conn, "SELECT * FROM taxonomy")[1:10,]
dbGetQuery(conn, "SELECT * FROM reference_included")

# Species richness
richness <- dbGetQuery(conn, "SELECT entity_ID, native_proc, endemic_proc_min FROM geoentities_specs WHERE taxon_ID = 2")


# Get all checklists based on certain criteria
checklists <- DB_get_checklists_conditional(entity_class = c("Island","Island/Mainland","Mainland","Island Group","Island Part"), 
                                            native_indicated = TRUE, natural_indicated = F, end_ref = F, end_list = F, 
                                            type_ref = 1:11, ref_included = c(1,2,3,4), tax_group = 2, suit_geo = T, 
                                            exclude_restricted = T, include_names_unique = F, return_query_only = F, complete_taxonomy = TRUE)

# Run the same thing with return_query_only = TRUE to get the entitity_ID's for which checklists data is available
lists <- DB_get_checklists_conditional(entity_class = c("Island","Island/Mainland","Mainland","Island Group","Island Part"), 
                                       native_indicated = TRUE, natural_indicated = F, end_ref = F, end_list = F, 
                                       type_ref = 1:11, ref_included = c(1,2,3,4), tax_group = 2, suit_geo = T, 
                                       exclude_restricted = T, include_names_unique = F, return_query_only = TRUE, complete_taxonomy = TRUE)


# Assign family names
checklists$family <- assign_higher_taxa(work_IDs=checklists$work_ID, taxon_lvl="family", higher_lvl=FALSE, return_ID=FALSE)

# Assign family names
checklists$group <- assign_higher_taxa(work_IDs=checklists$work_ID, taxon_lvl=NA, higher_lvl=TRUE, return_ID=FALSE)


### Download traits

# Retrieve all entries for certain traits
traits <- DB_get_traits(trait_IDs = c("1.1.1","1.2.1")) # woodiness and growth form 1: tree/shrub/herb


# Get species names for traits
working_names <- dbGetQuery(conn, "SELECT work_ID, species FROM names_work_unique")
traits <- join(traits,working_names, by="work_ID", type="left")


# Get all trees
trees <- dbGetQuery(conn, "SELECT names_work_unique.species, traits_final.trait_value, traits_final.`references`, traits_final.agreement
                    FROM traits_final INNER JOIN names_work_unique ON traits_final.work_ID = names_work_unique.work_ID
                    WHERE (traits_final.trait_ID='1.2.1' AND traits_final.restricted=0 AND ((traits_final.trait_value)='tree' Or (traits_final.trait_value)='shrub/tree' Or (traits_final.trait_value)='herb/tree'))")


### Download environmental information

# Miscellaneous environmental variables: for example area, isolation and biome
geoentities_env_misc <- dbGetQuery(conn, "SELECT entity_ID, area, dist, biome FROM geoentities_env_misc")

# Raster layers
geoentities_env_raster <- DB_get_env_raster(entity_IDs = NULL, layers = c("CHELSA_bio10_1", "CHELSA_bio10_12", "Crowther_Nature_Biome_Revision_01"), metrics = c("mean","med","n"))


### Spatial hierarchy

# Remove overlapping entities if you want
tokeep <- remove_overlapping_entities(unique(checklists$entity_ID), area_th_island = 0, area_th_mainland = 100, overlap_th = 0.1)


### Plotting

# Load geoentities shapefile
load(file = paste(wd_path,"Geodata/geoentities_simple.RDATA",sep=""))

# Project it
geoentities_simple <- spTransform(geoentities_simple, CRS("+proj=eck4"))


# Plot all entities with data for native vascular plants (default)
windows(width = 12, height = 8)
plot_geoentities(geoentities_simple, display = c("none","richness","trait_coverage","env_raster","env_misc"),tax_group = "Tracheophyta",
                 floristic_subset = c("native_proc","native_rst","native_raw","total_raw","total_proc","total_rst","naturalized_raw",
                                      "naturalized_proc","naturalized_rst","endemic_raw_min","endemic_raw_max","endemic_proc_min",
                                      "endemic_proc_max","endemic_rst_min","endemic_rst_max"),
                 trait_ID = "1.1.1", raster_layer = "mn30_grd", rst_summary = c("mean","min","max","med","sd","n"), misc_variable = "area",
                 logvar=FALSE, suit_geo = FALSE, remove_overlapping = FALSE, threshold_area = 25000, proj4string = as.character(geoentities_simple@proj4string),
                 color_gradient = c("blue", "yellow","red"), orderbyvalue_points = FALSE, make_layout = TRUE)

### Plot species richness of native angiosperms
plot_geoentities(geoentities_simple, display = "richness", tax_group = "Angiospermae", floristic_subset = "native_rst",
                 logvar=TRUE, suit_geo = FALSE, remove_overlapping = FALSE, threshold_area = 25000, proj4string = as.character(geoentities_simple@proj4string),
                 color_gradient = c("midnightblue","dodgerblue2","chartreuse4","gold","orangered1","darkred"), make_layout = TRUE, orderbyvalue_points = TRUE,
                 args_par = list(mar=c(0,0,2,0)), args_title = list(line=0, main="Species richness native Angiosperms"), args_poly = list(border = "grey", lwd=0.3),
                 args_points = list(cex=1), args_points_0 = list(cex=0.8, col = "grey"),
                 args_legend_par = list(mar = c(4.5,10,0,10), mgp = c(2.5, 0.5, 0)), args_legend = list())


### Plot trait coverage
plot_geoentities(geoentities_simple, display = "trait_coverage",tax_group = "Tracheophyta", floristic_subset = "native_rst",
                 trait_ID = "1.2.1", suit_geo = FALSE, remove_overlapping = FALSE, threshold_area = 25000, proj4string = as.character(geoentities_simple@proj4string),
                 color_gradient = c("red3","yellow","darkgreen"), make_layout = TRUE, orderbyvalue_points = TRUE,
                 args_par = list(mar=c(0,0,2,0)), args_title = list(line=0, main="Growth form (tree, shrub, herb)"), args_poly = list(border = "grey", lwd=0.3),
                 args_points = list(cex=1), args_points_0 = list(cex=0.8, col = "grey"),
                 args_legend_par = list(mar = c(4.5,10,0,10), mgp = c(2.5, 0.5, 0)), args_legend = list())


plot_geoentities(geoentities_simple, display = "trait_coverage",tax_group = "Tracheophyta", floristic_subset = "native_rst",
                 trait_ID = "1.6.2", suit_geo = FALSE, remove_overlapping = FALSE, threshold_area = 25000, proj4string = as.character(geoentities_simple@proj4string),
                 color_gradient = c("red3","yellow","darkgreen"), make_layout = TRUE, orderbyvalue_points = TRUE,
                 args_par = list(mar=c(0,0,2,0)), args_title = list(line=0, main="Plant height"), args_poly = list(border = "grey", lwd=0.3),
                 args_points = list(cex=1), args_points_0 = list(cex=0.8, col = "grey"),
                 args_legend_par = list(mar = c(4.5,10,0,10), mgp = c(2.5, 0.5, 0)), args_legend = list())

### Plot environmental variables
plot_geoentities(geoentities_simple, display = "env_misc", misc_variable = "SLMP", 
                 logvar=TRUE, suit_geo = FALSE, remove_overlapping = FALSE, threshold_area = 25000, proj4string = as.character(geoentities_simple@proj4string),
                 color_gradient = c("blue", "yellow","red"), make_layout = TRUE, orderbyvalue_points = TRUE,
                 args_par = list(mar=c(0,0,2,0)), args_title = list(line=0, main="Surrounding landmass"), args_poly = list(border = "grey", lwd=0.3),
                 args_points = list(cex=1), args_points_0 = list(cex=0.8, col = "grey"),
                 args_legend_par = list(mar = c(4.5,10,0,10), mgp = c(2.5, 0.5, 0)), args_legend = list(xlab="Surrounding landmass proportion"))


plot_geoentities(geoentities_simple, display = "env_raster", raster_layer = "mn30_grd", rst_summary = "mean",
                 logvar=FALSE, suit_geo = FALSE, remove_overlapping = FALSE, threshold_area = 25000, proj4string = as.character(geoentities_simple@proj4string),
                 color_gradient = c("blue", "yellow","red"), make_layout = TRUE, orderbyvalue_points = TRUE,
                 args_par = list(mar=c(0,0,2,0)), args_title = list(line=0, main="Elevation"), args_poly = list(border = "grey", lwd=0.3),
                 args_points = list(cex=1), args_points_0 = list(cex=0.8, col = "grey"),
                 args_legend_par = list(mar = c(4.5,10,0,10), mgp = c(2.5, 0.5, 0)), args_legend = list(xlab="Elevation a.s.l. (m)"))


species_ranges <- range_finder(work_IDs = c(1,2,3,118,188))

