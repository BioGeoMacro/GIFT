library(RMySQL)
library(plyr)
#library(doSNOW)
#library(parallel)
library(reshape2)
library(sp)
library(rgdal)
library(raster)

####################################################

get_credentials = function(){
  username <- readline("Enter username: ")
  password <- readline("Enter password: ")
  assign(x = "credentials", value = list(username,password), envir = globalenv())
  lockBinding("credentials", globalenv())
}


####################################################

DB_get_ID_lookup = function(){
  conn = DB_connect()
  lookup = DBI::dbGetQuery(conn, "SELECT DISTINCT `species_original`.`orig_ID`, `species_original`.`name_ID`, `names_matched`.`work_ID`
                             FROM `species_original` INNER JOIN `names_matched`
                             ON `species_original`.`name_ID` = `names_matched`.`name_ID`")
  DBI::dbDisconnect(conn)
  return(lookup)
}

####################################################

DB_get_checklists_raw = function(list_IDs){
  conn = DB_connect()
  clist = DBI::dbGetQuery(conn, paste("SELECT species_original.list_ID, 
                                 names_matched.work_ID, 
                                 names_work_unique.species, 
                                 species_original.questionable, 
                                 species_original.native, 
                                 species_original.quest_native, 
                                 species_original.naturalized, 
                                 species_original.endemic_ref, 
                                 species_original.quest_end_ref, 
                                 species_original.endemic_list, 
                                 species_original.quest_end_list 
                                 FROM species_original LEFT JOIN names_matched
                                    LEFT JOIN names_work_unique 
                                    ON names_matched.work_ID = names_work_unique.work_ID
                                 ON species_original.name_ID = names_matched.name_ID 
                                 WHERE species_original.list_ID IN (", paste(list_IDs, collapse = ","), ")", sep = ""))
  
  DBI::dbDisconnect(conn)
  clist = unique(clist[which(!is.na(clist$work_ID)),])
  return(clist)
}

#####################################################

DB_get_checklists_conditional = function(entity_class = c("Island","Island/Mainland","Mainland","Island Group","Island Part"), 
                                         native_indicated = F, natural_indicated = F, end_ref = F, end_list = F, 
                                         type_ref = 1:11, ref_included = c(1,2,3,4), tax_group = 1, suit_geo = T, 
                                         exclude_restricted = T, include_names_unique = F, return_query_only = F, complete_taxonomy = TRUE){
  # Function to get species lists that fulfill certain criteria and contain species that fall into a certain taxonomic group
  # Function returns only species that belong into the specified taxon. 
  conn = DB_connect()
  list_set = DBI::dbGetQuery(conn, "SELECT 
                             `lists`.*, 
                             `reference_tax`.`taxon_ID`,
                             `references`.`included`,
                             `references`.`native_indicated`,
                             `references`.`natural_indicated`,
                             `references`.`end_ref`,
                             `references`.`type_ref`,
                             `references`.`geo_entity_ref`,
                             `references`.`restricted`,
                             `geoentities`.`entity_class`,
                             `geoentities`.`entity_type`,
                             `geoentities`.`suit_geo`
                             FROM `lists` 
                             LEFT JOIN `references` ON `lists`.`ref_ID` = `references`.`ref_ID`
                             LEFT JOIN `geoentities` ON `lists`.`entity_ID` = `geoentities`.`entity_ID`
                             LEFT JOIN `reference_tax` ON `references`.`ref_ID` = `reference_tax`.`ref_ID`
                             WHERE `references`.`processed` = 1") 
  
  # include only lists for taxonomic groups that cover the target taxonomic group or fall into the target taxonomic group
  included_taxa <- DBI::dbGetQuery(conn, paste("SELECT taxon_ID FROM taxonomy WHERE 
                                               taxonomy.lft >= (SELECT taxonomy.lft FROM taxonomy WHERE taxonomy.taxon_ID = ", tax_group , ") AND 
                                               taxonomy.rgt <= (SELECT taxonomy.rgt FROM taxonomy WHERE taxonomy.taxon_ID = ", tax_group , ")",  sep=""))[,1]
  included_taxa <- c(included_taxa,DBI::dbGetQuery(conn, paste("SELECT taxon_ID FROM taxonomy WHERE 
                                                               taxonomy.lft < (SELECT taxonomy.lft FROM taxonomy WHERE taxonomy.taxon_ID = ", tax_group , ") AND 
                                                               taxonomy.rgt > (SELECT taxonomy.rgt FROM taxonomy WHERE taxonomy.taxon_ID = ", tax_group , ")",  sep=""))[,1])
  
  # Subset list_set 
  list_set = list_set[which(list_set$suit_list == 1),] # use only suitable lists
  list_set = list_set[which(list_set$taxon_ID %in% included_taxa),] # tax_group
  list_set = list_set[which(list_set$included %in% ref_included),] # status
  list_set = list_set[which(list_set$type_ref %in% type_ref),] # type_ref
  list_set = list_set[which(list_set$entity_class %in% entity_class),] # entity_class
  
  if(native_indicated){list_set = list_set[which(list_set$native_indicated == 1),]} # native_indicated
  if(natural_indicated){list_set = list_set[which(list_set$natural_indicated == 1),]} # natural_indicated
  if(end_ref){list_set = list_set[which(list_set$end_ref == 1),]} # end_ref
  if(end_list){list_set = list_set[which(list_set$end_list == 1),]} # end_list
  if(suit_geo){list_set = list_set[which(list_set$suit_geo == 1),]} # suit_geo
  if(exclude_restricted){list_set = list_set[which(list_set$restricted == 0),]} # restricted
  
  # exclude geoentities for which the taxonomic group is incompletely covered
  taxonomy <- DBI::dbGetQuery(conn, "SELECT * FROM taxonomy")
  
  if(complete_taxonomy){
    list_set = ddply(list_set, .variables = .(entity_ID), .fun = function(x){
      range_covered <- max(apply(taxonomy[which(taxonomy$taxon_ID %in% x$taxon_ID),c("lft","rgt")],1,diff))
      range_taxgroup <- apply(taxonomy[which(taxonomy$taxon_ID == tax_group),c("lft","rgt")],1,diff)
      
      if(range_covered < range_taxgroup){return(NULL)}
      return(x)
    })
  }
  #TODO account for cases in which we don't have a list for a taxonomic group but for all subgroups included
  
  
  # Produce output
  if(return_query_only){
    DBI::dbDisconnect(conn)
    return(list_set)
  }
  
  checklists = DBI::dbGetQuery(conn, paste("SELECT species_original.list_ID,",
                                           ifelse(include_names_unique,
                                                  "names_unique.name_ID,
                                            names_unique.genus,
                                            names_unique.species_epithet,
                                            names_unique.subtaxon,
                                            names_unique.author,
                                            names_matched.matched,
                                            names_matched.epithetscore,
                                            names_matched.overallscore,
                                            names_matched.resolved,
                                            names_matched.service,",""),
                                           "names_matched.work_ID, 
                                            names_work_unique.species,
                                            species_original.questionable, 
                                            species_original.native, 
                                            species_original.quest_native, 
                                            species_original.naturalized, 
                                            species_original.endemic_ref, 
                                            species_original.quest_end_ref, 
                                            species_original.endemic_list, 
                                            species_original.quest_end_list,
                                            species_original.cons_status
                                            FROM species_original ",
                                           ifelse(include_names_unique,"LEFT JOIN names_unique ",""),
                                           "LEFT JOIN names_matched
                                            LEFT JOIN names_work_unique
                                            LEFT JOIN taxonomy
                                            ON names_work_unique.genus = taxonomy.taxon_ID
                                            ON names_matched.work_ID = names_work_unique.work_ID ",
                                           ifelse(include_names_unique,"ON names_unique.name_ID = names_matched.name_ID ",""),
                                           "ON species_original.name_ID = names_",ifelse(include_names_unique,"unique","matched"),".name_ID
                                            WHERE taxonomy.lft>(SELECT taxonomy.lft FROM taxonomy WHERE taxonomy.taxon_ID=",tax_group,") 
                                            AND taxonomy.rgt<(SELECT taxonomy.rgt FROM taxonomy WHERE taxonomy.taxon_ID=",tax_group,")", 
                                           sep = ""))
  DBI::dbDisconnect(conn)
  checklists = checklists[which(checklists$list_ID %in% list_set$list_ID),] # makes it a bit faster
  checklists = unique(checklists[which(!is.na(checklists$work_ID)),])
  checklists = join(unique(list_set[,c("list_ID","ref_ID","entity_ID")]),checklists,by="list_ID",type="inner")
  return(checklists)
}


####################################################
# function to remove overlapping entities

remove_overlapping_entities <- function(entity_IDs, area_th_island = 0, area_th_mainland = 100, overlap_th = 0.1, geoentities_overlap = NULL){
  
  if(is.null(geoentities_overlap)){
    conn = DB_connect()
    geoentities_overlap <- dbGetQuery(conn, "SELECT geoentities_overlap.entity1, geoentities_overlap.entity2, geoentities_overlap.overlap12, geoentities_overlap.overlap21, 
                                    geoentities_env_misc.area AS area1, geoentities_env_misc_1.area AS area2, geoentities.entity_class AS entity_class2
                                    FROM geoentities INNER JOIN (geoentities_env_misc INNER JOIN (geoentities_env_misc AS geoentities_env_misc_1 INNER JOIN geoentities_overlap 
                                    ON geoentities_env_misc_1.entity_ID = geoentities_overlap.entity2) ON geoentities_env_misc.entity_ID = geoentities_overlap.entity1) 
                                    ON geoentities.entity_ID = geoentities_overlap.entity2")
    dbDisconnect(conn)
  }
  geoentities_overlap <- geoentities_overlap[which(geoentities_overlap$entity1 %in% entity_IDs & geoentities_overlap$entity2 %in% entity_IDs),]
  geoentities_overlap <- geoentities_overlap[which(geoentities_overlap$overlap12 > overlap_th | geoentities_overlap$overlap21 > overlap_th),]
  
  #geoentities_overlap <- geoentities_overlap[order(geoentities_overlap$area1, decreasing = TRUE),]
  
  entity_IDs_tocheck <- unique(geoentities_overlap$entity1)
  
  # take the smaller entity if larger than threshhold
  for (i in 1:length(entity_IDs_tocheck)){
    th <- ifelse(geoentities_overlap$entity_class2[which(geoentities_overlap$entity2 == entity_IDs_tocheck[i])][1] %in% c("Mainland","Island/Mainland"),area_th_mainland,area_th_island)
    subset.tocheck <- geoentities_overlap[which(geoentities_overlap$entity1 == entity_IDs_tocheck[i]),]
    subset.tocheck$th <- ifelse(subset.tocheck$entity_class2 %in% c("Mainland","Island/Mainland"),area_th_mainland,area_th_island)
    if(length(which(subset.tocheck$area1>subset.tocheck$area2 & subset.tocheck$area2>subset.tocheck$th))>0 |
       length(which(subset.tocheck$area1<subset.tocheck$area2 & subset.tocheck$area1<th))>0) {
      entity_IDs <- entity_IDs[which(entity_IDs!=entity_IDs_tocheck[i])]
    }
  }
  
  geoentities_overlap <- geoentities_overlap[which(geoentities_overlap$entity1 %in% entity_IDs & geoentities_overlap$entity2 %in% entity_IDs),]
  # make warning if this data.frame still contains data
  
  return(entity_IDs)
}


####################################################
trait_IDs = c("1.3.1","1.2.1")
agreement = 0.66; bias_ref = 0; bias_deriv = 0; restricted = 0

DB_get_traits = function(trait_IDs = c(""), agreement = 0.66, bias_ref = 0, bias_deriv = 0, restricted = 0){
  # trait_IDs can either be "all" or a vector of trait_IDs

    # make error message when no trait given
  # decrease memory usage in php to reasonable amount: https://stackoverflow.com/questions/415801/allowed-memory-size-of-33554432-bytes-exhausted-tried-to-allocate-43148176-byte
  
  trait_list <- list()

  for (i in 1:length(trait_IDs)){
    trait_list[[i]] <- read_json(paste0("http://",credentials[[1]],":",credentials[[2]],"@gift.uni-goettingen.de/api/extended/index.php?query=traits&traitid=",trait_IDs[i],"&restricted=",restricted), simplifyVector = TRUE)
  }
  
  
  #trait_list <- traits_list[which(trait_list$agreement > agreement & trait_list$bias_by_reference <= bias_ref & trait_list$bias_by_derivation <= bias_deriv),]
  
  #trait_list = reshape2::dcast(data = trait_list, formula = work_ID ~ trait_ID, value.var = "trait_value")
  return(trait_list)
}



####################################################

DB_get_env_raster = function(entity_IDs = NULL, 
                             layers = c("bio_1", "bio_4", "bio_12", "bio_15", "elev"), 
                             metrics = c("med")){
  conn = DB_connect()
  query = paste("SELECT `entity_ID`, `layer_name`, ", paste(metrics, collapse = ","), " FROM `geoentities_env_raster` WHERE `layer_name` IN(", paste(shQuote(layers), collapse = ","), ")", sep = "")
  env_table = dbGetQuery(conn, query)
  env_table = data.table::dcast(data.table::setDT(env_table), entity_ID ~ layer_name, value.var = colnames(env_table)[3:ncol(env_table)])
  dbDisconnect(conn)
  env_table = as.data.frame(env_table)
  if(is.null(entity_IDs)){
    return(env_table)
  } else {
    return(subset(env_table, entity_ID %in% entity_IDs))
  }
}

####################################################

assign_higher_taxa <- function(work_IDs, taxon_lvl=NA, higher_lvl=FALSE, return_ID=FALSE){
  conn = DB_connect()
  on.exit(dbDisconnect(conn))
  species = dbGetQuery(conn, "SELECT work_ID,genus FROM names_work_unique")
  if(!all(work_IDs %in% species$work_ID)) stop("Not all work_IDs found!")
  species = species[match(work_IDs,species$work_ID),]
  genera = unique(species$genus)
  taxonomy = suppressWarnings(dbGetQuery(conn, "SELECT * FROM taxonomy"))
  taxa = sapply(genera, function(x) {
    taxa = taxonomy[which(taxonomy$lft < taxonomy$lft[which(taxonomy$taxon_ID == x)] 
                          & taxonomy$rgt > taxonomy$rgt[which(taxonomy$taxon_ID == x)]),]
    if(higher_lvl){
      taxa = taxa[grep("level",taxa$taxon_lvl),]
    }
    if(is.na(taxon_lvl)) {
      taxa = taxa[which.min(taxa$rgt-taxa$lft),]
    } else {
      taxa = taxa[which(taxa$taxon_lvl == taxon_lvl),]
    }
    ifelse(return_ID, taxa[,"taxon_ID"], taxa[,"taxon_name"])
  })
  return(taxa[match(species$genus,genera)])
}


#######################################################
# universal function for plotting geoentities based on 
# richness or environmental data

plot_geoentities = function(geoentities, entity_properties = NULL, display = c("none","richness","trait_coverage","env_raster","env_misc","own_values"),tax_group = "Tracheophyta",
                            floristic_subset = c("native_proc","native_rst","native_raw","total_raw","total_proc","total_rst","naturalized_raw",
                                                 "naturalized_proc","naturalized_rst","endemic_raw_min","endemic_raw_max","endemic_proc_min",
                                                 "endemic_proc_max","endemic_rst_min","endemic_rst_max"),
                            trait_ID = "1.1.1", raster_layer = "mn30_grd", rst_summary = c("mean","min","max","med","sd","n"), misc_variable = "area",
                            logvar=FALSE, transfactor = 1, suit_geo = FALSE, remove_overlapping = FALSE, threshold_area = 25000, proj4string = "+proj=eck4",
                            color_gradient = c("blue", "yellow","red"), orderbyvalue_points = FALSE, make_layout = TRUE,
                            args_layout = list(), args_par = list(), args_title = list(), args_poly = list(), args_points = list(), args_points_0 = list(), args_legend_par = list(), args_legend = list()){
  
  conn = DB_connect()
  
  if (is.null(entity_properties)){
    if (display[1] %in% c("none","richness")){
      entity_properties = dbGetQuery(conn, paste("SELECT `geoentities_specs`.`entity_ID`, ",floristic_subset[1]," AS value FROM `geoentities_specs` 
                                                 LEFT JOIN `taxonomy` ON `geoentities_specs`.`taxon_ID` = `taxonomy`.`taxon_ID`
                                                 WHERE `",floristic_subset[1],"` IS NOT NULL AND `taxon_name` = '",tax_group,"'",sep=""))
    } else if (display[1] == "trait_coverage"){
      entity_properties = dbGetQuery(conn, paste("SELECT `geoentities_trait_coverage`.`entity_ID`, ",floristic_subset[1]," AS value FROM `geoentities_trait_coverage` 
                                                 LEFT JOIN `taxonomy` ON `geoentities_trait_coverage`.`taxon_ID` = `taxonomy`.`taxon_ID`
                                                 WHERE `",floristic_subset[1],"` IS NOT NULL AND `taxon_name` = '",tax_group,"' AND `trait_ID` = '",trait_ID,"'",sep=""))
    } else if (display[1] == "env_raster"){
      entity_properties = dbGetQuery(conn, paste("SELECT `geoentities_env_raster`.`entity_ID`, ",rst_summary[1]," AS value FROM `geoentities_env_raster` 
                                                 WHERE `",rst_summary[1],"` IS NOT NULL AND `layer_name` = '",raster_layer,"'",sep=""))
    } else if (display[1] == "env_misc"){
      entity_properties = dbGetQuery(conn, paste("SELECT `geoentities_env_misc`.`entity_ID`, ",misc_variable," AS value FROM `geoentities_env_misc` 
                                                 WHERE `",misc_variable[1],"` IS NOT NULL",sep=""))
    }
  }
  dbDisconnect(conn)  
  
  # subset geoentities shapefile  
  geoentities <- geoentities[which(geoentities@data$entity_ID %in% unique(entity_properties$entity_ID)),] # polygons to plot
  geoentities@data <- join(geoentities@data,entity_properties, by="entity_ID", type="inner")
  
  if(suit_geo){ geoentities <- geoentities[which(geoentities$suit_geo == 1),]}
  
  # remove overlapping entities
  if(remove_overlapping) {
    geoentities <- geoentities[which(geoentities@data$entity_ID %in% remove_overlapping_entities(geoentities@data$entity_ID, area_th_island = 0, area_th_mainland = 100, overlap_th = 0.1)),]
  }
  
  # Transformation  
  toadd <- ifelse(min(geoentities@data$value)<1,1-min(geoentities@data$value),0)
  if(logvar) {geoentities@data$value <- log10(geoentities@data$value + toadd)}
  
  # create colour gradient
  if(display[1] == "trait_coverage" & logvar==FALSE){
    colors =  colorRamp(color_gradient)(geoentities@data$value)
  } else {
    colors =  colorRamp(color_gradient)((geoentities@data$value-(min(geoentities@data$value)))/(max(geoentities@data$value)-min(geoentities@data$value)))
  }
  geoentities@data$color = rgb(colors[,1], colors[,2], colors[,3], maxColorValue = 255)
  
  # create Spatial points data.frame for entities smaller than threshold area
  if (threshold_area > min(geoentities@data$area,na.rm = TRUE)){
    geoentities_points <- SpatialPointsDataFrame(geoentities@data[which(geoentities@data$area < threshold_area),c("point_x","point_y")],
                                                 geoentities@data[which(geoentities@data$area < threshold_area),], proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    if (orderbyvalue_points){
      geoentities_points = geoentities_points[order(geoentities_points@data$value),]
    }
    if (as.character(geoentities_points@proj4string) != proj4string){
      geoentities_points = spTransform(geoentities_points, CRS(proj4string))
    }
  }  
  if(length(ls(envir = globalenv(), pattern = "continents")) == 0){
    load(paste(wd_path,"Geodata/Continents.RData",sep=""))
  }
  if(length(ls(envir = globalenv(), pattern = "worldmapframe")) == 0){
    load(paste(wd_path,"Geodata/Worldmapframe.RData",sep=""))
  }
  
  if (as.character(geoentities@proj4string) != proj4string){
    geoentities = spTransform(geoentities, CRS(proj4string))
  }
  if (as.character(continents@proj4string) != proj4string){
    continents = spTransform(continents, CRS(proj4string))
    worldmapframe = spTransform(worldmapframe, CRS(proj4string))
  }
  
  ### default plotting arguments
  # layout
  if(is.null(args_layout$heights)){args_layout$heights = c(8.6,1.4)}
  # par
  if(is.null(args_par$mar)){args_par$mar = c(0,1,2,1)} 
  # title
  if(is.null(args_title$main)){args_title$main = paste(ifelse(display[1]=="none", "Geoentities", paste(toupper(substr(display[1],1,1)),substr(display[1],2,nchar(display[1])),sep="")), 
                                                       switch(display[1], none = paste(tax_group, floristic_subset[1]),
                                                              richness = paste(tax_group, floristic_subset[1]),
                                                              trait_coverage = paste(tax_group, floristic_subset[1], trait_ID),
                                                              env_raster = paste(rst_summary[1], raster_layer),
                                                              env_misc = misc_variable,
                                                              own_values = ""))}
  if(is.null(args_title$line)){args_title$line = 0}
  # poly
  if(is.null(args_poly$col)){args_poly$col = rgb(1,0,0, alpha = 0.3)} 
  if(display[1]!="none"){args_poly$col = geoentities@data$color} 
  
  if(is.null(args_poly$border)){
    if(display[1]=="none"){
      args_poly$border = "tomato"
    } else {
      args_poly$border = geoentities@data$color
    } 
  }
  
  # points_0
  if(is.null(args_points_0$cex)){args_points_0$cex = 1}
  if(is.null(args_points_0$pch)){args_points_0$pch = 1}
  
  # points
  if(is.null(args_points$col)){args_points$col = rgb(1,0,0, alpha = 0.3)}
  if(display[1]!="none"){args_points$col = geoentities_points@data$color} 
  if(is.null(args_points$cex)){args_points$cex = 1}
  if(is.null(args_points$pch)){args_points$pch = 20}
  
  # par_legend
  if(is.null(args_legend_par$mar)){args_legend_par$mar = c(4,3,0.5,3)} 
  if(is.null(args_legend_par$mgp)){args_legend_par$mgp = c(2.5, 1, 0)} 
  
  # legend
  if(is.null(args_legend$xlab)){
    args_legend$xlab = switch(display[1], none = NA, richness = "Species number", trait_coverage = "Trait coverage", env_raster = raster_layer, env_misc = misc_variable, own_values = "Own variable")
  }
  
  if (display[1]!="none" & make_layout==TRUE){
    do.call(layout, unlist(list(list(mat=matrix(1:2, nrow = 2)), args_layout), recursive = FALSE))
  }
  
  do.call(par, unlist(list(list(xaxs="r"),args_par), recursive = FALSE))
  
  plot(continents, border = "lightgrey", col = "lightgrey", lwd=0.5)
  
  do.call(title, args_title)
  
  plot(worldmapframe,border="#CCCCCC",add=T)
  
  do.call(plot, unlist(list(list(geoentities, add = T), args_poly), recursive = FALSE))
  if (threshold_area > min(geoentities@data$area,na.rm = TRUE)){
    if(!is.null(args_points_0$col)){do.call(plot, unlist(list(list(geoentities_points, add = T), args_points_0), recursive = FALSE))}
    do.call(plot, unlist(list(list(geoentities_points, add = T), args_points), recursive = FALSE))
  }
  if (display[1]!="none"){
    legend_cols = colorRamp(color_gradient)(seq(0,1,length.out = 25))
    legend_image <- as.raster(matrix(rgb(legend_cols[,1],legend_cols[,2],legend_cols[,3], maxColorValue = 255), nrow = 1))
    
    do.call(par, unlist(list(list(xaxs="i"),args_legend_par), recursive = FALSE))
    do.call(plot, unlist(list(list(c(0,1),c(0,1),type = 'n', axes = F, ylab=''), args_legend), recursive = FALSE))
    box()
    rasterImage(legend_image, 0, 0, 1, 1)
    if(display[1] == "trait_coverage" & logvar==FALSE){
      axis(1)
    } else {
      at_loc = c(0,0.2,0.4,0.6,0.8,1)
      at_values = seq(min(geoentities@data$value),max(geoentities@data$value),length.out = 6)
      
      if(logvar) {at_values <- 10^at_values - toadd}
      at_values <- at_values * transfactor
      
      digits <- 4
      at_values <- round(at_values,digits)
      if ((at_values[1] != 0 | at_values[2] > 0.0009) & max(abs(at_values)) >= 0.1) { digits <- 3 }
      if ((at_values[1] != 0 | at_values[2] > 0.009) & max(abs(at_values)) >= 1) { digits <- 2 }
      if ((at_values[1] != 0 | at_values[2] > 0.09) & max(abs(at_values)) >= 10) { digits <- 1 }
      if ((at_values[1] != 0 | at_values[2] > 0.9) & max(abs(at_values)) >= 100) { digits <- 0 }
      at_values <- round(at_values,digits)
      if (max(abs(at_values)) >= 1000) { digits <- NULL } 
      at_values <- formatC(at_values, big.mark=",", format = ifelse(max(abs(at_values))>=1000,"fg","f"), digits = digits)
      
      axis(1, at=at_loc, labels = at_values)
    }
  }
}

################################################################
# Obtain summary statistics about species ranges
range_finder <- function(work_IDs, tax_group = 1, native = TRUE, exclude_restricted = FALSE, consider_endemism = TRUE){
  
  # get all species records from GIFT
  species <- DB_get_checklists_conditional(entity_class = c("Island","Island/Mainland","Mainland","Island Group","Island Part"), 
                                           native_indicated = native, natural_indicated = FALSE, end_ref = FALSE, end_list = FALSE, 
                                           type_ref = 1:11, ref_included = c(1:9), tax_group = tax_group, suit_geo = FALSE, 
                                           exclude_restricted = exclude_restricted, include_names_unique = TRUE, return_query_only = FALSE, complete_taxonomy = FALSE)
  
  if (native) {
    # make subset of native species
    species <- species[which(species$native==1),]
  }
  
  # create empty data.frame for results
  ranges <- data.frame(work_ID = work_IDs, mainland = NA, island_mainland_min = NA, island_mainland_max = NA, island = NA, oceanic_island = NA, range_size_min = NA, range_size_max = NA, max_dist = NA, continents = NA, takhtajan = NA, global = NA)
  
  # subset all species by requested work_IDs
  species <- species[which(species$work_ID %in% work_IDs),]
  
  # load geoentities and geoentities_overlap
  conn = DB_connect()
  geoentities_overlap <- dbGetQuery(conn, "SELECT geoentities_overlap.entity1, geoentities_overlap.entity2, geoentities_overlap.overlap12, geoentities_overlap.overlap21, 
                                    geoentities_env_misc.area AS area1, geoentities_env_misc_1.area AS area2, geoentities.entity_class AS entity_class2
                                    FROM geoentities INNER JOIN (geoentities_env_misc INNER JOIN (geoentities_env_misc AS geoentities_env_misc_1 INNER JOIN geoentities_overlap 
                                    ON geoentities_env_misc_1.entity_ID = geoentities_overlap.entity2) ON geoentities_env_misc.entity_ID = geoentities_overlap.entity1) 
                                    ON geoentities.entity_ID = geoentities_overlap.entity2")
  
  geoentities <- dbGetQuery(conn, "SELECT geoentities.entity_ID, geoentities.geo_entity, geoentities.entity_class, geoentities.overlap_checked, geoentities_polygons.polygon_source, 
                            geoentities_env_misc.area, geoentities_env_misc.dist, geoentities_env_misc.GMMC, geoentities_env_misc.botanical_continent, geoentities_env_misc.biome, geoentities_env_misc.takhtajan
                            FROM (geoentities LEFT JOIN geoentities_polygons ON geoentities.entity_ID = geoentities_polygons.entity_ID) 
                            LEFT JOIN geoentities_env_misc ON geoentities.entity_ID = geoentities_env_misc.entity_ID")
  dbDisconnect(conn) 
  geoentities$polygon_source[which(geoentities$polygon_source == "none")] <- NA
  geoentities$polygon_source[!is.na(geoentities$polygon_source)] <- 1
  geoentities$polygon_source[is.na(geoentities$polygon_source)] <- 0
  geoentities$polygon_source <- as.numeric(geoentities$polygon_source)
  
  geoentities <- unique(geoentities)
  
  # join species and geoentities
  species <- join(species, geoentities, by="entity_ID", type="left")
  
  if (0 %in% unique(species$polygon_source)) warning("Not all regions have polygons assigned!!!")
  species <- species[which(species$polygon_source == 1),]
  
  if (0 %in% unique(species$overlap_checked)) warning("Spatial overlap has not yet been checked for all regions!!! Run DB_update_geoentities_overlap() first!")
  
  if(!all(!is.na(species$area))) warning("NA-values in entity area!!!")
  
  print(0)
  for (i in 1:nrow(ranges)){
    if((i %% 100) == 0) print(i)
    
    species_i <- species[which(species$work_ID == ranges$work_ID[i]),]
    
    if(nrow(species_i) > 0){
      
      # remove overlapping entities within refs for each species seperately
      # sometimes we have a species occurrences e.g. in a country but not in the subregions
      # this is usually the reason why we include the larger units per ref
      species_i <- ldply(lapply(unique(species_i$ref_ID), function(x) {
        species_i[which(species_i$ref_ID == x & species_i$entity_ID %in% remove_overlapping_entities(unique(species_i$entity_ID[which(species_i$ref_ID==x)]), area_th_island = 0, area_th_mainland = 0, overlap_th = 0.1, geoentities_overlap = geoentities_overlap)),]
      }))
      
      # remove overlapping entities; keep the smaller?
      tokeep_min <- remove_overlapping_entities(unique(species_i$entity_ID), area_th_island = 0, area_th_mainland = 0, overlap_th = 0.1, geoentities_overlap = geoentities_overlap)
      
      # remove overlapping entities; keep the larger?
      tokeep_max <- remove_overlapping_entities(unique(species_i$entity_ID), area_th_island = 100000000, area_th_mainland = 100000000, overlap_th = 0.1, geoentities_overlap = geoentities_overlap)
      
      area <- unique(species_i[,c("entity_ID","area")])
      
      ranges$range_size_min[i] <- sum(area$area[which(area$entity_ID %in% tokeep_min)], na.rm = TRUE)
      ranges$range_size_max[i] <- sum(area$area[which(area$entity_ID %in% tokeep_max)], na.rm = TRUE)
      
      ranges$max_dist[i] <- max(species_i$dist, na.rm = TRUE)
      ranges$oceanic_island[i] <- as.numeric(min(species_i$GMMC, na.rm = TRUE)==0)
      
      ranges$mainland[i] <- as.numeric(nrow(species_i[which(species_i$entity_class == "Mainland"),])>0)
      ranges$island[i] <- as.numeric(nrow(species_i[which(species_i$entity_class %in% c("Island","Island Group","Island Part")),])>0)
      
      ranges$island_mainland_min[i] <- as.numeric(nrow(species_i[which(species_i$entity_class  == "Island/Mainland" & species_i$entity_ID %in% tokeep_min),])>0)
      ranges$island_mainland_max[i] <- as.numeric(nrow(species_i[which(species_i$entity_class  == "Island/Mainland" & species_i$entity_ID %in% tokeep_max),])>0)
      
      ranges$global[i] <- as.numeric(nrow(species_i[which(species_i$ref_ID %in% c(10321,10193,10251,10253)),])>0 | length(which(species_i$endemic_list==1 | species_i$endemic_ref==1 & is.na(species_i$subtaxon)))>0)
      
      if (native & consider_endemism) {
        if(length(which(species_i$endemic_list==1 & is.na(species_i$subtaxon) & paste(species_i$genus, species_i$species_epithet) == species_i$species & species_i$entity_class %in% c("Island","Island Group","Island Part")))>0) {
          ranges$island_mainland_min[i] <- 0
          ranges$island_mainland_max[i] <- 0
          ranges$mainland[i] <- 0
        } else {
          subset.i.ref <- species_i[which(species_i$endemic_ref==1 & is.na(species_i$subtaxon) & paste(species_i$genus, species_i$species_epithet) == species_i$species ),]
          if(nrow(subset.i.ref)>0 & all(subset.i.ref$entity_class %in% c("Island","Island Group","Island Part"))){
            ranges$island_mainland_min[i] <- 0
            ranges$island_mainland_max[i] <- 0
            ranges$mainland[i] <- 0
          }
        }
        
        if(length(which(species_i$endemic_list==1 & is.na(species_i$subtaxon) & paste(species_i$genus, species_i$species_epithet) == species_i$species & species_i$entity_class=="Mainland"))>0) {
          ranges$island_mainland_min[i] <- 0
          ranges$island_mainland_max[i] <- 0
          ranges$island[i] <- 0
        } else {
          subset.i.ref <- species_i[which(species_i$endemic_ref==1 & is.na(species_i$subtaxon) & paste(species_i$genus, species_i$species_epithet) == species_i$species ),]
          if(nrow(subset.i.ref)>0 & all(subset.i.ref$entity_class=="Mainland")){
            ranges$island_mainland_min[i] <- 0
            ranges$island_mainland_max[i] <- 0
            ranges$island[i] <- 0
          }
        }
      }
    }
    
    takhtajan <- unique(species_i$takhtajan)
    takhtajan <- takhtajan[order(takhtajan)]
    ranges$takhtajan[i] <- paste(takhtajan, collapse="; ")
    
    continents <- unique(species_i$botanical_continent)
    continents <- continents[order(continents)]
    ranges$continents[i] <- paste(continents, collapse="; ")
    
  }
  return(ranges)
}



