
GIFT_checklist_conditional <- function(
  tax_group = 1,
  ## below are arguments from db_get_checklist_conditional()
  entity_class = c("Island","Island/Mainland","Mainland","Island Group","Island Part"), 
  native_indicated = F, natural_indicated = F, end_ref = F, end_list = F, 
  type_ref = 1:11,
  ref_included = c(1,2,3,4), #
  tax_group = 1, suit_geo = T, 
  exclude_restricted = T, include_names_unique = F, return_query_only = F,
  complete_taxonomy = TRUE){
  
  # 1. Controls ----
  # Package dependencies
  require(dplyr)
  require(jsonlite)
  
  # Arguments
  if(!is.logical(restricted)){
    stop("'restricted' must be a boolean indicating whether you want access to restricted data.")
  }
  
  # 2. Query ----
  tmp <- jsonlite::read_json(paste0(api, "?query=lists"),
                             simplifyVector = TRUE)
  
  # Second query coming
  # SELECT taxon_ID FROM taxonomy WHERE taxonomy.lft >= (SELECT taxonomy.lft FROM taxonomy WHERE taxonomy.taxon_ID = 26) AND taxonomy.rgt <= (SELECT taxonomy.rgt FROM taxonomy WHERE taxonomy.taxon_ID = 26) 
  # SELECT taxon_ID FROM taxonomy WHERE taxonomy.lft < (SELECT taxonomy.lft FROM taxonomy WHERE taxonomy.taxon_ID = 26) AND taxonomy.rgt > (SELECT taxonomy.rgt FROM taxonomy WHERE taxonomy.taxon_ID = 26) 
  
  taxon_query <- jsonlite::read_json(paste0(api, "?query=taxonomy"),
                                     simplifyVector = TRUE)
  
  # Numeric columns
  taxon_query[c("lft", "rgt")] <-
    sapply(taxon_query[c("lft", "rgt")], as.numeric)
  
  left_border <- taxon_query[which(taxon_query$taxon_ID == tax_group), "lft"]
  right_border <- taxon_query[which(taxon_query$taxon_ID == tax_group), "rgt"]
  
  included_taxa_below <- taxon_query[which(taxon_query$lft >= left_border &
                                             taxon_query$rgt <= right_border),
                                     "taxon_ID"]
  included_taxa_above <- taxon_query[which(taxon_query$lft < left_border &
                                             taxon_query$rgt > right_border),
                                     "taxon_ID"]
  included_taxa <- c(included_taxa_below, included_taxa_above)
  
  return(tmp)
  
  ##
  # Function to get species lists that fulfill certain criteria and contain species that fall into a certain taxonomic group
  # Function returns only species that belong into the specified taxon. 
  # conn = DB_connect()
  # list_set = DBI::dbGetQuery(conn, "SELECT 
  #                            `lists`.*, 
  #                            `reference_tax`.`taxon_ID`,
  #                            `references`.`included`,
  #                            `references`.`native_indicated`,
  #                            `references`.`natural_indicated`,
  #                            `references`.`end_ref`,
  #                            `references`.`type_ref`,
  #                            `references`.`geo_entity_ref`,
  #                            `references`.`restricted`,
  #                            `geoentities`.`entity_class`,
  #                            `geoentities`.`entity_type`,
  #                            `geoentities`.`suit_geo`,
  #                            `geoentities`.`suit_geo_rst`
  #                            FROM `lists` 
  #                            LEFT JOIN `references` ON `lists`.`ref_ID` = `references`.`ref_ID`
  #                            LEFT JOIN `geoentities` ON `lists`.`entity_ID` = `geoentities`.`entity_ID`
  #                            LEFT JOIN `reference_tax` ON `references`.`ref_ID` = `reference_tax`.`ref_ID`
  #                            WHERE `references`.`processed` = 1") 
  
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
  if(suit_geo){ # suit_geo
    if(exclude_restricted){
      list_set = list_set[which(list_set$suit_geo == 1),]
    } else {
      list_set = list_set[which(list_set$suit_geo_rst == 1),]
    }
  }
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
