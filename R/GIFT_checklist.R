#' GIFT checklists
#'
#' Retrieve GIFT checklists that fulfill specific criteria.
#'
#' @param taxon_name Character string corresponding to the taxonomic group
#' of interest.
#' 
#' @param complete_taxon Boolean, default TRUE.
#' 
#' @param floristic_group Character among the following options: 'all',
#' 'native', 'endemic', 'naturalized'
#' 
#' @param complete_floristic Boolean, default TRUE.
#' 
#' @param geo_type Character, either 'Mainland' or 'Island'.
#' 
#' @param suit_geo Boolean.
#' 
#' @param shp Shapefile provided by the user.
#'
#' @param coordinates Custom set of coordinates. The format is a two columns,
#' the first one being longitudes and the second being latitudes. If 4
#' coordinates are given, the function assumes that these are the four corners
#' of a bounding box.
#' 
#' @param overlap character vector or list defining the raster
#' data to retrieve..
#' 
#' @param namesmatched Logical
#' 
#' @param api character string defining from which API the data will be retrieved.
#'  
#' @return
#' List with two elements: the checklist with species and the list of ID.
#'
#' @details Blabla.
#'
#' @references
#'      Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
#'      Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
#'      https://doi.org/10.1111/jbi.13623
#'
#' @seealso [GIFT::GIFT_checklist_raw()]
#'
#' @examples
#' \dontrun{
#' ex <- GIFT_checklist()
#' }
#' 
#' @export

GIFT_checklist <- function(
  # actual arguments user needs
  taxon_name = "Tracheophyta",
  complete_taxon = TRUE, # make a figure with orchids and angiosperms for ex.
  floristic_group = c("all", "native", "endemic", "naturalized")[2],
  # if native given => native_indicated = T in GIFT_checklist_conditional()
  complete_floristic = TRUE,
  
  geo_type = c("Mainland", "Island"), # Island gets you to Island, Island Group & Island Part
  # Mainland gets you to Mainland & Island/Mainland
  # c("Mainland", "Island")? gives you everything
  suit_geo = FALSE, # find better names for arguments
  
  # complete_floristic = T, # if you want native => are list with only endemics enough or not?
  # function ran twice in the case where you have a list for a region and an additional one with trees only => combine them
  # this last case may(has to?) be included in GIFT_checklist_conditional() 
  
  shp = NULL, coordinates = NULL, overlap = "centroid_inside",
  
  remove_overlap = FALSE, area_th_island = 0, 
  area_th_mainland = 100, overlap_th = 0.1,
  
  namesmatched = FALSE,
  list_set_only = FALSE, 
  
  GIFT_version = NULL, 
  
  api = "http://gift.uni-goettingen.de/api/extended/"
  ##
  # GIFT_checklist_conditional() below
  # tax_group = 2, # control: length 1 (no several tax_groups)
  # ref_included = c("all", "native", "native and naturalized",
  #                  "native and historically introduced", "endangered",
  #                  "endemic", "naturalized", "other subset")[1:4],
  # type_ref = c("Account", "Catalogue", "Checklist","Flora",
  #              "Herbarium collection", "Key", "Red list", "Report",
  #              "Species Database", "Survey"),
  # entity_class = c("Island", "Island/Mainland", "Mainland", "Island Group",
  #                  "Island Part"),
  # native_indicated = FALSE,
  # natural_indicated = FALSE,
  # end_ref = FALSE,
  # end_list = FALSE,
  # suit_geo = TRUE,
  # complete_taxon = TRUE,
  
  # GIFT_spatial() arguments below
  # shp = NULL, coordinates = NULL, overlap = "centroid_inside",
  
  # remove overlapping entities => to include
  
  # api = "http://gift.uni-goettingen.de/api/extended/index.php"
){
  
  # Relies on GIFT_checklist_conditional(); GIFT_checklist_raw(); GIFT_spatial()
  
  # 1. Control ----
  if(!is.character(api)){
    stop("api must be a character string indicating which API to use.")
  }
  
  # 2. Function ----
  ## 2.1. GIFT_checklist_conditional ---- 
  GIFT_conditional_arg <- c("all", "native", "native and naturalized",
                            "native and historically introduced", "endangered",
                            "endemic", "naturalized", "other subset")
  
  arg_list <-
    list("all" = GIFT_conditional_arg,
         "native" = GIFT_conditional_arg[c(1:6, 8)],
         "endemic" = GIFT_conditional_arg[c(1:6, 8)],
         "naturalized" = GIFT_conditional_arg[c(1, 3, 4, 7, 8)])
  
  arg_list_second <-
    list("all" = GIFT_conditional_arg[1], # double check
         "native" = GIFT_conditional_arg[c(1:4)],
         "endemic" = GIFT_conditional_arg[c(1:4, 6)],
         "naturalized" = GIFT_conditional_arg[c(1, 3, 7)])
  
  # Converting geo_type
  entity_class <- c()
  if("Mainland" %in% geo_type){
    entity_class <- append(entity_class, c("Island/Mainland", "Mainland"))
  }
  if("Island" %in% geo_type){
    entity_class <- append(entity_class, c("Island", "Island Group",
                                           "Island Part"))
  } 
  
  # List_set query
  list_set <- jsonlite::read_json(paste0(api, "index",
                                         ifelse(is.null(GIFT_version), "", GIFT_version),
                                         ".php?query=lists"),
                                  simplifyVector = TRUE)
  
  # Taxonomy query
  taxonomy <- jsonlite::read_json(paste0(api, "index",
                                         ifelse(is.null(GIFT_version), "", GIFT_version),
                                         ".php?query=taxonomy"),
                                  simplifyVector = TRUE)
  
  # If the user asks for floristic_group = native & geo_type = Island
  # In GIFT_checklist_conditional()
  lists <- GIFT::GIFT_checklist_conditional(
    taxon_name = taxon_name,
    ref_included = arg_list[[floristic_group]],
    entity_class = entity_class,
    native_indicated = (floristic_group == "native"),
    natural_indicated = (floristic_group == "naturalized"),
    end_ref = (floristic_group == "endemic"),
    end_list = FALSE,
    suit_geo = suit_geo,
    complete_taxon = complete_taxon,
    GIFT_version = GIFT_version,
    api = api,
    list_set = list_set,
    taxonomy = taxonomy)
  
  # If complete_floristic == TRUE => running _conditional() a second time
  # subsetting first call with the entity_IDs got in the second one
  if(complete_floristic == TRUE){
    floristic_subset <- GIFT::GIFT_checklist_conditional(
      taxon_name = taxon_name,
      ref_included = arg_list_second[[floristic_group]],
      entity_class = entity_class,
      native_indicated = (floristic_group == "native"),
      natural_indicated = (floristic_group == "naturalized"),
      end_ref = (floristic_group == "endemic"),
      end_list = FALSE,
      suit_geo = suit_geo,
      complete_taxon = complete_taxon,
      GIFT_version = GIFT_version,
      api = api,
      list_set = list_set,
      taxonomy = taxonomy)
    
    # Subset
    lists <- lists[which(lists$entity_ID %in% floristic_subset$entity_ID), ]
  }
  
  ## 2.2. Spatial filtering ----
  if(!is.null(shp) | !is.null(coordinates)){
    spatial_filter <- GIFT::GIFT_spatial(shp = shp,
                                         coordinates = coordinates,
                                         overlap = overlap, api = api,
                                         entity_ID = unique(lists$entity_ID))
    
    lists <- lists[which(lists$entity_ID %in% spatial_filter$entity_ID), ]
    
    if(nrow(lists) == 0){
      message("No checklist match your spatial criteria.")
      return(lists)
    }
  }
  
  ## 2.3. Overlapping entities ----
  # overlapped entities are removed => subseting lists based on entity_ID again)
  if(remove_overlap == TRUE){
    no_overlap <- GIFT_no_overlap(entity_IDs = lists$entity_ID, area_th_island = area_th_island, 
                                  area_th_mainland = area_th_mainland, overlap_th = overlap_th, 
                                  geoentities_overlap = NULL, 
                                  api = api, 
                                  GIFT_version = GIFT_version)
    lists <- lists[which(lists$entity_ID %in% no_overlap), ]
  }
  
  ## 2.4. Downloading ----
  # When downloading the species, this whole filtering process has to happen again
  # Output of the function: species distribution in lists AND metadata for lists

  # Match argument name with GIFT_checklist_raw()
  if(floristic_group == "all"){
    floristic_group <- NULL
  } else if(floristic_group == "endemic"){
    floristic_group <- "endemic_ref"
  }
  
  checklists <- NA
  if (!list_set_only){
    checklists <- GIFT::GIFT_checklist_raw(list_ID = unique(lists$list_ID),
                                           taxon_name = taxon_name,
                                           namesmatched = namesmatched,
                                           floristic_group = floristic_group,
                                           GIFT_version = GIFT_version,
                                           api = api,
                                           list_set = list_set,
                                           taxonomy = taxonomy)
  }
  return(list(lists, checklists))
}
