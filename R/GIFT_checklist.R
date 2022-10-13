#' GIFT checklists
#'
#' Retrieve GIFT checklists that fulfill specific criteria.
#'
#' @param taxon_name Character string corresponding to the taxonomic group
#' of interest.
#' 
#' @param complete_taxon Boolean stating you want to retrieve checklists that
#' only contain the exhaustive list of the 'taxon_name' argument or as well
#' incomplete lists.
#' 
#' @param floristic_group Character among the following options: 'all',
#' 'native', 'endemic', 'naturalized'.
#' 
#' @param complete_floristic Boolean stating you want to retrieve checklists
#' that only contain the exhaustive list of the 'floristic_group' argument or
#' as well incomplete lists.
#' 
#' @param geo_type Character string, either 'Mainland', 'Island' or
#' 'Mainland, Island'. Island gets you to Island, Island Group &
#' Island Part. Mainland gets you to Mainland & Island/Mainland.
#' 
#' @param suit_geo Boolean, whether only suitable polygons should be retrieved.
#' 
#' @param shp Shapefile provided by the user.
#'
#' @param coordinates Custom set of coordinates. The format is a two columns,
#' the first one being longitudes and the second being latitudes. If 4
#' coordinates are given, the function assumes that these are the four corners
#' of a bounding box.
#' 
#' @param overlap A character string defining the criteria to use in order to
#' retrieve checklists. Available options are 'centroid_inside',
#' 'extent_intersect', 'shape_intersect' and 'shape_inside'. For example,
#' 'extent_intersect' means that every polygon from GIFT for which the extent
#' intersects the provided shape/coordinates will be retrieved.
#' 
#' @param remove_overlap a boolean stating whether you want to
#' retrieve checklists that overlap or not.
#' 
#' @param area_th_island A number stating from which surface the smallest
#' overlapping polygon is kept. By default set to 0 square kilometer
#' (meaning that by default the smallest islands will be conserverd).
#' 
#' @param area_th_mainland When two polygons overlap, the smallest or the
#' biggest one can be kept. When the surface of the smallest polygon exceeds
#' this number, the smallest polygon is kept. Otherwise, we keep the bigger
#' one. Set by default 100 square-kilometers.
#' 
#' @param overlap_th A number ranging from 0 to 1, indicating at what
#' percentage of overlap, partially overlapping polygons should be kept. 
#' 
#' @param by_ref_ID logical indicating whether the removal of overlapping
#' regions shall be applied by ref_ID only. Note that regions overlapping with
#' other regions from the same resource will be removed even if there are other
#' references available for those regions.
#' 
#' @param taxonomic_group Boolean. When set to TRUE, two additional columns
#' ('family' and 'tax_group') are available in the checklists.
#' 
#' @param namesmatched Boolean. FALSE by default, set to TRUE if you want the
#' original species name as they came in the references as well as details on
#' the taxonomic harmonization.
#' 
#' @param list_set_only Boolean. Stating whether you only want the metadata or
#' if you also want to retrieve the species lists.
#' 
#' @param api character string defining from which API the data will be
#' retrieved.
#'
#' @param GIFT_version character string defining the version of the GIFT
#'  database to use. The function retrieves by default the most up-to-date
#'  version.
#'
#' @return
#' List with two data frames: the checklist with species and the list of ID.
#'
#' @details Here is the detail of each data.frame and their columns:
#' #' 'ref_ID' - Identification number of each reference.
#' 'type'- What type the source is.
#' 'subset'- What information regarding the status of species is available.
#' 'native_indicated'- Whether native status of species is available in the
#' source.
#' 'natural_indicated' - Whether naturalized status of species is available in
#' the source.
#' 'end_ref' - Whether endemism information is available in the source.
#' 'restricted' - Whether the access to this reference is restricted.
#' 'taxon_ID'- Identification number of species.
#' 'list_ID'- Identification number of each list.
#' 'end_list' - Whether endemism information is available in the list.
#' 'entity_ID'- Identification number of the polygon of the list.
#' 'geo_entity'- Name of the location.
#' 'suit_geo'- Is the polygon suitable.
#' 'entity_class'- Type of polygon.
#' 'entity_type'- Name of the location.
#' 'taxon_name'- Name of the group of taxa available.
#' 
#' For the second data frame with the species, each column refers to:
#' 'ref_ID' - Identification number of each reference.
#' 'list_ID'- Identification number of each list
#' 'work_ID' - Identification number of each species name, after taxonomic
#'  harmonization.
#' 'genus_ID' - Identification number of each genus, after taxonomic
#'  harmonization.
#' 'species' - Species name, after taxonomic harmonization.
#' 'questionable' - Whether the species occurrence is questionable.
#' 'native' - Whether the species is native.
#' 'quest_native' - Whether the native information is questionable.
#' 'naturalized' - Whether the species is naturalized.
#' 'endemic_ref' - Whether the species is endemic within the reference.
#' 'quest_end_ref' - Whether the endemic_ref information is questionable.
#' 'endemic_list'- Whether the species is endemic within the list.
#' 'quest_end_list' - Whether the endemic_list information is questionable.
#' 'cons_status' - Conservation status of the species.
#' 'family' - Family of the species.
#' 'tax_group' - Taxonomic group of the species.
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
#' data("med")
#' ex <- GIFT_checklist(shp = med, overlap = "centroid_inside",
#' taxon_name = "Angiospermae")
#' 
#' ex2 <- GIFT_checklist(shp = med, overlap = "centroid_inside",
#' taxon_name = "Angiospermae", list_set_only = TRUE)
#' }
#' 
#' @importFrom jsonlite read_json
#' 
#' @export

GIFT_checklist <- function(
    taxon_name = "Tracheophyta", complete_taxon = TRUE,
    floristic_group = c("all", "native", "endemic", "naturalized")[2],
    complete_floristic = TRUE, geo_type = "Mainland, Island",
    suit_geo = FALSE, shp = NULL, coordinates = NULL,
    overlap = "centroid_inside", remove_overlap = FALSE, area_th_island = 0,
    area_th_mainland = 100, overlap_th = 0.1, by_ref_ID = FALSE,
    taxonomic_group = TRUE, namesmatched = FALSE, list_set_only = FALSE,
    GIFT_version = "latest",
    api = "http://gift.uni-goettingen.de/api/extended/"
){
  # 1. Controls ----
  if(length(taxon_name) != 1 || is.na(taxon_name) ||
     !is.character(taxon_name)){
    stop("'taxon_name' is incorrect. It must be a character string among one of
         the taxonomic groups available in GIFT. To check them all, run
         'GIFT_taxonomy()'.")
  }
  
  if(length(complete_taxon) != 1 || !is.logical(complete_taxon) ||
     is.na(complete_taxon)){
    stop("'complete_taxon' must be a boolean stating whether you want to
    retrieve checklists that only contain the exhaustive list of the
    'taxon_name' argument or as well incomplete lists.")
  }
  
  if(length(floristic_group) != 1 || is.na(floristic_group) ||
     !is.character(floristic_group) || 
     !(floristic_group %in% c("all", "native", "endemic", "naturalized"))){
    stop(c("'floristic_group' must be a character string. Available options are
    'all', 'native', 'endemic' and 'naturalized'."))
  }
  
  if(length(complete_floristic) != 1 || !is.logical(complete_floristic) ||
     is.na(complete_floristic)){
    stop("'complete_floristic' must be a boolean stating whether you want to
    retrieve checklists that only contain the exhaustive list of the
    'floristic_group' argument or as well incomplete lists.")
  }
  
  if(is.na(geo_type) || !is.character(geo_type) || 
     !(geo_type %in% c("Mainland", "Island", "Mainland, Island"))){
    stop(c("'geo_type' must be a character string stating what geographic
    type you want to retrieve. Available options are 'Mainland', 'Island' or
    'Mainland, Island')."))
  }
  
  if(length(suit_geo) != 1 || !is.logical(suit_geo) || is.na(suit_geo)){
    stop("'suit_geo' must be a boolean stating whether you want to
    retrieve only suitable polygons or not.")
  }
  
  if(!is.character(overlap) || length(overlap) != 1 ||
     !(all(overlap %in% c("centroid_inside", "shape_inside", "shape_intersect",
                          "extent_intersect")))){
    stop("overlap is a character string indicating whether you want to use
         centroid or extent of GIFT polygons to overlap with your shapefile.\n
         It has to be 'centroid_inside', 'shape_inside', 'shape_intersect' or
         'extent_intersect'.")
  }
  
  if(!is.null(shp) && !("sf" %in% class(shp))){
    stop("'shp' must be an object of classes 'sf' and 'data.frame', with a CRS
         set to WGS84 (EPSG: 4326).")
  }
  
  if(!is.null(shp) && nrow(shp) > 1){
    warning("Several polygons are passed in the shp object. They will be
            treated at the same time. To know what polygon covers what
            checklist, please use repeteadly GIFT_spatial().")
  }
  
  if(!is.null(shp) && "sfc_POINT" %in% class(sf::st_as_sfc(shp)) &&
     overlap %in% c("shape_inside", "centroid_inside")){
    stop("With a point, use either 'shape_intersect' or
             'extent_intersect' only.")
  }
  
  if(!is.null(shp) && "sfc_MULTIPOINT" %in% class(sf::st_as_sfc(shp)) &&
     overlap %in% c("shape_inside", "centroid_inside")){
    stop("With a point, use either 'shape_intersect' or
             'extent_intersect' only.")
  }
  
  if(!is.null(shp) && "sfc_LINESTRING" %in% class(sf::st_as_sfc(shp)) &&
     overlap %in% c("shape_inside", "centroid_inside")){
    stop("With a linestring, use either 'shape_intersect' or
             'extent_intersect' only.")
  }
  
  if(!is.null(shp) && "sfc_MULTILINESTRING" %in% class(sf::st_as_sfc(shp)) &&
     overlap %in% c("shape_inside", "centroid_inside")){
    stop("With a linestring, use either 'shape_intersect' or
             'extent_intersect' only.")
  }
  
  if(!is.null(coordinates)){
    if(is.na(coordinates) || is.character(coordinates)){
      stop("'coordinates' object does not have the right format. It should be
           a vector of XY coordinates. See help page.")
    }
    
    if(nrow(coordinates) == 1){
      if(overlap %in% c("shape_inside", "centroid_inside")){
        stop("With a point, use either 'shape_intersect' or
             'extent_intersect' only.")
      }
      
      shp <- sf::st_point(coordinates)
      shp <- sf::st_sfc(shp, crs = 4326)
      shp <- sf::st_sf(shp) # making a sf object
    } else if(nrow(coordinates) == 2){
      message("4 coordinates provided: an extent box was drawn, assuming that
            minimum X and Y are on row 1, and maximum X and Y on row 2.")
      
      make_box <- function(xmin, xmax, ymin, ymax){
        x_shp <- sf::st_polygon(list(matrix(
          c(xmin, ymin,
            xmax, ymin,
            xmax, ymax,
            xmin, ymax,
            xmin, ymin), ncol = 2, byrow = TRUE)))
        return(x_shp)
      }
      
      shp <- make_box(xmin = coordinates[1, 1],
                      xmax = coordinates[2, 1],
                      ymin = coordinates[1, 2],
                      ymax = coordinates[2, 2])
      shp <- sf::st_sfc(shp, crs = 4326)
      shp <- sf::st_sf(shp) # making a sf object
    }else if(nrow(coordinates) > 2){
      if((coordinates[1, 1] != coordinates[nrow(coordinates), 1]) &
         (coordinates[1, 2] != coordinates[nrow(coordinates), 2])){
        warning("Provided polygon did not have a closed shape.")
        coordinates <- rbind(coordinates, coordinates[1, ])
      }
      shp <- sf::st_polygon(list(coordinates))
      shp <- sf::st_sfc(shp, crs = 4326)
      shp <- sf::st_sf(shp) # making a sf object
    } else{
      stop("'coordinates' object does not have the right format. It should be
           a vector of XY coordinates. See help page.")
    }
  }
  
  if(length(remove_overlap) != 1 || !is.logical(remove_overlap) ||
     is.na(remove_overlap)){
    stop("'remove_overlap' must be a boolean stating whether you want to
    retrieve checklists that overlap or not.")
  }
  
  if(!is.numeric(area_th_island) || area_th_island < 0){
    stop("'area_th_island' is a surface in km^2 indicating from which
    surface the smallest overlapping polygon is kept.")
  }
  
  if(!is.numeric(area_th_mainland) || area_th_mainland < 0){
    stop("'area_th_mainland' is a surface in km^2 indicating from which
    surface the smallest overlapping polygon is kept.")
  }
  
  if(!is.numeric(overlap_th) || overlap_th < 0 || overlap_th > 1){
    stop("'overlap_th' is a number ranging from 0 to 1, indicating at what 
         percentage of overlap, partially overlapping polygons should be
         kept.")
  }
  
  if(length(by_ref_ID) != 1 || !is.logical(by_ref_ID) ||
     is.na(by_ref_ID)){
    stop("'by_ref_ID' must be a boolean stating whether indicating whether the
         removal of overlapping regions shall be applied only at the
         reference level.")
  }
  
  # taxonomic_group
  if(length(taxonomic_group) != 1 || !is.logical(taxonomic_group) ||
     is.na(taxonomic_group)){
    stop("'taxonomic_group' must be a boolean. When set to TRUE, two additional
    columns ('family' and 'tax_group') are available in the checklists.")
  }
  
  if(length(list_set_only) != 1 || !is.logical(list_set_only) ||
     is.na(list_set_only)){
    stop("'list_set_only' must be a boolean stating whether you only want the
    metadata or if you also want to retrieve the species lists.")
  }
  
  if(!is.character(api)){
    stop("api must be a character string indicating which API to use.")
  }
  
  gift_version <- jsonlite::read_json(
    "https://gift.uni-goettingen.de/api/index.php?query=versions",
    simplifyVector = TRUE)
  
  if(length(GIFT_version) != 1 || is.na(GIFT_version) ||
     !is.character(GIFT_version) || 
     !(GIFT_version %in% c(unique(gift_version$version),
                           "latest", "beta"))){
    stop(c("'GIFT_version' must be a character string stating what version
    of GIFT you want to use. Available options are 'latest' and the different
           versions."))
  }
  if(GIFT_version == "latest"){
    GIFT_version <- gift_version[nrow(gift_version), "version"]
  }
  if(GIFT_version == "beta"){
    message("You are asking for the beta-version of GIFT which is subject to
            updates and edits. Consider using 'latest' for the latest stable
            version.")
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
  if(geo_type == "Mainland"){
    entity_class <- append(entity_class, c("Island/Mainland", "Mainland"))
  } else if(geo_type == "Island"){
    entity_class <- append(entity_class, c("Island", "Island Group",
                                           "Island Part"))
  } else if(geo_type == "Mainland, Island"){
    entity_class <- append(entity_class, c("Island/Mainland", "Mainland",
                                           "Island", "Island Group",
                                           "Island Part"))
  } 
  
  # List_set query
  list_set <- jsonlite::read_json(
    paste0(api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
           ".php?query=lists"), simplifyVector = TRUE)
  
  # Taxonomy query
  taxonomy <- jsonlite::read_json(
    paste0(api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
           ".php?query=taxonomy"), simplifyVector = TRUE)
  
  # If the user asks for floristic_group = native & geo_type = Island
  # In GIFT_checklist_conditional()
  lists <- suppressMessages(
    GIFT::GIFT_checklist_conditional(
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
      taxonomy = taxonomy))
  
  # If complete_floristic == TRUE => running _conditional() a second time
  # subsetting first call with the entity_IDs got in the second one
  if(complete_floristic == TRUE){
    floristic_subset <- suppressMessages(
      GIFT::GIFT_checklist_conditional(
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
        taxonomy = taxonomy))
    
    # Subset
    lists <- lists[which(lists$entity_ID %in% floristic_subset$entity_ID), ]
  }
  
  ## 2.2. Spatial filtering ----
  if(!is.null(shp) | !is.null(coordinates)){
    spatial_filter <- suppressMessages(
      GIFT::GIFT_spatial(shp = shp,
                         coordinates = coordinates,
                         overlap = overlap, api = api,
                         entity_ID = unique(lists$entity_ID),
                         GIFT_version = GIFT_version))
    
    lists <- lists[which(lists$entity_ID %in% spatial_filter$entity_ID), ]
    
    if(nrow(lists) == 0){
      message("No checklist match your spatial criteria.")
      return(lists)
    }
  }
  
  ## 2.3. Overlapping entities ----
  # overlapped entities are removed => subset lists based on entity_ID again)
  if(remove_overlap == TRUE){
    
    if(!by_ref_ID){
      no_overlap <- suppressMessages(
        GIFT::GIFT_no_overlap(
          entity_IDs = lists$entity_ID, area_th_island = area_th_island, 
          area_th_mainland = area_th_mainland, overlap_th = overlap_th, 
          geoentities_overlap = NULL, api = api, GIFT_version = GIFT_version))
      
      lists <- lists[which(lists$entity_ID %in% no_overlap), ]
      
    } else {
      
      geoentities_overlap <- jsonlite::read_json(
        paste0(api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
               ".php?query=overlap"), simplifyVector = TRUE)
      
      to_remove <- tapply(lists$entity_ID, lists$ref_ID, function(x) { 
        to_keep <- suppressMessages(
          GIFT::GIFT_no_overlap(entity_IDs = x, 
                                area_th_island = area_th_island, 
                                area_th_mainland = area_th_mainland, 
                                overlap_th = overlap_th, 
                                geoentities_overlap = geoentities_overlap, 
                                api = api, GIFT_version = GIFT_version))
        to_remove <- x[which(!x %in% to_keep)]
      })
      to_remove <- unlist(to_remove)
      
      lists <- lists[which(!lists$entity_ID %in% to_remove),]
    }
  }
  
  ## 2.4. Downloading ----
  # When downloading the species, whole filtering process has to happen again
  # Output of the function: species distribution in lists & metadata for lists
  
  # Match argument name with GIFT_checklist_raw()
  if(floristic_group == "endemic"){
    floristic_group <- "endemic_ref"
  }
  
  checklists <- NA
  if (!list_set_only){
    checklists <- suppressMessages(
      GIFT::GIFT_checklist_raw(list_ID = unique(lists$list_ID),
                               taxon_name = taxon_name,
                               namesmatched = namesmatched,
                               floristic_group = floristic_group,
                               GIFT_version = GIFT_version,
                               api = api,
                               list_set = list_set,
                               taxonomy = taxonomy))
    if (taxonomic_group){
      
      species <- unique(checklists[,c("work_ID","genus_ID","work_species")])
      names(species)[2] <- "genus"
      
      checklists$family <-
        suppressMessages(GIFT::GIFT_taxgroup(work_ID = checklists$work_ID,
                                             taxon_lvl = "family",
                                             return_ID = FALSE,
                                             GIFT_version = GIFT_version,
                                             api = api,
                                             taxonomy = taxonomy,
                                             species = species))
      checklists$tax_group <-
        suppressMessages(GIFT::GIFT_taxgroup(work_ID = checklists$work_ID,
                                             taxon_lvl = "higher_lvl",
                                             return_ID = FALSE,
                                             GIFT_version = GIFT_version,
                                             api = api,
                                             taxonomy = taxonomy,
                                             species = species))
    }
  }
  
  if(list_set_only == FALSE){
    message("Be cautious, species indicated as endemic were stated like this in
          the source reference/checklist. It can be that these species appear
          in other checklists.")
  }
  
  return(list(lists, checklists))
}
