#' GIFT checklists
#'
#' Retrieve GIFT checklists that fulfill specific criteria.
#'
#' @param taxon_name Character string corresponding to the taxonomic group
#' of interest.
#' 
#' @param complete_taxon logical stating you want to retrieve checklists that
#' only contain the exhaustive list of the `taxon_name` argument or as well
#' incomplete lists.
#' 
#' @param floristic_group Character among the following options: `all`,
#' `native`, `endemic`, `naturalized`.
#' 
#' @param complete_floristic logical stating you want to retrieve checklists
#' that only contain the exhaustive list of the `floristic_group` argument or
#' as well incomplete lists.
#' 
#' @param geo_type Character string, either `Mainland`, `Island` or
#' `All`. `Island` gets you to Island, Island Group &
#' Island Part. `Mainland` gets you to Mainland & Island/Mainland. `All` gets
#' you all types.
#' 
#' @param ref_excluded A vector listing potential ref_IDs that shall be ignored 
#' when assembling the set of regions and checklists fulfilling the given 
#' criteria. Checklists from these references will not be returned. NULL by 
#' default.
#'  
#' @param suit_geo logical indicating whether only regions classified as 
#' suit_geo should be considered (see details).
#' 
#' @param shp Shapefile provided by the user.
#'
#' @param coordinates Custom set of coordinates. The format is a two columns,
#' the first one being longitudes and the second being latitudes. If 4
#' coordinates are given, the function assumes that these are the four corners
#' of a bounding box.
#' 
#' @param overlap A character string defining the criteria to use in order to
#' retrieve checklists. Available options are `centroid_inside`,
#' `extent_intersect`, `shape_intersect` and `shape_inside`. For example,
#' `extent_intersect` means that every polygon from GIFT for which the extent
#' intersects the provided shape/coordinates will be retrieved.
#' 
#' @param remove_overlap a logical stating whether you want to
#' retrieve checklists that overlap or not.
#' 
#' @param area_threshold_island A number stating from which surface the
#' smallest overlapping polygon is kept. By default set to 0 square kilometer
#' (meaning that by default the smallest islands will be conserved).
#' 
#' @param area_threshold_mainland When two polygons overlap, the smallest or
#' the biggest one can be kept. When the surface of the smallest polygon
#' exceeds this number, the smallest polygon is kept. Otherwise, we keep the
#' bigger one. Set by default 100 square-kilometers.
#' 
#' @param overlap_threshold A number ranging from 0 to 1, indicating at what
#' percentage of overlap, partially overlapping polygons should be kept. 
#' 
#' @param by_ref_ID logical indicating whether the removal of overlapping
#' regions shall be applied by \emph{ref_ID} only. Note that regions
#' overlapping with other regions from the same resource will be removed even
#' if there are other references available for those regions.
#' 
#' @param taxonomic_group logical. When set to `TRUE`, two additional columns
#' (\emph{family} and \emph{tax_group}) are available in the checklists.
#' 
#' @param namesmatched logical. `FALSE` by default, set to `TRUE` if you want
#' the original species name as they came in the references as well as details
#' on the taxonomic harmonization.
#' 
#' @param list_set_only logical stating whether you only want the metadata or
#' if you also want to retrieve the species lists.
#'
#' @template GIFT_version_api
#'
#' @return
#' List with two data frames: the checklist with species and the list of ID.
#'
#' @details Here is the detail of each data.frame and their columns:
#' 
#' \emph{ref_ID} - Identification number of each reference.\cr
#' \emph{type}- What type the source is.\cr
#' \emph{subset}- What information regarding the status of species is
#'  available.\cr
#' \emph{native_indicated}- Whether native status of species is available in
#'  the source.\cr
#' \emph{natural_indicated} - Whether naturalized status of species is
#'  available in the source.\cr
#' \emph{end_ref} - Whether endemism information is available in the
#'  source.\cr
#' \emph{restricted} - Whether the access to this reference is
#' restricted.\cr
#' \emph{taxon_ID}- Identification number of species.\cr
#' \emph{list_ID} - Identification number of each list.\cr
#' \emph{end_list} - Whether endemism information is available in the list.\cr
#' \emph{entity_ID}- Identification number of the polygon of the list.\cr
#' \emph{geo_entity} - Name of the location.\cr
#' \emph{suit_geo} - Is the polygon suitable.\cr
#' \emph{entity_class} - Type of polygon.\cr
#' \emph{entity_type} - Name of the location.\cr
#' \emph{taxon_name} - Name of the group of taxa available.
#' 
#' For the second data frame with the species, each column refers to:
#' 
#' \emph{ref_ID} - Identification number of each reference.\cr
#' \emph{list_ID} - Identification number of each list\cr
#' \emph{work_ID} - Identification number of each species name, after taxonomic
#'  harmonization.\cr
#' \emph{genus_ID} - Identification number of each genus, after taxonomic
#'  harmonization.\cr
#' \emph{species} - Species name, after taxonomic harmonization.\cr
#' \emph{questionable} - Whether the species occurrence is questionable.\cr
#' \emph{native} - Whether the species is native.\cr
#' \emph{quest_native} - Whether the native information is questionable.\cr
#' \emph{naturalized} - Whether the species is naturalized.\cr
#' \emph{endemic_ref} - Whether the species is endemic within the reference.\cr
#' \emph{quest_end_ref} - Whether the endemic_ref information is
#'  questionable.\cr
#' \emph{endemic_list} - Whether the species is endemic within the list.\cr
#' \emph{quest_end_list} - Whether the endemic_list information is
#'  questionable.\cr
#' \emph{cons_status} - Conservation status of the species.\cr
#' \emph{family} - Family of the species.\cr
#' \emph{tax_group} - Taxonomic group of the species.
#'
#' While the arguments `taxon_name` in combination with `complete_taxon = TRUE` 
#' and `floristic_group` in combination with `complete_floristic = TRUE` make 
#' sure to only get back checklists for regions for which GIFT has lists 
#' aiming at covering both the entire taxonomic group and floristic subset (for 
#' example native vascular plants), it does not mean that the checklists are 
#' complete (include all species). We therefore flagged regions in GIFT for 
#' which the combination of all checklists is obviously incomplete as 
#' `suit_geo = 0`. This has however only been done only for native angiosperms 
#' and the assessment has been subjective. Set `suit_geo = TRUE` if you only
#' want to consider regions classified as `suit_geo`.
#'
#' @references
#'      Denelle, P., Weigelt, P., & Kreft, H. (2023). GIFT—An R package to
#'      access the Global Inventory of Floras and Traits. Methods in Ecology
#'      and Evolution, 14, 2738-2748.
#'      https://doi.org/10.1111/2041-210X.14213
#' 
#'      Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
#'      Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
#'      https://doi.org/10.1111/jbi.13623
#'
#' @seealso [GIFT::GIFT_checklists_raw()]
#'
#' @examples
#' \donttest{
#' data("western_mediterranean")
#' ex <- GIFT_checklists(shp = western_mediterranean,
#' overlap = "centroid_inside", taxon_name = "Angiospermae",
#' list_set_only = TRUE) # set to FALSE to get species composition
#' }
#' 
#' @importFrom jsonlite read_json
#' 
#' @export

GIFT_checklists <- function(
    taxon_name = "Tracheophyta", complete_taxon = TRUE,
    floristic_group = c("all", "native", "endemic", "naturalized")[2],
    complete_floristic = TRUE, geo_type = c("All", "Mainland", "Island")[1],
    ref_excluded = NULL, suit_geo = FALSE, shp = NULL, coordinates = NULL,
    overlap = "centroid_inside", remove_overlap = FALSE,
    area_threshold_island = 0, area_threshold_mainland = 100,
    overlap_threshold = 0.1, by_ref_ID = FALSE,
    taxonomic_group = TRUE, namesmatched = FALSE, list_set_only = FALSE,
    GIFT_version = "latest",
    api = "https://gift.uni-goettingen.de/api/extended/"
){
  # 1. Controls ----
  api_check <- check_api(api)
  if(is.null(api_check)){
    return(NULL)
  } else{
    check_taxon_name(taxon_name)
    check_complete_taxon(complete_taxon)
    check_floristic_group(floristic_group)
    check_complete_floristic(complete_floristic)
    check_geo_type(geo_type)
    check_ref_excluded(ref_excluded)
    check_suit_geo(suit_geo)
    check_overlap(overlap)
    check_shp(shp = shp, overlap = overlap)
    coord_check <- check_coordinates(coordinates = coordinates, shp = shp,
                                     overlap = overlap)
    shp <- coord_check[["shp"]]; coordinates <- coord_check[["coordinates"]]
    check_remove_overlap(remove_overlap)
    check_area_threshold_island(area_threshold_island)
    check_area_threshold_mainland(area_threshold_mainland)
    check_overlap_threshold(overlap_threshold)
    check_by_ref_ID(by_ref_ID)
    check_taxonomic_group(taxonomic_group)
    check_namesmatched(namesmatched)
    check_list_set_only(list_set_only)
    GIFT_version <- check_gift_version(GIFT_version)
    
    # 2. Function ----
    ## 2.1. GIFT_checklists_conditional ---- 
    GIFT_conditional_arg <- c("all", "native", "native and naturalized",
                              "native and historically introduced",
                              "endangered", "endemic", "naturalized",
                              "other subset")
    
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
    } else if(geo_type == "All"){
      entity_class <- append(entity_class, c("Island/Mainland", "Mainland",
                                             "Island", "Island Group",
                                             "Island Part"))
    } 
    
    # List_set query
    list_set <- jsonlite::read_json(
      paste0(api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
             ".php?query=lists"), simplifyVector = TRUE)
    
    message("\nMetadata for lists retrieved.\n")
    
    # Taxonomy query
    taxonomy <- jsonlite::read_json(
      paste0(api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
             ".php?query=taxonomy"), simplifyVector = TRUE)
    
    message("GIFT taxonomy downloaded.\n")
    
    # If the user asks for floristic_group = native & geo_type = Island
    # In GIFT_checklists_conditional()
    lists <- suppressMessages(
      GIFT::GIFT_checklists_conditional(
        taxon_name = taxon_name,
        floristic_scope = arg_list[[floristic_group]],
        ref_excluded = ref_excluded,
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
        GIFT::GIFT_checklists_conditional(
          taxon_name = taxon_name,
          floristic_scope = arg_list_second[[floristic_group]],
          ref_excluded = ref_excluded,
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
      spatial_filter <- 
        suppressMessages(GIFT::GIFT_spatial(
          shp = shp,
          coordinates = coordinates,
          overlap = overlap,
          entity_ID = unique(lists$entity_ID),
          api = api,
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
            entity_IDs = lists$entity_ID,
            area_threshold_island = area_threshold_island, 
            area_threshold_mainland = area_threshold_mainland,
            overlap_threshold = overlap_threshold, 
            geoentities_overlap = NULL, api = api,
            GIFT_version = GIFT_version))
        
        lists <- lists[which(lists$entity_ID %in% no_overlap), ]
        
      } else {
        
        geoentities_overlap <- jsonlite::read_json(
          paste0(api, "index", ifelse(GIFT_version == "beta", "",
                                      GIFT_version),
                 ".php?query=overlap"), simplifyVector = TRUE)
        
        to_remove <- tapply(lists$entity_ID, lists$ref_ID, function(x) { 
          to_keep <- suppressMessages(
            GIFT::GIFT_no_overlap(
              entity_IDs = x, 
              area_threshold_island = area_threshold_island, 
              area_threshold_mainland = area_threshold_mainland, 
              overlap_threshold = overlap_threshold, 
              geoentities_overlap = geoentities_overlap, 
              api = api, GIFT_version = GIFT_version))
          to_remove <- x[which(!x %in% to_keep)]
        })
        to_remove <- unlist(to_remove)
        
        lists <- lists[which(!lists$entity_ID %in% to_remove),]
      }
      
      message("Overlapping and nested regions removed.\n")
    }
    
    ## 2.4. Downloading ----
    # When downloading the species, whole filtering process has to happen again
    # Output of the function: species distribution in lists & metadata for lists
    
    # Match argument name with GIFT_checklists_raw()
    if(floristic_group == "endemic"){
      floristic_group <- "endemic_ref"
    }
    
    checklists <- NA
    if (!list_set_only){
      message("Downloading checklists.\n")
      
      checklists <- 
        suppressMessages(
          GIFT::GIFT_checklists_raw(list_ID = unique(lists$list_ID),
                                    taxon_name = taxon_name,
                                    namesmatched = namesmatched,
                                    floristic_group = floristic_group,
                                    GIFT_version = GIFT_version,
                                    api = api,
                                    list_set = list_set,
                                    taxonomy = taxonomy))
      if(taxonomic_group){
        species <- unique(checklists[, c("work_ID","genus_ID","work_species")])
        
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
    
    return(list(lists = lists, checklists = checklists))
  }
}
