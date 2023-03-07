#' Select non-overlapping regions
#' 
#' Identify overlapping regions in a set of GIFT regions and choose only 
#' non-overlapping regions based on size and overlap criteria
#'
#' @param entity_IDs A vector of IDs of the regions for which we want to check
#' overlap
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
#' @param geoentities_overlap A table coming from GIFT indicating the
#' overlap in km^2 between pairs of polygons.
#' 
#' @template GIFT_version_api
#' 
#' @return A vector of entity_IDs (identification numbers of polygons)
#' non-overlapping.
#'
#' @references
#'      Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
#'      Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
#'      https://doi.org/10.1111/jbi.13623
#'
#' @seealso [GIFT::GIFT_checklists()]
#'
#' @examples
#' \dontrun{
#' ex <- GIFT_no_overlap(entity_IDs = c(10071, 12078)) # Andalusia and Spain. 
#' # We get Andalusia because it is smaller than Spain and larger than 100 km²
#' ex2 <- GIFT_no_overlap(entity_IDs = c(10071, 12078), 
#' area_threshold_mainland = 100000) # since Andalusia is smaller than
#' # 100,000 km² large, the larger entity (Spain) is chosen here.
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom dplyr mutate_at
#' 
#' @export

GIFT_no_overlap <- function(
    entity_IDs = NULL, area_threshold_island = 0,
    area_threshold_mainland = 100, overlap_threshold = 0.1,
    geoentities_overlap = NULL, 
    api = "https://gift.uni-goettingen.de/api/extended/",
    GIFT_version = "latest"){
  
  # 1. Controls ----
  if(is.null(entity_IDs) || length(entity_IDs) == 0){
    stop("Please provide the ID numbers of the regions you want to check the
         overlap of.")
  }
  
  if(!is.numeric(area_threshold_island) || area_threshold_island < 0){
    stop("'area_threshold_island' is a surface in km^2 indicating from which
    surface the smallest overlapping polygon is kept.")
  }
  
  if(!is.numeric(area_threshold_mainland) || area_threshold_mainland < 0){
    stop("'area_threshold_mainland' is a surface in km^2 indicating from which
    surface the smallest overlapping polygon is kept.")
  }
  
  if(!is.numeric(overlap_threshold) || overlap_threshold < 0 ||
     overlap_threshold > 1){
    stop("'overlap_threshold' is a number ranging from 0 to 1, indicating at
    what percentage of overlap, partially overlapping polygons should be
         kept.")
  }
  
  if(!is.null(geoentities_overlap) && !is.data.frame(geoentities_overlap) &&
     ncol(geoentities_overlap) != 7 &&
     colnames(geoentities_overlap) != c("entity1", "entity2", "overlap12",
                                        "overlap21", "area1", "area2",
                                        "entity_class2")){
    stop("'geoentities_overlap' is a table coming from GIFT indicating the
         overlap in km^2 between pairs of polygons. It is automatically
         retrieved when 'geoentities_overlap' = NULL (default value of the
         function).")
  }
  
  check_api(api)
  GIFT_version <- check_gift_version_simple(GIFT_version)
  
  # 2. Function ----
  if(is.null(geoentities_overlap)){
    geoentities_overlap <- jsonlite::read_json(
      paste0(api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
             ".php?query=overlap"), simplifyVector = TRUE)
  }
  
  # Convert columns as numeric
  geoentities_overlap <- 
    dplyr::mutate_at(geoentities_overlap, c("entity1", "entity2", "overlap12",
                                            "overlap21","area1","area2"),
                     as.numeric)
  
  geoentities_overlap <- geoentities_overlap[
    which(geoentities_overlap$entity1 %in% entity_IDs &
            geoentities_overlap$entity2 %in% entity_IDs), ]
  
  geoentities_overlap <- geoentities_overlap[
    which(geoentities_overlap$overlap12 > overlap_threshold |
            geoentities_overlap$overlap21 > overlap_threshold), ]
  
  # geoentities_overlap <- geoentities_overlap[order(geoentities_overlap$area1,
  #                                                  decreasing = TRUE), ]
  
  entity_IDs_tocheck <- unique(geoentities_overlap$entity1)
  
  # Take the smaller entity if larger than threshold
  if(length(entity_IDs_tocheck)> 0){
    for(i in seq_along(entity_IDs_tocheck)){
      th <- ifelse(geoentities_overlap$entity_class2[
        which(geoentities_overlap$entity2 == entity_IDs_tocheck[i])][1] %in%
          c("Mainland", "Island/Mainland"), area_threshold_mainland,
        area_threshold_island)
      
      subset.tocheck <- geoentities_overlap[
        which(geoentities_overlap$entity1 == entity_IDs_tocheck[i]), ]
      
      subset.tocheck$th <- ifelse(subset.tocheck$entity_class2 %in%
                                    c("Mainland", "Island/Mainland"),
                                  area_threshold_mainland,
                                  area_threshold_island)
      
      if(length(which(subset.tocheck$area1 > subset.tocheck$area2 &
                      subset.tocheck$area2 > subset.tocheck$th)) > 0 |
         length(which(subset.tocheck$area1 < subset.tocheck$area2 &
                      subset.tocheck$area1<th)) > 0) {
        entity_IDs <- entity_IDs[which(entity_IDs != entity_IDs_tocheck[i])]
      }
    }
  }
  
  geoentities_overlap <- geoentities_overlap[
    which(geoentities_overlap$entity1 %in% entity_IDs &
            geoentities_overlap$entity2 %in% entity_IDs), ]
  
  return(entity_IDs)
}
