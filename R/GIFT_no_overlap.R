#' GIFT_no_overlap
#' 
#' Identify overlapping regions in a set of GIFT regions and choose only 
#' non-overlapping regions based on size and overlap criteria
#'
#' @param entity_IDs A vector of IDs of the regions for which we want to check
#' overlap
#' 
#' @param area_th_island A number stating from which surface the smallest
#' overlapping polygon is kept. By default set to 0 square kilometer
#' (meaning that by default the smallest islands will be conserved).
#' 
#' @param area_th_mainland When two polygons overlap, the smallest or the
#' biggest one can be kept. When the surface of the smallest polygon exceeds
#' this number, the smallest polygon is kept. Otherwise, we keep the bigger
#' one. Set by default 100 square-kilometers.
#' 
#' @param overlap_th A number ranging from 0 to 1, indicating at what
#' percentage of overlap, partially overlapping polygons should be kept. 
#' 
#' @param geoentities_overlap A table coming from GIFT indicating the
#' overlap in km^2 between pairs of polygons.
#' 
#' @param api Character string defining from which API the data will be
#' retrieved.
#' 
#' @param GIFT_version Character string defining the version of the GIFT
#'  database to use. The function retrieves by default the most up-to-date
#'  version.
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
#' ex <- GIFT_no_overlap(entity_IDs = c(10071, 12078)) # Andalusia and Spain
#' }
#' 
#' @importFrom jsonlite read_json
#' 
#' @export

GIFT_no_overlap <- function(
    entity_IDs = NULL, area_th_island = 0, area_th_mainland = 100,
    overlap_th = 0.1, geoentities_overlap = NULL, 
    api = "http://gift.uni-goettingen.de/api/extended/",
    GIFT_version = "latest"){
  
  # 1. Controls ----
  if(is.null(entity_IDs) || length(entity_IDs) == 0){
    stop("Please provide the ID numbers of the regions you want to check the
         overlap of.")
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
  
  # GIFT_version
  if(length(GIFT_version) != 1 || is.na(GIFT_version) ||
     !is.character(GIFT_version)){
    stop(c("'GIFT_version' must be a character string stating what version
    of GIFT you want to use. Available options are 'latest' and the different
           versions."))
  }
  if(GIFT_version == "latest"){
    gift_version <- jsonlite::read_json(
      "https://gift.uni-goettingen.de/api/index.php?query=versions",
      simplifyVector = TRUE)
    GIFT_version <- gift_version[nrow(gift_version), "version"]
  }
  if(GIFT_version == "beta"){
    message("You are asking for the beta-version of GIFT which is subject to
            updates and edits. Consider using 'latest' for the latest stable
            version.")
  }
  
  if(!is.character(api)){
    stop("api must be a character string indicating which API to use.")
  }
  
  # 2. Function ----
  if(is.null(geoentities_overlap)){
    geoentities_overlap <- jsonlite::read_json(
      paste0(api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
             ".php?query=overlap"), simplifyVector = TRUE)
  }
  
  geoentities_overlap <- geoentities_overlap[
    which(geoentities_overlap$entity1 %in% entity_IDs &
            geoentities_overlap$entity2 %in% entity_IDs), ]
  
  geoentities_overlap <- geoentities_overlap[
    which(geoentities_overlap$overlap12 > overlap_th |
            geoentities_overlap$overlap21 > overlap_th), ]
  
  # geoentities_overlap <- geoentities_overlap[order(geoentities_overlap$area1,
  #                                                  decreasing = TRUE), ]
  
  entity_IDs_tocheck <- unique(geoentities_overlap$entity1)
  
  # Take the smaller entity if larger than threshold
  if(length(entity_IDs_tocheck)> 0){
    for(i in seq_len(entity_IDs_tocheck)){
      th <- ifelse(geoentities_overlap$entity_class2[
        which(geoentities_overlap$entity2 == entity_IDs_tocheck[i])][1] %in%
          c("Mainland", "Island/Mainland"), area_th_mainland,area_th_island)
      
      subset.tocheck <- geoentities_overlap[
        which(geoentities_overlap$entity1 == entity_IDs_tocheck[i]), ]
      
      subset.tocheck$th <- ifelse(subset.tocheck$entity_class2 %in%
                                    c("Mainland", "Island/Mainland"),
                                  area_th_mainland, area_th_island)
      
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
  # TODO make warning if this data.frame still contains data
  
  return(entity_IDs)
  # TODO allow for removing overlapping regions only within ref_IDs <- apply
  # this function by ref_ID
}
