#' GIFT_no_overlap
#' 
#' Identify overlapping regions in a set of GIFT regions and choose only 
#' non-overlapping regions based on size and overlap criteria
#'
#' @param entity_IDs A vector of IDs of the regions to check overlaps of
#' 
#' 
#' @return
#' a vector
#'
#' @details Blabla.
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
#' ex <- GIFT_shape(entity_ID = c(677, 200))
#' plot(st_geometry(geodata), col=geodata$entity_ID)
#' }
#' 
#' @importFrom sf st_read
#' 
#' 
#' 
#' @export

GIFT_no_overlap <- function(entity_IDs = NULL, area_th_island = 0, 
                            area_th_mainland = 100, overlap_th = 0.1, 
                            geoentities_overlap = NULL, 
                            api = "http://gift.uni-goettingen.de/api/extended/", 
                            GIFT_version = NULL){
  
  # 1. Controls ----
  # Arguments
  if(is.null(entity_IDs)){
    stop("Please provide the ID numbers of the regions you want 
         to check the overlap of.")
  }
  
  
  
  if(is.null(geoentities_overlap)){
    geoentities_overlap <- jsonlite::read_json(paste0(api, "index",
                                           ifelse(is.null(GIFT_version), "", GIFT_version),
                                           ".php?query=overlap"),
                                    simplifyVector = TRUE)
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
  # TODO make warning if this data.frame still contains data
  
  return(entity_IDs)
  # TODO allow for removing overlapping regions only within ref_IDs <- apply this function by ref_ID
}
