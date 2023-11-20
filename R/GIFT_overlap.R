#' Spatial overlap between GIFT polygons and external polygons
#'
#' Calculate the spatial overlap between GIFT polygons and shapefiles coming
#' from other resources
#'
#' @param resource A character string indicating from which resource the
#' spatial overlap is calculated. Available options are `glonaf` and `gmba`.
#' glonaf stands for Global Naturalized Alien Flora and gmba for Global
#' Mountain Biodiversity Assessment.
#' 
#' @template GIFT_version_api
#' 
#' @return A data frame with the spatial overlap.
#'
#' @details The columns of the data.frame are the following:
#' 
#' \emph{entity_ID} - Identification number of the GIFT polygon\cr
#' \emph{glonaf_ID} (or \emph{gmba_ID}) - Identification number of the polygon
#'  from the other resource\cr
#' \emph{overlap12} - Spatial overlap in percentage between GIFT polygon and
#'  the external polygon\cr
#' \emph{overlap21} - The other way around
#'
#' @references
#'      Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
#'      Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
#'      https://doi.org/10.1111/jbi.13623
#'
#' @seealso [GIFT::GIFT_lists()]
#'
#' @examples
#' \donttest{
#' glonaf <- GIFT_overlap(resource = "glonaf")
#' gmba <- GIFT_overlap(resource = "gmba")
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom dplyr select mutate_all
#' 
#' @export

GIFT_overlap <- function(
    resource = "glonaf", GIFT_version = "latest",
    api = "https://gift.uni-goettingen.de/api/extended/"){
  
  # 1. Controls ----
  api_check <- check_api(api)
  if(is.null(api_check)){
    return(NULL)
  } else{
    if(length(resource) != 1 || !is.character(resource) || 
       !(resource %in% c("glonaf", "gmba", "gaptani", "wdpa"))){
      stop("resource must be a character string indicating from which external
         resource you want to calculate the spatial overlap. Available options
         are 'glonaf', 'gmba' or 'wdpa'.")
    }
    
    GIFT_version <- check_gift_version(GIFT_version)
    
    ID <- NULL
    
    # 2. Query ----
    tmp <- jsonlite::read_json(paste0(
      api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
      ".php?query=overlap_misc&layer=", resource), simplifyVector = TRUE)
    
    tmp <- dplyr::select(tmp, -ID)
    
    tmp <- dplyr::mutate_all(tmp, as.numeric)
    
    return(tmp)
  }
}
