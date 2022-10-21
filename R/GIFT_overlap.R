#' Spatial overlap between GIFT polygons and external polygons
#'
#' Calculate the spatial overlap between GIFT polygons and shapefiles coming
#' from other resources
#'
#' @param resource A character string indicating from which resource the
#' spatial overlap is calculated. Available options are 'glonaf' and 'gmba'.
#' glonaf stands for Global Naturalized Alien Flora and gmba for Global
#' Mountain Biodiversity Assessment.
#' 
#' @param GIFT_version character string defining the version of the GIFT
#'  database to use. The function retrieves by default the most up-to-date
#'  version.
#' 
#' @param api character string defining from which API the data will be
#' retrieved.
#' 
#' @return A data frame with the spatial overlap.
#'
#' @details The columns of the data.frame are the following:
#' entity_ID - Identification number of the GIFT polygon
#' glonaf_ID (or gmba_ID) - Identification number of the polygon from the other
#' resource.
#' overlap12 - Spatial overlap in percentage between GIFT polygon and the
#' external polygon.
#' overlap21 - The other way around.
#'
#' @references
#'      Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
#'      Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
#'      https://doi.org/10.1111/jbi.13623
#'
#' @seealso [GIFT::GIFT_lists()]
#'
#' @examples
#' \dontrun{
#' glonaf <- GIFT_overlap(resource = "glonaf")
#' gmba <- GIFT_overlap(resource = "gmba")
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom dplyr select
#' 
#' @export

GIFT_overlap <- function(
    resource = "glonaf", GIFT_version = "latest",
    api = "http://gift.uni-goettingen.de/api/extended/"){
  
  # 1. Controls ----
  if(length(resource) != 1 || !is.character(resource) || 
     !(resource %in% c("glonaf","gmba","gaptani"))){
    stop("resource must be a character string indicating from which external
         resource you want to calculate the spatial overlap. Available options
         are 'glonaf' or 'gmba'.")
  }
  
  if(length(api) != 1 || !is.character(api)){
    stop("api must be a character string indicating which API to use.")
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
  
  ID <- NULL
  
  # 2. Query ----
  tmp <- jsonlite::read_json(paste0(
    api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
    ".php?query=overlap_misc&layer=", resource), simplifyVector = TRUE)
  
  tmp <- dplyr::select(tmp, -ID)
  
  tmp <- dplyr::mutate_all(tmp, as.numeric)
  
  return(tmp)
}
