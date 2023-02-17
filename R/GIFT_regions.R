#' GIFT regions
#'
#' Retrieves miscellaneous information for GIFT regions.
#'
#' @template GIFT_version_api
#' 
#' @return
#' A data frame with 7 columns.
#'
#' @details Here is the detail of each column:
#' 
#' \emph{entity_ID} - Identification number of GIFT polygons\cr
#' \emph{geo_entity} - Name of GIFT polygons\cr
#' \emph{suit_geo} - Whether the polygon is suitable\cr
#' \emph{entity_class} - Class of the polygon\cr
#' \emph{entity_type} - Type of the polygon\cr
#' \emph{TDWG_lvl3_ID} - Whether the polygon is a TDWG region (see
#' https://www.tdwg.org/)\cr
#' \emph{country} - Whether the polygon is a country
#'
#' @references
#'      Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
#'      Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
#'      https://doi.org/10.1111/jbi.13623
#'
#' @seealso [GIFT::GIFT_env_meta_misc()]
#'
#' @examples
#' \dontrun{
#' ex <- GIFT_regions()
#' }
#' 
#' @importFrom jsonlite read_json
#' 
#' @export

GIFT_regions <- function(api = "https://gift.uni-goettingen.de/api/extended/",
                         GIFT_version = "latest"){
  check_api(api)
  GIFT_version <- check_gift_version_simple(GIFT_version)
  
  tmp <- jsonlite::read_json(paste0(
    api,"index", ifelse(GIFT_version == "beta", "", GIFT_version),
    ".php?query=regions"), simplifyVector = TRUE)
  
  tmp$entity_ID <- as.numeric(tmp$entity_ID)
  
  return(tmp)
}
