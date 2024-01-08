#' Metadata for GIFT regions
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
#'      Denelle, P., Weigelt, P., & Kreft, H. (2023). GIFT—An R package to
#'      access the Global Inventory of Floras and Traits. Methods in Ecology
#'      and Evolution, 14, 2738-2748.
#'      https://doi.org/10.1111/2041-210X.14213
#' 
#'      Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
#'      Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
#'      https://doi.org/10.1111/jbi.13623
#'
#' @seealso [GIFT::GIFT_env_meta_misc()]
#'
#' @examples
#' \donttest{
#' ex <- GIFT_regions()
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom dplyr mutate_at
#' 
#' @export

GIFT_regions <- function(api = "https://gift.uni-goettingen.de/api/extended/",
                         GIFT_version = "latest"){
  api_check <- check_api(api)
  if(is.null(api_check)){
    return(NULL)
  } else{
    GIFT_version <- check_gift_version_simple(GIFT_version)
    
    tmp <- jsonlite::read_json(paste0(
      api,"index", ifelse(GIFT_version == "beta", "", GIFT_version),
      ".php?query=regions"), simplifyVector = TRUE)
    
    tmp <- dplyr::mutate_at(
      tmp, c("entity_ID", "suit_geo", "country"), as.numeric)
    
    return(tmp)
  }
}
