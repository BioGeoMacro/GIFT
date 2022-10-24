#' GIFT regions
#'
#' Retrieves miscellaneous information for GIFT regions.
#'
#' @param GIFT_version character string defining the version of the GIFT
#'  database to use. The function retrieves by default the most up-to-date
#'  version.
#' 
#' @param api character string defining from which API the data will be
#' retrieved.
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

GIFT_regions <- function(api = "http://gift.uni-goettingen.de/api/extended/",
                         GIFT_version = "latest"){
  # 1. Controls ----
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
  
  # 2. Query ----
  tmp <- jsonlite::read_json(paste0(
    api,"index", ifelse(GIFT_version == "beta", "", GIFT_version),
    ".php?query=regions"), simplifyVector = TRUE)
  return(tmp)
}
