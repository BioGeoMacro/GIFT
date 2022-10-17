#' Species richness and trait coverage in GIFT
#'
#' Retrieve environmental data associated to each GIFT checklists.
#' They can come as rasters or shapefiles (miscellaneous)
#'
#' @param taxon_ID Identification number of the taxonomic group you want to
#' retrieve.
#'
#' @param trait_ID Identification number of the trait you want to retrieve.
#' 
#' @param what Species richness or trait coverage.
#' 
#' @param GIFT_version character string defining the version of the GIFT
#'  database to use. The function retrieves by default the most up-to-date
#'  version.
#' 
#' @param api character string defining from which API the data will be
#' retrieved.
#' 
#' @return
#' data frame.
#'
#' @details Blabla.
#'
#' @references
#'      Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
#'      Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
#'      https://doi.org/10.1111/jbi.13623
#'
#' @seealso [GIFT::GIFT_traits_meta()]
#'
#' @examples
#' \dontrun{
#' ex <- GIFT_env()
#' 
#' }
#' 
#' @importFrom jsonlite read_json
#' 
#' @export

GIFT_richness <- function(
    GIFT_version = "latest",
    api = "http://gift.uni-goettingen.de/api/extended/"){
  
  # 1. Controls ----
  # Arguments
  if(!is.character(api)){
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
    api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
    ".php?query=traits_cov"),
    simplifyVector = TRUE)
  
  return(tmp)
  
}

