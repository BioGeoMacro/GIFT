#' References available in GIFT
#'
#' Retrieve the metadata of every reference accessible in GIFT
#'
#' @param GIFT_version character string defining the version of the GIFT
#'  database to use. The function retrieves by default the most up-to-date
#'  version.
#' 
#' @param api character string defining from which API the data will be retrieved.
#' 
#' @return
#' A data frame with 14 columns.
#'
#' @details Here is what each column refers to:
#' 'ref_ID' - Identification number of the reference.
#' 'ref_long'- Full reference for the reference.
#' 'geo_entity_ref'- Name of the location.
#' 'type'- What type the source is.
#' 'subset'- What information regarding the status of species is available.
#' 'taxon_ID'- Identification number of the group of taxa available.
#' 'taxon_name'- Name of the group of taxa available.
#' 'checklist'- Is the source a checklist.
#' 'native_indicated'- Whether native status of species is available in the
#' source.
#' 'natural_indicated' - Whether naturalized status of species is available in
#' the source.
#' 'end_ref' - Whether endemism information is available in the source.
#' 'traits' - Whether trait information is available in the source.
#' 'restricted' - Whether the access to this reference is restricted.
#' 'proc_date' - When the source was processed.
#'
#' @references
#'      Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
#'      Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
#'      https://doi.org/10.1111/jbi.13623
#'
#' @seealso [GIFT::GIFT_checklist()]
#'
#' @examples
#' \dontrun{
#' ex <- GIFT_references()
#' }
#' 
#' @importFrom jsonlite read_json
#' 
#' @export

GIFT_references <- function(
    api = "http://gift.uni-goettingen.de/api/extended/",
    GIFT_version = "latest"){
  # 1. Controls ----
  if(!is.character(api)){
    stop("api must be a character string indicating which API to use.")
  }
  
  # GIFT_version
  gift_version <- jsonlite::read_json(
    "https://gift.uni-goettingen.de/api/index.php?query=versions",
    simplifyVector = TRUE)
  if(length(GIFT_version) != 1 || is.na(GIFT_version) ||
     !is.character(GIFT_version) || 
     !(GIFT_version %in% c(unique(gift_version$version),
                           "latest", "beta"))){
    stop(c("'GIFT_version' must be a character string stating what version
    of GIFT you want to use. Available options are 'latest' and the different
           versions."))
  }
  if(GIFT_version == "latest"){
    GIFT_version <- gift_version[nrow(gift_version), "version"]
  }
  if(GIFT_version == "beta"){
    message("You are asking for the beta-version of GIFT which is subject to updates and edits. Consider using 'latest' for the latest stable version.")
  }
  
  # 2. Query ----
  tmp <- jsonlite::read_json(paste0(
    api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
    ".php?query=references"), simplifyVector = TRUE)
  
  return(tmp)
}
