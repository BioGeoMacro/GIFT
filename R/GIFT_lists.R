#' References available in GIFT
#'
#' Retrieves the metadata of each reference within GIFT.
#'
#' @param GIFT_version character string defining the version of the GIFT
#'  database to use. The function retrieves by default the most up-to-date
#'  version.
#' 
#' @param api character string defining from which API the data will be
#' retrieved.
#' 
#' @return
#' A data frame with 15 columns.
#'
#' @details Column 'ref_ID' indicates the identification number of each
#' reference. Columns 'type' and 'subset' indicate what information can be
#' found in each reference. Similarly, '', '' and '' indicate respectively
#' whether native, naturalized and endemic species were stated in the
#' reference. 'restricted' refers to the availability of the reference,
#' 'taxon_ID' to the taxonomic group available in a reference. 'list_ID' is the
#' identification number of a checklist within a reference, 'entity_ID' of the
#' associated polygon. 'geo_entity' associates a name to this identification
#' number. 'suit_geo' indicates whether the checklist is suitable for use,
#' 'entity_class' and 'entity_unit' give additional details about the polygon.
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
#' ex <- GIFT_lists()
#' }
#' 
#' @importFrom jsonlite read_json
#' 
#' @export

GIFT_lists <- function(api = "http://gift.uni-goettingen.de/api/extended/",
                       GIFT_version = "latest"){
  # 1. Controls ----
  # Arguments
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
    ".php?query=lists"), simplifyVector = TRUE)
  return(tmp)
}
