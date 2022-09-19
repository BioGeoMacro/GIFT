#' References available in GIFT
#'
#' Retrieves the metadata of each reference within GIFT.
#'
#' @param GIFT_version character string defining the version of the GIFT
#'  database to use. The function retrieves by default the most up-to-date
#'  version.
#' 
#' @param api character string defining from which API the data will be retrieved.
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
                       GIFT_version = NULL){
  # 1. Controls ----
  # Arguments
  if(!is.character(api)){
    stop("api must be a character string indicating which API to use.")
  }
  
  # 2. Query ----
  tmp <- jsonlite::read_json(paste0(
    api,"index", ifelse(is.null(GIFT_version), "", GIFT_version),
    ".php?query=lists"), simplifyVector = TRUE)
  return(tmp)
}
