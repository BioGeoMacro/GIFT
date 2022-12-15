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
#' @return A data frame with 15 columns.
#'
#' @details Here is what each column refers to:
#' 
#' \emph{ref_ID} - Identification number of each reference.\cr
#' Columns \emph{type} and \emph{subset} indicate what information can be
#' found in each reference. Similarly, \emph{native_indicated},
#' \emph{natural_indicated} and \emph{end_ref} indicate respectively
#' whether native, naturalized and endemic species were stated in the
#' reference. \emph{restricted} refers to the availability of the reference,
#' \emph{taxon_ID} to the taxonomic group available in a reference.
#' \emph{list_ID} is the identification number of a checklist within a
#' reference, \emph{entity_ID} of the associated polygon. \emph{geo_entity}
#' associates a name to this identification number. \emph{suit_geo} indicates
#' whether the checklist is suitable for use, \emph{entity_class} and
#' \emph{entity_unit} give additional details about the polygon.
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
#' ex <- GIFT_lists()
#' }
#' 
#' @importFrom jsonlite read_json
#' 
#' @export

GIFT_lists <- function(api = "https://gift.uni-goettingen.de/api/extended/",
                       GIFT_version = "latest"){
  check_api(api)
  GIFT_version <- check_gift_version_simple(GIFT_version)
  
  tmp <- jsonlite::read_json(paste0(
    api,"index", ifelse(GIFT_version == "beta", "", GIFT_version),
    ".php?query=lists"), simplifyVector = TRUE)
  return(tmp)
}
