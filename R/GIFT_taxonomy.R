#' Taxonomy of GIFT
#'
#' Retrieves the taxonomy of GIFT.
#'
#' @param GIFT_version character string defining the version of the GIFT
#'  database to use. The function retrieves by default the most up-to-date
#'  version.
#' 
#' @param api character string defining from which API the data will be retrieved.
#' 
#' @return
#' A data frame with 6 columns.
#'
#' @details Column taxon_ID indicates the identification number of each
#' taxonomic entry. The names describing taxa are in the column taxon_name.
#' The third column spells the author name for a given taxon. The column
#' taxon_lvl splits every taxon in genus, family, order or superior orders.
#' Columns lft and rgt are used internally.
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
#' ex <- GIFT_taxonomy()
#' }
#' 
#' @importFrom jsonlite read_json
#' 
#' @export

GIFT_taxonomy <- function(GIFT_version = NULL, 
                          api = "http://gift.uni-goettingen.de/api/extended/"){
  # 1. Controls ----
  if(!is.character(api)){
    stop("api must be a character string indicating which API to use.")
  }
  
  # Control for GIFT_version to add
  
  # 2. Query ----
  tmp <- read_json(paste0(
    api, "index", ifelse(is.null(GIFT_version), "", GIFT_version),
    ".php?query=taxonomy"), simplifyVector = TRUE)
  
  return(tmp)
}
