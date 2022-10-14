#' Taxonomy of GIFT
#'
#' Retrieves the taxonomy of GIFT.
#'
#' @param GIFT_version character string defining the version of the GIFT
#'  database to use. The function retrieves by default the most up-to-date
#'  version.
#' 
#' @param api character string defining from which API the data will be
#' retrieved.
#' 
#' @return
#' A data frame with 6 columns.
#'
#' @details Column taxon_ID indicates the identification number of each
#' taxonomic entry. The names describing taxa are in the column taxon_name.
#' The third column spells the author name for a given taxon. The column
#' taxon_lvl splits every taxon in genus, family, order or superior orders.
#' Taxonomy is a linear sequence of left and right borders for each taxon.
#' This is nested, for example left and right borders of a genus would fall
#' between the left and right borders of the corresponding family. Columns lft
#' and rgt respectively refer to these left and right borders.
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
#' @importFrom dplyr mutate_at
#' 
#' @export

GIFT_taxonomy <- function(GIFT_version = "latest", 
                          api = "http://gift.uni-goettingen.de/api/extended/"){
  # 1. Controls ----
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
  taxonomy <- read_json(paste0(
    api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
    ".php?query=taxonomy"), simplifyVector = TRUE)
  
  # Convert columns as numeric
  taxonomy <- dplyr::mutate_at(taxonomy, c("taxon_ID", "lft", "rgt"),
                               as.numeric)

  return(taxonomy)
}
