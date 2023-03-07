#' Taxonomy of GIFT
#'
#' Retrieves the taxonomy of GIFT.
#'
#' @template GIFT_version_api
#' 
#' @return A data frame with 6 columns.
#'
#' @details Here is what each column refers to:
#' 
#' \emph{taxon_ID} - the identification number of each taxonomic entry.\cr
#' \emph{taxon_name} - names describing taxa.\cr
#' \emph{taxon_name} - author name for a given taxon.\cr
#' \emph{taxon_lvl} - splits every taxon in genus, family, order or
#' superior orders. Taxonomy is a linear sequence of left and right borders
#' for each taxon. This is nested, for example left and right borders of a
#' genus would fall between the left and right borders of the corresponding
#' family. \cr
#' \emph{lft} - left border of one taxon in the taxonomic sequence.\cr
#' \emph{rgt} - right border of one taxon in the taxonomic sequence.\cr
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

GIFT_taxonomy <- function(
    GIFT_version = "latest", 
    api = "https://gift.uni-goettingen.de/api/extended/"){
  check_api(api)
  GIFT_version <- check_gift_version_simple(GIFT_version)
  
  taxonomy <- read_json(paste0(
    api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
    ".php?query=taxonomy"), simplifyVector = TRUE)
  
  # Convert columns as numeric
  taxonomy <- dplyr::mutate_at(taxonomy, c("taxon_ID", "lft", "rgt"),
                               as.numeric)
  
  return(taxonomy)
}
