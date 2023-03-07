#' Species list in GIFT
#'
#' Retrieve the whole set of plant species available in GIFT.
#'
#' @template GIFT_version_api
#' 
#' @return
#' A data frame with 5 columns.
#'
#' @details Here is what each column refers to:
#' 
#' \emph{work_ID} - Identification number of the species\cr
#' \emph{genus_ID} - Identification number of the genus\cr
#' \emph{work_genus} - Genus name after taxonomic harmonization\cr
#' \emph{work_species} - Species name after taxonomic harmonization\cr
#' \emph{work_author} - Author who described the species (after taxonomic
#'  harmonization)
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
#' ex <- GIFT_species()
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom dplyr bind_rows mutate_at
#' 
#' @export

GIFT_species <- function(api = "https://gift.uni-goettingen.de/api/extended/",
                         GIFT_version = "latest"){
  check_api(api)
  GIFT_version <- check_gift_version(GIFT_version)
  
  # Return the species names
  tmp <- list()
  for (i in seq_len(6)){
    tmp[[i]] <- jsonlite::read_json(paste0(
      api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
      ".php?query=species&startat=", as.integer((i-1)*100000)), 
      simplifyVector = TRUE)
  }
  tmp <- dplyr::bind_rows(tmp)
  
  tmp <- dplyr::mutate_at(tmp, c("work_ID", "genus_ID"), as.numeric)
  tmp$work_author <- as.character(tmp$work_author)
  
  return(tmp)
}
