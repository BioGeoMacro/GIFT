#' Species richness and trait coverage in GIFT
#'
#' Retrieve either species richness per polygon or trait coverage for a given
#' trait.
#'
#' @param taxon_name Name of the taxonomic group you want to retrieve.
#'
#' @param GIFT_version character string defining the version of the GIFT
#'  database to use. The function retrieves by default the most up-to-date
#'  version.
#' 
#' @param api character string defining from which API the data will be
#' retrieved.
#' 
#' @return A data frame with species richness for different floristic statuses
#' per GIFT polygon.
#'
#' @details The output has 5 columns:
#' 
#' \emph{entity_ID} - Identification number of GIFT polygons\cr
#' \emph{total} - total species richness\cr
#' \emph{native} - number of native species\cr
#' \emph{naturalized} - number of naturalized species\cr
#' \emph{endemic_min} - number of endemic species
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
#' ex <- GIFT_richness(taxon_name = "Angiospermae")
#' 
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom dplyr mutate_at
#' 
#' @export

GIFT_richness <- function(taxon_name = "Embryophyta", GIFT_version = "latest",
                          api = "https://gift.uni-goettingen.de/api/extended/"){
  
  # 1. Controls ----
  if(length(taxon_name) != 1 || is.na(taxon_name) ||
     !is.character(taxon_name)){
    stop("'taxon_name' is incorrect. It must be a character string among one of
         the taxonomic groups available in GIFT. To check them all, run
         'GIFT_taxonomy()'.")
  }
  check_api(api)
  GIFT_version <- check_gift_version_simple(GIFT_version)
  
  # 2. Query ----
  # Taxonomy query
  taxonomy <- jsonlite::read_json(
    paste0(api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
           ".php?query=taxonomy"), simplifyVector = TRUE)
  
  # Define tax_group
  tax_group <- taxonomy[which(taxonomy$taxon_name == taxon_name), "taxon_ID"]
  
  # Query
  tmp <- jsonlite::read_json(paste0(
    api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
    ".php?query=species_num&taxonid=", tax_group), simplifyVector = TRUE)
  
  tmp <- dplyr::mutate_at(tmp, c("entity_ID", "total", "native",
                                 "naturalized", "endemic_min"), as.numeric)
  
  return(tmp)
}
