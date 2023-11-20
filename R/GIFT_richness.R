#' Species richness per geographic region and taxonomic group in GIFT
#'
#' Retrieve species richness of all species, native species, naturalized 
#' species and endemic species per taxonomic group and geographic region 
#' combination.
#'
#' @param taxon_name Taxonomic group to retrieve species richness for.
#'
#' @template GIFT_version_api
#' 
#' @return A data frame with species richness values for different floristic 
#' subsets per geographic region in GIFT.
#'
#' @details The output has 5 columns:
#' 
#' \emph{entity_ID} - Identification number of the geographic region\cr
#' \emph{total} - total species richness\cr
#' \emph{native} - number of native species\cr
#' \emph{naturalized} - number of naturalized species\cr
#' \emph{endemic_min} - number of endemic species
#'
#' The number of endemic species is a conservative count not counting 
#' occurrences of species which go back to infraspecific taxa. 
#'
#' @references
#'      Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
#'      Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
#'      https://doi.org/10.1111/jbi.13623
#'
#' @seealso [GIFT::GIFT_traits_meta()]
#'
#' @examples
#' \donttest{
#' ex <- GIFT_richness(taxon_name = "Angiospermae")
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom dplyr mutate_at
#' 
#' @export

GIFT_richness <- function(taxon_name = "Embryophyta", GIFT_version = "latest",
                          api = "https://gift.uni-goettingen.de/api/extended/"){
  
  # 1. Controls ----
  api_check <- check_api(api)
  if(is.null(api_check)){
    return(NULL)
  } else{
    if(length(taxon_name) != 1 || is.na(taxon_name) ||
       !is.character(taxon_name)){
      stop(
        "'taxon_name' is incorrect. It must be a character string among one of
         the taxonomic groups available in GIFT. To check them all, run
         'GIFT_taxonomy()'.")
    }
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
}
