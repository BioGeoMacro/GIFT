#' Taxonomic and trait coverage per geographic region and taxonomic group in 
#' GIFT
#'
#' Retrieve taxonomic or trait coverage (for a given trait) of all species, 
#' native species, naturalized species and endemic species per taxonomic group 
#' and geographic region combination.
#' 
#' @param what character indicating whether `taxonomic_coverage` or 
#' `trait_coverage` shall be retrieved.
#'
#' @param taxon_name Name of the taxonomic group you want to retrieve coverage 
#' for. See [GIFT::GIFT_taxonomy()] for details.
#'
#' @param trait_ID Identification number of the trait you want to retrieve 
#' coverage for. See [GIFT::GIFT_traits_meta()] for details.
#' 
#' @template GIFT_version_api
#' 
#' @return A data frame with either taxonomic or trait coverage per
#' GIFT polygon.
#'
#' @details The output has 9 columns:
#' 
#' \emph{entity_ID} - Identification number of GIFT polygons\cr
#' \emph{total} - taxonomic or trait coverage for all species\cr
#' \emph{total_rst} - taxonomic or coverage for all species considering 
#' restricted resources\cr
#' \emph{native} - taxonomic or trait coverage for native species\cr
#' \emph{native_rst} - taxonomic or trait coverage for native species 
#' considering restricted resources\cr
#' \emph{naturalized} - taxonomic or trait coverage for naturalized species\cr
#' \emph{naturalized_rst} - taxonomic or trait coverage for naturalized species 
#' considering restricted resources\cr
#' \emph{endemic_min} - taxonomic or trait coverage for endemic species\cr
#' \emph{endemic_min_rst} - taxonomic or trait coverage for endemic species 
#' considering restricted resources
#' 
#' In the case of taxonomic coverage, a '1' means that species composition data
#' is available for the given combination of taxonomic group and geographic 
#' region while 'NA' means that no data is available. This can differ depending 
#' on whether restricted data in GIFT is considered or not (columns with or 
#' without _rst at the end).
#' 
#' In the case of trait coverage, the proportion of species of a given 
#' taxonomic group with information on the defined trait is reported per 
#' geographic region.
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
#' ex <- GIFT_coverage(what = "taxonomic_coverage", taxon_name = "Angiospermae")
#' ex2 <- GIFT_coverage(what = "trait_coverage", taxon_name = "Angiospermae",
#' trait_ID = "1.2.1")
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom dplyr mutate_at mutate
#' 
#' @export

GIFT_coverage <- function(
    what = "taxonomic_coverage", taxon_name = "Embryophyta",
    trait_ID = "1.1.1", GIFT_version = "latest",
    api = "https://gift.uni-goettingen.de/api/extended/"){
  
  # 1. Controls ----
  if(length(what) != 1 || is.na(what) || !is.character(what) ||
     !(all(what %in% c("taxonomic_coverage", "trait_coverage")))){
    stop("'what' is incorrect. It must be a character string equal to either 
         'taxonomic_coverage' or 'trait_coverage'.")
  }
  
  if(length(taxon_name) != 1 || is.na(taxon_name) ||
     !is.character(taxon_name)){
    stop("'taxon_name' is incorrect. It must be a character string among one of
         the taxonomic groups available in GIFT. To check them all, run
         'GIFT_taxonomy()'.")
  }
  
  if(length(trait_ID) != 1){
    stop("Please provide one trait_ID only.")
  }
  
  if(is.na(trait_ID) || !is.character(trait_ID)){
    stop("'trait_ID' is incorrect. It must be a character string of the
    identification number of a trait. To check these IDs, run
         'GIFT_traits_meta()'.")
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
  if(what == "trait_coverage"){
    tmp <- jsonlite::read_json(paste0(
      api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
      ".php?query=traits_cov&traitid=",
      paste(trait_ID, collapse = ","), "&taxonid=", tax_group),
      simplifyVector = TRUE)
    
  } else if(what == "taxonomic_coverage"){
    tmp <- jsonlite::read_json(paste0(
      api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
      ".php?query=species_cov&taxonid=", tax_group), simplifyVector = TRUE)
  }
  
  tmp <- dplyr::mutate_at(tmp, c("entity_ID", "total", "total_rst", "native", "native_rst",
                                 "naturalized", "naturalized_rst", "endemic_min", "endemic_min_rst"), as.numeric)
  return(tmp)
}
