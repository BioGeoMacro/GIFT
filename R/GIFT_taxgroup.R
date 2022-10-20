#' GIFT taxgroup
#'
#' Assign taxonomic groups of various hierarchical level to species from GIFT 
#' (work_ID)
#'
#' @param work_ID A vector defining the IDs of the species to retrieve
#' taxonomic groups for. `Null` by default. 
#' 
#' @param taxon_lvl taxonomic level to retrieve names for. "family" by default.
#' Check `GIFT_taxonomy()` for available levels. In addition to the available
#' levels one can put "higher_lvl" to retrieve the higher level groups
#' "Anthocerotophyta", "Marchantiophyta", "Bryophyta", "Lycopodiophyta",
#' "Monilophyta", "Gymnospermae", and "Angiospermae".
#' 
#' @param return_ID logical indicating whether to give back taxon_IDs instead
#' of names.
#' 
#' @param taxonomy option to supply taxonomy object here if loaded already to 
#' avoid double loading. For internal use within GIFT functions. If `NULL` 
#' (default) taxonomy will be loaded within this function.
#' 
#' @param species option to supply species names object here if loaded already
#' to avoid double loading. For internal use within GIFT functions. If `NULL` 
#' (default) species will be loaded within this function. 
#'  
#' @param api character string defining from which API the data will be
#' retrieved.
#' 
#' @param GIFT_version character string defining the version of the GIFT
#'  database to use. The function retrieves by default the most up-to-date
#'  version.
#' 
#' @return
#' A vector with the taxonomic group of the species used as input.
#'
#' @references
#'      Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
#'      Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
#'      https://doi.org/10.1111/jbi.13623
#'
#' @seealso [GIFT::GIFT_taxonomy()]
#'
#' @examples
#' \dontrun{
#' ex <- GIFT_taxgroup(work_ID = c(1:5), taxon_lvl = "family")
#' ex2 <- GIFT_taxgroup(work_ID = c(1:5), taxon_lvl = "higher_lvl")
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom dplyr bind_rows mutate_at
#' 
#' @export

GIFT_taxgroup <- function(work_ID = NULL,
                          taxon_lvl = c("family", "order", "higher_lvl")[1], 
                          return_ID = FALSE,
                          GIFT_version = "latest",
                          api = "http://gift.uni-goettingen.de/api/extended/",
                          taxonomy = NULL, species = NULL){
  
  # 1. Controls ----
  # Arguments
  if(is.null(work_ID)){
    stop("Please provide the ID numbers of the species you want 
         taxonomic groups for.")
  }
  
  if(length(taxon_lvl) != 1 || is.na(taxon_lvl) ||
     !is.character(taxon_lvl) || 
     !(taxon_lvl %in% c("family","order","higher_lvl"))){
    stop(c("'taxon_lvl' must be a character string stating what taxonomic
           level you want to retrieve. Available options are 'family',
           'genus' and 'higher_lvl'."))
  }
  
  if(length(return_ID) != 1 || !is.logical(return_ID)){
    stop(c("'return_ID' must be a boolean stating whether you want taxonomic
           names of IDs."))
  }
  
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
  
  if(!is.null(taxonomy)){
    if(!is.data.frame(taxonomy) ||
       (any(!c("taxon_ID", "taxon_name", "taxon_author", "taxon_lvl", "lft",
               "rgt") %in% colnames(taxonomy)))){
      stop("'taxonomy' must be a dataframe with specific column names.
         See GIFT_taxonomy().")
    }
  }
  
  if(!is.null(species)){
    if(!is.data.frame(species) ||
       !(all(c("work_ID", "genus_ID") %in% colnames(species)))){
      stop("'species' must be a dataframe with specific column names.
         See GIFT_species().")
    }
  }
  
  # 2. Queries ----
  
  ## 2.0 species names query
  if(is.null(species)){
    species <- GIFT_species(api = api, GIFT_version = GIFT_version)
  }
  
  species <- dplyr::mutate_at(species, c("work_ID", "genus_ID"), as.numeric)
  
  if(!all(work_ID %in% species$work_ID)) stop("Not all work_IDs found!")
  
  species <- species[match(work_ID,species$work_ID), ]
  genera <- unique(species$genus_ID)
  
  ## 2.0 taxonomy query
  if(is.null(taxonomy)){
    taxonomy <- jsonlite::read_json(
      paste0(api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
             ".php?query=taxonomy"), simplifyVector = TRUE)
  }
  
  taxonomy <- dplyr::mutate_at(taxonomy, c("taxon_ID", "lft", "rgt"),
                               as.numeric)
  
  taxa <- c()
  for(i in seq_along(genera)){
    tmp <- taxonomy[
      which(taxonomy$lft < taxonomy$lft[which(taxonomy$taxon_ID == genera[i])]
            & taxonomy$rgt > taxonomy$rgt[which(taxonomy$taxon_ID ==
                                                  genera[i])]), ]
    if(taxon_lvl == "higher_lvl"){
      tmp <- tmp[grep("level", tmp$taxon_lvl), ]
      tmp <- tmp[which.min(tmp$rgt - tmp$lft), ]
    } else {
      tmp <- tmp[which(tmp$taxon_lvl == taxon_lvl), ]
    }
    if(nrow(tmp) == 0){
      taxa[i] <- NA
    }else{
      if(return_ID){
        taxa[i] <- tmp[, "taxon_ID"]
      }else{
        taxa[i] <- tmp[, "taxon_name"]
      }
    }
  }
  
  return(taxa[match(species$genus_ID, genera)])
}
