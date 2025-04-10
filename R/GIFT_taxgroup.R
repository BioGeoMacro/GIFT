#' Taxonomic group of species
#'
#' Assign taxonomic groups of various hierarchical level to species from GIFT 
#' (`work_ID`).
#'
#' @param work_ID A vector defining the IDs of the species to retrieve
#' taxonomic groups for. `NULL` by default. 
#' 
#' @param taxon_lvl taxonomic level to retrieve names for. `family` by default.
#' Check `GIFT_taxonomy()` for available levels. In addition to the available
#' levels one can put `higher_lvl` to retrieve the higher level groups
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
#' @template GIFT_version_api
#' 
#' @return A vector with the taxonomic group of the species used as input.
#'
#' @references
#'      Denelle, P., Weigelt, P., & Kreft, H. (2023). GIFT—An R package to
#'      access the Global Inventory of Floras and Traits. Methods in Ecology
#'      and Evolution, 14, 2738-2748.
#'      https://doi.org/10.1111/2041-210X.14213
#' 
#'      Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
#'      Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
#'      https://doi.org/10.1111/jbi.13623
#'
#' @seealso [GIFT::GIFT_taxonomy()]
#'
#' @examples
#' \donttest{
#' ex <- GIFT_taxgroup(work_ID = c(1, 4, 7, 8), taxon_lvl = "family")
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
                          api = "https://gift.uni-goettingen.de/api/extended/",
                          taxonomy = NULL, species = NULL){
  
  # 1. Controls ----
  api_check <- check_api(api)
  if(is.null(api_check)){
    return(NULL)
  } else{
    if(is.null(work_ID)){
      stop("Please provide the ID numbers of the species you want 
         taxonomic groups for.")
    }
    
    if(length(taxon_lvl) != 1 || is.na(taxon_lvl) ||
       !is.character(taxon_lvl) || 
       !(taxon_lvl %in% c("family","order","higher_lvl"))){
      stop(c("'taxon_lvl' must be a character string stating what taxonomic
           level you want to retrieve. Available options are 'family',
           'order' and 'higher_lvl'."))
    }
    
    if(length(return_ID) != 1 || !is.logical(return_ID)){
      stop(c("'return_ID' must be a logical stating whether you want taxonomic
           names of IDs."))
    }
    
    GIFT_version <- check_gift_version_simple(GIFT_version)
    
    if(!is.null(taxonomy)){
      if(!is.data.frame(taxonomy) ||
         !(all(c("taxon_ID", "taxon_name", "taxon_author", "taxon_lvl", "lft",
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
      species <- suppressMessages(
        GIFT_species(api = api, GIFT_version = GIFT_version))
    }
    
    species <- dplyr::mutate_at(species, c("work_ID", "genus_ID"), as.numeric)
    
    if(!all(work_ID %in% species$work_ID)){
      if(length(work_ID[work_ID %in% species$work_ID]) <= 20){
        stop(paste0("The following work_IDs were not found:",
                    paste(work_ID[!(work_ID %in% species$work_ID)],
                          collapse = " ")))
      } else{
        stop("More than 20 work_IDs were not found!")
      }
    } 
    
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
}
