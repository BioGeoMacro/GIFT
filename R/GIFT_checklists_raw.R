#' GIFT checklists
#'
#' Raw checklists, to combine with other functions.
#'
#' @param ref_ID A vector defining the IDs of the references to retrieve.
#' `NULL` by default.
#' 
#' @param list_ID A vector defining the IDs of the lists to retrieve.
#' `NULL` by default. These lists are retrieved in addition to the lists 
#' contained in the references in `ref_ID`.
#' 
#' @param taxon_name Character string corresponding to the taxonomic group
#' of interest.
#' 
#' @param namesmatched Boolean. `FALSE` by default, set to `TRUE` if you want
#' the original species name as they came in the references as well as details
#' on the taxonomic harmonization.
#' 
#' @param floristic_group Character string among these options:
#' `all`, `native`, `naturalized`, `endemic_list`, `endemic_ref`.
#'
#' @param list_set `NULL` by default. If not, it has to be the list table
#' (see [GIFT::GIFT_lists()]). Used internally in [GIFT::GIFT_checklists()] to
#' avoid downloading the table of lists many times.
#' 
#' @param taxonomy `NULL` by default. If not, it has to be the taxonomy table
#' (see [GIFT::GIFT_taxonomy()]). Used internally in [GIFT::GIFT_checklists()]
#' to avoid downloading the taxonomy table many times.
#' 
#' 
#' 
#' @template GIFT_version_api
#' 
#' @return
#' A data frame with 15 or 29 columns (depending on namesmatched). This
#' data frame contains the species checklist for a given reference/list.
#'
#' @details  Here is what each column refers to:
#' 
#' \emph{ref_ID} - Identification number of each reference\cr
#' \emph{list_ID} - Identification number of each list\cr
#' \emph{orig_ID} - Identification number of each species name, unchanged from
#'  the sources\cr
#' \emph{name_ID} - Identification number of each reference\cr
#' \emph{genus} - Genus of each species\cr
#' \emph{species_epithet} - Epithet of each species\cr
#' \emph{subtaxon} - If needed, subtaxon of the species\cr
#' \emph{author} - Name of the author who described the species\cr
#' \emph{matched} - Whether a match was found when using a taxonomic
#' backbone\cr
#' \emph{epithetscore} - Matching score for the epithet\cr
#' \emph{overallscore} - Matching score for the overall species name\cr
#' \emph{resolved} - Whether the species name was resolved\cr
#' \emph{service} - Service used for the taxonomic harmonization\cr
#' \emph{work_ID} - Identification number of each species name, after
#'  taxonomic  harmonization\cr
#' \emph{genus_ID} - Identification number of each genus, after taxonomic
#'  harmonization\cr
#' \emph{species} - Species name, after taxonomic harmonization\cr
#' \emph{questionable} - Whether the species occurrence is questionable\cr
#' \emph{native} - Whether the species is native\cr
#' \emph{quest_native} - Whether the native information is questionable\cr
#' \emph{naturalized} - Whether the species is naturalized\cr
#' \emph{endemic_ref} - Whether the species is endemic within the reference\cr
#' \emph{quest_end_ref} - Whether the endemic_ref information is
#' questionable\cr
#' \emph{endemic_list}- Whether the species is endemic within the list\cr
#' \emph{quest_end_list} - Whether the endemic_list information is
#'  questionable\cr
#' \emph{cons_status} - Conservation status of the species
#' 
#' @references
#'      Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
#'      Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
#'      https://doi.org/10.1111/jbi.13623
#'
#' @seealso [GIFT::GIFT_checklists()]
#'
#' @examples
#' \donttest{
#' ex <- GIFT_checklists_raw(list_ID = c(1,5))
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom dplyr bind_rows mutate_at relocate
#' @importFrom utils setTxtProgressBar txtProgressBar
#' 
#' @export

GIFT_checklists_raw <- function(
    ref_ID = NULL, list_ID = NULL, namesmatched = FALSE,
    taxon_name = "Tracheophyta", floristic_group = "all",
    list_set = NULL, taxonomy = NULL, GIFT_version = "latest",
    api = "https://gift.uni-goettingen.de/api/extended/"
){
  
  # 1. Controls ----
  # Arguments
  if(is.null(list_ID) & is.null(ref_ID)){
    stop("Please provide the ID numbers of the references and/or lists you want 
         to load.")
  }
  
  if(!is.null(ref_ID) & !is.character(ref_ID) & !is.numeric(ref_ID)){
    stop("'ref_ID' must be a character string or a numeric stating the
         identification numbers of the references you want to retrieve.")
  }
  
  if(!is.null(list_ID) & !is.character(list_ID) & !is.numeric(list_ID)){
    stop("'list_ID' must be a character string or a numeric stating the
         identification numbers of the lists you want to retrieve.")
  }
  
  if(!is.logical(namesmatched)){
    stop("'namesmatched' must be a logical indicating whether you want to
         retrieve the original names of species, as they came in the
         references.")
  }
  
  check_taxon_name(taxon_name)
  
  taxon_check <- suppressMessages(
    GIFT::GIFT_taxonomy(api = api, GIFT_version = GIFT_version))
  if(!(taxon_name %in% taxon_check$taxon_name)){
    stop("The 'taxon_name' you specified is not available in GIFT. Run
         GIFT_taxonomy() to look at the available options (column 'taxon_name'
         of the output).")
  }
  
  if(length(floristic_group) != 1 || is.na(floristic_group) ||
     !is.character(floristic_group) || 
     !(floristic_group %in% c("all", "native", "naturalized", "endemic_ref",
                              "endemic_list"))){
    stop(c("'floristic_group' must be a character string. Available options are
    'all', 'native', 'naturalized', 'endemic_ref' and 'endemic_list'."))
  }
  
  # list_set
  
  
  # taxonomy
  if(!is.null(taxonomy)){
    if(!is.data.frame(taxonomy) ||
       !(all(c("taxon_ID", "taxon_name", "taxon_author", "taxon_lvl", "lft",
               "rgt") %in% colnames(taxonomy)))){
      stop("'taxonomy' must be a dataframe with specific column names.
         See GIFT_taxonomy().")
    }
  }
  
  check_api(api)
  GIFT_version <- check_gift_version_simple(GIFT_version)
  
  # 2. Query ----
  ## 2.0 Lists query
  if(!is.null(ref_ID)){
    if(is.null(list_set)){
      list_set <- jsonlite::read_json(
        paste0(api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
               ".php?query=lists"),
        simplifyVector = TRUE)
    }
    list_ID <- unique(append(list_ID,
                             as.numeric(list_set$list_ID[
                               which(list_set$ref_ID %in% ref_ID)])))
  }
  
  ## 2.1. Taxonomy ----
  # Taxonomy query
  if(is.null(taxonomy)){
    taxonomy <- jsonlite::read_json(
      paste0(api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
             ".php?query=taxonomy"),
      simplifyVector = TRUE)
  }
  
  # Define tax_group
  taxonid <- taxonomy[which(taxonomy$taxon_name == taxon_name), "taxon_ID"]
  
  ## 2.2. Loop ----
  list_raw <- list()
  if (length(unique(list_ID))>0){
    progress <- utils::txtProgressBar(min = 0, max = length(unique(list_ID)),
                                      initial = 0) 
    for(i in seq_along(list_ID)){
      list_raw[[i]] <- jsonlite::read_json(paste0(
        api, "index", ifelse(GIFT_version == "beta", "", GIFT_version), 
        ".php?query=checklists&listid=",
        as.numeric(list_ID[i]), "&taxonid=", as.numeric(taxonid),
        "&namesmatched=", as.numeric(namesmatched),
        ifelse(floristic_group == "all",
               "", paste0("&filter=", floristic_group)))
        , simplifyVector = TRUE)
      
      utils::setTxtProgressBar(progress, i)
    }
    # list_raw <- purrr::map(
    #   .x = seq_along(list_ID),
    #   .f = function(x){
    #     jsonlite::read_json(paste0(
    #       api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
    #       ".php?query=checklists&listid=",
    #       as.numeric(list_ID[x]), "&taxonid=", as.numeric(taxonid),
    #       "&namesmatched=", as.numeric(namesmatched),
    #       ifelse(floristic_group == "all",
    #              "", paste0("&filter=", floristic_group)))
    #       , simplifyVector = TRUE)
    #   },
    #   .progress = paste0("Retrieving ", length(unique(list_ID)),
    #                      " checklists"))
    
    message("\n")
  }
  
  list_raw <- dplyr::bind_rows(list_raw)
  
  # Data.frame
  list_raw <- as.data.frame(list_raw)
  
  # Format of the output
  if(nrow(list_raw) == 0){
    if(namesmatched){
      list_raw <- data.frame(ref_ID = numeric(), list_ID = numeric(),
                             entity_ID = numeric(),orig_ID = numeric(), 
                             name_ID = numeric(), cf_genus = numeric(), 
                             genus = character(), cf_species = numeric(), 
                             aff_species = numeric(), 
                             species_epithet = character(),
                             subtaxon = character(), author = character(),
                             matched = numeric(), epithetscore = numeric(),
                             overallscore = numeric(), resolved = numeric(),
                             synonym = numeric(), matched_subtaxon = numeric(), 
                             accepted = numeric(),
                             service = character(), work_ID = numeric(),
                             genus_ID = numeric(), work_species = character(),
                             work_author = character(),
                             questionable = numeric(), native = numeric(),
                             quest_native = numeric(), naturalized = numeric(),
                             endemic_ref = numeric(),
                             quest_end_ref = numeric(),
                             endemic_list = numeric(),
                             quest_end_list = numeric(),
                             cons_status = logical())
    } else{
      list_raw <- data.frame(ref_ID = numeric(), list_ID = numeric(), 
                             entity_ID = numeric(),
                             work_ID = numeric(), genus_ID = numeric(),
                             work_species = character(), 
                             work_author = character(),
                             questionable = numeric(),
                             native = numeric(), quest_native = numeric(),
                             naturalized = numeric(), endemic_ref = numeric(),
                             quest_end_ref = numeric(),
                             endemic_list = numeric(),
                             quest_end_list = numeric(),
                             cons_status = logical())
    }
  } else{
    # Some columns have to be numeric
    list_raw <- dplyr::mutate_at(
      list_raw, c("ref_ID", "list_ID", "entity_ID", "genus_ID", "work_ID",
                  "questionable", "native", "quest_native", "naturalized",
                  "endemic_ref", "quest_end_ref", "endemic_list",
                  "quest_end_list"), as.numeric)
    
    list_raw$cons_status <- as.character(list_raw$cons_status)
    
    if(namesmatched){
      list_raw <- dplyr::mutate_at(
        list_raw, c("orig_ID", "name_ID", "cf_genus", "cf_species",
                    "aff_species", "matched", "epithetscore", "overallscore",
                    "resolved","synonym","matched_subtaxon","accepted"), 
        as.numeric)
    }
  }
  
  # Reordering column 'work_author' if available
  list_raw <- dplyr::relocate(list_raw, "work_author",.after = "work_species")
  
  message(
    "Be cautious, species indicated as endemic were stated like this in the
  source reference/checklist. It can be that these species appear in other
  checklists.")
  
  message(
    "The taxonomic status corresponds to the original taxon names (including
  subspecies and synonyms) and may not be valid for the taxonomically
  standardized species names (column 'work_species').")
  
  return(list_raw)
}
