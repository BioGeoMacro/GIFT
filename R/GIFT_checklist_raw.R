#' GIFT checklists
#'
#' Raw checklist, to combine with other functions
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
#' @param namesmatched Boolean. FALSE by default, set to TRUE if you want the
#' original species name as they came in the references as well as details on
#' the taxonomic harmonization.
#' 
#' @param floristic_group Character string among these options:
#' 'all', 'native', 'naturalized', 'endemic_list', 'endemic_ref'.
#' 
#' @param GIFT_version character string defining the version of the GIFT
#'  database to use. The function retrieves by default the most up-to-date
#'  version.
#' 
#' @param api character string defining from which API the data will be
#' retrieved.
#' 
#' @return
#' A data frame with 15 or 29 columns (depending on namesmatched). This
#' data frame contains the species checklist for a given reference/list.
#'
#' @details  Here is what each column refers to:
#' 'ref_ID' - Identification number of each reference.
#' 'list_ID'- Identification number of each list
#' 'orig_ID'- Identification number of each species name, unchanged from the
#'  sources.
#' 'name_ID'- Identification number of each reference.
#' 'genus'- Genus of each species.
#' 'species_epithet'- Epithet of each species.
#' 'subtaxon'- If needed, subtaxon of the species.
#' 'author'- Name of the author who described the species.
#' 'matched' - Whether a match was found when using a taxonomic backbone.
#' 'epithetscore' - Matching score for the epithet.
#' 'overallscore' - Matching score for the overall species name.
#' 'resolved' - Whether the species name was resolved.
#' 'service' - Service used for the taxonomic harmonization.
#' 'work_ID' - Identification number of each species name, after taxonomic
#'  harmonization.
#' 'genus_ID' - Identification number of each genus, after taxonomic
#'  harmonization.
#' 'species' - Species name, after taxonomic harmonization.
#' 'questionable' - Whether the species occurrence is questionable.
#' 'native' - Whether the species is native.
#' 'quest_native' - Whether the native information is questionable.
#' 'naturalized' - Whether the species is naturalized.
#' 'endemic_ref' - Whether the species is endemic within the reference.
#' 'quest_end_ref' - Whether the endemic_ref information is questionable.
#' 'endemic_list'- Whether the species is endemic within the list.
#' 'quest_end_list' - Whether the endemic_list information is questionable.
#' 'cons_status' - Conservation status of the species
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
#' ex <- GIFT_checklist_raw(list_ID = c(1,5))
#' ex2 <- GIFT_checklist_raw(list_ID = c(1,5), namesmatched = TRUE)
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom dplyr bind_rows
#' 
#' @export

GIFT_checklist_raw <- function(
    ref_ID = NULL, list_ID = NULL, namesmatched = FALSE,
    taxon_name = "Tracheophyta", floristic_group = "all",
    list_set = NULL, taxonomy = NULL, GIFT_version = "latest",
    api = "http://gift.uni-goettingen.de/api/extended/"
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
  
  if(!is.character(taxon_name)){
    stop("'taxon_name' must be a character string stating what taxonomical
         group you want to retrieve. Set to 'Tracheophyta' by default.")
  }
  
  if(!is.character(api)){
    stop("api must be a character string indicating which API to use.")
  }
  
  taxon_check <- GIFT::GIFT_taxonomy(api = api)
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
    if(!is.data.frame(taxonomy) |
       sum(c("taxon_ID", "taxon_name", "taxon_author", "taxon_lvl", "lft",
             "rgt") %in% colnames(taxonomy)) != 6){
      stop("'taxonomy' is NULL by default, which means that the whole
         taxonomy of GIFT is retrieved. If you already loaded it, you can
         provide it here as an argument. In that case, it must be a data
         frame containing the following columns 'taxon_ID', 'taxon_name',
         'taxon_author', 'taxon_lvl', 'lft' and 'rgt'.")
    }
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
    message("You are asking for the beta-version of GIFT which is subject to updates and edits. Consider using 'latest' for the latest stable version.")
  }
  
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
  list_raw <- c()
  for(i in seq_along(list_ID)){
    tmp <- jsonlite::read_json(paste0(
      api, "index", ifelse(GIFT_version == "beta", "", GIFT_version), 
      ".php?query=checklists&listid=",
      as.numeric(list_ID[i]), "&taxonid=", as.numeric(taxonid),
      "&namesmatched=", as.numeric(namesmatched),
      ifelse(floristic_group == "all",
             "", paste0("&filter=", floristic_group)))
      , simplifyVector = TRUE)
    
    list_raw <- dplyr::bind_rows(list_raw, tmp)
  }
  
  # Data.frame
  list_raw <- as.data.frame(list_raw)
  
  # Format of the output
  if(nrow(list_raw) == 0){
    if(namesmatched){
      list_raw <- data.frame(ref_ID = numeric(), list_ID = numeric(),
                             orig_ID = numeric(), 
                             name_ID = numeric(), genus = character(),
                             species_epithet = character(),
                             subtaxon = character(), author = character(),
                             matched = numeric(), epithetscore = numeric(),
                             overallscore = numeric(), resolved = numeric(),
                             synonym = numeric(),matched_subtaxon = numeric(),accepted = numeric(),
                             service = character(), work_ID = numeric(),
                             genus_ID = numeric(), species = character(),
                             questionable = numeric(), native = numeric(),
                             quest_native = numeric(), naturalized = numeric(),
                             endemic_ref = numeric(), quest_end_ref = numeric(),
                             endemic_list = numeric(),
                             quest_end_list = numeric(),
                             cons_status = logical())
    } else{
      list_raw <- data.frame(ref_ID = numeric(), list_ID = numeric(),
                             work_ID = numeric(), genus_ID = numeric(),
                             species = character(), questionable = numeric(),
                             native = numeric(), quest_native = numeric(),
                             naturalized = numeric(), endemic_ref = numeric(),
                             quest_end_ref = numeric(), endemic_list = numeric(),
                             quest_end_list = numeric(),
                             cons_status = logical())
    }
  } else{
    # Some columns have to be numeric
    list_raw[, c("ref_ID", "list_ID", "genus_ID", "work_ID", "questionable",
                 "native", "quest_native", "naturalized", "endemic_ref",
                 "quest_end_ref", "endemic_list", "quest_end_list")] <- 
      sapply(list_raw[, c("ref_ID", "list_ID", "genus_ID", "work_ID",
                          "questionable", "native", "quest_native",
                          "naturalized", "endemic_ref", "quest_end_ref",
                          "endemic_list", "quest_end_list")], as.numeric)
    list_raw$cons_status <- as.character(list_raw$cons_status)
    
    if(namesmatched){
      list_raw[, c("orig_ID", "name_ID", "matched", "epithetscore", "overallscore",
                   "resolved")] <- 
        sapply(list_raw[, c("orig_ID", "name_ID", "matched", "epithetscore",
                            "overallscore", "resolved")], as.numeric)
    }
    
    if(all(c("synonym","matched_subtaxon","accepted") %in% names(list_raw))){
      list_raw[, c("synonym","matched_subtaxon","accepted")] <- 
        sapply(list_raw[, c("synonym","matched_subtaxon","accepted")], as.numeric)
    }
    
  }
  
  message("Be cautious, species indicated as endemic were stated like this in the source reference/checklist. It can be that these species appear in other checklists.")
  
  return(list_raw)
}
