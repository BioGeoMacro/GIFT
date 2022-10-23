#' Species list in GIFT
#'
#' Retrieve all name matching information for one taxonomic name. All results 
#' are returned, where the name is either found in the unstandardized or 
#' taxonomically standardized names.
#'
#' @param genus character string defining the genus name to be looked for.
#' 
#' @param epithet character string defining the specific epithet to be looked
#' for.
#' 
#' @param namesmatched Boolean. FALSE by default, set to TRUE if you want to 
#' look for the species not only in the standardized species names but also 
#' in the original species names as they came in the original resources.
#' 
#' @param GIFT_version character string defining the version of the GIFT
#'  database to use. The function retrieves by default the most up-to-date
#'  version.
#' 
#' @param api character string defining from which API the data will be
#' retrieved.
#' 
#' @return
#' A data frame with 19 columns (or 24 if namesmatched = TRUE).
#'
#' @details Here is what each column refers to:
#' \emph{orig_ID} - Identification number of the species before taxonomic
#' harmonization\cr
#' \emph{orig_genus} - Genus before taxonomic harmonization\cr
#' \emph{name_ID} - Identification number of the genus before taxonomic
#' harmonization\cr
#' \emph{cf_genus}- \cr
#' \emph{genus}- Genus before taxonomic harmonization\cr
#' \emph{cf_species}- \cr
#' \emph{aff_species}-\cr
#' \emph{species_epithet}- Epithet of the species before taxonomic
#'  harmonization\cr
#' \emph{subtaxon}- Subtaxon of the species before taxonomic harmonization\cr
#' \emph{author}- Author who described the species (before taxonomic
#'  harmonization)\cr
#' \emph{matched}- Is the species matched in the taxonomic backbone\cr
#' \emph{epithetscore}- Matching score for the epithet\cr
#' \emph{overallscore}- Overall matching score for the species\cr
#' \emph{resolved}- Is the species name resolved in the taxonomic backbone\cr
#' \emph{synonym}- Is the species name a synonym in the taxonomic backbone\cr
#' \emph{matched_subtaxon}- Is the subtaxon matched in the taxonomic backbone\cr
#' \emph{accepted}- Is the species name accepted in the taxonomic backbone\cr
#' \emph{service}- Service use for the taxonomic harmonization\cr
#' \emph{work_ID}- Identification number of the species after taxonomic
#' harmonization\cr
#' \emph{taxon_ID}- Identification number of the taxonomic group\cr
#' \emph{work_genus}- Identification number of the genus after taxonomic
#' harmonization\cr
#' \emph{work_species_epithet}- Identification number of the species epithet
#' after taxonomic harmonization\cr
#' \emph{work_species} - Species name (after taxonomic harmonization)\cr
#' \emph{work_author}-  Author who described the species (after taxonomic
#' harmonization)
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
#' ex <- GIFT_species_lookup(genus = "Fagus", epithet = "sylvatica")
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom dplyr mutate_at
#' 
#' @export

GIFT_species_lookup <-
  function(genus = "", epithet = "", namesmatched = FALSE, 
           api = "http://gift.uni-goettingen.de/api/extended/",
           GIFT_version = "latest"){
    # 1. Controls ----
    # Arguments
    if(length(api) != 1 || !is.character(api)){
      stop("api must be a character string indicating which API to use.")
    }
    
    if(!is.character(genus)){
      stop("genus must be a character string indicating genus to look for.")
    }
    
    if(!is.character(epithet)){
      stop("epithet must be a character string indicating the specific epithet
      to look for.")
    }
    
    if(length(namesmatched) != 1 || !is.logical(namesmatched) ||
       is.na(namesmatched)){
      stop("'namesmatched' must be a boolean stating whether you only want to 
    look for the species not only in the standardized species names or also 
    in the original species names as they came in the original resources")
    }
    
    # GIFT_version
    gift_version <- jsonlite::read_json(
      "https://gift.uni-goettingen.de/api/index.php?query=versions",
      simplifyVector = TRUE)
    if(length(GIFT_version) != 1 || is.na(GIFT_version) ||
       !is.character(GIFT_version) || 
       !(GIFT_version %in% c(unique(gift_version$version),
                             "latest", "beta"))){
      stop(c("'GIFT_version' must be a character string stating what version
    of GIFT you want to use. Available options are 'latest' and the different
           versions."))
    }
    if(GIFT_version == "latest"){
      GIFT_version <- gift_version[nrow(gift_version), "version"]
    }
    if(GIFT_version == "beta"){
      message("You are asking for the beta-version of GIFT which is subject to
              updates and edits. Consider using 'latest' for the latest stable
              version.")
    }
    
    # 2. Function ----
    # Return the name matching information
    if(namesmatched){
      tmp <- jsonlite::read_json(paste0(
        api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
        ".php?query=names_matched&genus=", genus, "&epithet=", epithet
      ), simplifyVector = TRUE)
      
      if(length(tmp)>0){
        tmp <- dplyr::mutate_at(
          tmp, c("orig_ID", "name_ID", "cf_genus", "cf_species", "aff_species",
                 "matched", "epithetscore", "overallscore", "resolved",
                 "synonym", "matched_subtaxon", "accepted", "work_ID",
                 "taxon_ID"),
          as.numeric)
      } else {
        tmp <- data.frame(orig_ID = numeric(), orig_genus = character(), 
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
                          taxon_ID = numeric(), work_genus = character(),
                          work_species_epithet = character(), 
                          work_species = character(),
                          work_author = character())
      }
    } else {
      tmp <- jsonlite::read_json(paste0(
        api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
        ".php?query=names_matched_unique&genus=", genus, "&epithet=", epithet
      ), simplifyVector = TRUE)
      
      if(length(tmp)>0){
        tmp <- dplyr::mutate_at(
          tmp, c("name_ID", "matched", "epithetscore", "overallscore", 
                 "resolved", "synonym", "matched_subtaxon", "accepted", 
                 "work_ID", "taxon_ID"),
          as.numeric)
      } else {
        tmp <- data.frame(name_ID = numeric(), genus = character(),
                          species_epithet = character(),
                          subtaxon = character(), author = character(),
                          matched = numeric(), epithetscore = numeric(),
                          overallscore = numeric(), resolved = numeric(),
                          synonym = numeric(), matched_subtaxon = numeric(), 
                          accepted = numeric(),
                          service = character(), work_ID = numeric(),
                          taxon_ID = numeric(), work_genus = character(),
                          work_species_epithet = character(), 
                          work_species = character(),
                          work_author = character())
      }
    }
    if(nrow(tmp)== 0){
      message("No species names found.")
    }
    return(tmp)
  }
