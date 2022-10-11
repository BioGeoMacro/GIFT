#' Species list in GIFT
#'
#' Retrieve all name matching information for one taxonomic name. All results 
#' are returned, where the name is either found in the unstandardized or 
#' taxonomically standardized names.
#'
#' @param genus character string defining the genus name to be looked for.
#' 
#' @param epithet character string defining the specific apithet to be looked for.
#' 
#' @param GIFT_version character string defining the version of the GIFT
#'  database to use. The function retrieves by default the most up-to-date
#'  version.
#' 
#' @param api character string defining from which API the data will be retrieved.
#' 
#' @return
#' A data frame with 24 columns.
#'
#' @details Here is what each column refers to:
#' 'orig_ID' - 
#' 'orig_genus'- 
#' 'name_ID'- 
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
#' 
#' @export

GIFT_species_lookup <- function(genus = "", epithet = "", api = "http://gift.uni-goettingen.de/api/extended/",
                         GIFT_version = "latest"){
  # 1. Controls ----
  # Arguments
  if(!is.character(api)){
    stop("api must be a character string indicating which API to use.")
  }
  
  if(!is.character(genus)){
    stop("genus must be a character string indicating genus to look for.")
  }
  
  if(!is.character(epithet)){
    stop("epithet must be a character string indicating the specific epithet to 
         look for.")
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
    message("You are asking for the beta-version of GIFT which is subject to updates and edits. Consider using 'latest' for the latest stable version.")
  }
  
  # 2. Function ----
  # Return the name matching information
  tmp <- jsonlite::read_json(paste0(
    api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
    ".php?query=names_matched&genus=", genus, "&epithet=", epithet
    ), simplifyVector = TRUE)
  
  tmp[, c("orig_ID","name_ID","cf_genus","cf_species","aff_species","matched",
          "epithetscore","overallscore","resolved","synonym","matched_subtaxon",
          "accepted","work_ID","taxon_ID")] <- 
    sapply(tmp[, c("orig_ID","name_ID","cf_genus","cf_species","aff_species",
                   "matched","epithetscore","overallscore","resolved","synonym",
                   "matched_subtaxon","accepted","work_ID","taxon_ID")], as.numeric)
  return(tmp)
}
