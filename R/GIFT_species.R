#' Species list in GIFT
#'
#' Retrieve the whole set of plant species available in GIFT
#'
#' @param GIFT_version character string defining the version of the GIFT
#'  database to use. The function retrieves by default the most up-to-date
#'  version.
#' 
#' @param api character string defining from which API the data will be
#' retrieved.
#' 
#' @return
#' A data frame with 5 columns.
#'
#' @details Here is what each column refers to:
#' 'work_ID' - Identification number of the species
#' 'genus_ID' - Identification number of the genus
#' 'work_genus' - Genus name after taxonomic harmonization
#' 'work_species'- Species name after taxonomic harmonization
#' 'work_author'- Author who described the species (after taxonomic
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

GIFT_species <- function(api = "http://gift.uni-goettingen.de/api/extended/",
                         GIFT_version = "latest"){
  # 1. Controls ----
  # Arguments
  if(!is.character(api)){
    stop("api must be a character string indicating which API to use.")
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
  # Return the species names
  tmp <- list()
  for (i in seq_len(10)){
    tmp[[i]] <- jsonlite::read_json(paste0(
      api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
      ".php?query=species&workidmin=", as.integer(i*100000-100000+1), 
      "&workidmax=", as.integer(i*100000)), simplifyVector = TRUE)
  }
  tmp <- dplyr::bind_rows(tmp)
  
  tmp <- dplyr::mutate_at(tmp, c("work_ID", "genus_ID"), as.numeric)
  tmp$work_author <- as.character(tmp$work_author)
  
  return(tmp)
}
