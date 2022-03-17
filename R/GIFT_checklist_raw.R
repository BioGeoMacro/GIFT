#' GIFT checklists
#'
#' Raw checklist, to combine with other functions
#'
#' @param list_ID A vector defining the ID of the lists to retrieve.
#' `NULL` by default, in that case, every list from GIFT is retrieved.
#' 
#' @param taxon_name Character.
#' 
#' @param namesmatched Boolean: do you want the full species name.
#' 
#' @param floristic_group NULL or character among these options: 'native',
#' 'naturalized', 'endemic_list', 'endemic_ref'.
#' 
#' @param api character string defining from which API the data will be retrieved.
#' 
#' @return
#' A data frame with 12 columns, if namesmatched = FALSE.
#'
#' @details Blabla.
#'
#' @references
#'      Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
#'      Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
#'      https://doi.org/10.1111/jbi.13623
#'
#' @seealso [GIFT::GIFT_checklist_raw()]
#'
#' @examples
#' \dontrun{
#' ex <- GIFT_checklist_raw(list_ID = c(1,5), api = api)
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom dplyr bind_rows
#' 
#' @export

GIFT_checklist_raw <- function(
  list_ID = NULL, namesmatched = FALSE,
  taxon_name = "Tracheophyta",
  floristic_group = NULL,
  # valid options are: c("native", "naturalized", "endemic_list", "endemic_ref"),
  api = "http://gift.uni-goettingen.de/api/extended/index.php"
  # potential arguments not implemented yet
  # endemic_ref = 0, endemic_list = 0, native = 0, naturalized = 0,
  # ref_ID
){
  
  # 1. Controls ----
  # Arguments
  if(is.null(list_ID)){
    stop("Please provide the ID numbers of the checklists you want to load.")
  }
  
  # if(!is.numeric(taxonid)){
  #   stop("'taxonid' is a numeric describing what taxonomic group you want.
  #        See help of the function.")
  # }
  
  if(!is.logical(namesmatched)){
    stop("'namesmatched' must be a logical indicating whether you want access to original name.")
  }
  
  if(!is.character(api)){
    stop("api must be a character string indicating which API to use.")
  }
  
  # 2. Query ----
  ## 2.1. Taxonomy ----
  # Taxonomy query
  taxonomy <- jsonlite::read_json(paste0(api, "?query=taxonomy"),
                                  simplifyVector = TRUE)
  
  # Define tax_group
  taxonid <- taxonomy[which(taxonomy$taxon_name == taxon_name), "taxon_ID"]
  
  ## 2.2. Loop ----
  list_raw <- c()
  for(i in seq_along(list_ID)){
    tmp <- jsonlite::read_json(paste0(
      api, "?query=checklists&listid=",
      as.numeric(list_ID[i]), "&taxonid=", as.numeric(taxonid),
      "&namesmatched=", as.numeric(namesmatched),
      ifelse(is.null(floristic_group),
             "", paste0("&filter=", floristic_group)))
      , simplifyVector = TRUE)
    
    list_raw <- dplyr::bind_rows(list_raw, tmp)
  }
  
  return(list_raw)
}
