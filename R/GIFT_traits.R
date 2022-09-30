#' GIFT_traits
#'
#' Retrieve specific trait values.
#'
#' @param trait_IDs a character string indicating which trait you want to
#' retrieve. Traits must belong to the available list of traits.
#' 
#' @param agreement Traits desired.
#' 
#' @param bias_ref Traits desired.
#' 
#' @param bias_deriv Traits desired.
#' 
#' @param api Character string with the API.
#'
#' @param GIFT_version character string defining the version of the GIFT
#'  database to use. The function retrieves by default the most up-to-date
#'  version.
#' 
#' @return
#' A long-format data frame with 6 columns: `trait_ID`, `work_ID`, `species`,
#' `trait_value`, `agreement` and `references`.
#'
#' @details Blabla.
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
#' wood <- GIFT_traits(trait_IDs = c("1.1.1", "1.2.1"), agreement = 0.66,
#' bias_ref = FALSE, bias_deriv = FALSE)
#' 
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom dplyr bind_rows left_join relocate
#' @importFrom tidyr pivot_wider
#' 
#' @export
#' 
GIFT_traits <- function(
    trait_IDs = "", agreement = 0.66, bias_ref = TRUE, bias_deriv = TRUE,
    api = "http://gift.uni-goettingen.de/api/extended/",
    GIFT_version = "latest"){
  
  # 1. Controls ----
  # Arguments
  if(!is.character(trait_IDs)){
    stop("trait_IDs must be a character string indicating which trait you want
         to retrieve.")
  }
  
  if(!is.numeric(agreement)){
    stop("agreement must be a numeric between 0 and 1 indicating .")
  } else if(agreement > 1 | agreement < 0){
    stop("agreement must be a numeric between 0 and 1 indicating .")
  }
  
  if(!is.logical(bias_ref)){
    stop("bias_ref must be a logical.")
  }
  
  if(!is.logical(bias_deriv)){
    stop("bias_deriv must be a logical.")
  }
  
  if(!is.character(api)){
    stop("api must be a character string indicating which API to use.")
  }
  
  # Load traits_metadata to check if the provided IDs are available
  tmp <- GIFT::GIFT_traits_meta(api = api, GIFT_version = GIFT_version)
  if(!all(trait_IDs %in% tmp$Lvl3)){
    stop("trait_IDs must belong to the available list of traits. To see which
           traits are available, run 'traits_meta() and look at column
           'Lvl3'.")
  }
  
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
  
  # 2. Function ----
  # Initiating list
  trait_list <- list()
  
  # for-loop
  for (i in 1:length(trait_IDs)){
    trait_list[[i]] <- jsonlite::read_json(
      paste0(api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
             ".php?query=traits&traitid=",
             trait_IDs[i], "&biasref=", as.numeric(bias_ref),
             "&biasderiv=", as.numeric(bias_deriv)),
      simplifyVector = TRUE)
    trait_list[[i]]$trait_ID <- trait_IDs[i]
  }
  
  # Formating trait_list as a data.frame
  trait_list <- dplyr::bind_rows(trait_list)
  trait_list <- trait_list[which(trait_list$agreement >= agreement |
                                   is.na(trait_list$agreement)), ]
  
  # Add species names
  species <- GIFT::GIFT_species()
  trait_list <- dplyr::left_join(trait_list,
                                 species[, c("work_ID", "species")],
                                 by = "work_ID")
  
  # Reordering columns
  trait_list <- trait_list[, c("species", "work_ID", "trait_ID", "trait_value",
                               "agreement", "references")]
  
  # Wider format
  trait_list <- tidyr::pivot_wider(trait_list, names_from = "trait_ID",
                                   values_from = "trait_value")
  
  trait_list <- dplyr::relocate(trait_list, c("agreement", "references"),
                                .after = last_col())
  
  return(trait_list)
}
