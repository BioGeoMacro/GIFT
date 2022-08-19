#' GIFT_traits_raw
#'
#' Retrieve non-aggregated trait values at the level of the bibliographic 
#' references and un-standardized species names in GIFT.
#'
#' @param trait_IDs a character string indicating which traits you want to
#' retrieve. Traits must belong to the available list of traits. See 
#' GIFT_traits_meta()
#' 
#' @param derived include logically derived traits.
#' 
#' @param bias_ref Traits desired.
#' 
#' @param bias_deriv Traits desired.
#' 
#' @param api Character string with the API.
#'
#' @return
#' A 
#'
#' @details Blabla.
#'
#' @references
#'      Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
#'      Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
#'      https://doi.org/10.1111/jbi.13623
#'
#' @seealso [GIFT::GIFT_traits_meta()] and [GIFT::GIFT_traits()]
#'
#' @examples
#' \dontrun{
#' 
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom dplyr bind_rows left_join
#' 
#' @export
#' 
GIFT_traits_raw <- function(
  trait_IDs = "", derived = TRUE, bias_ref = TRUE,
  bias_deriv = TRUE,
  api = "http://gift.uni-goettingen.de/api/extended/",
  GIFT_version = NULL){
  
  # 1. Controls ----
  # Arguments
  if(!is.character(trait_IDs)){
    stop("trait_IDs must be a character string indicating which trait you want
         to retrieve.")
  }
  
  if(!is.logical(bias_ref)){
    stop("bias_ref must be a logical.")
  }
  
  if(!is.logical(bias_deriv)){
    stop("bias_deriv must be a logical.")
  }
  
  if(!is.logical(derived)){
    stop("traits_derived must be a logical.")
  }

  if(!is.character(api)){
    stop("api must be a character string indicating which API to use.")
  }
  
  # Load traits_metadata to check if the provided IDs are available
  tmp <- GIFT::GIFT_traits_meta(api = api, GIFT_version = GIFT_version)
  if(!all(trait_IDs %in% tmp$Lvl3)){
    stop("trait_IDs must belong to the available list of traits. To see which
           traits are available, run 'traits_meta() and look at column
           'Lvl3'.") # TODO: Give back trait IDs that are not valid?
  }
  
  # 2. Function ----
  # Initiating list
  trait_list <- list()
  
  # for-loop
  for (i in 1:length(trait_IDs)){
    trait_list[[i]] <- jsonlite::read_json(
      paste0(api, "index", ifelse(is.null(GIFT_version), "", GIFT_version),
             ".php?query=traits_raw&traitid=",
             trait_IDs[i], "&deriv=", as.numeric(derived),
             "&biasref=", as.numeric(bias_ref),
             "&biasderiv=", as.numeric(bias_deriv)),
      simplifyVector = TRUE)
    trait_list[[i]]$trait_ID <- trait_IDs[i]
  }
  
  # Formatting trait_list as a data.frame
  trait_list <- dplyr::bind_rows(trait_list)

  # Add species names
  # species <- species_names()
  # trait_list <- dplyr::left_join(trait_list, species[, c("work_ID","species")],
  #                                by = "work_ID")
  
  # Reordering columns
  # trait_list <- trait_list[, c("trait_ID", "work_ID", "species", "trait_value",
  #                             "agreement", "references")]        
  
  return(trait_list)
}


