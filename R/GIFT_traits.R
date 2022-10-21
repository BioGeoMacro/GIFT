#' GIFT_traits
#'
#' Retrieve specific trait values.
#'
#' @param trait_IDs a character string indicating which trait you want to
#' retrieve. Traits must belong to the available list of traits.
#' 
#' @param agreement Percentage of resources that agree on an aggregated trait
#' value, entries below this threshold will be omitted. 
#' 
#' @param bias_ref When FALSE, exclude entries that are only based on a
#' resource that potentially introduces a bias (e.g. a resource only including
#' trees).
#' 
#' @param bias_deriv When FALSE, exclude entries that are only based on a
#' derivation that potentially introduces a bias (e.g. all phanerophytes being
#' woody but some life forms being ambiguous).
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
#' @details Here is the detail of each column:
#' trait_ID - Identification number of the trait
#' work_ID - Identification number of the taxonomically harmonized species
#' species - Species name
#' trait_value - Value of the trait
#' agreement - Agreement score between the different sources for that trait
#' value
#' references - ref_ID from which we got the trait information
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
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom dplyr bind_rows left_join relocate mutate_at
#' @importFrom tidyr pivot_wider
#' @importFrom utils setTxtProgressBar txtProgressBar
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
  
  if(length(api) != 1 || !is.character(api)){
    stop("api must be a character string indicating which API to use.")
  }
  
  # Load traits_metadata to check if the provided IDs are available
  tmp <- GIFT_traits_meta(api = api, GIFT_version = GIFT_version)
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
  
  if(GIFT_version == "1.0" & (bias_ref == FALSE | bias_deriv == FALSE)){
    message(
      "Warning: In GIFT version 1.0 it is not yet possible to filter trait 
      values for biases. bias_ref and bias_deriv arguments are ignored.")
  }
  
  
  # 2. Function ----

  # Initiating list
  trait_list <- list()
  
  n <- ceiling(tmp$count[which(tmp$Lvl3 %in% trait_IDs)]/10000)
  progress <- utils::txtProgressBar(min = 0, max = sum(n)+1, initial = 0) 
  
  # Get species names
  species <- suppressMessages(GIFT_species(GIFT_version = GIFT_version, 
                                           api = api))
  
  count <- 1
  utils::setTxtProgressBar(progress, count)
  
  for(i in seq_along(trait_IDs)){
    trait_list_i <- list()
    
    for (k in seq_len(n[i])){
      trait_list_i[[k]] <- jsonlite::read_json(
        paste0(api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
               ".php?query=traits&traitid=",
               trait_IDs[i], "&biasref=", as.numeric(bias_ref),
               "&biasderiv=", as.numeric(bias_deriv), 
               "&startat=", as.integer((k-1)*10000)),
        simplifyVector = TRUE)
      count <- count + 1
      utils::setTxtProgressBar(progress, count)
    }
    trait_list[[i]] <- dplyr::bind_rows(trait_list_i)
    trait_list[[i]]$trait_ID <- trait_IDs[i]
    
  }
  message("\n")
  
  # Formatting trait_list as a data.frame
  trait_list <- dplyr::bind_rows(trait_list)
  trait_list <- trait_list[which(trait_list$agreement >= agreement |
                                   is.na(trait_list$agreement)), ]
  
  # Make certain columns numeric
  trait_list <- dplyr::mutate_at(trait_list, c("work_ID", "agreement"),
                                 as.numeric)
  
  # Add species names
  trait_list <- dplyr::left_join(trait_list,
                                 species[, c("work_ID", "work_species", 
                                             "work_author")],
                                 by = "work_ID")
  
  # Round agreement score
  trait_list$agreement <- round(trait_list$agreement, 3)
  
  # Reordering columns
  trait_list <- trait_list[, c("work_ID", "work_species", "work_author",
                               "trait_ID", "trait_value", "agreement",
                               "references")]
  
  # Wider format
  trait_list <- tidyr::pivot_wider(
    trait_list, names_from = "trait_ID",
    values_from = c("trait_value", "agreement", "references"))
  
  # Make data.frame
  trait_list <- as.data.frame(trait_list)
  
  return(trait_list)
}
