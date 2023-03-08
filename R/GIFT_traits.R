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
#' @template GIFT_version_api
#' 
#' @return
#' A long-format data frame with 6 columns: `trait_ID`, `work_ID`, `species`,
#' `trait_value`, `agreement` and `references`.
#'
#' @details Here is the detail of each column:
#' 
#' \emph{trait_ID} - Identification number of the trait\cr
#' \emph{work_ID} - Identification number of the taxonomically harmonized
#'  species\cr
#' \emph{species} - Species name\cr
#' \emph{trait_value} - Value of the trait\cr
#' \emph{agreement} - Agreement score between the different sources for that
#' trait value, only for categorical traits\cr
#' \emph{cv} - Coefficient of variation for the different sources for that
#' trait value, only for numeric traits\cr
#' \emph{n} - Number of sources leading to the trait value\cr
#' \emph{references} - ref_ID from which we got the trait information
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
    api = "https://gift.uni-goettingen.de/api/extended/",
    GIFT_version = "latest"){
  
  # 1. Controls ----
  # Arguments
  check_trait_IDs(trait_IDs)
  
  if(!is.numeric(agreement)){
    stop("agreement must be a numeric between 0 and 1 indicating .")
  } else if(agreement > 1 | agreement < 0){
    stop("agreement must be a numeric between 0 and 1 indicating .")
  }
  
  check_bias_ref(bias_ref)
  check_bias_deriv(bias_deriv)
  check_api(api)
  GIFT_version <- suppressMessages(check_gift_version_simple(GIFT_version))
  
  # Load traits_metadata to check if the provided IDs are available
  tmp <- GIFT_traits_meta(api = api, GIFT_version = GIFT_version)
  if(!all(trait_IDs %in% tmp$Lvl3)){
    stop("trait_IDs must belong to the available list of traits. To see which
           traits are available, run 'traits_meta() and look at column
           'Lvl3'.")
  }
  
  if(GIFT_version == "1.0" & (bias_ref == FALSE | bias_deriv == FALSE)){
    message(
      "Warning: In GIFT version 1.0 it is not yet possible to filter trait 
      values for biases. bias_ref and bias_deriv arguments are ignored.")
  }
  
  
  # 2. Function ----
  # Get species names
  message("Retrieving species' names.\n")
  
  species <- suppressMessages(GIFT_species(GIFT_version = GIFT_version, 
                                           api = api))
  
  message(paste0("Preparing the download of trait data for ",
                 length(unique(trait_IDs)),
                 " trait(s)).\n"))
  
  # Initiating list
  trait_list <- list()
  
  n <- ceiling(tmp$count[which(tmp$Lvl3 %in% trait_IDs)]/10000)
  
  progress <- utils::txtProgressBar(min = 0, max = sum(n)+1, initial = 0) 
  
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
  
  # trait_list <- purrr::map(
  #   .x = seq_along(trait_IDs),
  #   .f = function(x){
  #     trait_list_x <- list()
  #     
  #     trait_list_x <- purrr::map(
  #       .x = seq_len(n[x]),
  #       .f = function(y){
  #         jsonlite::read_json(
  #           paste0(api, "index", ifelse(GIFT_version == "beta", "",
  #                                       GIFT_version),
  #                  ".php?query=traits&traitid=",
  #                  trait_IDs[x], "&biasref=", as.numeric(bias_ref),
  #                  "&biasderiv=", as.numeric(bias_deriv),
  #                  "&startat=", as.integer((y-1)*10000)),
  #           simplifyVector = TRUE)
  #       },
  #       .progress = paste0("Retrieving trait number ", x))
  #     
  #     trait_list_x <- dplyr::bind_rows(trait_list_x)
  #     trait_list_x$trait_ID <- trait_IDs[x]
  #     return(trait_list_x)
  #   },
  #   .progress = TRUE)
  
  message("\n")
  
  # Formatting trait_list as a data.frame
  trait_list <- dplyr::bind_rows(trait_list)
  trait_list <- trait_list[which(trait_list$agreement >= agreement |
                                   is.na(trait_list$agreement)), ]
  
  # Make certain columns numeric
  trait_list <- dplyr::mutate_at(
    trait_list, c("work_ID", "agreement", "coeff_var", "n"), as.numeric)
  
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
                               "coeff_var", "n", "references")]
  
  # Renaming column
  colnames(trait_list)[colnames(trait_list) == "coeff_var"] <- "cv"
  
  # Wider format
  trait_list <- tidyr::pivot_wider(
    trait_list, names_from = "trait_ID",
    values_from = c("trait_value", "agreement", "cv", "n", "references"))
  
  # Make data.frame
  trait_list <- as.data.frame(trait_list)
  
  # Remove agreement columns for continuous traits and coeff_var for
  # categorical traits
  numeric_traits <- tmp[which(tmp$type == "numeric"), "Lvl3"]
  numeric_columns <-
    paste("agreement", numeric_traits, sep = "_")[
      paste("agreement", numeric_traits, sep = "_") %in% colnames(trait_list)]
  
  categorical_traits <- tmp[which(tmp$type != "numeric"), "Lvl3"]
  categorical_columns <-
    paste("cv", categorical_traits, sep = "_")[
      paste("cv", categorical_traits, sep = "_") %in% colnames(trait_list)]
  
  trait_list <- trait_list[, !(colnames(trait_list) %in% numeric_columns)]
  trait_list <- trait_list[, !(colnames(trait_list) %in% categorical_columns)]
  
  return(trait_list)
}
