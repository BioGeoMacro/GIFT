#' GIFT_traits_tax
#'
#' Retrieve specific trait values at a high taxonomic level.
#'
#' @param trait_IDs a character string indicating which trait you want to
#' retrieve. Traits must belong to the available list of traits.
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
#' A long-format data frame with 7 columns: `taxon_ID`, `taxon_name`,
#' `trait_value`, `agreement`, `references` and `negative`.
#'
#' @details Here is the detail of each column:
#' 
#' \emph{taxon_ID} - Identification number of the taxon\cr
#' \emph{taxon_name} - Name of the taxon\cr
#' \emph{agreement} - Agreement score between the different sources for that
#' trait value, only for categorical traits\cr
#' \emph{references} - Source of the trait values (ref_ID)\cr
#' \emph{negative} - Value of the trait\cr
#' 
#' and then one column per trait with the respective trait values
#' 
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
#' ex <- GIFT_traits_tax(trait_IDs = c("1.1.1", "1.2.1", "1.4.1",
#' bias_ref = FALSE, bias_deriv = FALSE)
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom dplyr bind_rows left_join mutate mutate_at ungroup group_by
#' @importFrom dplyr row_number
#' @importFrom tidyr pivot_wider
#' @importFrom utils setTxtProgressBar txtProgressBar
#' 
#' @export
#' 
GIFT_traits_tax <- function(
    trait_IDs = "", bias_ref = TRUE, bias_deriv = TRUE,
    api = "https://gift.uni-goettingen.de/api/extended/",
    GIFT_version = "latest"){
  
  # 1. Controls ----
  # Arguments
  check_trait_IDs(trait_IDs)
  check_bias_ref(bias_ref)
  check_bias_deriv(bias_deriv)
  check_api(api)
  GIFT_version <- check_gift_version_simple(GIFT_version)
  
  # Load traits_metadata to check if the provided IDs are available
  tmp <- suppressMessages(GIFT_traits_meta(api = api,
                                           GIFT_version = GIFT_version))
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
  
  message(paste0("Preparing the download of trait data (",
                 length(unique(trait_IDs)),
                 " traits asked).\n"))
  
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
               ".php?query=traits_tax&traitid=",
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
  # trait_list <- trait_list[which(trait_list$agreement >= agreement |
  #                                  is.na(trait_list$agreement)), ]
  
  # Make certain columns numeric
  trait_list <- dplyr::mutate_at(
    trait_list, c("taxon_ID", "agreement", "negative"), as.numeric)
  
  # Add species names
  # trait_list <- dplyr::left_join(trait_list,
  #                                species[, c("work_ID", "work_species", 
  #                                            "work_author")],
  #                                by = "work_ID")
  
  # Round agreement score
  trait_list$agreement <- round(trait_list$agreement, 3)
  
  # Reordering columns
  trait_list <- trait_list[, c("trait_ID", "taxon_ID", "taxon_name",
                               "trait_value", "agreement", "references",
                               "negative")]
  
  # Wider format
  # trait_list <- tidyr::pivot_wider(
  #   trait_list, names_from = "trait_ID",
  #   values_from = c("trait_value", "agreement", "negative", "references"))
  
  trait_list <- dplyr::group_by(trait_list, trait_ID)
  trait_list <- dplyr::mutate(trait_list, row = dplyr::row_number())
  trait_list <- tidyr::pivot_wider(trait_list,
                                   names_from = "trait_ID",
                                   values_from = "trait_value")
  trait_list <- dplyr::select(trait_list, -row)
  trait_list <- dplyr::ungroup(trait_list)
  
  # Make data.frame
  trait_list <- as.data.frame(trait_list)
  
  # Message if some traits are not available at the family level
  if(length(trait_IDs[trait_IDs %in% colnames(trait_list)]) !=
     length(trait_IDs)){
    message(paste0(
      "The following traits were not available at the taxonomic level: ",
      trait_IDs[!(trait_IDs %in% colnames(trait_list))]))
  }
  
  return(trait_list)
}
