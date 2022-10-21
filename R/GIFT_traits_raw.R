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
#' @return A data.frame with 26 columns.
#'
#' @details Here is the detail of each column:
#' trait_derived_ID -
#' ref_ID - Identification number of the reference
#' orig_ID - Identification number of the species, as it came in the source
#' trait_ID - Identification number of the trait
#' trait_value - Value of the trait
#' derived - Is the trait value derived from another information (e.g.
#' phanerophytes are woody)
#' bias_deriv - Is the derivation potentially introducing a bias
#' name_ID - Identification number of the species before being resolved
#' cf_genus -
#' genus - Genus of the species
#' cf_species -
#' aff_species -
#' species_epithet - Epithet of the species
#' subtaxon - Sub-taxon name
#' author - Author who described the species
#' matched - Was the species name matched in the taxonomic backbone
#' epithetscore - Matching score for the epithet
#' overallscore - Overall matching score
#' resolved - Was the species name resolved in the taxonomic backbone
#' service - Taxonomic backbone used for taxonomic harmonization
#' work_ID - Identification number of the taxonomically harmonized species
#' bias_ref - Is the resource potentially introducing a bias
#' species - Species name
#' geo_entity_ref - Name of the region of the reference
#' ref_long - Full reference to cite
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
#' succulence_carnivory <- GIFT_traits_raw(trait_IDs = c("4.10.1", "4.16.1"))
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom dplyr bind_rows left_join mutate_at ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom utils setTxtProgressBar txtProgressBar
#' 
#' @export
#' 
GIFT_traits_raw <- function(
    trait_IDs = "", derived = TRUE, bias_ref = TRUE, bias_deriv = TRUE,
    api = "http://gift.uni-goettingen.de/api/extended/",
    GIFT_version = "latest"){
  
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
  
  if(length(api) != 1 || !is.character(api)){
    stop("api must be a character string indicating which API to use.")
  }
  
  # Load traits_metadata to check if the provided IDs are available
  tmp <- GIFT::GIFT_traits_meta(api = api, GIFT_version = GIFT_version)
  if(!all(trait_IDs %in% tmp$Lvl3)){
    stop("trait_IDs must belong to the available list of traits. To see which
           traits are available, run 'traits_meta() and look at column
           'Lvl3'.") # TODO: Give back trait IDs that are not valid?
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
  
  # Visible binding for global variable
  ref_ID <- trait_ID <- bias <- NULL
  
  # 2. Function ----
  
  # Getting available ref_ID/trait_ID combinations including derived traits
  ref_IDs <- jsonlite::read_json(
    paste0(api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
           ".php?query=reference_traits"), simplifyVector = TRUE)
  
  ref_IDs$priority <- NULL
  ref_IDs$ID <- NULL # Control for additional column in version 1.0
  
  if(!"bias" %in% names(ref_IDs)){
    ref_IDs$bias <- NA
  }
  
  # Convert to long table and get rid of duplicates
  ref_IDs <- unique(
    tidyr::pivot_longer(ref_IDs,
                        cols = grep("^trait", colnames(ref_IDs), value = TRUE),
                        names_to = NULL,
                        values_to = "trait_ID",
                        values_drop_na = TRUE))
  
  # Subset for desired traits
  ref_IDs <- ref_IDs[which(ref_IDs$trait_ID %in% trait_IDs), ]
  
  if (!bias_ref){
    ref_IDs <- ref_IDs[which(ref_IDs$bias == 0 | is.na(ref_IDs$bias)), ]
  }
  
  # Get rid of multiple entries introduced due to multiple bias values
  ref_IDs <- dplyr::group_by(ref_IDs, ref_ID, trait_ID)
  ref_IDs <- dplyr::summarise(ref_IDs, bias = min(bias))
  ref_IDs <- unique(dplyr::ungroup(ref_IDs))
  
  # Initiating list
  trait_list <- list()
  
  progress <- utils::txtProgressBar(min = 0, max = nrow(ref_IDs),
                                    initial = 0)
  
  for (i in seq_len(nrow(ref_IDs))){
    trait_list[[i]] <- jsonlite::read_json(
      paste0(api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
             ".php?query=traits_raw&traitid=",
             ref_IDs$trait_ID[i], "&deriv=", as.numeric(derived),
             "&biasderiv=", as.numeric(bias_deriv),
             "&refid=", as.numeric(ref_IDs$ref_ID[i])), simplifyVector = TRUE)
    
    if(length(trait_list[[i]]) > 0){
      trait_list[[i]]$trait_ID <- ref_IDs$trait_ID[i]
      trait_list[[i]]$bias_ref <- ref_IDs$bias[i]
    }
    utils::setTxtProgressBar(progress, i)
  }
  
  # Formatting trait_list as a data.frame
  trait_list <- dplyr::bind_rows(trait_list)
  
  trait_list <- dplyr::mutate_at(
    trait_list, c("trait_derived_ID","ref_ID","orig_ID", "derived",
                  "bias_deriv", "bias_ref", "name_ID"), as.numeric)
  # TODO expand making numeric for new names matched columns, but only if
  # names_matched == TRUE
  
  # join standardized species names
  species <- suppressMessages(GIFT_species(api = api, 
                                           GIFT_version = GIFT_version))
  trait_list$work_ID <- as.numeric(trait_list$work_ID)
  trait_list <- dplyr::left_join(trait_list, species, by = "work_ID")
  # TODO rename columns
  
  # TODO relocate bias ref
  
  # Join references
  references <- suppressMessages(GIFT_references(api = api, 
                                                 GIFT_version = GIFT_version))
  references <- unique(references[, c("ref_ID", "geo_entity_ref", "ref_long")])
  
  trait_list <- dplyr::left_join(trait_list, references, by = "ref_ID")
  
  # Add standardized species names
  
  return(trait_list)
}
