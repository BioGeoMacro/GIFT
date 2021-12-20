#' GIFT checklists
#'
#' Retrieve GIFT checklists that fulfill specific criteria.
#'
#' @param taxon_name Character string corresponding to the taxonomic group
#' of interest.
#' 
#' @param ref_included Character, options are 'all', 'native',
#' 'native and naturalized', 'native and historically introduced',
#' 'endangered', 'endemic', 'naturalized', 'other subset'
#' 
#' @param type_ref Character, options are 'Account', 'Catalogue', 'Checklist',
#' 'Flora', 'Herbarium collection', 'Key', 'Red list', 'Report',
#' 'Species Database', 'Survey'
#' 
#' @param entity_class Character, options are 'Island', 'Island/Mainland',
#' 'Mainland', 'Island Group', 'Island Part'
#' 
#' @param native_indicated Boolean.
#' 
#' @param natural_indicated Boolean.
#' 
#' @param end_ref Boolean.
#' 
#' @param end_list Boolean.
#' 
#' @param suit_geo Boolean.
#' 
#' @param complete_taxon Boolean, default TRUE.
#'
#' 
#' @return
#' data frame with list of entity_ID and the different criteria.
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
#' ex <- GIFT_checklist_conditional(taxon_name = "Embryophyta", 
#' ref_included = c("all", "native", "native and naturalized",
#' "native and historically introduced", "endangered",
#' "endemic", "naturalized", "other subset")[1:4],
#' type_ref = c("Account", "Catalogue", "Checklist","Flora",
#' "Herbarium collection", "Key", "Red list", "Report", "Species Database",
#'  "Survey"),
#'  entity_class = c("Island", "Island/Mainland", "Mainland", "Island Group",
#'  "Island Part"),
#'  native_indicated = FALSE, natural_indicated = FALSE, end_ref = FALSE,
#'  end_list = FALSE, suit_geo = TRUE, complete_taxon = TRUE)
#' 
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom dplyr left_join mutate group_by ungroup filter select
#' 
#' @export

GIFT_checklist_conditional <- function(
  taxon_name = "Tracheophyta",
  ref_included = c("all", "native", "native and naturalized",
                   "native and historically introduced", "endangered",
                   "endemic", "naturalized", "other subset")[1:4],
  type_ref = c("Account", "Catalogue", "Checklist","Flora",
               "Herbarium collection", "Key", "Red list", "Report",
               "Species Database", "Survey"),
  entity_class = c("Island", "Island/Mainland", "Mainland", "Island Group",
                   "Island Part"),
  native_indicated = FALSE, natural_indicated = FALSE, end_ref = FALSE,
  end_list = FALSE, suit_geo = FALSE,
  complete_taxon = TRUE,
  api = "http://gift.uni-goettingen.de/api/extended/index.php"){
  ## below are arguments from db_get_checklist_conditional()
  # entity_class = c("Island","Island/Mainland","Mainland","Island Group","Island Part"), 
  # native_indicated = F, natural_indicated = F, end_ref = F, end_list = F, 
  # type_ref = 1:11,
  # ref_included = c(1,2,3,4),
  # tax_group = 1,
  # suit_geo = T, 
  # exclude_restricted = T, # not available anymore
  # include_names_unique = F, return_query_only = F
  # complete_taxonomy = TRUE # renamed complete_taxon
  
  # 1. Controls ----
  # Arguments

  # 2. Query ----
  # Lists query
  list_set <- jsonlite::read_json(paste0(api, "?query=lists"),
                                  simplifyVector = TRUE)
  
  # Taxonomy query
  taxonomy <- jsonlite::read_json(paste0(api, "?query=taxonomy"),
                                  simplifyVector = TRUE)
  
  # Define tax_group
  tax_group <- taxonomy[which(taxonomy$taxon_name == taxon_name), "taxon_ID"]
  
  # Numeric columns
  taxonomy[c("lft", "rgt")] <-
    sapply(taxonomy[c("lft", "rgt")], as.numeric)
  
  left_border <- taxonomy[which(taxonomy$taxon_ID == tax_group), "lft"]
  right_border <- taxonomy[which(taxonomy$taxon_ID == tax_group), "rgt"]
  
  included_taxa_below <- taxonomy[which(taxonomy$lft >= left_border &
                                          taxonomy$rgt <= right_border),
                                  "taxon_ID"]
  included_taxa_above <- taxonomy[which(taxonomy$lft < left_border &
                                          taxonomy$rgt > right_border),
                                  "taxon_ID"]
  included_taxa <- c(included_taxa_below, included_taxa_above)
  
  # Subset of lists based on the taxonomy
  list_set <- list_set[which(list_set$taxon_ID %in% included_taxa), ]
  
  # Subset of lists based on floristic coverage (alien, endemics, etc)
  list_set <- list_set[which(list_set$subset %in% ref_included), ]
  
  # Subset of lists based on reference type (checklist, flora, etc)
  list_set <- list_set[which(list_set$type %in% type_ref), ]
  
  # Subset of lists based on entity class (island, mainland, etc)
  list_set <- list_set[which(list_set$entity_class %in% entity_class), ]
  
  # Subset of lists based on whether nativeness is indicated
  if(native_indicated){
    list_set <- list_set[which(list_set$native_indicated == "1"), ] # see TODO
  }
  
  # Subset of lists based on whether naturalization is indicated
  if(natural_indicated){
    list_set <- list_set[which(list_set$natural_indicated == "1"), ] # see TODO
  }
  
  # Subset of lists based on whether endemic ref is indicated
  if(end_ref){
    list_set <- list_set[which(list_set$end_ref == "1"), ] # see TODO
  }
  
  # Subset of lists based on whether endemic list is indicated
  if(end_list){
    list_set <- list_set[which(list_set$end_list == "1"), ]
  }
  
  # Only regions covered by lists
  if(suit_geo){
    list_set <- list_set[which(list_set$suit_geo == "1"), ]
  }
  
  # Add full taxonomic names
  list_set <- dplyr::left_join(list_set, taxonomy, by = "taxon_ID")
  
  # Whether regions cover entirely or not the required taxonomic group
  if(complete_taxon){
    range_taxgroup <- taxonomy[which(taxonomy$taxon_ID == tax_group), "rgt"] -
      taxonomy[which(taxonomy$taxon_ID == tax_group), "lft"]
    
    # With the pipe
    # list_set <- list_set %>%
    #   dplyr::mutate(range_covered = rgt - lft) %>%
    #   dplyr::group_by(entity_ID) %>%
    #   dplyr::mutate(range_covered_max = max(range_covered)) %>%
    #   dplyr::ungroup(entity_ID) %>%
    #   dplyr::filter(range_covered_max >= range_taxgroup) %>%
    #   as.data.frame()
    
    # Without the pipe
    list_set <- dplyr::mutate(list_set, range_covered = rgt - lft)
    list_set <- dplyr::group_by(list_set, entity_ID)
    list_set <- dplyr::mutate(list_set, range_covered_max = max(range_covered))
    list_set <- dplyr::ungroup(list_set)
    list_set <- dplyr::filter(list_set, range_covered_max >= range_taxgroup)
    list_set <- as.data.frame(list_set)
    
    # Remove unnecessary columns
    list_set <- dplyr::select(list_set, -range_covered, -range_covered_max)
    
    # TO DO: allow for cases when you can "sum" for taxonomic subsets
    # TO DO account for cases in which we don't have a list for a taxonomic group but for all subgroups included
  }
  
  # Remove unnecessary columns from join with taxonomy
  list_set <- dplyr::select(list_set, -taxon_author, -taxon_lvl, -lft, -rgt)
  
  # Output
  return(list_set)
}
