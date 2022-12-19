#' Phylogeny of the species in GIFT
#'
#' Retrieve a phylogeny of the plant species available in GIFT. 
#'
#' @param taxon_name Character string indicating the taxonomic group
#' of interest corresponding to the node labels in the phylogeny.
#' 
#' @param as_newick Boolean, whether you want the phylogeny to be in 
#' Newick tree format (TRUE) or in table format (FALSE). TRUE by default.
#' 
#' @param GIFT_version character string defining the version of the GIFT
#'  database to use. The function retrieves by default the latest stable 
#'  version.
#' 
#' @param api character string defining the API from which the data will be
#' retrieved.
#' 
#' @return
#' A data frame with 5 columns or a tree object.
#'
#' @details Here is what each column refers to:
#' 
#' \emph{taxon_label} - Name of the taxonomic group\cr
#' \emph{work_ID} - Standardized species name IDs for the species at the tips 
#' of the tree\cr
#' \emph{edge_length} - Edge length\cr
#' \emph{lft} - Left border of a given taxon in the Newick sequence\cr
#' \emph{rgt} - Right border of a given taxon in the Newick sequence\cr
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
#' ex <- GIFT_phylogeny(taxon_name = "Tracheophyta", as_newick = TRUE)
#' ex2 <- GIFT_phylogeny(taxon_name = "Tracheophyta", as_newick = FALSE)
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom dplyr bind_rows mutate_at
#' @importFrom phytools read.newick
#' 
#' @export

GIFT_phylogeny <- function(
    taxon_name = "Tracheophyta",
    as_newick = TRUE,
    api = "https://gift.uni-goettingen.de/api/extended/",
    GIFT_version = "latest"){
  
  check_api(api)
  GIFT_version <- check_gift_version(GIFT_version)
  
  if(GIFT_version != "beta"){
    stop("Phylogeny table only available for GIFT_version = 'beta'.")
  }
  
  if(length(taxon_name) != 1 || is.na(taxon_name) ||
     !is.character(taxon_name)){
    stop("'taxon_name' must be a character string corresponding to the node 
         labels in the phylogeny. Not all major taxonomic groups are 
         labelled.")
  }
  
  if(length(as_newick) != 1 || !is.logical(as_newick) || is.na(as_newick)){
    stop("'as_newick' must be a boolean stating whether you want to retrieve
         the phylogeny as a tree object.")
  }
  
  # Return the phylogeny table
  phylogeny <- list()
  for(i in seq_len(6)){
    phylogeny[[i]] <- jsonlite::read_json(paste0(
      api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
      ".php?query=phylogeny&taxon=", taxon_name, "&startat=",
      as.integer((i-1)*100000)), 
      simplifyVector = TRUE)
  }
  phylogeny <- dplyr::bind_rows(phylogeny)
  
  if (nrow(phylogeny) > 1){
    phylogeny <- dplyr::mutate_at(phylogeny, c("lft", "rgt","work_ID"), as.numeric)
  
  # Newick format
  if(as_newick){
    phylogeny[which(is.na(phylogeny$taxon_label)), "taxon_label"] <- ""
    phylogeny[which(is.na(phylogeny$edge_length)), "edge_length"] <- ""
    
    tax_newick <- c()
    tax_newick[phylogeny[, "lft"]] <- "("
    tax_newick[phylogeny[, "rgt"]] <- paste0(phylogeny[, "taxon_label"], ":",
                                             phylogeny[, "edge_length"], ")")
    
    # concatenate all vector
    tax_newick <- paste0(tax_newick, collapse = "")
    
    # replace )( with ,
    tax_newick <- gsub(")(", ",", x = tax_newick, fixed = TRUE)
    
    # ; at the very end
    tax_newick <- paste0(tax_newick, ";")
    
    tax_newick <- phytools::read.newick(text = tax_newick)
    
    return(tax_newick)
  } else{
    phylogeny <- dplyr::mutate_at(phylogeny, c("edge_length"), as.numeric)
    return(phylogeny)
  }
  } else {
    message("Taxon_name not found among the node labels of the phylogeny. 
            Returning NULL")
    return(NULL)
  }
}
