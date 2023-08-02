#' Phylogeny of the species in GIFT
#'
#' Retrieve a phylogeny of the plant species available in GIFT. The phylogeny
#' table is not available for GIFT_version 1.0, 2.0, 2.1 and 2.2. 
#'
#' @param clade Character string indicating the taxonomic group
#' of interest corresponding to the node labels in the phylogeny.
#' 
#' @param as_tree Logical, whether you want the phylogeny to be returned as a 
#' phylogenetic tree  (`TRUE`) or in a table (`FALSE`). `TRUE` by default.
#' 
#' @param return_work_ID Logical, whether you want to retrieve the species'
#' names or their identification number (work_ID) in the GIFT database.
#' `FALSE` by default.
#' 
#' @param work_ID_subset A vector of work_ID to prune the phylogenetic tree.
#' `NULL` by default.
#' 
#' @template GIFT_version_api
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
#' @seealso [GIFT::GIFT_checklists()]
#'
#' @examples
#' \donttest{
#' ex <- GIFT_phylogeny(clade = "Tracheophyta", as_tree = FALSE,
#' GIFT_version = "beta")
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom dplyr bind_rows mutate_at
#' @importFrom phytools read.newick
#' @importFrom ape keep.tip
#' 
#' @export

GIFT_phylogeny <- function(
    clade = "Tracheophyta", as_tree = TRUE,
    return_work_ID = FALSE,
    work_ID_subset = NULL,
    api = "https://gift.uni-goettingen.de/api/extended/",
    GIFT_version = "latest"){
  
  check_api(api)
  GIFT_version <- check_gift_version(GIFT_version)
  
  if(GIFT_version %in% c("1.0", "2.0", "2.1", "2.2")){
    stop("The phylogeny table is not available for GIFT_version 1.0, 2.0,
         2.1 and 2.2.")
  }
  
  if(length(clade) != 1 || is.na(clade) ||
     !is.character(clade)){
    stop("'clade' must be a character string corresponding to the node 
         labels in the phylogeny. Not all major taxonomic groups are 
         labelled.")
  }
  
  if(length(as_tree) != 1 || !is.logical(as_tree) || is.na(as_tree)){
    stop("'as_tree' must be a logical stating whether you want to retrieve
         the phylogeny as a tree object.")
  }
  
  if(length(return_work_ID) != 1 || !is.logical(return_work_ID) ||
     is.na(return_work_ID)){
    stop("'return_work_ID' must be a logical stating whether you want to
    get the species names or their work_ID as tip labels.")
  }
  
  if(!is.null(work_ID_subset)){
    if(!is.numeric(work_ID_subset)){
      stop("work_ID_subset must be a numeric vector listing the work_ID you
           want to use to prune the phylogenetic tree.")
    }
  }
  
  # Return the phylogeny table
  phylogeny <- list()
  for(i in seq_len(6)){
    phylogeny[[i]] <- jsonlite::read_json(paste0(
      api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
      ".php?query=phylogeny&taxon=", clade, "&startat=",
      as.integer((i-1)*100000)), 
      simplifyVector = TRUE)
  }
  phylogeny <- dplyr::bind_rows(phylogeny)
  
  if (nrow(phylogeny) > 1){
    phylogeny <- dplyr::mutate_at(phylogeny, c("lft", "rgt","work_ID"),
                                  as.numeric)
    
    # Newick format
    if(as_tree){
      # Starting the Newick string in the right place
      start_phy <- min(phylogeny$lft, na.rm = TRUE)
      phylogeny$lft <- phylogeny$lft - start_phy + 1
      phylogeny$rgt <- phylogeny$rgt - start_phy + 1
      
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
      
      tree <- phytools::read.newick(text = tax_newick)
      
      if(return_work_ID){
        message("Querying the species table from GIFT.")
        gift_sp <- suppressMessages(
          GIFT_species(api = api, GIFT_version = GIFT_version))
        message("Replacing species names with work_ID.")
        
        tree$tip.label <- gsub(pattern = "_", replacement = " ",
                               x = tree$tip.label, fixed = TRUE)
        tree$tip.label <- gift_sp$work_ID[match(tree$tip.label,
                                                gift_sp$work_species)]
        
        if(!is.null(work_ID_subset)){
          tree <- ape::keep.tip(phy = tree, tip = work_ID_subset)
        }
      }
      
      return(tree)
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
