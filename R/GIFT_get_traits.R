
GIFT_get_traits <- function(trait_IDs = "", agreement = 0.66, bias_ref = FALSE,
                            bias_deriv = FALSE, restricted = FALSE){
  
  # 1. Controls ----
  # Package dependencies
  require(dplyr)
  require(jsonlite)
  
  # Arguments
  if(!is.character(trait_IDs)){
    stop("trait_IDs must be a character string indicating which trait you want to retrieve.")
  }
  
  # Load metadata for traits to check if the provided IDs are available
  tmp <- traits_meta(restricted = restricted)
  if(!all(trait_IDs %in% tmp$Lvl3)){
    stop("trait_IDs must belong to the available list of traits. To see which
           traits are available, run 'traits_meta() and look at column
           'Lvl3'.")
  }
  
  
  if(!is.numeric(agreement)){
    stop("'agreement' must be a numeric.")
  }
  
  if(!is.logical(bias_ref)){
    stop("'bias_ref' must be a boolean.")
  }
  
  if(!is.logical(bias_deriv)){
    stop("'bias_deriv' must be a boolean.")
  }
  
  if(!is.logical(restricted)){
    stop("'restricted' must be a boolean indicating whether you want access to restricted data.")
  }
  
  # 2. Function ----
  # Initiating list
  trait_list <- list()
  # query (two different, according 'restricted' value)
  if(restricted){
    for (i in 1:length(trait_IDs)){
      trait_list[[i]] <- read_json(paste0("http://",credentials[[1]],":",credentials[[2]],
                                          "@gift.uni-goettingen.de/api/restricted/index.php?query=traits&traitid=",
                                          trait_IDs[i], "&biasref=", as.numeric(bias_ref), "&biasderiv=", as.numeric(bias_deriv)),
                                   simplifyVector = TRUE)
      trait_list[[i]]$trait_ID <- trait_IDs[i]
    }
  } else {
    for (i in 1:length(trait_IDs)){
      trait_list[[i]] <- read_json(paste0("http://",credentials[[1]],":",credentials[[2]],
                                          "@gift.uni-goettingen.de/api/extended/index.php?query=traits&traitid=",
                                          trait_IDs[i], "&biasref=", as.numeric(bias_ref), "&biasderiv=", as.numeric(bias_deriv)),
                                   simplifyVector = TRUE)
      trait_list[[i]]$trait_ID <- trait_IDs[i]
    }
  }
  
  # Formating trait_list as a data.frame
  trait_list <- bind_rows(trait_list)
  trait_list <- trait_list[which(trait_list$agreement >= agreement), ]
  
  # add species names
  species <- species_names()
  trait_list <- left_join(trait_list, species[, c("work_ID","species")],
                          by = "work_ID")
  
  # reordering columns
  trait_list <- trait_list[, c("trait_ID", "work_ID", "species", "trait_value",
                               "agreement", "references")]        
  
  return(trait_list)
}
