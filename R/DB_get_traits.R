DB_get_traits = function(trait_IDs = c(""), agreement = 0.66, bias_ref = FALSE, bias_deriv = FALSE, restricted = FALSE){
  require(dplyr)

  # make error message when no trait given
  trait_list <- list()
  
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
  
  trait_list <- bind_rows(trait_list)
  
  trait_list <- trait_list[which(trait_list$agreement >= agreement),]
  
  ### match species names
  species <- species_names()
  trait_list <- left_join(trait_list, species[,c("work_ID","species")], by="work_ID")
  
  trait_list <- trait_list[,c("trait_ID","work_ID","species","trait_value","agreement","references")]        
  
  return(trait_list)
}
