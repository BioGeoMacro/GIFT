DB_get_traits = function(trait_IDs = c(""), agreement = 0.66, bias_ref = FALSE, bias_deriv = FALSE, restricted = FALSE){
  require(dplyr)

  # make error message when no trait given
  # decrease memory usage in php to reasonable amount: https://stackoverflow.com/questions/415801/allowed-memory-size-of-33554432-bytes-exhausted-tried-to-allocate-43148176-byte
  
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
  
  
  #trait_list = reshape2::dcast(data = trait_list, formula = work_ID ~ trait_ID, value.var = "trait_value")
  return(trait_list)
}
