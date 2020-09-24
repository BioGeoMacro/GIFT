DB_get_traits = function(trait_IDs = c(""), agreement = 0.66, bias_ref = 0, bias_deriv = 0, restricted = TRUE){
  require(dplyr)# trait_IDs can either be "all" or a vector of trait_IDs
  
  # make error message when no trait given
  # decrease memory usage in php to reasonable amount: https://stackoverflow.com/questions/415801/allowed-memory-size-of-33554432-bytes-exhausted-tried-to-allocate-43148176-byte
  
  trait_list <- list()
  
  if(restricted){
    for (i in 1:length(trait_IDs)){
      trait_list[[i]] <- read_json(paste0("http://",credentials[[1]],":",credentials[[2]],"@gift.uni-goettingen.de/api/restricted/index.php?query=traits&traitid=",trait_IDs[i]), simplifyVector = TRUE)
    }
  } else {
    for (i in 1:length(trait_IDs)){
      trait_list[[i]] <- read_json(paste0("http://",credentials[[1]],":",credentials[[2]],"@gift.uni-goettingen.de/api/extended/index.php?query=traits&traitid=",trait_IDs[i]), simplifyVector = TRUE)
    }
  }
  
  trait_list <- bind_rows(trait_list)
  
  #trait_list <- traits_list[which(trait_list$agreement > agreement & trait_list$bias_by_reference <= bias_ref & trait_list$bias_by_derivation <= bias_deriv),]
  
  #trait_list = reshape2::dcast(data = trait_list, formula = work_ID ~ trait_ID, value.var = "trait_value")
  return(trait_list)
}
