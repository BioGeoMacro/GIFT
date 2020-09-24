
species_names <- function(){
  require(jsonlite)
  require(dplyr)
  
  # Return the species names
  tmp <- read_json(paste0(
    "http://",credentials[[1]], ":",credentials[[2]],
    "@gift.uni-goettingen.de/api/extended/index.php?query=species"),
    simplifyVector = TRUE)
  
  return(tmp)
}

