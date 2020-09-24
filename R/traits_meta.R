
traits_meta <- function(){
  require(jsonlite)
  require(dplyr)

  # Return the miscellaneous environmental information as a data frame
  tmp <- read_json(paste0(
    "http://", credentials[[1]], ":",credentials[[2]],
    "@gift.uni-goettingen.de/api/extended/index.php?query=traits_meta"),
    simplifyVector = TRUE)
  
  return(tmp)
}

