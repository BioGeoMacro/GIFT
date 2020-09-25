
GIFT_taxonomy <- function(){
  # Packages
  require(jsonlite)
  require(dplyr)

  # Query
  tmp <- read_json(paste0(
    "http://",credentials[[1]], ":",credentials[[2]],
    "@gift.uni-goettingen.de/api/extended/index.php?query=taxonomy"),
    simplifyVector = TRUE)

  return(tmp)
}

