
GIFT_species <- function(api = "http://gift.uni-goettingen.de/api/extended/index.php"){
  require(jsonlite)
  require(dplyr)
  
  # Return the species names
  tmp <- read_json(paste0(
    api,
    "?query=species"),
    simplifyVector = TRUE)
  
  return(tmp)
}

