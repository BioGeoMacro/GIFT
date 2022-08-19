
GIFT_species <- function(api = "http://gift.uni-goettingen.de/api/extended/",
                         GIFT_version = NULL){
  require(jsonlite)
  require(dplyr)
  
  # Return the species names
  tmp <- read_json(paste0(
    api, "index", ifelse(is.null(GIFT_version), "", GIFT_version),
    ".php?query=species"),
    simplifyVector = TRUE)
  
  return(tmp)
}

