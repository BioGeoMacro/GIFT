
GIFT_lists <- function(api = "http://gift.uni-goettingen.de/api/extended/", GIFT_version = NULL){
  
  # 1. Controls ----
  # Package dependencies
  require(dplyr)
  require(jsonlite)
  
  # Arguments

  # 2. Query ----
  tmp <- read_json(paste0(
    api,"index",
    ifelse(is.null(GIFT_version), "", GIFT_version),
    ".php?query=lists"),
    simplifyVector = TRUE)
  
  return(tmp)
}
