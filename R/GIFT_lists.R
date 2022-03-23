
GIFT_lists <- function(api = "http://gift.uni-goettingen.de/api/extended/index.php"){
  
  # 1. Controls ----
  # Package dependencies
  require(dplyr)
  require(jsonlite)
  
  # Arguments

  # 2. Query ----
  tmp <- read_json(paste0(
    api,
    "?query=lists"),
    simplifyVector = TRUE)
  
  return(tmp)
}
