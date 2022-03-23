
GIFT_references <- function(api = "http://gift.uni-goettingen.de/api/extended/index.php"){
  # 1. Controls ----
  # Package dependencies
  require(jsonlite)
  require(dplyr)

  # 2. Query ----
  tmp <- read_json(paste0(
    api,
    "?query=references"),
    simplifyVector = TRUE)
  
  return(tmp)
}

