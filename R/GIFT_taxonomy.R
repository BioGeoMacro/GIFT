
GIFT_taxonomy <- function(api = "http://gift.uni-goettingen.de/api/extended/index.php"){
  # Packages
  require(jsonlite)
  require(dplyr)

  # Query
  tmp <- read_json(paste0(
    api,
    "?query=taxonomy"),
    simplifyVector = TRUE)

  return(tmp)
}

