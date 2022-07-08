
GIFT_taxonomy <- function(GIFT_version = NULL, 
                          api = "http://gift.uni-goettingen.de/api/extended/"){
  # Packages
  require(jsonlite)

  # Query
  tmp <- read_json(paste0(
    api, "index",
    ifelse(is.null(GIFT_version), "", GIFT_version),
    ".php?query=taxonomy"),
    simplifyVector = TRUE)

  return(tmp)
}

