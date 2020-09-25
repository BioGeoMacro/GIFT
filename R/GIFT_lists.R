
GIFT_lists <- function(restricted = FALSE){
  
  # 1. Controls ----
  # Package dependencies
  require(dplyr)
  require(jsonlite)
  
  # Arguments
  if(!is.logical(restricted)){
    stop("'restricted' must be a boolean indicating whether you want access to restricted data.")
  }
  
  # 2. Query ----
  tmp <- read_json(paste0(
    "http://", credentials[[1]], ":",credentials[[2]],
    "@gift.uni-goettingen.de/api/extended/index.php?query=lists&restricted=",
    as.numeric(restricted)),
    simplifyVector = TRUE)
  
  return(tmp)
}
