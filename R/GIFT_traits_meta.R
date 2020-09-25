
GIFT_traits_meta <- function(restricted = FALSE){
  # 1. Controls ----
  # Package dependencies
  require(jsonlite)
  require(dplyr)

  if(!is.logical(restricted)){
    stop("'restricted' must be a boolean indicating whether you want access to
         restricted data.")
  }
  
  # 2. Query ----
  tmp <- read_json(paste0(
    "http://", credentials[[1]], ":",credentials[[2]],
    "@gift.uni-goettingen.de/api/extended/index.php?query=traits_meta&restricted=",
    as.numeric(restricted)),
    simplifyVector = TRUE)
  
  return(tmp)
}

