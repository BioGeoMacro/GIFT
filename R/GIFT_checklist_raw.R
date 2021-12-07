
GIFT_checklist_raw <- function(
  list_ID, taxonid = 1, include_names_unique = FALSE,
  api = "http://gift.uni-goettingen.de/api/extended/",
  # potential arguments not implemented yet
  # endemic_ref = 0, endemic_list = 0, native = 0, naturalized = 0,
  # ref_ID
){
  
  # 1. Controls ----
  # Package dependencies
  require(dplyr)
  require(jsonlite)
  
  # Arguments
  if(!is.logical(include_names_unique)){
    stop("'include_names_unique' must be a boolean indicating whether you want access to original name.")
  }
  
  # 2. Query ----
  tmp <- read_json(paste0(
    "https://", credentials[[1]], ":",credentials[[2]],
    "@gift.uni-goettingen.de/api/extended/index.php?query=checklists&listid=",
    as.numeric(list_ID), "&taxonid=", as.numeric(taxonid),
    "&include_names_unique=", include_names_unique),
    simplifyVector = TRUE)
  
  return(tmp)
  
  # https://gift.uni-goettingen.de/api/extended/?query=checklists&listid=100&taxonid=5&namesmatched=1
  
}