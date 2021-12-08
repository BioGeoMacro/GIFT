
GIFT_checklist_raw <- function(
  list_ID, taxonid = 1, namesmatched = TRUE,
  api = "http://gift.uni-goettingen.de/api/extended/"
  # potential arguments not implemented yet
  # endemic_ref = 0, endemic_list = 0, native = 0, naturalized = 0,
  # ref_ID
){
  
  # 1. Controls ----
  # Package dependencies
  require(dplyr)
  require(jsonlite)
  
  # Arguments
  if(length(list_ID) == 0){
    stop("Please provide the ID numbers of the checklists you want to load.")
  }
  
  if(!is.numeric(taxonid)){
    stop("'taxonid' is a numeric describing what taxonomic group you want.
         See help of the function.")
  }
  
  if(!is.logical(namesmatched)){
    stop("'namesmatched' must be a boolean indicating whether you want access to original name.")
  }
  
  # 2. Query ----
  list_raw <- c()
  for(i in seq_along(list_ID)){
    tmp <- read_json(paste0(
      "https://", credentials[[1]], ":",credentials[[2]],
      "@gift.uni-goettingen.de/api/extended/index.php?query=checklists&listid=",
      as.numeric(list_ID[i]), "&taxonid=", as.numeric(taxonid),
      "&namesmatched=", as.numeric(namesmatched)),
      simplifyVector = TRUE)
    
    list_raw <- bind_rows(list_raw, tmp)
  }

  return(list_raw)
}
