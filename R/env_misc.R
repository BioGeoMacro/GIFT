
env_misc <- function(){
  require(jsonlite)
  require(dplyr)

  # Return the miscellaneous environmental information as a data frame
  tmp <- read_json(paste0(
    "http://",credentials[[1]], ":",credentials[[2]],
    "@gift.uni-goettingen.de/api/extended/index.php?query=env_misc"),
    simplifyVector = TRUE)
  
  # Extract Citavi number from each list element
  tmp$citavi_ID <- gsub("^.*\\#", "", tmp$citavi_ID)
  tmp$citavi_ID <- substr(tmp$citavi_ID, 1, (nchar(tmp$citavi_ID)-1))
  
  # Merging complete reference names
  refs <- read_json(paste0(
    "http://",credentials[[1]], ":",credentials[[2]],
    "@gift.uni-goettingen.de/api/extended/index.php?query=references_citavi"),
    simplifyVector = TRUE)
  
  tmp <- left_join(tmp,refs, by = c("citavi_ID" = "citavi_seq_no"))
  tmp$citavi_ID <- NULL
  
  return(tmp)
}

