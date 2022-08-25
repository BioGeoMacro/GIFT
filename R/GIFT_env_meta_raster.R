
GIFT_env_meta_raster <- function(api = "http://gift.uni-goettingen.de/api/extended/",
                                 GIFT_version = NULL){
  require(jsonlite)
  require(dplyr)
  # Return the raster environmental information
  tmp <- read_json(paste0(
    api, "index", ifelse(is.null(GIFT_version), "", GIFT_version),
    ".php?query=env_raster"), simplifyVector = TRUE)
 
  # Extract Citavi number from each list element
  tmp$citavi_ID <- gsub("^.*\\#", "", tmp$citavi_ID)
  tmp$citavi_ID <- substr(tmp$citavi_ID, 1, (nchar(tmp$citavi_ID)-1))
  
  refs <- read_json(paste0(
    api, "index", ifelse(is.null(GIFT_version), "", GIFT_version),
    ".php?query=references_citavi"), simplifyVector = TRUE)
  
  tmp <- left_join(tmp,refs, by=c("citavi_ID" = "citavi_seq_no"))
  tmp$citavi_ID <- NULL
  
  return(tmp)
}

