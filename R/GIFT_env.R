
GIFT_env <- function(
  entity_ID = NULL,
  # envvar = "area", layername = "",
  miscellaneous = "area", rasterlayer = "",
  sumstat = "mean",
  api = "http://gift.uni-goettingen.de/api/extended/index.php"){
  
  # one argument only
  # check => whether the (list of) argument(s)
  
  # sumstat argument valid for raster only (naturlich)
  # it can be formated as a single value, a vector or a list
  # in later case => each list element correspond to the position of the vector or raster names
  # control for that argument: length of the vector or of the list has to be
  # equal to the length of the raster layers
  
  # default value => mean repeated to length of layername
  
  # 1. Controls ----
  # Package dependencies
  require(dplyr)
  require(jsonlite)
  
  # Arguments
  
  # check if sumstats are available
  
  # length of sumstat has to equal length rasterlayer
  # if 1 value, repeat it to the length of rasterlayer
  # if(length(sumstat) > 1 & length(sumstat) != length(rasterlayer)){
  #   stop("Please provide the same number of summary statistics as you have
  #        raster layers.")
  # }
  
  # 2. Query ----
  ## 2.1. Miscellaneous data ----
  if(is.null(miscellaneous) | length(miscellaneous) == 0){
    tmp_misc <- read_json(paste0(
      api, "?query=geoentities_env_misc"),
      simplifyVector = TRUE)
  } else{
    tmp_misc <- read_json(paste0(
      api, "?query=geoentities_env_misc&envvar=",
      paste(miscellaneous, collapse = ",")),
      simplifyVector = TRUE)
  }
  
  ## 2.2. Raster data ----
  # Preparing sumstat => list with sumstats repeated
  if(is.vector(sumstat) & !is.list(sumstat)){
    sumstat <- list(sumstat)
    sumstat <- rep(sumstat, length(rasterlayer))
  }
  
  # Collapsing summary statistics together
  sumstat_collapse <- lapply(sumstat,
                             function(x) paste(x, collapse = ","))
  
  # Query
  if(!(is.null(rasterlayer) | length(rasterlayer) == 0 |
       is.null(sumstat) | length(sumstat) == 0)){
    
    tmp_raster <- list()
    
    for(i in seq_along(rasterlayer)){
      tmp_raster[[i]] <- read_json(paste0(
        api, "?query=geoentities_env_raster&layername=", rasterlayer[i],
        "&sumstat=", sumstat_collapse[[i]]),
        simplifyVector = TRUE)
      
      # Spreading tmp
      tmp_raster[[i]] <- tidyr::pivot_wider(
        tmp_raster[[i]],
        names_from = "layer_name",
        values_from = sumstat[[i]],
        names_glue = "{.value}_{layer_name}")
      
    }
    
    # Join list elements together
    tmp_raster <- purrr::reduce(tmp_raster, full_join, by = "entity_ID")
    
    # Combining with tmp_misc
    tmp_misc <- left_join(tmp_misc, tmp_raster, by = "entity_ID")
  }
  
  # Sub-setting the entity_ID
  if(!is.null(entity_ID)){
    tmp_misc <- tmp_misc[tmp_misc$entity_ID %in% entity_ID, ]
  }
  
  return(tmp_misc)
}
