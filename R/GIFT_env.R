#' Environmental data for GIFT checklists
#'
#' Retrieve environmental data associated to each GIFT checklists.
#' They can come as rasters or shapefiles (miscellaneous)
#'
#' @param entity_ID A vector defining the ID of the lists to retrieve.
#' `NULL` by default, in that case, every list from GIFT is retrieved.
#'
#' @param miscellaneous character vector or list defining the miscellaneous
#' data to retrieve.
#' 
#' @param rasterlayer character vector or list defining the raster
#' data to retrieve.
#' 
#' @param sumstat Vector or list indicating the desired summary statistics out 
#' of c("min", "q05", "q10", "q20", "q25", "q30", "q40", "med", "q60", "q70", 
#' "q75", "q80", "q90", "q95", "max", "mean", "sd", "modal", "unique_n", "H", 
#' "n") used to aggregate the information coming from the raster layers. If 
#' sumstat is a vector, the same summary statistics are used for all raster 
#' layers. If sumstat is a list, the first element defines the summary 
#' statistics for the first raster layer, the second for the second and so on.
#' 
#' @param GIFT_version character string defining the version of the GIFT
#'  database to use. The function retrieves by default the most up-to-date
#'  version.
#' 
#' @param api character string defining from which API the data will be
#' retrieved.
#' 
#' @return A data frame with the environmental values per polygon (entity_ID).
#'
#' @details The columns of the data.frame are the following:
#' entity_ID - Identification number of the polygon
#' geo_entity - Name of the polygon
#' The other columns relate to the environmental variables the user asked for.
#'
#' @references
#'      Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
#'      Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
#'      https://doi.org/10.1111/jbi.13623
#'
#' @seealso [GIFT::GIFT_checklist_raw()]
#'
#' @examples
#' \dontrun{
#' ex <- GIFT_env(miscellaneous = "perimeter")
#' ex <- GIFT_env(entity_ID = c(1,5), miscellaneous = c("perimeter", "biome"))
#' 
#' ex <- GIFT_env(entity_ID = c(1,5),
#'                miscellaneous = c("perimeter", "biome"),
#'                rasterlayer = c("mn30_grd", "wc2.0_bio_30s_01"),
#'                sumstat = "mean")
#' 
#' ex <- GIFT_env(entity_ID = c(1,5),
#'                miscellaneous = c("perimeter", "biome"),
#'                rasterlayer = c("mn30_grd", "wc2.0_bio_30s_01"),
#'                sumstat = c("mean", "med"))
#' 
#' ex <- GIFT_env(entity_ID = c(1,5),
#'                miscellaneous = c("perimeter", "biome"),
#'                rasterlayer = c("mn30_grd", "wc2.0_bio_30s_01"),
#'                sumstat = list(c("mean", "med"), "max"))
#' 
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom tidyr pivot_wider
#' @importFrom purrr reduce
#' @importFrom dplyr left_join full_join
#' 
#' @export

GIFT_env <- function(
    entity_ID = NULL,
    # envvar = "area", layername = "",
    miscellaneous = "area", rasterlayer = NULL,
    sumstat = "mean",
    GIFT_version = "latest",
    api = "http://gift.uni-goettingen.de/api/extended/"){
  
  # one argument only
  # check => whether the (list of) argument(s)
  
  # sumstat argument valid for raster only (naturlich)
  # it can be formatted as a single value, a vector or a list
  # in later case => each list element correspond to the position of the vector
  # or raster names
  # control for that argument: length of the vector or of the list has to be
  # equal to the length of the raster layers
  
  # default value => mean repeated to length of layername
  
  # 1. Controls ----
  # Arguments
  if(length(api) != 1 || !is.character(api)){
    stop("api must be a character string indicating which API to use.")
  }
  
  # GIFT_version
  if(length(GIFT_version) != 1 || is.na(GIFT_version) ||
     !is.character(GIFT_version)){
    stop(c("'GIFT_version' must be a character string stating what version
    of GIFT you want to use. Available options are 'latest' and the different
           versions."))
  }
  if(GIFT_version == "latest"){
    gift_version <- jsonlite::read_json(
      "https://gift.uni-goettingen.de/api/index.php?query=versions",
      simplifyVector = TRUE)
    GIFT_version <- gift_version[nrow(gift_version), "version"]
  }
  if(GIFT_version == "beta"){
    message("You are asking for the beta-version of GIFT which is subject to
            updates and edits. Consider using 'latest' for the latest stable
            version.")
  }
  
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
    tmp_misc <- jsonlite::read_json(paste0(
      api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
      ".php?query=geoentities_env_misc"),
      simplifyVector = TRUE)
  } else{
    tmp_misc <- jsonlite::read_json(paste0(
      api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
      ".php?query=geoentities_env_misc&envvar=",
      paste(miscellaneous, collapse = ",")),
      simplifyVector = TRUE)
  }
  
  ## 2.2. Raster data ----
  
  # Query
  if(!(is.null(rasterlayer) | length(rasterlayer) == 0 |
       is.null(sumstat) | length(sumstat) == 0)){
    
    # Preparing sumstat => list with sumstats repeated
    if(is.vector(sumstat) & !is.list(sumstat)){
      sumstat <- list(sumstat)
      sumstat <- rep(sumstat, length(rasterlayer))
    }
    
    # Collapsing summary statistics together
    sumstat_collapse <- lapply(sumstat,
                               function(x) paste(x, collapse = ","))
    tmp_raster <- list()
    
    for(i in seq_along(rasterlayer)){
      tmp_raster[[i]] <- jsonlite::read_json(paste0(
        api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
        ".php?query=geoentities_env_raster&layername=", rasterlayer[i],
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
    tmp_raster <- purrr::reduce(tmp_raster, dplyr::full_join, by = "entity_ID")
    
    # Combining with tmp_misc
    tmp_misc <- dplyr::left_join(tmp_misc, tmp_raster, by = "entity_ID")
  }
  
  # Sub-setting the entity_ID
  if(!is.null(entity_ID)){
    tmp_misc <- tmp_misc[tmp_misc$entity_ID %in% entity_ID, ]
  }
  
  return(tmp_misc)
}
