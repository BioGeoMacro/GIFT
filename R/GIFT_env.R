#' Environmental data for GIFT checklists
#'
#' Retrieve environmental data associated to each GIFT checklists.
#' Sources of environmental variables can come from raster layers or
#' from shape files (miscellaneous). Users need to define what variables they
#' are interested in and then ask for a set of summary statistics (in case of
#' raster layers).
#'
#' @param entity_ID A vector defining the ID of the lists to retrieve.
#' `NULL` by default, in that case, every list from GIFT is retrieved.
#'
#' @param miscellaneous character vector or list specifying the miscellaneous
#' data to retrieve. . A list of all miscellaneous layers for which precomputed
#' information exists in the database can be viewed in the output table
#' returned by `GIFT_env_meta_misc()`.
#' 
#' @param rasterlayer character vector or list specifying the raster
#' data to retrieve. A list of all raster layers for which precomputed
#' information exists in the database can be viewed in the output table
#' returned by `GIFT_env_meta_raster()`.
#' 
#' @param sumstat Vector or list indicating the desired summary statistics out 
#' of `c("min", "q05", "q10", "q20", "q25", "q30", "q40", "med", "q60", "q70", 
#' "q75", "q80", "q90", "q95", "max", "mean", "sd", "modal", "unique_n", "H", 
#' "n")` used to aggregate the information coming from the raster layers. If 
#' `sumstat` is a vector, the same summary statistics are used for all raster 
#' layers. If `sumstat` is a list, the first element defines the summary 
#' statistics for the first raster layer, the second for the second and so
#' on.\cr
#' 
#' \strong{Important note}\cr Some summary statistics may not be informative
#' depending on the environmental layer you ask for. For example, it is not
#' relevant to retrieve the mean of soil classes for a polygon. The mode or
#' Shannon index are more suitable in that case.
#' 
#' @template GIFT_version_api
#' 
#' @return A data frame with the environmental values per polygon (entity_ID).
#'
#' @details The columns of the data.frame are the following:
#' 
#' \emph{entity_ID} - Identification number of the polygon\cr
#' \emph{geo_entity} - Name of the polygon\cr
#' The other columns relate to the environmental variables the user asked for.
#'
#' @references
#'      Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
#'      Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
#'      https://doi.org/10.1111/jbi.13623
#'
#' @seealso [GIFT::GIFT_env_meta_misc()] and [GIFT::GIFT_env_meta_raster()]
#'
#' @examples
#' \donttest{
#' ex <- GIFT_env(entity_ID = c(1,5),
#'                miscellaneous = c("perimeter", "biome"),
#'                rasterlayer = c("mn30_grd", "wc2.0_bio_30s_01"),
#'                sumstat = list(c("mean", "med"), "max"))
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom tidyr pivot_wider
#' @importFrom purrr reduce
#' @importFrom dplyr left_join full_join mutate_if mutate_at
#' 
#' @export

GIFT_env <- function(
    entity_ID = NULL,
    miscellaneous = if (is.null(rasterlayer)) "area" else NULL, 
    rasterlayer = NULL,
    sumstat = "mean",
    GIFT_version = "latest",
    api = "https://gift.uni-goettingen.de/api/extended/"){
  
  # 1. Controls ----
  api_check <- check_api(api)
  if(is.null(api_check)){
    return(NULL)
  } else{
    check_sumstat(sumstat)
    GIFT_version <- check_gift_version_simple(GIFT_version)
    
    gift_env_meta_misc <- suppressMessages(
      GIFT_env_meta_misc(api = api, GIFT_version = GIFT_version))
    
    if(!is.null(miscellaneous)){
      if(length(miscellaneous[miscellaneous %in%
                              gift_env_meta_misc$variable]) == 0){
        stop(
          "None of the miscellaneous variables asked for is available in GIFT.
           Run GIFT_env_meta_misc() to see available options.")
      }
      
      if(length(miscellaneous[miscellaneous %in%
                              gift_env_meta_misc$variable]) !=
         length(miscellaneous)){
        message(paste0(
          "The following miscellaneous variable(s) are not available in GIFT: ",
          miscellaneous[!(miscellaneous %in% gift_env_meta_misc$variable)]))
        
        miscellaneous <- miscellaneous[(miscellaneous %in%
                                          gift_env_meta_misc$variable)]
      }
    }
    
    suppressMessages(
      gift_env_meta_raster <- GIFT_env_meta_raster(api = api,
                                                   GIFT_version = GIFT_version))
    if(!is.null(rasterlayer)){
      if(length(rasterlayer[rasterlayer %in%
                            gift_env_meta_raster$layer_name]) == 0){
        stop("None of the raster layers asked for is available in GIFT.
       Run GIFT_env_meta_raster() to see available options.")
      }
      
      if(length(rasterlayer[rasterlayer %in%
                            gift_env_meta_raster$layer_name]) !=
         length(rasterlayer)){
        message(paste0(
          "The following raster layer(s) are not available in GIFT: ",
          rasterlayer[!(rasterlayer %in% gift_env_meta_raster$layer_name)]))
        
        rasterlayer <- rasterlayer[(rasterlayer %in%
                                      gift_env_meta_raster$layer_name)]
      }
    }
    
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
        
        # Numeric raster columns
        tmp_raster[[i]] <- dplyr::mutate_if(tmp_raster[[i]], is.character,
                                            as.numeric)
      }
      
      # Join list elements together
      tmp_raster <- purrr::reduce(tmp_raster, dplyr::full_join,
                                  by = "entity_ID")
      
      # Combining with tmp_misc
      tmp_misc$entity_ID <- as.numeric(tmp_misc$entity_ID)
      tmp_misc <- dplyr::left_join(tmp_misc, tmp_raster, by = "entity_ID")
    }
    
    # Sub-setting the entity_ID
    if(!is.null(entity_ID)){
      tmp_misc <- tmp_misc[tmp_misc$entity_ID %in% entity_ID, ]
    }
    
    # Remove rows where all columns but entity_ID and geo_entity are NAs
    tmp_misc <- tmp_misc[rowSums(is.na(tmp_misc)) != (ncol(tmp_misc)-2), ]
    
    # Convert numeric miscellaneous layers to numeric
    miscellaneous_num <-
      gift_env_meta_misc[which(gift_env_meta_misc$variable %in% miscellaneous &
                                 gift_env_meta_misc$num == 1),
                         "variable"]
    tmp_misc <- dplyr::mutate_at(tmp_misc, miscellaneous_num, as.numeric)
    
    # entity_ID numeric
    tmp_misc$entity_ID <- as.numeric(tmp_misc$entity_ID)
    
    return(tmp_misc)
  }
}
