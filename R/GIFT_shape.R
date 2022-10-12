#' GIFT shape
#'
#' Get shapefile of GIFT regions for selected entities
#'
#' @param entity_ID A vector defining the IDs of the regions.
#' 
#' @param GIFT_version character string defining the version of the GIFT
#'  database to use. The function retrieves by default the most up-to-date
#'  version.
#' 
#' @param api character string defining from which API the data will be
#' retrieved.
#' 
#' @return
#' A spatial data.frame
#'
#' @details Blabla.
#'
#' @references
#'      Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
#'      Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
#'      https://doi.org/10.1111/jbi.13623
#'
#' @seealso [GIFT::GIFT_plot()], [GIFT::GIFT_env()]
#'
#' @examples
#' \dontrun{
#' ex <- GIFT_shape(entity_ID = c(677, 200))
#' plot(st_geometry(ex), col = ex$entity_ID)
#' }
#' 
#' @importFrom sf st_read st_is_valid st_make_valid st_set_precision
#' 
#' @export

GIFT_shape <- function(entity_ID = NULL, 
                       api = "http://gift.uni-goettingen.de/api/extended/", 
                       GIFT_version = "latest"){
  
  # 1. Controls ----
  # Arguments
  if(is.null(entity_ID)){
    stop("Please provide the ID numbers of the regions you want 
         polygons for.")
  }
  
  if(!is.character(api)){
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
  
  # 2. Function ----
  GIFT_entities <- GIFT::GIFT_env(miscellaneous = "area",
                                  api = api, GIFT_version = GIFT_version)
  GIFT_entities <- GIFT_entities[complete.cases(GIFT_entities$area), ]
  
  # TODO give back warning if not all entity_IDs have polygons?
  
  entity_ID <- entity_ID[which(entity_ID %in% GIFT_entities$entity_ID)]
  
  geodata <- list()
  
  for (i in seq_along(unique(entity_ID))) {
    # TODO Put old polygons of old versions into respective folders and paste
    # version here
    tmp_geo <- st_read(
      paste0("http://gift.uni-goettingen.de/geojson/geojson_smaller/",
             entity_ID[i], ".geojson"), quiet = TRUE)
    
    # Control if sf geometry is not valid (i = 68 & 257)
    if(!(sf::st_is_valid(tmp_geo))){
      tmp_geo <- sf::st_make_valid(sf::st_set_precision(
        tmp_geo, 1e2))
    }
    geodata[[i]] <- tmp_geo
  }
  
  geodata <- do.call(rbind, geodata)
  geodata <- geodata[order(geodata$area, decreasing = TRUE),]
  return(geodata)
}
