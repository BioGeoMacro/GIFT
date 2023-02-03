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
#' A spatial data.frame with 21 columns.
#'
#' @details Here is the detail of each column:
#' 
#' \emph{entity_ID} - Identification number of the polygon\cr
#' \emph{geo_entity} - Name of the polygon\cr
#' \emph{point_x} - Longitude of the centroid of the polygon\cr
#' \emph{point_y} - Latitude of the centroid of the polygon\cr
#' \emph{area} - Area in km2 of the polygon\cr
#' \emph{x_min} - Minimum longitude of the polygon\cr
#' \emph{x_max} - Maximum longitude of the polygon\cr
#' \emph{y_min} - Minimum latitude of the polygon\cr
#' \emph{y_max} - Maximum latitude of the polygon\cr
#' \emph{entity_class} - Class of the polygon\cr
#' \emph{entity_type} - Type of the entity\cr
#' \emph{polygon_source} - Source of the polygon\cr
#' \emph{geometry} - Geometry column from sf
#'
#' @references
#'      Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
#'      Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
#'      https://doi.org/10.1111/jbi.13623
#'
#' @seealso [GIFT::GIFT_env()]
#'
#' @examples
#' \dontrun{
#' ex <- GIFT_shape(entity_ID = c(677, 200))
#' plot(st_geometry(ex), col = ex$entity_ID)
#' }
#' 
#' @importFrom sf st_read st_is_valid st_make_valid st_set_precision
#' @importFrom dplyr select
#' @importFrom utils setTxtProgressBar txtProgressBar
#' 
#' @export

GIFT_shape <- function(entity_ID = NULL, 
                       api = "https://gift.uni-goettingen.de/api/extended/", 
                       GIFT_version = "latest"){
  
  # 1. Controls ----
  check_api(api)
  GIFT_version <- check_gift_version_simple(GIFT_version)
  
  # Visible binding for global variable
  suit_geo <- suit_geo_rst <- overlap_checked <- NULL
  overlap_glonaf_checked <- overlap_gmba_checked <- NULL
  overlap_gaptani_checked <- priority <- NULL
  
  # 2. Function ----
  GIFT_entities <- suppressMessages(
    GIFT_env(miscellaneous = "area",
             api = api, GIFT_version = GIFT_version))
  GIFT_entities <- GIFT_entities[complete.cases(GIFT_entities$area), ]
  
  if(is.null(entity_ID)){
    message("No ID number of the regions was given. All GIFT regions are
            downloaded, it may take a while.")
    entity_ID <- GIFT_entities$entity_ID
  }else{
    initial_nb_asked <- length(entity_ID)
    entity_ID <- entity_ID[which(entity_ID %in% GIFT_entities$entity_ID)]
    
    if(length(entity_ID) == 0){
      stop("None of the provided entity_IDs has a shape available.")
    }
    
    if(length(entity_ID) != initial_nb_asked){
      if(length(entity_ID) == 1){
        message(paste0("Only ", length(entity_ID), " entity_ID out of ",
                       initial_nb_asked, " had a shape available."))
      } else{
        message(paste0("Only ", length(entity_ID), " entity_IDs out of ",
                       initial_nb_asked, " had a shape available."))
      }
    }
  }
  
  # TODO give back warning if not all entity_IDs have polygons?
  
  geodata <- list()
  
  progress <- utils::txtProgressBar(min = 0, max = length(unique(entity_ID)),
                                    initial = 0)
  
  for(i in seq_along(unique(entity_ID))) {
    # TODO Put old polygons of old versions into respective folders and paste
    # version here
    tmp_geo <- st_read(
      paste0("https://gift.uni-goettingen.de/geojson/geojson_smaller", 
             ifelse(GIFT_version == "beta", "", GIFT_version), "/",
             entity_ID[i], ".geojson"), quiet = TRUE)
    
    # Control if sf geometry is not valid (i = 68 & 257)
    if(!(sf::st_is_valid(tmp_geo))){
      tmp_geo <- sf::st_make_valid(sf::st_set_precision(
        tmp_geo, 1e2))
    }
    geodata[[i]] <- tmp_geo
    
    utils::setTxtProgressBar(progress, i)
  }
  
  geodata <- do.call(rbind, geodata)
  geodata <- geodata[order(geodata$area, decreasing = TRUE), ]
  
  geodata <- dplyr::select(geodata, -suit_geo, -suit_geo_rst, -overlap_checked,
                           -overlap_glonaf_checked, -overlap_gmba_checked,
                           -overlap_gaptani_checked, -priority, -factor)
  
  return(geodata)
}
