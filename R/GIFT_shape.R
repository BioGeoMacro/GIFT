#' GIFT shape
#'
#' Get shapefile of GIFT regions for selected entities
#'
#' @param entity_ID A vector defining the IDs of the regions
#' 
#' 
#' @return
#' a spatial data.frame
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
#' plot(st_geometry(geodata), col=geodata$entity_ID)
#' }
#' 
#' @importFrom sf st_read st_is_valid st_make_valid st_set_precision
#' 
#' @export

GIFT_shape <- function(entity_ID = NULL, 
                       api = "http://gift.uni-goettingen.de/api/extended/"){
  
  # 1. Controls ----
  # Arguments
  if(is.null(entity_ID)){
    stop("Please provide the ID numbers of the regions you want 
         polygons for.")
  }
  
  GIFT_entities <- GIFT::GIFT_env(miscellaneous = "area",
                                   api = api)
  GIFT_entities <- GIFT_entities[complete.cases(GIFT_entities$area), ]
  
  # TODO give back warning if not all entity_IDs have polygons?
  
  entity_ID <- entity_ID[which(entity_ID %in% GIFT_entities$entity_ID)]
  
  geodata <- list()
  
  for (i in seq_along(unique(entity_ID))) {
    
    tmp_geo <- st_read(paste0("http://gift.uni-goettingen.de/geojson/geojson_smaller/",entity_ID[i],".geojson"),
                            quiet = TRUE)
    
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

  