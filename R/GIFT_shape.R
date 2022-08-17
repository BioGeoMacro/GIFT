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
#' @importFrom geojsonsf geojson_sf
#' 
#' @export

GIFT_shape <- function(entity_ID = NULL){
  
  # 1. Controls ----
  # Arguments
  if(is.null(entity_ID)){
    stop("Please provide the ID numbers of the regions you want 
         polygons for.")
  }
  
  
  geodata <- list()
  
  for (i in seq_along(unique(entity_ID))) {
    
    geodata[[i]] <- geojson_sf(paste0("http://gift.uni-goettingen.de/geojson/geojson_smaller/",entity_ID[i],".geojson"))
    
    
  }
  
  geodata <- do.call(rbind, geodata)
  return(geodata)
}

  