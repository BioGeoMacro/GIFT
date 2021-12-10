#' Environmental data for GIFT checklists
#'
#' Retrieve environmental data associated to each GIFT checklists.
#' They can come as rasters or shapefiles (miscellaneous)
#'
#' @param shp Shapefile provided by the user.
#'
#' @param extent Extent box provided by the user.
#' 
#' @param overlap character vector or list defining the raster
#' data to retrieve..
#' 
#' @param api character string defining from which API the data will be retrieved.
#' 
#' @return
#' data frame with list of entity_ID.
#'
#' @details Blabla.
#'
#' @references
#'      Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
#'      Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
#'      https://doi.org/10.1111/jbi.13623
#'
#' @seealso [GIFT::GIFT_checklists()]
#'
#' @examples
#' \dontrun{
#' data("med")
#' ex <- GIFT_spatial(shp = med, overlap = "centroid_inside")
#' 
#' }
#' 
#' @importFrom sf st_polygon st_as_sf st_intersection st_geometry
#' 
#' @export

GIFT_spatial <- function(
  shp = NULL, extent = NULL, overlap = "centroid_inside",
  api = "http://gift.uni-goettingen.de/api/extended/index.php"){
  
  # 1. Controls ----
  # shp is the shapefile provided by the user
  # it has to be in WGS84 projection: st_crs() == "" EPSG: 4326
  
  if(is.null(shp) & is.null(extent)){
    stop("Please provide a shapefile or an extent box.")
  }
  
  # Overlap: 4 options
  if(!is.character(overlap)){
    stop("overlap is a character string indicating whether you want to use
         centroid or extent of GIFT polygons to overlap with your shapefile.\n
         It has to be 'centroid_inside', 'shape_inside', 'shape_intersect' or
         'extent_intersect'.")
  }
  
  if(!(overlap %in% c("centroid_inside", "shape_inside", "shape_intersect",
                      "extent_intersect"))){
    stop("overlap is a character string indicating whether you want to use
         centroid or extent of GIFT polygons to overlap with your shapefile.\n
         It has to be 'centroid', 'shape_inside', 'shape_intersect' or
         'extent_intersect'.")
  }
  
  # Making a shapefile out of provided extent
  if(!is.null(extent)){
    extent_shp <- sf::st_polygon(list(matrix(
      c(extent[1], extent[3],
        extent[2], extent[3],
        extent[2], extent[4],
        extent[1], extent[4],
        extent[1], extent[3]), ncol = 2, byrow = TRUE)))
  }
  
  # 2. Query ----
  
  # Find what GIFT polygons overlap with provided shape
  if(overlap == "centroid_inside"){
    # Query the centroid using GIFT_env()
    GIFT_centroids <- GIFT::GIFT_env(miscellaneous = c("longitude", "latitude"),
                                     api = api)
    GIFT_centroids <- GIFT_centroids[complete.cases(GIFT_centroids$longitude), ]
    GIFT_centroids$longitude <- as.numeric(GIFT_centroids$longitude)
    GIFT_centroids$latitude <- as.numeric(GIFT_centroids$latitude)
    
    # Subset: only GIFT centroids overlapping with provided shapefile
    # tmp <- st_multipoint(as.matrix(GIFT_centroids[,
    #                                               c("longitude", "latitude")]))
    # tmp <- st_sfc(tmp)
    # st_crs(tmp) <- 4326 # WGS84
    # 
    # tmp <- st_intersection(tmp, med)
    
    # Alternative
    GIFT_centroids_sf <- sf::st_as_sf(GIFT_centroids,
                                      coords = c("longitude", "latitude"),
                                      crs = 4326)
    
    tmp <- sf::st_intersection(pnts_sf, med) # CONTROL for warning message
    
    gift_overlap <- as.data.frame(tmp[, c("entity_ID", "geo_entity")])
    sf::st_geometry(gift_overlap) <- NULL
    
  } else if(overlap == "shape_inside"){
    # Query the extent using GIFT_env()
    GIFT_extents <- GIFT::GIFT_env(miscellaneous = c("x_min", "x_max", "y_min",
                                                     "y_max"), api = api)
    
    # geodata <- geojson_read("http://gift.uni-goettingen.de/geojson/geojson_smaller/200.geojson", what = "sp")
    # test <- st_read("http://gift.uni-goettingen.de/geojson/geojson_smaller/200.geojson")
    # number = entt_ID
    
    # https://gis.stackexchange.com/questions/34535/detect-whether-there-is-a-spatial-polygon-in-a-spatial-extent
    # sp is the shapefile provided by user
    # e is the extent of GIFT polygons => 
    # in <- intersect(e, raster::extent(sp)) # filter
    # if (in) { in <- rgeos::gIntersects(as(e, 'SpatialPolygons'), sp) }
    
  } else if(overlap == "shape_intersect"){
    # Query the extent using GIFT_env()
    GIFT_extents <- GIFT::GIFT_env(miscellaneous = c("x_min", "x_max", "y_min",
                                                     "y_max"), api = api)
    
  } else if(overlap == "extent_intersect"){
    # Query the extent using GIFT_env()
    GIFT_extents <- GIFT::GIFT_env(miscellaneous = c("x_min", "x_max", "y_min",
                                                     "y_max"), api = api)
    
    
  }
  
  return(gift_overlap)
  
}
