#' Environmental data for GIFT checklists
#'
#' Retrieve environmental data associated to each GIFT checklists.
#' They can come as rasters or shapefiles (miscellaneous)
#'
#' @param shp Shapefile provided by the user.
#'
#' @param coordinates Coordinates (point, extent box or polygon) provided by
#' the user.
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
#' ex_extent <- c(-15, 40, 30, 45)
#' ex_extent <- c(120, 124, -13, -12)
#' 
#' ex <- GIFT_spatial(shp = med, overlap = "extent_intersect")
#' ex2 <- GIFT_spatial(shp = med, overlap = "shape_intersect")
#' ex3 <- GIFT_spatial(shp = med, overlap = "shape_inside")
#' 
#' # Example with custom polygon: first and last row need to be repeated
#' # Example with matrix(, byrow = TRUE) or own matrix with repeated x and y
#' # Basically: matrix with two columns: first X, second Y
#' 
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom sf st_polygon st_sfc st_as_sf st_intersection st_geometry st_read st_is_valid st_make_valid st_set_precision st_area
#' 
#' @export

GIFT_spatial <- function(
  shp = NULL, coordinates = NULL, overlap = "centroid_inside",
  api = "http://gift.uni-goettingen.de/api/extended/index.php"){
  
  # 1. Controls ----
  ## 1.1. shp, coordinates ----
  # shp is the shapefile provided by the user
  # it has to be in WGS84 projection: st_crs() == "" EPSG: 4326
  
  if(is.null(shp) & is.null(coordinates)){
    stop("Please provide a shapefile or a set of XY coordinates.")
  }
  
  if(!is.null(shp) & !is.null(coordinates)){
    warning("Both shapefile and coordinates are provided. We use the XY
    coordinates. If you want to use the shapefile instead,
            set 'coordinates = NULL'.")
  }
  
  if(!is.null(shp) & !("sf" %in% class(shp))){
    stop("The provided shape has to be an 'sf' object.")
  }
  
  # Making a shapefile out of provided extent
  make_box <- function(x){
    x_shp <- sf::st_polygon(list(matrix(
      c(x[1], x[3],
        x[2], x[3],
        x[2], x[4],
        x[1], x[4],
        x[1], x[3]), ncol = 2, byrow = TRUE)))
    return(x_shp)
  }
  
  # Define shp as coordinates
  if(!is.null(coordinates)){
    if(length(coordinates) == 2){
      shp <- sf::st_point(coordinates)
      shp <- sf::st_sfc(shp, crs = 4326)
    } else if(length(coordinates) == 4){
      shp <- make_box(coordinates)
      shp <- sf::st_sfc(shp, crs = 4326)
    } else if(length(coordinates) > 4 &
              coordinates %% 2 == 0){ # even nb of coordinates
      shp <- sf::st_polygon(coordinates)
      shp <- sf::st_sfc(shp, crs = 4326)
    } else{
      stop("'coordinates' object does not have the right format. It should be
           a vector of XY coordinates. See help page.")
    }
  }
  
  ## 1.2. overlap ----
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
         'extent_in tersect'.")
  }
  
  # 2. Query ----
  # Depending upon the overlap argument, we either query the centroids or
  # the extent from GIFT
  # if(overlap == "centroid_inside"){
  # 
  # }
  
  ## 2.1. centroid_inside ----
  if(overlap == "centroid_inside"){
    # Query the centroid using GIFT_env()
    GIFT_centroids <- GIFT::GIFT_env(miscellaneous = c("longitude", "latitude"),
                                     api = api)
    
    # Removing NAs
    GIFT_centroids <- GIFT_centroids[complete.cases(GIFT_centroids$longitude), ]
    
    # Numeric columns
    GIFT_centroids[c("longitude", "latitude")] <-
      sapply(GIFT_centroids[c("longitude", "latitude")], as.numeric)
    
    # Subset: only GIFT centroids overlapping with provided shapefile
    GIFT_centroids_sf <- sf::st_as_sf(GIFT_centroids,
                                      coords = c("longitude", "latitude"),
                                      crs = 4326)
    
    tmp <- sf::st_intersection(pnts_sf, shp) # CONTROL for warning message
    
    gift_overlap <- as.data.frame(tmp[, c("entity_ID", "geo_entity")])
    sf::st_geometry(gift_overlap) <- NULL
    
    # Add coverage column
    gift_overlap$coverage <- NA
    
  } else if(overlap %in% c("extent_intersect", "shape_intersect",
                           "shape_inside")){
    # Query the extent using GIFT_env()
    GIFT_extents <- GIFT::GIFT_env(
      miscellaneous = c("x_min", "x_max", "y_min", "y_max"), api = api)
    
    # Removing NAs
    GIFT_extents <- GIFT_extents[complete.cases(GIFT_extents$x_max), ]
    
    # Checking what extent boxes overlap
    GIFT_extents$keep <- 0
    for(i in 1:nrow(GIFT_extents)){
      # If all coordinates are equal, extend a bit the coordinates
      if(as.numeric(GIFT_extents[i, "x_min"]) -
         as.numeric(GIFT_extents[i, "x_max"]) == 0){
        GIFT_extents[i, "x_min"] <- as.numeric(GIFT_extents[i, "x_min"]) -
          0.005
      }
      if(as.numeric(GIFT_extents[i, "y_min"]) -
         as.numeric(GIFT_extents[i, "y_max"]) == 0){
        GIFT_extents[i, "y_min"] <- as.numeric(GIFT_extents[i, "y_min"]) -
          0.005
      }
      
      tmp <- make_box(as.numeric(
        GIFT_extents[i, c("x_min", "x_max", "y_min", "y_max")]))
      
      tmp <- sf::st_sfc(tmp, crs = 4326)
      
      tmp <- sf::st_intersection(tmp, shp)
      if(length(tmp) > 0){
        GIFT_extents[i, "keep"] <- 1
      }
    }
    
    # Subset: only boxes that overlap with provided shape
    GIFT_extents <- GIFT_extents[which(GIFT_extents$keep == 1), ]
    
    if(overlap == "extent_intersect"){
      ## 2.2. extent_intersect ----
      gift_overlap <- GIFT_extents[, c("entity_ID", "geo_entity")]
      
      # Add coverage column
      gift_overlap$coverage <- NA
      
    } else if(overlap %in% c("shape_intersect", "shape_inside")){
      # Add coverage column
      GIFT_extents$coverage <- NA
      
      # Downloading geojson for which extent boxes overlap with provided shape
      for(i in 1:nrow(GIFT_extents)){
        tmp_geo <- sf::st_read(paste0(
          "http://gift.uni-goettingen.de/geojson/geojson_smaller/",
          GIFT_extents[i, "entity_ID"],
          ".geojson"), quiet = TRUE)
        
        # Control if sf geometry is not valid (i = 68 & 257)
        if(!(sf::st_is_valid(tmp_geo))){
          tmp_geo <- sf::st_make_valid(sf::st_set_precision(
            tmp_geo, 1e2))
        }
        
        # Control plot
        # ggplot() +
        #   geom_sf(data = shp, fill = "blue") +
        #   geom_sf(data = tmp_geo[shp, ], color = "red", fill = NA)
        
        # Calculate overlap
        tmp <- sf::st_intersection(tmp_geo, shp)
        
        if(nrow(tmp) > 0){
          GIFT_extents[i, "coverage"] <- round(100*sf::st_area(tmp)/
                                                 sf::st_area(tmp_geo), 2)
        } else{
          GIFT_extents[i, "coverage"] <- NA
        }
      }
      
      if(overlap == "shape_intersect"){
        ## 2.3. shape_intersect ----
        GIFT_extents <- GIFT_extents[which(GIFT_extents$coverage > 0), ]
        
      } else if(overlap == "shape_inside"){
        ## 2.4. shape_inside ----
        # Checking if GIFT polygons are fully inside provided shape
        # tmp <- sf::st_within(tmp_geo, shp)
        # tmp <- lengths(tmp)
        GIFT_extents <- GIFT_extents[which(GIFT_extents$coverage == 100), ]
      }
      
      gift_overlap <- GIFT_extents[, c("entity_ID", "geo_entity", "coverage")]
    }
  }
  
  return(gift_overlap)
}

# X. Code resources ----
# https://gis.stackexchange.com/questions/34535/detect-whether-there-is-a-spatial-polygon-in-a-spatial-extent
# sp is the shapefile provided by user
# e is the extent of GIFT polygons => 
# in <- intersect(e, raster::extent(sp)) # filter
# if (in) { in <- rgeos::gIntersects(as(e, 'SpatialPolygons'), sp) }

# Defining pipe
# `%>%` <- magrittr::`%>%`
# plot(sf::st_geometry(tmp_geo[sf::st_within(tmp_geo, shp) %>%
#                                lengths > 0, ]), col = "grey")
# dim(tmp_geo)
# dim(tmp_geo[sf::st_within(tmp_geo, shp) %>% lengths > 0, ])
