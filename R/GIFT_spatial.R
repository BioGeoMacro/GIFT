#' Spatial selection of GIFT checklists
#'
#' Retrieve checklists overlapping with a shape file or a set of coordinates.
#'
#' @param shp Shapefile provided by the user. Its Coordinate Reference System
#' (CRS) must be set to WGS84 (EPSG code 4326).
#'
#' @param coordinates Custom set of coordinates. The format is a two columns
#' data.frame, the first one being longitudes and the second being latitudes
#' of the vertices of a polygon. If the data.frame only includes two rows,
#' the function assumes that the values are the four limits (min and max.
#' longitude and latitude) of a bounding box.
#' 
#' @param overlap A character string defining the criteria to use in order to
#' retrieve checklists. Available options are `centroid_inside`,
#' `extent_intersect`, `shape_intersect` and `shape_inside`. For example,
#' `extent_intersect` means that every polygon from GIFT for which the extent
#' intersects the provided shape/coordinates will be retrieved.
#' 
#' @param entity_ID Constrain the list of regions to be received by a
#' predefined set of entity_IDs. E.g. this list could come from
#' GIFT_checklists_conditional().
#' 
#' @template GIFT_version_api
#' 
#' @return A data frame with 3 columns: \emph{entity_ID} the identification
#'  number of a polygon, \emph{geo_entity_ref} its name, and \emph{coverage}
#'  which indicates the percentage of overlap between the provided shape and
#'  the different polygons of GIFT.
#'
#' @references
#'      Denelle, P., Weigelt, P., & Kreft, H. (2023). GIFT—An R package to
#'      access the Global Inventory of Floras and Traits. Methods in Ecology
#'      and Evolution, 14, 2738-2748.
#'      https://doi.org/10.1111/2041-210X.14213
#' 
#'      Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
#'      Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
#'      https://doi.org/10.1111/jbi.13623
#'
#' @seealso [GIFT::GIFT_checklists()]
#'
#' @examples
#' \donttest{
#' # With a shapefile
#' data("western_mediterranean")
#' ex <- GIFT_spatial(shp = western_mediterranean, overlap = "centroid_inside")
#' 
#' # With a shapefile coming from GIFT
#' spain <- GIFT_shapes(entity_ID = 10071)
#' ex_spain <- GIFT_spatial(shp = spain)
#' 
#' # With a point 
#' custom_point <- cbind(9.9, 51)
#' ex2 <- GIFT_spatial(coordinates = custom_point,
#' overlap = "extent_intersect")
#' 
#' # With an extent
#' custom_extent <- cbind(c(-13, -18), c(27.5, 29.3))
#' ex3 <- GIFT_spatial(coordinates = custom_extent,
#' overlap = "extent_intersect")
#' 
#' # With a custom polygon
#' custom_polygon <- cbind(c(-18, -16.9, -13, -13, -18, -18),
#' c(29.3, 33, 29.3, 27.5, 27.5, 29.3))
#' ex4 <- GIFT_spatial(coordinates = custom_polygon,
#' overlap = "extent_intersect")
#' 
#' #With a linestring
#' custom_linestring <- rbind(c(9.9, 51), c(2.35, 48.9))
#' custom_linestring <- sf::st_as_sf(as.data.frame(custom_linestring),
#' coords = c("V1", "V2"))
#' custom_linestring <- dplyr::summarise(custom_linestring,
#' geometry = sf::st_combine(geometry))
#' sf::st_crs(custom_linestring) <- sf::st_crs(western_mediterranean)
#' ex5 <- GIFT_spatial(shp = custom_linestring, overlap = "extent_intersect")
#' 
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom sf st_polygon st_sf st_sfc st_as_sf st_as_sfc st_intersection
#' @importFrom sf st_geometry st_read st_is_valid st_make_valid
#' @importFrom sf st_set_precision st_area st_agr
#' @importFrom dplyr mutate
#' @importFrom stats complete.cases
#' 
#' @export

GIFT_spatial <- function(
    # 3 arguments: polygon, extent, point, line and removing shp?
  # so far, not perfect
  shp = NULL, coordinates = NULL, overlap = "centroid_inside",
  entity_ID = NULL, GIFT_version = "latest", 
  api = "https://gift.uni-goettingen.de/api/extended/"){
  
  # 1. Controls ----
  api_check <- check_api(api)
  if(is.null(api_check)){
    return(NULL)
  } else{
    check_overlap(overlap)
    GIFT_version <- check_gift_version_simple(GIFT_version)
    
    if(is.null(shp) & is.null(coordinates)){
      stop("Please provide a shapefile or a set of XY coordinates.")
    }
    
    if(!is.null(shp) & !is.null(coordinates)){
      warning("Both shapefile and coordinates are provided. We use the XY
    coordinates. If you want to use the shapefile instead,
            set 'coordinates = NULL'.")
    }
    
    shp <- check_shp(shp = shp, overlap = overlap)
    
    # Visible binding for global variable
    x_min <- x_max <- y_min <- y_max <- NULL
    
    # Define shp as coordinates, only one format accepted
    coord_check <- check_coordinates(coordinates = coordinates, shp = shp,
                                     overlap = overlap)
    shp <- coord_check[["shp"]]; coordinates <- coord_check[["coordinates"]]
    
    # 2. Query ----
    ## 2.0. GIFT_env() & subset entity_ID ----
    # Depending upon the overlap argument, we either query the centroids or
    # the extent from GIFT
    if(overlap == "centroid_inside"){
      # Query the centroid using GIFT_env()
      GIFT_centroids <- suppressMessages(
        GIFT::GIFT_env(miscellaneous = c("longitude", "latitude"),
                       api = api, GIFT_version = GIFT_version))
      # Removing NAs
      GIFT_centroids <-
        GIFT_centroids[stats::complete.cases(GIFT_centroids$longitude), ]
      
      # Numeric columns
      GIFT_centroids <- dplyr::mutate_at(
        GIFT_centroids, c("longitude", "latitude"), as.numeric)
      
      # Filter for entity_ID
      if(!is.null(entity_ID)){
        GIFT_centroids <- GIFT_centroids[which(GIFT_centroids$entity_ID %in%
                                                 entity_ID), ]
      }
      if(nrow(GIFT_centroids) == 0){
        message("No polygon matches the shape provided.")
        return(data.frame(entity_ID = character(), geo_entity_ref = character(),
                          coverage = character()))
      }
      
    } else if(overlap %in% c("extent_intersect", "shape_intersect",
                             "shape_inside")){
      # Query the extent using GIFT_env()
      GIFT_extents <- GIFT::GIFT_env(
        miscellaneous = c("x_min", "x_max", "y_min", "y_max"),
        api = api, GIFT_version = GIFT_version)
      
      # Removing NAs
      GIFT_extents <- GIFT_extents[stats::complete.cases(GIFT_extents$x_max), ]
      
      # Filter for entity_ID
      if(!is.null(entity_ID)){
        GIFT_extents <- GIFT_extents[which(GIFT_extents$entity_ID %in%
                                             entity_ID), ]
      }
      if(nrow(GIFT_extents) == 0){
        message("No polygon matches the shape provided.")
        return(data.frame(entity_ID = character(),
                          geo_entity_ref = character(),
                          coverage = character()))
      }
      
      # If all coordinates are equal, extend a bit the coordinates
      GIFT_extents <- dplyr::mutate_at(
        GIFT_extents, c("x_min", "x_max", "y_min", "y_max"), as.numeric)
      
      GIFT_extents <- dplyr::mutate(GIFT_extents,
                                    x_min = ifelse((x_min - x_max) == 0,
                                                   x_min - 0.005, x_min),
                                    y_min = ifelse((y_min - y_max) == 0,
                                                   y_min - 0.005, y_min))
    }
    
    ## 2.1. centroid_inside ----
    if(overlap == "centroid_inside"){
      # Subset: only GIFT centroids overlapping with provided shape file
      GIFT_centroids_sf <- sf::st_as_sf(GIFT_centroids,
                                        coords = c("longitude", "latitude"),
                                        crs = 4326)
      
      sf::st_agr(GIFT_centroids_sf) <- "constant"
      sf::st_agr(shp) <- "constant"
      
      tmp <- sf::st_intersection(GIFT_centroids_sf, shp)
      sf::st_geometry(tmp) <- NULL
      
      if(nrow(tmp) == 0){
        message("No polygon matches the shape provided.")
        return(data.frame(entity_ID = character(),
                          geo_entity_ref = character(),
                          coverage = character()))
      }
      
      gift_overlap <- as.data.frame(tmp[, c("entity_ID", "geo_entity")])
      
      # Add coverage column
      gift_overlap$coverage <- NA
      
    } else if(overlap %in% c("extent_intersect", "shape_intersect",
                             "shape_inside")){
      # Checking what extent boxes overlap
      GIFT_extents$keep <- 0
      for(i in seq_len(nrow(GIFT_extents))){
        tmp <- make_box(xmin = as.numeric(GIFT_extents[i, "x_min"]),
                        xmax = as.numeric(GIFT_extents[i, "x_max"]),
                        ymin = as.numeric(GIFT_extents[i, "y_min"]),
                        ymax = as.numeric(GIFT_extents[i, "y_max"]))
        
        tmp <- sf::st_sfc(tmp, crs = 4326)
        
        tmp <- sf::st_intersection(tmp, shp)
        if(length(tmp) > 0){
          GIFT_extents[i, "keep"] <- 1
        }
      }
      
      # Subset: only boxes that overlap with provided shape
      GIFT_extents <- GIFT_extents[which(GIFT_extents$keep == 1), ]
      
      if(nrow(GIFT_extents) == 0){
        message("No polygon matches the shape provided.")
        return(data.frame(entity_ID = character(),
                          geo_entity_ref = character(),
                          coverage = character()))
      }
      
      if(overlap == "extent_intersect"){
        ## 2.2. extent_intersect ----
        gift_overlap <- GIFT_extents[, c("entity_ID", "geo_entity")]
        
        # Add coverage column
        gift_overlap$coverage <- NA
        
      } else if(overlap %in% c("shape_intersect", "shape_inside")){
        # Add coverage column
        GIFT_extents$coverage <- NA
        
        # Downloading geojson for which extent boxes overlap with provided shape
        for(i in seq_len(nrow(GIFT_extents))){
          tmp_geo <- sf::st_read(paste0(
            "https://gift.uni-goettingen.de/geojson/geojson_smaller", 
            ifelse(GIFT_version == "beta", "", GIFT_version), "/",
            GIFT_extents[i, "entity_ID"],
            ".geojson"), quiet = TRUE)
          
          # Control if sf geometry is not valid (i = 68 & 257)
          if(!(sf::st_is_valid(tmp_geo))){
            tmp_geo <- sf::st_make_valid(sf::st_set_precision(
              tmp_geo, 1e2))
          }
          
          # Calculate overlap
          sf::st_agr(tmp_geo) <- "constant"
          
          sf::st_agr(shp) <- "constant"
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
          GIFT_extents <- GIFT_extents[which(GIFT_extents$coverage == 100), ]
        }
        
        gift_overlap <- GIFT_extents[, c("entity_ID", "geo_entity",
                                         "coverage")]
      }
    }
    return(gift_overlap)
  }
}
