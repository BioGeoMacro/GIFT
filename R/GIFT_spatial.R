#' Environmental data for GIFT checklists
#'
#' Retrieve environmental data associated to each GIFT checklists.
#' They can come as rasters or shapefiles (miscellaneous)
#'
#' @param shp Shapefile provided by the user.
#'
#' @param coordinates Custom set of coordinates. The format is a two columns,
#' the first one being longitudes and the second being latitudes. If 4
#' coordinates are given, the function assumes that these are the four corners
#' of a bounding box.
#' 
#' @param overlap A character string defining the criteria to use in order to
#' retrieve checklists. Available options are 'centroid_inside',
#' 'extent_intersect', 'shape_intersect' and 'shape_inside'. For example,
#' 'extent_intersect' means that every polygon from GIFT for which the extent
#' intersects the provided shape/coordinates will be retrieved.
#' 
#' @param entity_ID List of entity_ID to retrieve.
#' 
#' @param GIFT_version character string defining the version of the GIFT
#'  database to use. The function retrieves by default the most up-to-date
#'  version.
#' 
#' @param api character string defining from which API the data will be retrieved.
#' 
#' @return A data frame with 3 columns: entity_ID the identification number
#' of a polygon, 'geo_entity_ref' its name, and 'coverage' which indicates
#' the percentage of overlap between the provided shape and the different
#' polygons of GIFT.
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
#' ex2 <- GIFT_spatial(shp = med, overlap = "extent_intersect")
#' ex3 <- GIFT_spatial(shp = med, overlap = "shape_intersect")
#' ex4 <- GIFT_spatial(shp = med, overlap = "shape_inside")
#' 
#' custom_point <- cbind(9.9, 51)
#' ex5 <- GIFT_spatial(coordinates = custom_point,
#' overlap = "extent_intersect")
#' 
#' custom_extent <- cbind(c(-13, -18), c(27.5, 29.3))
#' ex6 <- GIFT_spatial(coordinates = custom_extent,
#' overlap = "extent_intersect")
#' 
#' custom_polygon <- cbind(c(-18, -16.9, -13, -13, -18, -18),
#' c(29.3, 33, 29.3, 27.5, 27.5, 29.3))
#' ex7 <- GIFT_spatial(coordinates = custom_polygon,
#' overlap = "extent_intersect")
#' 
#' custom_linestring <- rbind(c(9.9, 51), c(2.35, 48.9))
#' custom_linestring <- sf::st_as_sf(as.data.frame(custom_linestring),
#' coords = c("V1", "V2"))
#' custom_linestring <- dplyr::summarise(custom_linestring,
#' geometry = sf::st_combine(geometry))
#' sf::st_crs(custom_linestring) <- sf::st_crs(med)
#' ex8 <- GIFT_spatial(shp = custom_linestring, overlap = "extent_intersect")
#' 
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom sf st_polygon st_sf st_sfc st_as_sf st_as_sfc st_intersection st_geometry st_read st_is_valid st_make_valid st_set_precision st_area st_agr
#' @importFrom dplyr mutate
#' 
#' @export

GIFT_spatial <- function(
    # 3 arguments: polygon, extent, point, line and removing shp?
  # so far, not perfect
  shp = NULL, coordinates = NULL, overlap = "centroid_inside",
  entity_ID = NULL, GIFT_version = "latest", 
  api = "http://gift.uni-goettingen.de/api/extended/"){
  
  # 1. Controls ----
  
  if(!is.character(overlap) || length(overlap) != 1 ||
     !(all(overlap %in% c("centroid_inside", "shape_inside", "shape_intersect",
                          "extent_intersect")))){
    stop("overlap is a character string indicating whether you want to use
         centroid or extent of GIFT polygons to overlap with your shapefile.\n
         It has to be 'centroid_inside', 'shape_inside', 'shape_intersect' or
         'extent_intersect'.")
  }
  
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
    message("You are asking for the beta-version of GIFT which is subject to updates and edits. Consider using 'latest' for the latest stable version.")
  }
  
  if(!is.character(api)){
    stop("api must be a character string indicating which API to use.")
  }
  
  if(is.null(shp) & is.null(coordinates)){
    stop("Please provide a shapefile or a set of XY coordinates.")
  }
  
  if(!is.null(shp) & !is.null(coordinates)){
    warning("Both shapefile and coordinates are provided. We use the XY
    coordinates. If you want to use the shapefile instead,
            set 'coordinates = NULL'.")
  }
  
  if(!is.null(shp) && !("sf" %in% class(shp))){
    stop("'shp' must be an object of classes 'sf' and 'data.frame', with a CRS set to WGS84 (EPSG: 4326).")
  }
  
  if(!is.null(shp) && nrow(shp) > 1){
    warning("Several polygons are passed in the shp object. They will be treated at the same time. To know what polygon covers what checklist, please use repeteadly GIFT_spatial().")
  }
  
  if(!is.null(shp) && "sfc_POINT" %in% class(sf::st_as_sfc(shp)) &&
     overlap %in% c("shape_inside", "centroid_inside")){
    stop("With a point, use either 'shape_intersect' or
             'extent_intersect' only.")
  }
  
  if(!is.null(shp) && "sfc_MULTIPOINT" %in% class(sf::st_as_sfc(shp)) &&
     overlap %in% c("shape_inside", "centroid_inside")){
    stop("With a point, use either 'shape_intersect' or
             'extent_intersect' only.")
  }
  
  if(!is.null(shp) && "sfc_LINESTRING" %in% class(sf::st_as_sfc(shp)) &&
     overlap %in% c("shape_inside", "centroid_inside")){
    stop("With a linestring, use either 'shape_intersect' or
             'extent_intersect' only.")
  }
  
  if(!is.null(shp) && "sfc_MULTILINESTRING" %in% class(sf::st_as_sfc(shp)) &&
     overlap %in% c("shape_inside", "centroid_inside")){
    stop("With a linestring, use either 'shape_intersect' or
             'extent_intersect' only.")
  }
  
  # Making a shapefile out of provided extent
  make_box <- function(xmin, xmax, ymin, ymax){
    x_shp <- sf::st_polygon(list(matrix(
      c(xmin, ymin,
        xmax, ymin,
        xmax, ymax,
        xmin, ymax,
        xmin, ymin), ncol = 2, byrow = TRUE)))
    return(x_shp)
  }
  
  # Define shp as coordinates, only one format accepted
  if(!is.null(coordinates)){
    if(is.na(coordinates) || is.character(coordinates)){
      stop("'coordinates' object does not have the right format. It should be
           a vector of XY coordinates. See help page.")
    }
    
    if(nrow(coordinates) == 1){
      if(overlap %in% c("shape_inside", "centroid_inside")){
        stop("With a point, use either 'shape_intersect' or
             'extent_intersect' only.")
      }
      
      shp <- sf::st_point(coordinates)
      shp <- sf::st_sfc(shp, crs = 4326)
      shp <- sf::st_sf(shp) # making a sf object
    } else if(nrow(coordinates) == 2){
      message("4 coordinates provided: an extent box was drawn, assuming that
            minimum X and Y are on row 1, and maximum X and Y on row 2.")
      shp <- make_box(xmin = coordinates[1, 1],
                      xmax = coordinates[2, 1],
                      ymin = coordinates[1, 2],
                      ymax = coordinates[2, 2])
      shp <- sf::st_sfc(shp, crs = 4326)
      shp <- sf::st_sf(shp) # making a sf object
    }else if(nrow(coordinates) > 2){
      if((coordinates[1, 1] != coordinates[nrow(coordinates), 1]) &
         (coordinates[1, 2] != coordinates[nrow(coordinates), 2])){
        warning("Provided polygon did not have a closed shape.")
        coordinates <- rbind(coordinates, coordinates[1, ])
      }
      shp <- sf::st_polygon(list(coordinates))
      shp <- sf::st_sfc(shp, crs = 4326)
      shp <- sf::st_sf(shp) # making a sf object
    } else{
      stop("'coordinates' object does not have the right format. It should be
           a vector of XY coordinates. See help page.")
    }
  }
  
  # 2. Query ----
  ## 2.0. GIFT_env() & subset entity_ID ----
  # Depending upon the overlap argument, we either query the centroids or
  # the extent from GIFT
  if(overlap == "centroid_inside"){
    # Query the centroid using GIFT_env()
    GIFT_centroids <- GIFT::GIFT_env(miscellaneous = c("longitude", "latitude"),
                                     api = api, GIFT_version = GIFT_version)
    # Removing NAs
    GIFT_centroids <- GIFT_centroids[complete.cases(GIFT_centroids$longitude), ]
    
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
    GIFT_extents <- GIFT_extents[complete.cases(GIFT_extents$x_max), ]
    
    # Filter for entity_ID
    if(!is.null(entity_ID)){
      GIFT_extents <- GIFT_extents[which(GIFT_extents$entity_ID %in%
                                           entity_ID), ]
    }
    if(nrow(GIFT_extents) == 0){
      message("No polygon matches the shape provided.")
      return(data.frame(entity_ID = character(), geo_entity_ref = character(),
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
      return(data.frame(entity_ID = character(), geo_entity_ref = character(),
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
      return(data.frame(entity_ID = character(), geo_entity_ref = character(),
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
          "http://gift.uni-goettingen.de/geojson/geojson_smaller/",
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
        # Add control: if a custom point was provided, then coverage would be
        # NA
        GIFT_extents <- GIFT_extents[which(GIFT_extents$coverage > 0), ]
        
      } else if(overlap == "shape_inside"){
        ## 2.4. shape_inside ----
        GIFT_extents <- GIFT_extents[which(GIFT_extents$coverage == 100), ]
      }
      
      gift_overlap <- GIFT_extents[, c("entity_ID", "geo_entity", "coverage")]
    }
  }
  return(gift_overlap)
}
