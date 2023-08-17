
# Internal functions to check input and output errors
#
# Authors: Pierre Denelle & Patrick Weigelt
#

# Checking api argument
#
# Authors: Pierre Denelle
#
# Stop if api argument does not have the right format
#
# Args:
#   api a character
#
# Returns:
#   nothing, shows an error message

# Run the API and see if we get 'no query selected'
check_api <- function(api) {
  if(length(api) != 1 || !is.character(api)){
    stop("api must be a character string indicating which API to use.")
  }
  
  # First check: internet connection
  if(!curl::has_internet()) {
    stop("No internet connection found.")
  }
  
  # Second check: web API available
  req <- httr2::request(api)
  resp <- httr2::req_error(req, is_error = function(resp) FALSE)
  
  # test for non-existing APIs
  tryCatch(expr = httr2::req_perform(resp),
           error = function(expr) {
             stop("Either the API is wrongly specified or the server is down.")
           })
  
  # If the server was found, check for status
  resp_status <- httr2::resp_status_desc(httr2::req_perform(resp))
  if(resp_status == "Unauthorized"){
    stop("A password is needed for this restricted API.")
  } else if(resp_status != "OK"){
    stop("Either the API is wrongly specified or the server is down.")
  } else{
    resp <- httr2::req_perform(req)
    if(httr2::resp_body_string(resp) != "No query selected."){
      stop("Either the API is wrongly specified or the server is down.")
    }
  }
}

# Checking area_threshold_island argument
#
# Authors: Pierre Denelle
#
# Stop if area_threshold_island argument does not have the right format
#
# Args:
#   area_threshold_island a positive numeric
#
# Returns:
#   shows an error message if needed

check_area_threshold_island <- function(area_threshold_island) {
  if(!is.numeric(area_threshold_island) || area_threshold_island < 0){
    stop("'area_threshold_island' must be a numeric value indicating the 
    area threshold in km^2 above which smaller islands are preferred over 
    larger islands.")
  }
}

# Checking area_threshold_mainland argument
#
# Authors: Pierre Denelle
#
# Stop if area_threshold_mainland argument is not having the right format
#
# Args:
#   area_threshold_mainland a positive numeric
#
# Returns:
#   shows an error message if needed

check_area_threshold_mainland <- function(area_threshold_mainland) {
  if(!is.numeric(area_threshold_mainland) || area_threshold_mainland < 0){
    stop("'area_threshold_mainland' must be a numeric value indicating the 
    area threshold in km^2 above which smaller regions are preferred over 
    larger regions.")
  }
}

# Checking bias_deriv argument
#
# Authors: Pierre Denelle
#
# Stop if bias_deriv argument is not having the right format
#
# Args:
#   bias_deriv a logical
#
# Returns:
#   shows an error message if needed

check_bias_deriv <- function(bias_deriv) {
  if(length(bias_deriv) != 1 || !is.logical(bias_deriv) || is.na(bias_deriv)){
    stop("bias_deriv must be a logical.")
  }
}

# Checking bias_ref argument
#
# Authors: Pierre Denelle
#
# Stop if bias_ref argument is not having the right format
#
# Args:
#   bias_ref a logical
#
# Returns:
#   shows an error message if needed

check_bias_ref <- function(bias_ref) {
  if(length(bias_ref) != 1 || !is.logical(bias_ref) || is.na(bias_ref)){
    stop("bias_ref must be a logical.")
  }
}

# Checking by_ref_ID argument
#
# Authors: Pierre Denelle
#
# Stop if by_ref_ID argument is not having the right format
#
# Args:
#   by_ref_ID a logical
#
# Returns:
#   shows an error message if needed

check_by_ref_ID <- function(by_ref_ID) {
  if(length(by_ref_ID) != 1 || !is.logical(by_ref_ID) || is.na(by_ref_ID)){
    stop("'by_ref_ID' must be a logical indicating whether the
         removal of overlapping regions shall be applied only at the
         reference level (i.e. within references).")
  }
}

# Checking coordinates argument
#
# Authors: Pierre Denelle
#
# Stop if coordinates argument is not having the right format
#
# Args:
#   shp         a shapefile
#   coordinates set of numeric coordinates
#   overlap     a character
#
# Returns:
#   shows an error message if needed
#   shp and coordinates

check_coordinates <- function(coordinates, shp, overlap) {
  if(!is.null(coordinates)){
    if(any(is.na(as.numeric(coordinates)))){
      stop("'coordinates' object does not have the right format. It should be
           a vector of XY coordinates. See help page.")
    }
    
    if(nrow(coordinates) == 1){
      if(overlap %in% c("shape_inside", "centroid_inside")){
        stop("With 'coordinates' being a point, 'overlap' should be either 
        'shape_intersect' or 'extent_intersect'.")
      }
      
      shp <- sf::st_point(coordinates)
      shp <- sf::st_sfc(shp, crs = 4326)
      shp <- sf::st_sf(shp) # making a sf object
      coordinates <- NULL
    } else if(nrow(coordinates) == 2){
      message("4 coordinates provided: an extent box was drawn, assuming that
            minimum X and Y are on row 1, and maximum X and Y on row 2.")
      
      shp <- make_box(xmin = coordinates[1, 1],
                      xmax = coordinates[2, 1],
                      ymin = coordinates[1, 2],
                      ymax = coordinates[2, 2])
      shp <- sf::st_sfc(shp, crs = 4326)
      shp <- sf::st_sf(shp)
      coordinates <- NULL
    }else if(nrow(coordinates) > 2){
      if((coordinates[1, 1] != coordinates[nrow(coordinates), 1]) &
         (coordinates[1, 2] != coordinates[nrow(coordinates), 2])){
        warning("Provided polygon does not have a closed shape.")
        coordinates <- rbind(coordinates, coordinates[1, ])
      }
      shp <- sf::st_polygon(list(coordinates))
      shp <- sf::st_sfc(shp, crs = 4326)
      shp <- sf::st_sf(shp)
      coordinates <- NULL
    } else{
      stop("'coordinates' object does not have the right format. It should be
           a vector of XY coordinates. See help page.")
    }
  }
  return(list(shp = shp, coordinates = coordinates))
}

# Checking complete_floristic argument
#
# Authors: Pierre Denelle
#
# Stop if complete_floristic argument is not having the right format
#
# Args:
#   complete_floristic a logical
#
# Returns:
#   shows an error message if needed

check_complete_floristic <- function(complete_floristic) {
  if(length(complete_floristic) != 1 || !is.logical(complete_floristic) ||
     is.na(complete_floristic)){
    stop("'complete_floristic' must be a logical stating whether you want to
    retrieve checklists only for regions for which the 'floristic_group' is 
    entirely covered or also regions for which only a subset of the 
    the 'floristic_group' is covered.")
  }
}

# Checking complete_taxon argument
#
# Authors: Pierre Denelle
#
# Stop if complete_taxon argument is not having the right format
#
# Args:
#   complete_taxon a logical
#
# Returns:
#   shows an error message if needed

check_complete_taxon <- function(complete_taxon) {
  if(length(complete_taxon) != 1 || !is.logical(complete_taxon) ||
     is.na(complete_taxon)){
    stop("'complete_taxon' must be a logical stating whether you want to
    retrieve checklists for regions for which the 'taxon_name' argument is 
    entirely covered or also regions for which only a subset of the 
    the 'taxon_name' is covered.")
  }
}

# Checking floristic_group argument
#
# Authors: Pierre Denelle
#
# Stop if floristic_group argument is not having the right format
#
# Args:
#   floristic_group a character
#
# Returns:
#   shows an error message if needed

check_floristic_group <- function(floristic_group) {
  if(length(floristic_group) != 1 || is.na(floristic_group) ||
     !is.character(floristic_group) || 
     !(floristic_group %in% c("all", "native", "endemic", "naturalized"))){
    stop(c("'floristic_group' must be a character string. Available options are
    'all', 'native', 'endemic' and 'naturalized'."))
  }
}

# Checking geo_type argument
#
# Authors: Pierre Denelle
#
# Stop if geo_type argument is not having the right format
#
# Args:
#   geo_type a character
#
# Returns:
#   shows an error message if needed

check_geo_type <- function(geo_type) {
  if(is.na(geo_type) || !is.character(geo_type) || 
     !(geo_type %in% c("Mainland", "Island", "All"))){
    stop(c("'geo_type' must be a character string stating for what geographic
    type of regions you want to retrieve checklists. Available options are 
    'Mainland', 'Island' and 'Mainland, Island')."))
  }
}

# Checking GIFT_version argument
#
# Authors: Pierre Denelle & Matthias Grenié
#
# Stop if GIFT_version argument is not a proper version of the database
#
# Args:
#   GIFT_version a character
#
# Returns:
#   GIFT_version or shows an error message

check_gift_version <- function(GIFT_version) {
  tryCatch({
    gift_version <- jsonlite::read_json(
      "https://gift.uni-goettingen.de/api/index.php?query=versions",
      simplifyVector = TRUE)
  },
  error = function(expr) {
    stop("The API is correctly specified and the server is not down BUT the
           database is not responsive at the moment.")
  })
  
  if (length(GIFT_version) != 1 || is.na(GIFT_version) ||
      !is.character(GIFT_version) || 
      !(GIFT_version %in% c(unique(gift_version$version),
                            "latest", "beta"))) {
    stop(c("'GIFT_version' must be a character string stating what version
of GIFT you want to use. Available options are 'latest', 'beta' and the 
different named stable versions of GIFT."))
  }
  if (GIFT_version == "latest") {
    GIFT_version <- gift_version[nrow(gift_version), "version"]
    message(paste0(
      "You are asking for the latest stable version of GIFT which is ",
      GIFT_version, "."))
  }
  if (GIFT_version == "beta") {
    message("You are asking for the beta-version of GIFT which is subject to
updates and edits. Consider using 'latest' for the latest stable
version.")
  }
  return(GIFT_version)
}

# Checking GIFT_version argument
#
# Authors: Pierre Denelle
#
# Stop if GIFT_version argument is not a proper version of the database
# Contrarily to check_gift_version(), the version table is only called if
# version is 'latest'
#
# Args:
#   GIFT_version a character
#
# Returns:
#   GIFT_version or shows an error message

check_gift_version_simple <- function(GIFT_version) {
  if (length(GIFT_version) != 1 || is.na(GIFT_version) ||
      !is.character(GIFT_version)) {
    stop(c("'GIFT_version' must be a character string stating what version
of GIFT you want to use. Available options are 'latest', 'beta' and the 
different named stable versions of GIFT."))
  }
  if (GIFT_version == "latest") {
    gift_version <- jsonlite::read_json(
      "https://gift.uni-goettingen.de/api/index.php?query=versions",
      simplifyVector = TRUE)
    GIFT_version <- gift_version[nrow(gift_version), "version"]
    
    message(paste0(
      "You are asking for the latest stable version of GIFT which is ",
      GIFT_version, "."))
  }
  if (GIFT_version == "beta") {
    message("You are asking for the beta-version of GIFT which is subject to
updates and edits. Consider using 'latest' for the latest stable
version.")
  }
  return(GIFT_version)
}

# Checking list_set_only argument
#
# Authors: Pierre Denelle
#
# Stop if list_set_only argument is not having the right format
#
# Args:
#   list_set_only a logical
#
# Returns:
#   shows an error message if needed

check_list_set_only <- function(list_set_only) {
  if(length(list_set_only) != 1 || !is.logical(list_set_only) ||
     is.na(list_set_only)){
    stop("'list_set_only' must be a logical stating whether you only want 
    metadata of the checklists or if you also want to retrieve the species 
    lists.")
  }
}

# Checking namesmatched argument
#
# Authors: Pierre Denelle
#
# Stop if namesmatched argument is not having the right format
#
# Args:
#   namesmatched a logical
#
# Returns:
#   shows an error message if needed

check_namesmatched <- function(namesmatched) {
  if(length(namesmatched) != 1 || !is.logical(namesmatched) ||
     is.na(namesmatched)){
    stop("'namesmatched' must be a logical stating whether you only want the
    standardized species names or if you also want to retrieve original species 
    names and information on the name matching.")
  }
}

# Checking overlap argument
#
# Authors: Pierre Denelle
#
# Stop if overlap argument is not having the right format
# (GIFT_checklists and spatial)
#
# Args:
#   overlap a character
#
# Returns:
#   shows an error message if needed

check_overlap <- function(overlap) {
  if(!is.character(overlap) || length(overlap) != 1 ||
     !(all(overlap %in% c("centroid_inside", "shape_inside", "shape_intersect",
                          "extent_intersect")))){
    stop("'overlap' must be a character string indicating whether you want to 
    use the centroid or extent of the GIFT polygons to overlap with your 
    shapefile or coordinates.\n 
    It has to be 'centroid_inside', 'shape_inside', 'shape_intersect' or
    'extent_intersect'.")
  }
}

# Checking overlap_threshold argument
#
# Authors: Pierre Denelle
#
# Stop if overlap_threshold argument is not having the right format
#
# Args:
#   overlap_threshold a positive numeric
#
# Returns:
#   shows an error message if needed

check_overlap_threshold <- function(overlap_threshold) {
  if(!is.numeric(overlap_threshold) || overlap_threshold < 0 ||
     overlap_threshold > 1){
    stop("'overlap_threshold' is a number ranging from 0 to 1, indicating above
    what proportion of overlap, only one of the partially overlapping polygons 
    should be kept.")
  }
}

# Checking ref_excluded argument
#
# Authors: Pierre Denelle
#
# Stop if ref_excluded argument is not having the right format
# GIFT_checklists and GIFT_checklists_conditional
#
# Args:
#   ref_excluded a character
#
# Returns:
#   shows an error message if needed

check_ref_excluded <- function(ref_excluded) {
  if(!is.null(ref_excluded) & !is.character(ref_excluded) & 
     !is.numeric(ref_excluded)){
    stop("'ref_excluded' must be a character string or a numeric giving the
         identification numbers of references (ref_ID) that shall be 
         ignored.")
  }
}

# Checking remove_overlap argument
#
# Authors: Pierre Denelle
#
# Stop if remove_overlap argument is not having the right format
#
# Args:
#   remove_overlap a logical
#
# Returns:
#   shows an error message if needed

check_remove_overlap <- function(remove_overlap) {
  if(length(remove_overlap) != 1 || !is.logical(remove_overlap) ||
     is.na(remove_overlap)){
    stop("'remove_overlap' must be a logical stating whether you want to
    remove overlapping regions and their checklists from the output.")
  }
}

# Checking shp argument
#
# Authors: Pierre Denelle
#
# Stop if shp argument is not having the right format
#
# Args:
#   shp     a shapefile
#   overlap a character
#
# Returns:
#   shows an error message if needed

check_shp <- function(shp, overlap) {
  if(!is.null(shp) && !("sf" %in% class(shp))){
    stop("'shp' must be an object of classes 'sf' and 'data.frame', with a CRS
         set to WGS84 (EPSG: 4326).")
  }
  
  if(!is.null(shp) && nrow(shp) > 1){
    warning("Several features are included in the 'shp' object. They will be
            treated at once (not separately by ID). To retrieve checklists per 
            feature separately, please run GIFT_spatial() repeteadly.")
  }
  
  if(!is.null(shp) && "sfc_POINT" %in% class(sf::st_as_sfc(shp)) &&
     overlap %in% c("shape_inside", "centroid_inside")){
    stop("With 'shp' being a points layer, 'overlap' should be either 
        'shape_intersect' or 'extent_intersect'.")
  }
  
  if(!is.null(shp) && "sfc_MULTIPOINT" %in% class(sf::st_as_sfc(shp)) &&
     overlap %in% c("shape_inside", "centroid_inside")){
    stop("With 'shp' being a points layer, 'overlap' should be either 
        'shape_intersect' or 'extent_intersect'.")
  }
  
  if(!is.null(shp) && "sfc_LINESTRING" %in% class(sf::st_as_sfc(shp)) &&
     overlap %in% c("shape_inside", "centroid_inside")){
    stop("With shp being a linestring, 'overlap' should be either 
        'shape_intersect' or 'extent_intersect'.")
  }
  
  if(!is.null(shp) && "sfc_MULTILINESTRING" %in% class(sf::st_as_sfc(shp)) &&
     overlap %in% c("shape_inside", "centroid_inside")){
    stop("With shp being a linestring, 'overlap' should be either 
        'shape_intersect' or 'extent_intersect'.")
  }
  
  # EPSG should be 4326 (WGS84), if not, reprojection and warning message
  if(!is.null(shp)){
    if(is.na(sf::st_crs(shp))){
      warning("There is no CRS defined for your shapefile.
          We define it as being WGS 84 (EPSG 4326).")
      sf::st_crs(shp) <- 4326
    } else if(sf::st_crs(shp) != st_crs(4326)){
      warning(paste0(
        "The CRS of the supplied shapefile is not equal to sf::st_crs(4326) as
        it should be.\n
        Your shapefile will be reprojected accordingly."))
      
      shp <- sf::st_transform(shp, crs = 4326)
    }
  }
  return(shp)  
}

# Checking suit_geo argument
#
# Authors: Pierre Denelle
#
# Stop if suit_geo argument is not having the right format
#
# Args:
#   suit_geo a logical
#
# Returns:
#   shows an error message if needed

check_suit_geo <- function(suit_geo) {
  if(length(suit_geo) != 1 || !is.logical(suit_geo) || is.na(suit_geo)){
    stop("'suit_geo' must be a logical stating whether only regions classified 
    as suit_geo should be considered.")
  }
}

# Checking sumstat argument
#
# Authors: Pierre Denelle
#
# Stop if sumstat argument is not having the right format
#
# Args:
#   sumstat a character
#
# Returns:
#   shows an error message if needed

check_sumstat <- function(sumstat) {
  if(!is.character(unlist(sumstat)) || 
     !(all(unlist(sumstat) %in% c(
       "min", "q05", "q10", "q20", "q25", "q30", "q40", 
       "med", "q60", "q70", "q75", "q80", "q90", "q95", 
       "max", "mean", "sd", "modal", "unique_n", "H", "n")))
  ){
    stop("'sumstat' needs to be a character vector including one or more of the 
         following summary statistics: c('min', 'q05', 'q10', 'q20', 'q25', 
         'q30', 'q40', 'med', 'q60', 'q70', 'q75', 'q80', 'q90', 'q95', 'max', 
         'mean', 'sd', 'modal', 'unique_n', 'H', 'n')")
  }
}

# Checking taxon_name argument
#
# Authors: Pierre Denelle
#
# Stop if taxon_name argument is not having the right format
#
# Args:
#   taxon_name a character
#
# Returns:
#   shows an error message if needed

check_taxon_name <- function(taxon_name) {
  if(length(taxon_name) != 1 || is.na(taxon_name) ||
     !is.character(taxon_name)){
    stop("'taxon_name' is incorrect. It must be a character string among one of
         the taxonomic groups available in GIFT. To check them all, run
         'GIFT_taxonomy()'.")
  }
}

# Checking taxonomic_group argument
#
# Authors: Pierre Denelle
#
# Stop if taxonomic_group argument is not having the right format
#
# Args:
#   taxonomic_group a logical
#
# Returns:
#   shows an error message if needed

check_taxonomic_group <- function(taxonomic_group) {
  if(length(taxonomic_group) != 1 || !is.logical(taxonomic_group) ||
     is.na(taxonomic_group)){
    stop("'taxonomic_group' must be a logical. When set to TRUE, two additional
    columns ('family' and 'tax_group') will be available in the checklists.")
  }
}

# Checking trait_IDs argument
#
# Authors: Pierre Denelle
#
# Stop if trait_IDs argument is not having the right format
#
# Args:
#   trait_IDs a character
#
# Returns:
#   shows an error message if needed

check_trait_IDs <- function(trait_IDs) {
  if(!is.character(trait_IDs)){
    stop("trait_IDs must be a character string indicating which trait you want
         to retrieve. To see all options, run 'GIFT_traits_meta()")
  }
}

# Making a bounding box
#
# Authors: Pierre Denelle
#
# Make bounding box out of user supplied coordinates
#
# Args:
#   four corners
#
# Returns:
#   an extent box

make_box <- function(xmin, xmax, ymin, ymax){
  x_shp <- sf::st_polygon(list(matrix(
    c(xmin, ymin,
      xmax, ymin,
      xmax, ymax,
      xmin, ymax,
      xmin, ymin), ncol = 2, byrow = TRUE)))
  return(x_shp)
}
