# Internal functions to check inputs and outputs errors
#
# Authors: Pierre Denelle & Patrick Weigelt
#

# Checking api argument
#
# Authors: Pierre Denelle
#
# Stop if GIFT_version argument is not a proper version of the database
#
# Args:
#   api a character
#
# Returns:
#   nothing, shows an error message

check_api <- function(api) {
  if(length(api) != 1 || !is.character(api)){
    stop("api must be a character string indicating which API to use.")
  }
}

# Checking area_threshold_island argument
#
# Authors: Pierre Denelle
#
# Stop if area_threshold_island argument is not having the right format
#
# Args:
#   area_threshold_island a positive numeric
#
# Returns:
#   shows an error message if needed

check_area_threshold_island <- function(area_threshold_island) {
  if(!is.numeric(area_threshold_island) || area_threshold_island < 0){
    stop("'area_threshold_island' is a surface in km^2 indicating from which
    surface the smallest overlapping polygon is kept.")
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
    stop("'area_threshold_mainland' is a surface in km^2 indicating from which
    surface the smallest overlapping polygon is kept.")
  }
}

# Checking bias_deriv argument
#
# Authors: Pierre Denelle
#
# Stop if bias_deriv argument is not having the right format
#
# Args:
#   bias_deriv a boolean
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
#   bias_ref a boolean
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
#   by_ref_ID a boolean
#
# Returns:
#   shows an error message if needed

check_by_ref_ID <- function(by_ref_ID) {
  if(length(by_ref_ID) != 1 || !is.logical(by_ref_ID) || is.na(by_ref_ID)){
    stop("'by_ref_ID' must be a boolean stating whether indicating whether the
         removal of overlapping regions shall be applied only at the
         reference level.")
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
        stop("With a point, use either 'shape_intersect' or
             'extent_intersect' only.")
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
        warning("Provided polygon did not have a closed shape.")
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
#   complete_floristic a boolean
#
# Returns:
#   shows an error message if needed

check_complete_floristic <- function(complete_floristic) {
  if(length(complete_floristic) != 1 || !is.logical(complete_floristic) ||
     is.na(complete_floristic)){
    stop("'complete_floristic' must be a boolean stating whether you want to
    retrieve checklists that only contain the exhaustive list of the
    'floristic_group' argument or as well incomplete lists.")
  }
}

# Checking complete_taxon argument
#
# Authors: Pierre Denelle
#
# Stop if complete_taxon argument is not having the right format
#
# Args:
#   complete_taxon a boolean
#
# Returns:
#   shows an error message if needed

check_complete_taxon <- function(complete_taxon) {
  if(length(complete_taxon) != 1 || !is.logical(complete_taxon) ||
     is.na(complete_taxon)){
    stop("'complete_taxon' must be a boolean stating whether you want to
    retrieve checklists that only contain the exhaustive list of the
    'taxon_name' argument or as well incomplete lists.")
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
    stop(c("'geo_type' must be a character string stating what geographic
    type you want to retrieve. Available options are 'Mainland', 'Island' or
    'Mainland, Island')."))
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
  gift_version <- jsonlite::read_json(
    "https://gift.uni-goettingen.de/api/index.php?query=versions",
    simplifyVector = TRUE)
  
  if (length(GIFT_version) != 1 || is.na(GIFT_version) ||
      !is.character(GIFT_version) || 
      !(GIFT_version %in% c(unique(gift_version$version),
                            "latest", "beta"))) {
    stop(c("'GIFT_version' must be a character string stating what version
of GIFT you want to use. Available options are 'latest' and the different
versions."))
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
of GIFT you want to use. Available options are 'latest' and the different
versions."))
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
#   list_set_only a boolean
#
# Returns:
#   shows an error message if needed

check_list_set_only <- function(list_set_only) {
  if(length(list_set_only) != 1 || !is.logical(list_set_only) ||
     is.na(list_set_only)){
    stop("'list_set_only' must be a boolean stating whether you only want the
    metadata or if you also want to retrieve the species lists.")
  }
}

# Checking namesmatched argument
#
# Authors: Pierre Denelle
#
# Stop if namesmatched argument is not having the right format
#
# Args:
#   namesmatched a boolean
#
# Returns:
#   shows an error message if needed

check_namesmatched <- function(namesmatched) {
  if(length(namesmatched) != 1 || !is.logical(namesmatched) ||
     is.na(namesmatched)){
    stop("'namesmatched' must be a boolean stating whether you only want the
    standardized species names or if you also want to retrieve original species 
         names and information on the name matching.")
  }
}

# Checking overlap argument
#
# Authors: Pierre Denelle
#
# Stop if overlap argument is not having the right format
# (GIFT_checklist and spatial)
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
    stop("overlap is a character string indicating whether you want to use
         centroid or extent of GIFT polygons to overlap with your shapefile.\n
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
    stop("'overlap_threshold' is a number ranging from 0 to 1, indicating at
    what percentage of overlap, partially overlapping polygons should be
         kept.")
  }
}

# Checking ref_excluded argument
#
# Authors: Pierre Denelle
#
# Stop if ref_excluded argument is not having the right format
# GIFT_checklist and GIFT_checklist_conditional
#
# Args:
#   ref_excluded a character
#
# Returns:
#   shows an error message if needed

check_ref_excluded <- function(ref_excluded) {
  if(!is.null(ref_excluded) & !is.character(ref_excluded) & 
     !is.numeric(ref_excluded)){
    stop("'ref_excluded' must be a character string or a numeric stating the
         identification numbers of the references (ref_ID) that shall be 
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
#   remove_overlap a boolean
#
# Returns:
#   shows an error message if needed

check_remove_overlap <- function(remove_overlap) {
  if(length(remove_overlap) != 1 || !is.logical(remove_overlap) ||
     is.na(remove_overlap)){
    stop("'remove_overlap' must be a boolean stating whether you want to
    retrieve checklists that overlap or not.")
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
    warning("Several polygons are passed in the shp object. They will be
            treated at the same time. To know what polygon covers what
            checklist, please use repeteadly GIFT_spatial().")
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
}

# Checking suit_geo argument
#
# Authors: Pierre Denelle
#
# Stop if suit_geo argument is not having the right format
#
# Args:
#   suit_geo a boolean
#
# Returns:
#   shows an error message if needed

check_suit_geo <- function(suit_geo) {
  if(length(suit_geo) != 1 || !is.logical(suit_geo) || is.na(suit_geo)){
    stop("'suit_geo' must be a boolean stating whether you want to
    retrieve only suitable polygons or not.")
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
    stop('sumstat needs to be a character vector including one or more of the 
         following items: c("min", "q05", "q10", "q20", "q25", "q30", "q40", 
         "med", "q60", "q70", "q75", "q80", "q90", "q95", "max", "mean", "sd", 
         "modal", "unique_n", "H", "n")')
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
#   taxonomic_group a boolean
#
# Returns:
#   shows an error message if needed

check_taxonomic_group <- function(taxonomic_group) {
  if(length(taxonomic_group) != 1 || !is.logical(taxonomic_group) ||
     is.na(taxonomic_group)){
    stop("'taxonomic_group' must be a boolean. When set to TRUE, two additional
    columns ('family' and 'tax_group') are available in the checklists.")
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
         to retrieve.")
  }
}

# Making a bounding box
#
# Authors: Pierre Denelle
#
# Stop if trait_IDs argument is not having the right format
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
