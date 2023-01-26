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
