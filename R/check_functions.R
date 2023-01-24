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
