#' GIFT_traits_meta
#'
#' Retrieve metadata of the functional traits.
#'
#' @param api Character string corresponding to the API.
#' 
#' @return
#' data frame with metadata about traits available in GIFT.
#'
#' @details Blabla.
#'
#' @references
#'      Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
#'      Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
#'      https://doi.org/10.1111/jbi.13623
#'
#' @seealso [GIFT::GIFT_traits()]
#'
#' @examples
#' \dontrun{
#' ex <- GIFT_traits_meta()
#' }
#' 
#' @importFrom jsonlite read_json
#' 
#' @export

GIFT_traits_meta <- function(
  api = "http://gift.uni-goettingen.de/api/extended/",
  GIFT_version = "latest"){
  # 1. Controls ----
  if(!is.character(api)){
    stop("api must be a character string indicating which API to use.")
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
  
  # 2. Query ----
  tmp <- jsonlite::read_json(
    paste0(api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
           ".php?query=traits_meta"), simplifyVector = TRUE)
  
  return(tmp)
}
