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
  GIFT_version = NULL){
  # 1. Controls ----
  if(!is.character(api)){
    stop("api must be a character string indicating which API to use.")
  }
  
  # 2. Query ----
  tmp <- jsonlite::read_json(paste0(api, "index", 
                                    ifelse(is.null(GIFT_version), "", GIFT_version),
                                    ".php?query=traits_meta"),
                             simplifyVector = TRUE)
  
  return(tmp)
}
