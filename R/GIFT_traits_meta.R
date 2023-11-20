#' Trait metadata
#'
#' Retrieve metadata of the functional traits coming from GIFT.
#'
#' @param api Character string corresponding to the API.
#' 
#' @param GIFT_version character string defining the version of the GIFT
#'  database to use. The function retrieves by default the most up-to-date
#'  version.
#' 
#' @return A data frame with 10 columns.
#'
#' @details Here is what each column refers to:
#' 
#' \emph{Lvl1} - First level of the trait classification\cr
#' \emph{Category} - Name of the first level of classification\cr
#' \emph{Lvl2} - Second level of the trait classification\cr
#' \emph{Trait1} - Name of the second level of classification\cr
#' \emph{Lvl3} - Identification number of the trait\cr
#' \emph{Trait2} - Trait name\cr
#' \emph{Units} - Trait unit\cr
#' \emph{type} - Trait type\cr
#' \emph{comment} - Comment\cr
#' \emph{count} - How many entries for that traits are in the database
#'
#' @references
#'      Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
#'      Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
#'      https://doi.org/10.1111/jbi.13623
#'
#' @seealso [GIFT::GIFT_traits()]
#'
#' @examples
#' \donttest{
#' ex <- GIFT_traits_meta()
#' }
#' 
#' @importFrom jsonlite read_json
#' 
#' @export

GIFT_traits_meta <- function(
    api = "https://gift.uni-goettingen.de/api/extended/",
    GIFT_version = "latest"){
  
  api_check <- check_api(api)
  if(is.null(api_check)){
    return(NULL)
  } else{
    GIFT_version <- check_gift_version(GIFT_version)
    
    tmp <- jsonlite::read_json(
      paste0(api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
             ".php?query=traits_meta"), simplifyVector = TRUE)
    
    tmp$count <- as.numeric(tmp$count)
    
    return(tmp)
  }
}
