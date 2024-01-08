#' Versions of GIFT available
#'
#' Returns a table with information on the different versions of the database
#'
#' @return
#' A data frame with 4 columns.
#'
#' @details Here is what each column refers to:
#' 
#' \emph{ID} - Identification number of the version\cr
#' \emph{version} - Version number\cr
#' \emph{description} - What were the major updates about
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
#' ex <- GIFT_versions()
#' }
#' 
#' @importFrom jsonlite read_json
#' 
#' @export

GIFT_versions <- function(){
  if(!curl::has_internet()) {
    message("No internet connection found.")
    return(NULL)
  } else{
    
    gift_version <- jsonlite::read_json(
      "https://gift.uni-goettingen.de/api/index.php?query=versions",
      simplifyVector = TRUE)
    
    return(gift_version)
  }
}
