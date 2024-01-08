#' Metadata for the environmental rasters in GIFT
#'
#' Retrieve the metadata of every environmental raster accessible in GIFT.
#'
#' @template GIFT_version_api
#' 
#' @return
#' A data frame with 10 columns.
#'
#' @details Here is what each column refers to:
#' 
#' \emph{dataset} - Name of the source dataset\cr
#' \emph{layer_name} - Name of the environmental layer\cr
#' \emph{layer} - Full name\cr
#' \emph{description} - Description\cr
#' \emph{unit} - Unit\cr
#' \emph{coord_system} - Coordinate system\cr
#' \emph{resolution} - Resolution\cr
#' \emph{extent} - Extent\cr
#' \emph{version} - Version of the source\cr
#' \emph{ref_long} - Full reference to cite when using an environmental layer
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
#' @seealso [GIFT::GIFT_env()]
#'
#' @examples
#' \donttest{
#' ex <- GIFT_env_meta_raster()
#' }
#' 
#' @importFrom jsonlite read_json
#' @importFrom dplyr left_join
#' 
#' @export

GIFT_env_meta_raster <- function(
    api = "https://gift.uni-goettingen.de/api/extended/",
    GIFT_version = "latest"){
  
  api_check <- check_api(api)
  if(is.null(api_check)){
    return(NULL)
  } else{
    GIFT_version <- check_gift_version(GIFT_version)
    
    # Return the raster environmental information
    tmp <- jsonlite::read_json(paste0(
      api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
      ".php?query=env_raster"), simplifyVector = TRUE)
    
    # Extract Citavi number from each list element
    tmp$citavi_ID <- gsub("^.*\\#", "", tmp$citavi_ID)
    tmp$citavi_ID <- substr(tmp$citavi_ID, 1, (nchar(tmp$citavi_ID)-1))
    
    refs <- jsonlite::read_json(paste0(
      api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
      ".php?query=references_citavi"), simplifyVector = TRUE)
    
    tmp <- dplyr::left_join(tmp,refs, by=c("citavi_ID" = "citavi_seq_no"))
    tmp$citavi_ID <- NULL
    
    return(tmp)
  }
}
