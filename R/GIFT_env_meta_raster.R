#' Metadata for the environmental rasters in GIFT
#'
#' Retrieve the metadata of every environmental raster accessible in GIFT
#'
#' @param GIFT_version character string defining the version of the GIFT
#'  database to use. The function retrieves by default the most up-to-date
#'  version.
#' 
#' @param api character string defining from which API the data will be
#' retrieved.
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
#'      Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
#'      Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
#'      https://doi.org/10.1111/jbi.13623
#'
#' @seealso [GIFT::GIFT_env()]
#'
#' @examples
#' \dontrun{
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
  
  check_api(api)
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
