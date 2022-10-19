#' GIFT species distribution
#'
#' Retrieve the distribution of one species from GIFT checklits.
#'
#' @param genus Character string corresponding to the genus of the species 
#' of interest.
#' 
#' @param epithet Character string corresponding to the epithet of the species 
#' of interest.
#'  
#' @param namesmatched Boolean. FALSE by default, set to TRUE if you want to 
#' look for the species not only in the standardized species names but also 
#' in the original species names as they came in the original resources. Only 
#' works if `work_ID` = `NULL` and `genus` and `epithet` supplied.
#' 
#' @param remove_overlap a boolean stating whether you want to
#' retrieve checklists that overlap or not.
#' 
#' @param area_th_island A number stating from which surface the smallest
#' overlapping polygon is kept. By default set to 0 square kilometer
#' (meaning that by default the smallest islands will be conserverd).
#' 
#' @param area_th_mainland When two polygons overlap, the smallest or the
#' biggest one can be kept. When the surface of the smallest polygon exceeds
#' this number, the smallest polygon is kept. Otherwise, we keep the bigger
#' one. Set by default 100 square-kilometers.
#' 
#' @param overlap_th A number ranging from 0 to 1, indicating at what
#' percentage of overlap, partially overlapping polygons should be kept. 
#' 
#' @param by_ref_ID logical indicating whether the removal of overlapping
#' regions shall be applied by ref_ID only. Note that regions overlapping with
#' other regions from the same resource will be removed even if there are other
#' references available for those regions.
#' 
#' @param api character string defining from which API the data will be
#' retrieved.
#'
#' @param GIFT_version character string defining the version of the GIFT
#'  database to use. The function retrieves by default the most up-to-date
#'  version.
#'
#' @return
#' List with two data frames: the checklist with species and the list of ID.
#'
#' @details Here is the detail of each data.frame and their columns:
#'
#' @references
#'      Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
#'      Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
#'      https://doi.org/10.1111/jbi.13623
#'
#' @seealso [GIFT::GIFT_species_lookup()]
#'
#' @examples
#' \dontrun{
#' ex <- GIFT_species_distribution()
#' }
#' 
#' @importFrom jsonlite read_json
#' 
#' @export

GIFT_species_distribution <- function(
    genus = "Fagus", epithet = "sylvatica", work_ID = NULL, 
    namesmatched = FALSE, remove_overlap = FALSE, area_th_island = 0,
    area_th_mainland = 100, overlap_th = 0.1, by_ref_ID = FALSE,
    GIFT_version = "latest", api = "http://gift.uni-goettingen.de/api/extended/"
){
  # 1. Controls ----
  if(length(genus) != 1 || is.na(genus) ||
     !is.character(genus)){
    stop("'genus' is incorrect. It must be a character string indicating the 
      genus of the species you're looking for.")
  }
  if(length(epithet) != 1 || is.na(epithet) ||
     !is.character(epithet)){
    stop("'epithet' is incorrect. It must be a character string indicating the 
      epithet of the species you're looking for.")
  }
  
  if(length(namesmatched) != 1 || !is.logical(namesmatched) ||
     is.na(namesmatched)){
    stop("'namesmatched' must be a boolean stating whether you only want to 
    look for the species not only in the standardized species names or also 
    in the original species names as they came in the original resources")
  }
  
  if(length(remove_overlap) != 1 || !is.logical(remove_overlap) ||
     is.na(remove_overlap)){
    stop("'remove_overlap' must be a boolean stating whether you want to
    retrieve checklists that overlap or not.")
  }
  
  if(!is.numeric(area_th_island) || area_th_island < 0){
    stop("'area_th_island' is a surface in km^2 indicating from which
    surface the smallest overlapping polygon is kept.")
  }
  
  if(!is.numeric(area_th_mainland) || area_th_mainland < 0){
    stop("'area_th_mainland' is a surface in km^2 indicating from which
    surface the smallest overlapping polygon is kept.")
  }
  
  if(!is.numeric(overlap_th) || overlap_th < 0 || overlap_th > 1){
    stop("'overlap_th' is a number ranging from 0 to 1, indicating at what 
         percentage of overlap, partially overlapping polygons should be
         kept.")
  }
  
  if(length(by_ref_ID) != 1 || !is.logical(by_ref_ID) ||
     is.na(by_ref_ID)){
    stop("'by_ref_ID' must be a boolean stating whether indicating whether the
         removal of overlapping regions shall be applied only at the
         reference level.")
  }
  
  if(!is.character(api)){
    stop("api must be a character string indicating which API to use.")
  }
  
  gift_version <- jsonlite::read_json(
    "https://gift.uni-goettingen.de/api/index.php?query=versions",
    simplifyVector = TRUE)
  
  if(length(GIFT_version) != 1 || is.na(GIFT_version) ||
     !is.character(GIFT_version) || 
     !(GIFT_version %in% c(unique(gift_version$version),
                           "latest", "beta"))){
    stop(c("'GIFT_version' must be a character string stating what version
    of GIFT you want to use. Available options are 'latest' and the different
           versions."))
  }
  if(GIFT_version == "latest"){
    GIFT_version <- gift_version[nrow(gift_version), "version"]
  }
  
  # 2. Function ----
  ## 2.1. Look up species ---- 

  if(namesmatched){
    names <-GIFT_species_lookup(genus = genus, epithet = epithet, 
                                GIFT_version = GIFT_version, api = api)

    name_IDs <- unique(names$name_ID)
  } else {
    # TODO simple name look up just based on names_matched add to above function
    
  }
  
  ## 2.1. Get distribution ---- 
  
  # TODO: modify query to look for name_ID, to include cf, aff. stuff
  # to include names? No, match back afterwards!
  # make simple fast version
  
  
  
  
  ## 2.3. Overlapping entities ----
  # overlapped entities are removed => subset lists based on entity_ID again)
  if(remove_overlap == TRUE){
    
    if(!by_ref_ID){
      no_overlap <- suppressMessages(
        GIFT::GIFT_no_overlap(
          entity_IDs = lists$entity_ID, area_th_island = area_th_island, 
          area_th_mainland = area_th_mainland, overlap_th = overlap_th, 
          geoentities_overlap = NULL, api = api, GIFT_version = GIFT_version))
      
      lists <- lists[which(lists$entity_ID %in% no_overlap), ]
      
    } else {
      
      geoentities_overlap <- jsonlite::read_json(
        paste0(api, "index", ifelse(GIFT_version == "beta", "", GIFT_version),
               ".php?query=overlap"), simplifyVector = TRUE)
      
      to_remove <- tapply(lists$entity_ID, lists$ref_ID, function(x) { 
        to_keep <- suppressMessages(
          GIFT::GIFT_no_overlap(entity_IDs = x, 
                                area_th_island = area_th_island, 
                                area_th_mainland = area_th_mainland, 
                                overlap_th = overlap_th, 
                                geoentities_overlap = geoentities_overlap, 
                                api = api, GIFT_version = GIFT_version))
        to_remove <- x[which(!x %in% to_keep)]
      })
      to_remove <- unlist(to_remove)
      
      lists <- lists[which(!lists$entity_ID %in% to_remove),]
    }
  }
  

  return(list(lists, checklists))
}
