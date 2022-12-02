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
#' in the original species names as they came in the original resources. 
#' 
#' @param remove_overlap a boolean stating whether you want to
#' retrieve checklists that overlap or not.
#' 
#' @param area_th_island A number stating from which surface the smallest
#' overlapping polygon is kept. By default set to 0 square kilometer
#' (meaning that by default the smallest islands will be conserved).
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
#' @param aggregation A Boolean stating whether you want to aggregate in a
#' simpler way the floristic status of species per entity_ID. For example, two
#' lists associated to the same entity_ID could describe a species both as
#' native and non-native. In that case, the aggregation would consider the
#' species to be native. Reverse for naturalized and alien.
#' 
#' @param api character string defining from which API the data will be
#' retrieved.
#'
#' @param GIFT_version character string defining the version of the GIFT
#'  database to use. The function retrieves by default the most up-to-date
#'  version.
#'
#' @return A data frame with 33 columns.
#'
#' @details Here is the detail of each data.frame and their columns:
#' \emph{ref_ID} - Identification number of the reference\cr
#' \emph{list_ID} - Identification number of the list\cr
#' \emph{entity_ID} - Identification number of the polygon\cr
#' \emph{name_ID} - Identification number of the genus before taxonomic
#' harmonization\cr 
#' \emph{cf_genus} - Whether the genus name is uncertain\cr
#' \emph{cf_species} - Whether the species' epithet is uncertain\cr 
#' \emph{aff_species} - Species' epithet uncertain\cr
#' \emph{questionable} - Whether the species name is questionable\cr
#' \emph{native} - Is the species native\cr 
#' \emph{quest_native} - Is the native status questionable\cr
#' \emph{naturalized} - Is the species naturalized\cr 
#' \emph{endemic_ref} - Is the species endemic at the reference level\cr
#' \emph{quest_end_ref} - Is the endemic_ref status questionable\cr
#' \emph{endemic_list} - Is the species endemic at the list level\cr
#' \emph{quest_end_list} - Is the endemic_list status questionable\cr
#' \emph{genus} - Genus name before taxonomic harmonization\cr
#' \emph{species_epithet} - Epithet before taxonomic harmonization\cr
#' \emph{subtaxon} - Subtaxon name before taxonomic harmonization\cr
#' \emph{author} - Author who described the species before taxonomic
#'  harmonization\cr
#' \emph{matched} - Is the species name matched in the taxonomic backbone\cr
#' \emph{epithetscore} - Matching score for the epithet\cr
#' \emph{overallscore} - Overall matching score for the species\cr
#' \emph{resolved} - Is the species name resolved in the taxonomic backbone\cr
#' \emph{synonym} -Is the species a synonym in the taxonomic backbone\cr
#' \emph{matched_subtaxon} -Is the sub-species name matched in the taxonomic
#'  backbone\cr
#' \emph{accepted} - Is the species name accepted in the taxonomic backbone\cr
#' \emph{service} - Service use for the taxonomic harmonization\cr
#' \emph{work_ID} -Identification number of the species after taxonomic
#' harmonization\cr
#' \emph{taxon_ID} -Identification number of the taxonomic group\cr
#' \emph{work_genus} - Identification number of the genus after taxonomic
#' harmonization\cr
#' \emph{work_species_epithet} - Identification number of the species epithet
#' after taxonomic harmonization\cr
#' \emph{work_species} - Species name (after taxonomic harmonization)\cr
#' \emph{work_author} - Author who described the species (after taxonomic
#' harmonization)
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
#' @importFrom dplyr bind_rows mutate_all left_join ungroup distinct mutate
#' @importFrom dplyr group_by select
#' 
#' @export

GIFT_species_distribution <- function(
    genus = "Fagus", epithet = "sylvatica", 
    namesmatched = FALSE, remove_overlap = FALSE, area_th_island = 0,
    area_th_mainland = 100, overlap_th = 0.1, by_ref_ID = FALSE,
    aggregation = FALSE,
    GIFT_version = "latest",
    api = "https://gift.uni-goettingen.de/api/extended/"){
  
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
  
  if(length(aggregation) != 1 || !is.logical(aggregation) ||
     is.na(aggregation)){
    stop("'aggregation' must be a boolean stating whether you want to
    aggregate in a simpler way the floristic status of species per entity_ID.")
  }
  
  if(length(api) != 1 || !is.character(api)){
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
  
  endemic_list <- entity_ID <- native <- naturalized <- NULL
  cf_species <- aff_species <- questionable <- quest_native <- NULL
  endemic_ref <- quest_end_ref <- quest_end_list <- matched <- NULL
  epithetscore <- overallscore <- resolved <- synonym <- NULL
  matched_subtaxon <- accepted <- service <- NULL
  cf_genus <- author <- name_ID <- work_ID <- NULL
  ref_ID <- list_ID <- species_epithet <- subtaxon <- NULL
  
  # 2. Function ----
  ## 2.1. Look up species ---- 
  taxnames <- GIFT_species_lookup(genus = genus, epithet = epithet, 
                                  GIFT_version = GIFT_version, api = api,
                                  namesmatched = namesmatched)
  # TODO: simplify names lookup to not look in orig genus? Or filter here
  taxnames <- unique(taxnames[,c("name_ID", "genus", "species_epithet", 
                                 "subtaxon", "author", "matched", 
                                 "epithetscore", "overallscore", "resolved", 
                                 "synonym", "matched_subtaxon", "accepted", 
                                 "service", "work_ID", "taxon_ID", 
                                 "work_genus", "work_species_epithet", 
                                 "work_species", "work_author" )])
  
  name_IDs <- unique(taxnames$name_ID)
  
  lists <- list()
  
  if(length(name_IDs)>0){
    ## 2.2. Get distribution ---- 
    
    for(i in seq_along(name_IDs)){
      lists[[i]] <- jsonlite::read_json(paste0(
        api, "index", ifelse(GIFT_version == "beta", "", GIFT_version), 
        ".php?query=species_distr&nameid=", as.integer(name_IDs[i])), 
        simplifyVector = TRUE)
    }
    lists <- dplyr::bind_rows(lists)
    
    # Data.frame
    lists <- as.data.frame(lists)
    lists <- dplyr::mutate_all(lists, as.numeric)
  } 
  
  if (length(lists) > 0){
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
    
    ## 2.4. Join lists and names ----
    lists <- dplyr::left_join(lists, taxnames, by="name_ID")
  } else {
    lists <- data.frame(ref_ID = numeric(), list_ID = numeric(),
                        entity_ID = numeric(), name_ID = numeric(), 
                        cf_genus = numeric(), cf_species = numeric(), 
                        aff_species = numeric(), 
                        questionable = numeric(), native = numeric(),
                        quest_native = numeric(), naturalized = numeric(),
                        endemic_ref = numeric(),
                        quest_end_ref = numeric(),
                        endemic_list = numeric(),
                        quest_end_list = numeric(),
                        genus = character(),species_epithet = character(),
                        subtaxon = character(), author = character(),
                        matched = numeric(), epithetscore = numeric(),
                        overallscore = numeric(), resolved = numeric(),
                        synonym = numeric(), matched_subtaxon = numeric(), 
                        accepted = numeric(),
                        service = character(), work_ID = numeric(),
                        taxon_ID = numeric(), work_genus = character(),
                        work_species_epithet = character(), 
                        work_species = character(),
                        work_author = character())
  }
  
  ## 2.5. Aggregation ----
  if(aggregation){
    lists <- dplyr::group_by(lists, entity_ID, work_ID)
    lists <- dplyr::mutate(
      lists,
      conflict_native = ifelse(length(unique(native)) > 1, 1, 0),
      conflict_naturalized = ifelse(length(unique(naturalized)) > 1, 1, 0),
      conflict_endemic_list = ifelse(length(unique(endemic_list)) > 1, 1, 0),
      native = ifelse(1 %in% native, 1, ifelse(0 %in% native, 0, NA)),
      naturalized = ifelse(0 %in% naturalized, 0,
                           ifelse(1 %in% naturalized, 1, NA)),
      endemic_list = ifelse(0 %in% endemic_list, 0,
                            ifelse(1 %in% endemic_list, 1, NA))
    )
    lists <- dplyr::distinct(lists, native, naturalized, endemic_list,
                             .keep_all = TRUE)
    lists <-  dplyr::ungroup(lists)
    lists <- dplyr::select(lists, -cf_genus, -name_ID, -cf_species,
                           -aff_species, -questionable, -quest_native,
                           -endemic_ref, -quest_end_ref, -quest_end_list,
                           -author, -matched, -epithetscore, -overallscore,
                           -resolved, -synonym, -matched_subtaxon, -accepted,
                           -service, -ref_ID, -list_ID, -species_epithet, 
                           -subtaxon)
    
  }
  return(lists)
}
