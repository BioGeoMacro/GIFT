
# Relies on GIFT_checklist_conditional(); GIFT_checklist_raw(); GIFT_spatial()

GIFT <- function(
  # actual arguments user needs
  taxon_name = "Tracheophyta",
  floristic_group = c("all", "native",
                      "native and naturalized", # delete this one?
                      "endemic", "naturalized")[2],
  # if native given => native_indicated = T in GIFT_checklist_conditional()
  
  geo_type = c("Mainland", "Island"), # Island gets you to Island, Island Group & Island Part
  # Mainland gets you to Mainland & Island/Mainland
  # c("Mainland", "Island")? gives you everything
  suit_geo = FALSE, # find better names for arguments
  complete_taxon = TRUE, # make a figure with orchids and angiosperms for ex.
  
  # complete_floristic = T, # if you want native => are list with only endemics enough or not?
  # function ran twice in the case where you have a list for a region and an additional one with trees only => combine them
  # this last case may(has to?) be included in GIFT_checklist_conditional() 
  
  shp = NULL, coordinates = NULL, overlap = "centroid_inside",
  
  # remove_overlap = TRUE,
  
  api = "http://gift.uni-goettingen.de/api/extended/index.php"
  ##
  # GIFT_checklist_conditional() below
  # tax_group = 2, # control: length 1 (no several tax_groups)
  # ref_included = c("all", "native", "native and naturalized",
  #                  "native and historically introduced", "endangered",
  #                  "endemic", "naturalized", "other subset")[1:4],
  # type_ref = c("Account", "Catalogue", "Checklist","Flora",
  #              "Herbarium collection", "Key", "Red list", "Report",
  #              "Species Database", "Survey"),
  # entity_class = c("Island", "Island/Mainland", "Mainland", "Island Group",
  #                  "Island Part"),
  # native_indicated = FALSE,
  # natural_indicated = FALSE,
  # end_ref = FALSE,
  # end_list = FALSE,
  # suit_geo = TRUE,
  # complete_taxon = TRUE,
  
  # GIFT_spatial() arguments below
  # shp = NULL, coordinates = NULL, overlap = "centroid_inside",
  
  # remove overlapping entities => to include
  
  # api = "http://gift.uni-goettingen.de/api/extended/index.php"
  ){
  
  # 1. Control ----
  
  
  # 2. Function ----
  
  # convert back tax_group text as the corresponding integer
  
  # if coordinates & shp = NULL => GIFT_spatial() is not called
  
  # Starting with GIFT_checklist_conditional()
  
  # Based on the arguments given, find different ways to run GIFT_checklist_conditional()
  # running the function twice
  
  # Then if shape, GIFT_spatial, add a line in there to only download the geojson
  # that were given by GIFT_checklist_conditional() (to reduce downloading time)
  
  # remove_overlapping comes last
  
}
