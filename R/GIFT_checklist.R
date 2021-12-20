
# Relies on GIFT_checklist_conditional(); GIFT_checklist_raw(); GIFT_spatial()

GIFT <- function(
  # actual arguments user needs
  taxon_name = "Tracheophyta",
  complete_taxon = TRUE, # make a figure with orchids and angiosperms for ex.
  floristic_group = c("all", "native", "endemic", "naturalized")[2],
  # if native given => native_indicated = T in GIFT_checklist_conditional()
  
  complete_floristic = TRUE,
  
  geo_type = c("Mainland", "Island"), # Island gets you to Island, Island Group & Island Part
  # Mainland gets you to Mainland & Island/Mainland
  # c("Mainland", "Island")? gives you everything
  suit_geo = FALSE, # find better names for arguments
  
  
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
  # 
  GIFT_conditional_arg <- c("all", "native", "native and naturalized",
                            "native and historically introduced", "endangered",
                            "endemic", "naturalized", "other subset")
  
  arg_list <-
    list("all" = GIFT_conditional_arg,
         "native" = GIFT_conditional_arg[c(1:6, 8)],
         "endemic" = GIFT_conditional_arg[c(1:6, 8)],
         "naturalized" = GIFT_conditional_arg[c(1, 3, 4, 7, 8)])
  
  arg_list_second <-
    list("all" = GIFT_conditional_arg[1], # double check
         "native" = GIFT_conditional_arg[c(1:4)],
         "endemic" = GIFT_conditional_arg[c(1:4, 6)],
         "naturalized" = GIFT_conditional_arg[c(1, 3, 7)])
  
  # Converting geo_type
  entity_class <- c()
  if("Mainland" %in% geo_type){
    entity_class <- append(entity_class, c("Island/Mainland", "Mainland"))
  }
  if("Island" %in% geo_type){
    entity_class <- append(entity_class, c("Island", "Island Group",
                                           "Island Part"))
  } 
  
  # If the user asks for floristic_group = native & geo_type = Island
  # In GIFT_checklist_conditional()
  lists <- GIFT::GIFT_checklist_conditional(
    taxon_name = taxon_name,
    ref_included = arg_list[[floristic_group]],
    entity_class = entity_class,
    native_indicated = (floristic_group == "native"),
    natural_indicated = (floristic_group == "naturalized"),
    end_ref = (floristic_group == "endemic"),
    end_list = FALSE,
    suit_geo = suit_geo,
    complete_taxon = complete_taxon,
    api = api)
  
  # If complete_floristic == TRUE => running _conditional() a second time
  # subsetting first call with the entity_IDs got in the second one
  if(complete_floristic == TRUE){
    floristic_subset <- GIFT::GIFT_checklist_conditional(
      taxon_name = taxon_name,
      ref_included = arg_list_second[[floristic_group]],
      entity_class = entity_class,
      native_indicated = (floristic_group == "native"),
      natural_indicated = (floristic_group == "naturalized"),
      end_ref = (floristic_group == "endemic"),
      end_list = FALSE,
      suit_geo = suit_geo,
      complete_taxon = complete_taxon,
      api = api)
    
    # Subset
    lists <- lists[which(lists$entity_ID %in% floristic_subset$entity_ID), ]
  }
  
  # When downloading the species, this whole filtering process has to happen again
  # Output of the function: species distribution in lists AND metadata for lists
  
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
