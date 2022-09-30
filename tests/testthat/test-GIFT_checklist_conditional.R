# Tests for valid outputs ----

# Output should be a dataframe with 16 columns
test_that("data frame output format",
          {
            ex <-
              GIFT_checklist_conditional(
                taxon_name = "Embryophyta",
                ref_included = c("all", "native", "native and naturalized",
                                 "native and historically introduced",
                                 "endangered", "endemic", "naturalized",
                                 "other subset")[1:4],
                type_ref = c("Account", "Catalogue", "Checklist","Flora",
                             "Herbarium collection", "Key", "Red list",
                             "Report", "Species Database",
                             "Survey"),
                entity_class = c("Island", "Island/Mainland", "Mainland",
                                 "Island Group",
                                 "Island Part"),
                native_indicated = FALSE, natural_indicated = FALSE,
                end_ref = FALSE, end_list = FALSE, suit_geo = TRUE,
                complete_taxon = TRUE, list_set = NULL, taxonomy = NULL)
            
            expect_s3_class(ex, "data.frame")
            expect_identical(ncol(ex), c(16L))
            
          })

# Tests for invalid inputs ----

expect_error(
  GIFT_checklist_conditional(taxon_name =  NA),
  "taxon_name is incorrect. It must be a character string among one of
         the taxonomic groups available in GIFT. To check them all, run
         'GIFT_taxonomy()'.", fixed = TRUE)

expect_error(
  GIFT_checklist_conditional(ref_included = NA),
  "'ref_included' must be a character string stating what information
           should be available in the lists you retrieve (e.g. only references
           where endemic status is indicated). Available options are 'all',
           'native', 'native and naturalized',
           'native and historically introduced', 'endangered',
           'endemic', 'naturalized', 'other subset'", fixed = TRUE)

expect_error(
  GIFT_checklist_conditional(type_ref =  NA),
  "'type_ref' must be a character string stating what type of
    references you want to retrieve. Available options are 'Account',
    'Catalogue', 'Checklist','Flora', 'Herbarium collection', 'Key',
    'Red list', 'Report', 'Species Database', 'Survey'", fixed = TRUE)

expect_error(
  GIFT_checklist_conditional(entity_class = NA),
  "'entity_class' must be a character string stating what class of
    polygons you want to retrieve. Available options are 'Island',
    'Island/Mainland', 'Mainland', 'Island Group', 'Island Part'.",
  fixed = TRUE)

expect_error(
  GIFT_checklist_conditional(taxon_name =  NA),
  "taxon_name is incorrect. It must be a character string among one of
         the taxonomic groups available in GIFT. To check them all, run
         'GIFT_taxonomy()'.", fixed = TRUE)

expect_error(
  GIFT_checklist_conditional(native_indicated = NA),
  "'native_indicated' must be a boolean stating if you want the
         native status of species to be available.", fixed = TRUE)

expect_error(
  GIFT_checklist_conditional(natural_indicated = NA),
  "'natural_indicated' must be a boolean stating if you want to
         know whether species were naturalized or not.", fixed = TRUE)

expect_error(
  GIFT_checklist_conditional(end_ref =  NA),
  "'end_ref' must be a boolean stating if you want the endemic
         status at the reference level to be available.", fixed = TRUE)

expect_error(
  GIFT_checklist_conditional(end_list =  NA),
  "'end_list' must be a boolean stating if you want the endemic
         status at the list level to be available.", fixed = TRUE)

expect_error(
  GIFT_checklist_conditional(suit_geo =  NA),
  "'suit_geo' must be a boolean stating if you want to retrieve
         lists associated to a suitable polygon or not.", fixed = TRUE)

expect_error(
  GIFT_checklist_conditional(complete_taxon =  NA),
  "'complete_taxon' must be a boolean stating if you want to retrieve
         references that cover entirely or not the required taxonomic group.",
  fixed = TRUE)

expect_error(
  GIFT_checklist_conditional(api =  NA),
  "api must be a character string indicating which API to use.", fixed = TRUE)

expect_error(
  GIFT_checklist_conditional(GIFT_version =  NA),
  "'GIFT_version' must be a character string stating what version
    of GIFT you want to use. Available options are 'latest' and the different
           versions.", fixed = TRUE)

expect_error(
  GIFT_checklist_conditional(taxonomy =  data.frame(a = 1)),
  "Taxonomy must be a dataframe with specific column names.
         See GIFT_taxonomy().", fixed = TRUE)
