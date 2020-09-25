
devtools::load_all()


?env_raster

get_credentials()

test <- env_raster()



traits <- DB_get_traits(c("1.2.1","2.1"), bias_deriv = FALSE, bias_ref = FALSE, restricted=FALSE)

traits_restricted <- DB_get_traits(c("1.2.1","2.1.1"), bias_deriv = FALSE, bias_ref = FALSE, restricted=TRUE)
traits_restricted <- DB_get_traits(c("1.2.1","2.1.1"), bias_deriv = TRUE, bias_ref = FALSE, restricted=TRUE)
traits_restricted <- DB_get_traits(c("1.2.1","2.1.1"), bias_deriv = TRUE, bias_ref = TRUE, restricted=TRUE)



trait_IDs <- c("1.2.1","2.1.1")


species <- species_names()

names(traits)


### TODO

# decrease memory usage in php to reasonable amount: https://stackoverflow.com/questions/415801/allowed-memory-size-of-33554432-bytes-exhausted-tried-to-allocate-43148176-byte



# continue with db_get_checklists_conditional

# Two parts: 1) getting list_set and ") getting actual checklists (in individual function to be able to run it with broader conditions)

# continue going through examples

# ideas:

# 1) checklist for coordinates/shape

# 2) overlap/uncertainty (later)




list_set_new_rst <- read_json(paste0(
  "http://",credentials[[1]], ":",credentials[[2]],
  "@gift.uni-goettingen.de/api/extended/index.php?query=lists&restricted=1"),
  simplifyVector = TRUE)

list_set_new <- read_json(paste0(
  "http://",credentials[[1]], ":",credentials[[2]],
  "@gift.uni-goettingen.de/api/extended/index.php?query=lists&restricted=0"),
  simplifyVector = TRUE)
