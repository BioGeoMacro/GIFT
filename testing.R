
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

# decrease memory usage in php to reasonable amount: https://stackoverflow.com/questions/415801/allowed-memory-size-of-33554432-bytes-exhausted-tried-to-allocate-43148176-byte
