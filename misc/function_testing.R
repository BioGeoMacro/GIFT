### Heavy testing

#devtools::load_all()
devtools::install()
library(GIFT)

source("misc/api.R")


ex <- GIFT_env(entity_ID = c(1,5),
               miscellaneous = c("perimeter", "biome"),
               rasterlayer = c("mn30_grd", "wc2.0_bio_30s_01"),
               sumstat = list(c("mean", "med"), "max"))


