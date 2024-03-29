# 
# # Visible global definition
# ne_coastline <- ne_countries <- st_wrap <- NULL
# 
# # Needed objects ----------------------------------------------------------
# library("dplyr")
# library("ggplot2")
# library("sf")
# library("rnaturalearth")
# library("rnaturalearthdata")
# library("tidyr")
# library("patchwork")
# 
# world <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf")
# world_countries <- rnaturalearth::ne_countries(scale = "medium",
#                                                returnclass = "sf")
# # Fixing polygons crossing dateline
# world <- sf::st_wrap_dateline(world)
# world_countries <- sf::st_wrap_dateline(world_countries)
# 
# # Eckert IV projection
# eckertIV <-
#   "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# 
# # Background box
# xmin <- sf::st_bbox(world)[["xmin"]]; xmax <- st_bbox(world)[["xmax"]]
# ymin <- sf::st_bbox(world)[["ymin"]]; ymax <- st_bbox(world)[["ymax"]]
# bb <- sf::st_union(sf::st_make_grid(st_bbox(c(xmin = xmin,
#                                               xmax = xmax,
#                                               ymax = ymax,
#                                               ymin = ymin),
#                                             crs = st_crs(4326)),
#                                     n = 100))
# 
# # Equator line
# equator <- sf::st_linestring(matrix(c(-180, 0, 180, 0), ncol = 2,
#                                     byrow = TRUE))
# equator <- sf::st_sfc(equator, crs = st_crs(world))
# 
# # Color code from Barthlott 2007
# hexcode_barthlott2007 <- c("#fbf9ed", "#f3efcc", "#f6e39e", "#cbe784",
#                            "#65c66a", "#0e8d4a", "#4a6fbf",
#                            "#b877c2", "#f24dae", "#ed1c24")
# 
# # Retrieving all shapefiles -----------------------------------------------
# gift_shapes <- GIFT_shapes()
# 
# # Angiosperm richness map -------------------------------------------------
# angio_rich <- GIFT_richness(taxon_name = "Angiospermae")
# 
# rich_map <- dplyr::left_join(gift_shapes, angio_rich, by = "entity_ID") %>%
#   dplyr::filter(stats::complete.cases(total))
# 
# angio_rich_map <- ggplot2::ggplot(world) +
#   ggplot2::geom_sf(color = "gray50") +
#   ggplot2::geom_sf(data = rich_map, aes(fill = total + 1)) +
#   ggplot2::scale_fill_viridis_c(
#     "Species number\n(log-transformed)",
#     trans = "log10", labels = scales::number_format(accuracy = 1)) +
#   ggplot2::labs(title = "Angiosperms", subtitle = "Projection EckertIV") +
#   ggplot2::coord_sf(crs = eckertIV) +
#   ggplot2::theme_void()
# 
# ggplot2::ggsave("man/figures/angio_rich_map.png",
#                 plot = angio_rich_map,
#                 width = 20, height = 10, dpi = 150, units = "cm",
#                 bg = "white")
# 
# ## Fancy version ----
# angio_rich_map2 <- ggplot2::ggplot(world) +
#   ggplot2::geom_sf(data = bb, fill = "aliceblue") +
#   ggplot2::geom_sf(data = equator, color = "gray50", linetype = "dashed",
#                    linewidth = 0.1) +
#   ggplot2::geom_sf(data = world_countries, fill = "antiquewhite1",
#                    color = NA) +
#   ggplot2::geom_sf(color = "gray50", linewidth = 0.1) +
#   ggplot2::geom_sf(data = bb, fill = NA) +
#   ggplot2::geom_sf(data = rich_map,
#                    aes(fill = ifelse(rich_map$entity_class %in%
#                                        c("Island/Mainland", "Mainland",
#                                          "Island Group", "Island Part"),
#                                      total + 1, NA)),
#                    size = 0.1) +
#   ggplot2::geom_point(data = rich_map,
#                       aes(color = ifelse(rich_map$entity_class %in%
#                                            c("Island"),
#                                          total + 1, NA),
#                           geometry = geometry),
#                       stat = "sf_coordinates", size = 1, stroke = 0.5) +
#   ggplot2::scale_color_gradientn(
#     "Species number", trans = "log10", limits = c(1, 40000),
#     colours = hexcode_barthlott2007,
#     # colours = RColorBrewer::brewer.pal(5, name = "Greens"),
#     breaks = c(1, 10, 100, 1000, 10000, 40000),
#     labels = c(1, 10, 100, 1000, 10000, 40000),
#     na.value = "transparent") +
#   ggplot2::scale_fill_gradientn(
#     "Species number", trans = "log10", limits = c(1, 40000),
#     colours = hexcode_barthlott2007,
#     # colours = RColorBrewer::brewer.pal(5, name = "Greens"),
#     breaks = c(1, 10, 100, 1000, 10000, 40000),
#     labels = c(1, 10, 100, 1000, 10000, 40000),
#     na.value = "transparent") +
#   ggplot2::labs(title = "Angiosperms", subtitle = "Projection EckertIV") +
#   ggplot2::coord_sf(crs = eckertIV) +
#   ggplot2::theme_void()
# 
# ggplot2::ggsave("man/figures/angio_rich_map2.png",
#                 plot = angio_rich_map2,
#                 width = 20, height = 10, dpi = 150, units = "cm",
#                 bg = "white")
# 
# ## Mediterranean subset ---------------------------------------------------
# med_shape <- gift_shapes[which(gift_shapes$entity_ID %in%
#                                  unique(medit[[2]]$entity_ID)), ]
# 
# med_rich <- angio_rich[which(angio_rich$entity_ID %in%
#                                unique(medit[[2]]$entity_ID)), ]
# 
# med_map <- dplyr::left_join(med_shape, med_rich, by = "entity_ID") %>%
#   dplyr::filter(stats::complete.cases(total))
# 
# angio_medit <- ggplot2::ggplot(world) +
#   ggplot2::geom_sf(color = "gray50") +
#   ggplot2::geom_sf(data = western_mediterranean,
#                    fill = "darkblue", color = "black", alpha = 0.1,
#                    size = 1) +
#   ggplot2::geom_sf(data = med_map, aes(fill = total)) +
#   ggplot2::scale_fill_viridis_c("Species number") +
#   ggplot2::labs(title = "Angiosperms in the Western Mediterranean basin") +
#   ggplot2::lims(x = c(-20, 20), y = c(24, 48)) +
#   ggplot2::theme_void()
# 
# ggplot2::ggsave("man/figures/angio_medit.png",
#                 plot = angio_medit,
#                 width = 20, height = 10, dpi = 150, units = "cm",
#                 bg = "white")
# 
# # Anemone nemorosa --------------------------------------------------------
# anemone_distr <- GIFT_species_distribution(
#   genus = "Anemone", epithet = "nemorosa", aggregation = TRUE)
# 
# anemone_statuses <- anemone_distr %>%
#   dplyr::mutate(native = ifelse(native == 1, "native", "non-native"),
#                 naturalized = ifelse(naturalized == 1, "naturalized",
#                                      "non-naturalized"),
#                 endemic_list = ifelse(endemic_list == 1, "endemic_list",
#                                       "non-endemic_list")) %>%
#   dplyr::select(entity_ID, native, naturalized, endemic_list)
# 
# # We rename the statuses based on the distinct combinations
# anemone_statuses <- anemone_statuses %>%
#   dplyr::mutate(Status = case_when(
#     native == "native" & naturalized == "non-naturalized" ~ "native",
#     native == "native" & is.na(naturalized) ~ "native",
#     native == "non-native" & is.na(naturalized) ~ "non-native",
#     native == "non-native" & naturalized == "naturalized" ~ "naturalized",
#     native == "non-native" & naturalized == "non-naturalized" ~ "non-native",
#     is.na(native) & is.na(naturalized) ~ "unknown"
#   ))
# 
# # Merge with the shapes
# anemone_shape <- gift_shapes[which(gift_shapes$entity_ID %in%
#                                      unique(anemone_distr$entity_ID)), ]
# anemone_map <- dplyr::left_join(anemone_shape, anemone_statuses,
#                                 by = "entity_ID")
# 
# # Area of distribution with floristic status
# anemone_plot <- ggplot2::ggplot(world) +
#   ggplot2::geom_sf(color = "gray70") +
#   ggplot2::geom_sf(data = anemone_map, color = "black",
#                    aes(fill = as.factor(Status))) +
#   ggplot2::scale_fill_brewer("Status", palette = "Set2") +
#   ggplot2::labs(title = expression(paste("Distribution map of ",
#                                          italic("Anemone nemorosa"))),
#                 subtitle = "Unprojected (GCS: WGS84)") +
#   ggplot2::lims(x = c(-65, 170), y = c(-45, 70)) +
#   ggplot2::theme_void()
# 
# ggplot2::ggsave("man/figures/anemone_map.png",
#                 plot = anemone_plot,
#                 width = 20, height = 10, dpi = 150, units = "cm",
#                 bg = "white")
# 
# ## Fancy version ----------------------------------------------------------
# anemone_map_plot_bg_parts <-
#   ggplot2::ggplot(world) +
#   ggplot2::geom_sf(data = bb, fill = "aliceblue", color = NA) +
#   ggplot2::geom_sf(data = equator, color = "gray50", linetype = "dashed",
#                    linewidth = 0.1) +
#   ggplot2::geom_sf(data = world_countries, fill = "antiquewhite1",
#                    color = NA) +
#   ggplot2::geom_sf(color = "gray50", linewidth = 0.1) +
#   ggplot2::geom_sf(data = bb, fill = NA) +
#   ggplot2::geom_sf(data = anemone_map, color = "black",
#                    aes(fill = as.factor(Status))) +
#   ggplot2::scale_fill_manual("Status",
#                              values = c("native" = "#2c7bb6",
#                                         "naturalized" = "#d7191c",
#                                         "non-native" = "#fdae61",
#                                         "unknown" = "#abd9e9")) +
#   ggplot2::labs(title = expression(paste("b) Distribution map of ",
#                                          italic("Anemone nemorosa")))) +
#   ggplot2::theme_void() +
#   ggplot2::theme(axis.title = element_blank(),
#                  axis.text = element_blank(),
#                  axis.ticks = element_blank())
# 
# anemone_plot2 <-
#   (anemone_map_plot_bg_parts +
#      ggplot2::lims(x = c(-69, 61), y = c(37, 70)) + # Europe & Newfoundland
#      ggplot2::theme(panel.border = element_rect(fill = NA,
#                                                 linewidth = 1)) +
#      ggplot2::theme(legend.position = "bottom")) +
#   (anemone_map_plot_bg_parts +
#      ggplot2::lims(x = c(165, 178), y = c(-47, -35)) + # new zealand
#      ggplot2::labs(title = "") +
#      ggplot2::guides(fill = "none") +
#      ggplot2::theme(panel.border = element_rect(fill = NA,
#                                                 linewidth = 1))) +
#   patchwork::plot_layout()
# 
# ggplot2::ggsave("man/figures/anemone_map2.png",
#                 plot = anemone_plot2,
#                 width = 20, height = 10, dpi = 150, units = "cm",
#                 bg = "white")
# 
# # Height coverage ---------------------------------------------------------
# angio_height <- GIFT_coverage(what = "trait_coverage",
#                               taxon_name = "Angiospermae",
#                               trait_ID = "1.6.2")
# 
# angio_height_shape <-
#   gift_shapes[which(gift_shapes$entity_ID %in%
#                       unique(angio_height$entity_ID)), ]
# 
# angio_height_map <- dplyr::left_join(
#   angio_height_shape, angio_height, by = "entity_ID")
# 
# angio_height_map <-
#   angio_height_map[complete.cases(angio_height_map$native), ]
# 
# angio_height_plot <- ggplot2::ggplot(world) +
#   ggplot2::geom_sf(color = "gray50") +
#   ggplot2::geom_sf(data = angio_height_map[complete.cases(
#     angio_height_map$native), ],
#     aes(fill = native)) +
#   ggplot2::scale_fill_viridis_c("Coverage (%)") +
#   ggplot2::labs(
#     title = "Coverage for maximal vegetative height of Angiosperms",
#     subtitle = "Projection EckertIV") +
#   ggplot2::coord_sf(crs = eckertIV) +
#   ggplot2::theme_void()
# 
# ggplot2::ggsave("man/figures/angio_height_plot.png",
#                 plot = angio_height_plot,
#                 width = 20, height = 10, dpi = 150, units = "cm",
#                 bg = "white")
# 
# ## Fancy version ----------------------------------------------------------
# angio_height_plot2 <- ggplot2::ggplot(world) +
#   ggplot2::geom_sf(data = bb, fill = "aliceblue") +
#   ggplot2::geom_sf(data = equator, color = "gray50", linetype = "dashed",
#                    linewidth = 0.1) +
#   ggplot2::geom_sf(data = world_countries, fill = "antiquewhite1",
#                    color = NA) +
#   ggplot2::geom_sf(color = "gray50", linewidth = 0.1) +
#   ggplot2::geom_sf(data = bb, fill = NA) +
#   ggplot2::geom_sf(data = angio_height_map,
#                    aes(fill = ifelse(angio_height_map$entity_class %in%
#                                        c("Island/Mainland", "Mainland",
#                                          "Island Group", "Island Part"),
#                                      100*native, NA)), size = 0.1) +
#   ggplot2::geom_point(data = angio_height_map,
#                       aes(color = ifelse(angio_height_map$entity_class %in%
#                                            c("Island"),
#                                          100*native, NA),
#                           geometry = geometry),
#                       stat = "sf_coordinates", size = 1, stroke = 0.5) +
#   ggplot2::scale_color_gradientn(
#     "Coverage (%)",
#     colours = rev(RColorBrewer::brewer.pal(9, name = "PuBuGn")),
#     limits = c(0, 100),
#     na.value = "transparent") +
#   ggplot2::scale_fill_gradientn(
#     "Coverage (%)",
#     colours = rev(RColorBrewer::brewer.pal(9, name = "PuBuGn")),
#     limits = c(0, 100),
#     na.value = "transparent") +
#   ggplot2::labs(title = "Coverage for maximal vegetative height of Angiosperms",
#                 subtitle = "Projection EckertIV") +
#   ggplot2::coord_sf(crs = eckertIV) +
#   ggplot2::theme_void()
# 
# ggplot2::ggsave("man/figures/angio_height_plot2.png",
#                 plot = angio_height_plot2,
#                 width = 20, height = 10, dpi = 150, units = "cm",
#                 bg = "white")
# 
# # MAT ---------------------------------------------------------------------
# world_temp <- GIFT_env(entity_ID = unique(angio_rich$entity_ID),
#                        rasterlayer = c("wc2.0_bio_30s_01"),
#                        sumstat = c("mean"))
# 
# temp_shape <- gift_shapes[which(gift_shapes$entity_ID %in%
#                                   unique(angio_rich$entity_ID)), ]
# 
# temp_map <- dplyr::left_join(temp_shape, world_temp, by = "entity_ID")
# 
# temp_plot <- ggplot2::ggplot(world) +
#   ggplot2::geom_sf(color = "gray50") +
#   ggplot2::geom_sf(data = temp_map, aes(fill = mean_wc2.0_bio_30s_01)) +
#   ggplot2::scale_fill_viridis_c("Celsius degrees") +
#   ggplot2::labs(title = "Average temperature",
#                 subtitle = "Projection EckertIV") +
#   ggplot2::coord_sf(crs = eckertIV) +
#   ggplot2::theme_void()
# 
# ggplot2::ggsave("man/figures/temp_plot.png",
#                 plot = temp_plot,
#                 width = 20, height = 10, dpi = 150, units = "cm",
#                 bg = "white")
# 
# ## Fancy version ----------------------------------------------------------
# 
# temp_plot2 <- ggplot2::ggplot(world) +
#   ggplot2::geom_sf(data = bb, fill = "aliceblue") +
#   ggplot2::geom_sf(data = equator, color = "gray50", linetype = "dashed",
#                    linewidth = 0.1) +
#   ggplot2::geom_sf(data = world_countries, fill = "antiquewhite1",
#                    color = NA) +
#   ggplot2::geom_sf(color = "gray50", linewidth = 0.1) +
#   ggplot2::geom_sf(data = bb, fill = NA) +
#   ggplot2::geom_sf(data = temp_map,
#                    aes(fill = ifelse(temp_map$entity_class %in%
#                                        c("Island/Mainland", "Mainland",
#                                          "Island Group", "Island Part"),
#                                      mean_wc2.0_bio_30s_01, NA)), size = 0.1) +
#   ggplot2::geom_point(data = temp_map,
#                       aes(color = ifelse(temp_map$entity_class %in%
#                                            c("Island"),
#                                          mean_wc2.0_bio_30s_01, NA),
#                           geometry = geometry),
#                       stat = "sf_coordinates", size = 1, stroke = 0.5) +
#   ggplot2::scale_color_gradientn(
#     "°C",
#     colours = RColorBrewer::brewer.pal(9, name = "Reds"),
#     limits = c(-20, 30),
#     na.value = "transparent") +
#   ggplot2::scale_fill_gradientn(
#     "°C",
#     colours = RColorBrewer::brewer.pal(9, name = "Reds"),
#     limits = c(-20, 30),
#     na.value = "transparent") +
#   ggplot2::labs(title = "Average temperature",
#                 subtitle = "Projection EckertIV") +
#   ggplot2::coord_sf(crs = eckertIV) +
#   ggplot2::theme_void()
# 
# ggplot2::ggsave("man/figures/temp_plot2.png",
#                 plot = temp_plot2,
#                 width = 20, height = 10, dpi = 150, units = "cm",
#                 bg = "white")
# 
# # Phylogeny ---------------------------------------------------------------
# phy <- GIFT_phylogeny(clade = "Tracheophyta", GIFT_version = "beta")
# tax <- GIFT_taxonomy(GIFT_version = "beta")
# gift_sp <- GIFT_species(GIFT_version = "beta")
# 
# gf <- GIFT_traits(trait_IDs = "1.2.1", agreement = 0.66, bias_ref = FALSE,
#                   bias_deriv = FALSE, GIFT_version = "beta")
# 
# # Replacing space with _ for the species names
# gf$work_species <- gsub(" ", "_", gf$work_species, fixed = TRUE)
# 
# # Retrieving family of each species
# sp_fam <- GIFT_taxgroup(work_ID = unique(gift_sp$work_ID),
#                         taxon_lvl = "family", GIFT_version = "beta")
# sp_genus_fam <- data.frame(
#   work_ID = unique(gift_sp$work_ID),
#   work_species = unique(gift_sp$work_species),
#   family = sp_fam)
# sp_genus_fam <- dplyr::left_join(sp_genus_fam,
#                                  gift_sp[, c("work_ID", "work_genus")],
#                                  by = "work_ID")
# colnames(sp_genus_fam)[colnames(sp_genus_fam) == "work_genus"] <- "genus"
# 
# # Problem with hybrid species on the tip labels of the phylo tree
# phy$tip.label[substring(phy$tip.label, 1, 2) == "x_"] <-
#   substring(phy$tip.label[substring(phy$tip.label, 1, 2) == "x_"],
#             3,
#             nchar(phy$tip.label[substring(phy$tip.label, 1, 2) == "×_"]))
# 
# phy$tip.label[substring(phy$tip.label, 1, 2) == "×_"] <-
#   substring(phy$tip.label[substring(phy$tip.label, 1, 2) == "×_"],
#             3,
#             nchar(phy$tip.label[substring(phy$tip.label, 1, 2) == "×_"]))
# 
# sp_genus_fam <- dplyr::left_join(sp_genus_fam,
#                                  gf[, c("work_ID", "trait_value_1.2.1")],
#                                  by = "work_ID")
# 
# genus_gf <- sp_genus_fam %>%
#   dplyr::group_by(genus) %>%
#   dplyr::mutate(prop_gf = round(100*sum(is.na(trait_value_1.2.1))/n(),
#                                 2)) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(-work_ID, -work_species, -family, -trait_value_1.2.1) %>%
#   dplyr::distinct(.keep_all = TRUE)
# 
# fam_gf <- sp_genus_fam %>%
#   dplyr::group_by(family) %>%
#   dplyr::mutate(prop_gf = round(100*sum(is.na(trait_value_1.2.1))/n(),
#                                 2)) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(-work_ID, -work_species, -genus, -trait_value_1.2.1) %>%
#   dplyr::distinct(.keep_all = TRUE)
# 
# sp_genus_fam$species <- gsub("([[:punct:]])|\\s+", "_",
#                              sp_genus_fam$work_species)
# 
# # Keeping one species per genus only
# one_sp_per_gen <- data.frame()
# for(i in 1:n_distinct(sp_genus_fam$genus)){ # loop over genera
#   # Focal genus
#   focal_gen <- unique(sp_genus_fam$genus)[i]
#   # All species in that genus
#   gen_sp_i <- sp_genus_fam[which(sp_genus_fam$genus == focal_gen),
#                            "species"]
#   # Species from the genus available in the phylogeny
#   gen_sp_i <- gen_sp_i[gen_sp_i %in% phy$tip.label]
#   # Taking the first one (if at least one is available)
#   gen_sp_i <- gen_sp_i[1]
#   
#   one_sp_per_gen <- rbind(one_sp_per_gen,
#                           data.frame(species = gen_sp_i,
#                                      genus = focal_gen))
# }
# 
# # Adding the trait coverage per genus
# one_sp_per_gen <- dplyr::left_join(one_sp_per_gen, genus_gf, by = "genus")
# 
# # Adding the trait coverage per family
# one_sp_per_gen <- dplyr::left_join(
#   one_sp_per_gen,
#   sp_genus_fam[!duplicated(sp_genus_fam$genus),
#                c("genus", "family")],
#   by = "genus")
# colnames(one_sp_per_gen)[colnames(one_sp_per_gen) == "prop_gf"] <-
#   "prop_gf_gen"
# one_sp_per_gen <- left_join(one_sp_per_gen, fam_gf, by = "family")
# colnames(one_sp_per_gen)[colnames(one_sp_per_gen) == "prop_gf"] <-
#   "prop_gf_fam"
# 
# phy_gen <- ape::keep.tip(
#   phy = phy,
#   tip = one_sp_per_gen[complete.cases(one_sp_per_gen$species), "species"])
# 
# library("BiocManager")
# install("ggtree")
# library("ggtree")
# library("tidytree")
# install("ggtreeExtra")
# library("ggtreeExtra")
# 
# phy_tree_plot <- ggtree(phy_gen, color = "grey70",
#                         layout = "circular") %<+% one_sp_per_gen +
#   geom_fruit(geom = geom_tile,
#              mapping = aes(fill = prop_gf_gen),
#              width = 50,
#              offset = 0.1) +
#   geom_fruit(geom = geom_tile,
#              mapping = aes(color = prop_gf_fam, fill = prop_gf_fam),
#              width = 50,
#              offset = 0.1,
#              show.legend = FALSE) +
#   scale_color_viridis_c() +
#   scale_fill_viridis_c("Growth form availability per genus (%)") +
#   theme(legend.position = "bottom")
# 
# ggplot2::ggsave("man/figures/phy_tree_plot.png",
#                 plot = phy_tree_plot,
#                 width = 20, height = 10, dpi = 150, units = "cm",
#                 bg = "white")
# 
# # Advanced vignette -----------------------------------------------------------
# med_centroid_inside  <- GIFT_spatial(shp = western_mediterranean,
#                                      overlap = "centroid_inside")
# med_extent_intersect <- GIFT_spatial(shp = western_mediterranean,
#                                      overlap = "extent_intersect")
# med_shape_intersect <- GIFT_spatial(shp = western_mediterranean,
#                                     overlap = "shape_intersect")
# med_shape_inside <- GIFT_spatial(shp = western_mediterranean,
#                                  overlap = "shape_inside")
# 
# geodata_extent_intersect <- GIFT_shapes(med_extent_intersect$entity_ID)
# 
# geodata_shape_inside <-
#   geodata_extent_intersect[which(geodata_extent_intersect$entity_ID %in%
#                                    med_shape_inside$entity_ID), ]
# geodata_centroid_inside <-
#   geodata_extent_intersect[which(geodata_extent_intersect$entity_ID %in%
#                                    med_centroid_inside$entity_ID), ]
# geodata_shape_intersect <-
#   geodata_extent_intersect[which(geodata_extent_intersect$entity_ID %in%
#                                    med_shape_intersect$entity_ID), ]
# 
# png("man/figures/advanced_overlap.png",
#     width = 1440, height = 1440, units = "px",
#     bg = "white")
# par_overlap <- par(mfrow = c(2, 2), mai = c(0, 0.5, 1.5, 0.5))
# plot(sf::st_geometry(geodata_shape_inside),
#      col = geodata_shape_inside$entity_ID,
#      main = paste("shape inside\n",
#                   length(unique(med_shape_inside$entity_ID)),
#                   "polygons"),
#      cex.main = 5)
# plot(sf::st_geometry(western_mediterranean), lwd = 2, add = TRUE)
# 
# plot(sf::st_geometry(geodata_centroid_inside),
#      col = geodata_centroid_inside$entity_ID,
#      main = paste("centroid inside\n",
#                   length(unique(med_centroid_inside$entity_ID)),
#                   "polygons"),
#      cex.main = 5)
# points(geodata_centroid_inside$point_x, geodata_centroid_inside$point_y)
# plot(sf::st_geometry(western_mediterranean), lwd = 2, add = TRUE)
# 
# plot(sf::st_geometry(geodata_shape_intersect),
#      col = geodata_shape_intersect$entity_ID,
#      main = paste("shape intersect\n",
#                   length(unique(med_shape_intersect$entity_ID)),
#                   "polygons"),
#      cex.main = 5)
# plot(sf::st_geometry(western_mediterranean), lwd = 2, add = TRUE)
# 
# plot(sf::st_geometry(geodata_extent_intersect),
#      col = geodata_extent_intersect$entity_ID,
#      main = paste("extent intersect\n",
#                   length(unique(med_extent_intersect$entity_ID)),
#                   "polygons"),
#      cex.main = 5)
# plot(sf::st_geometry(western_mediterranean), lwd = 2, add = TRUE)
# par(par_overlap)
# dev.off()
## Overlap GMBA ---------------------------------------------------------------
# library("GIFT")
# 
# # Overlap table between GIFT and GMBA regions
# gmba_overlap <- GIFT_overlap(resource = "gmba", GIFT_version = "beta")
# library("sf")
# gmba <- st_read(
#   "../../polygon_resources/GMBA/GMBA_mountain_inventory_V1.0_ID.shp")
# geoentities <- st_read(
#   "../../polygon_resources/geoentities/geoentities_simple.shp")
# 
# st_crs(gmba) <- st_crs(geoentities)
# 
# gmba_overlap[which(gmba_overlap$entity_ID == 11861 &
#                      gmba_overlap$gmba_ID == 731), ]
# 
# library("ggplot2")
# ggplot(gmba[which(gmba$ID == 731), ]) +
#   geom_sf(color = "black", linewidth = 1) +
#   geom_sf(data = geoentities[which(geoentities$entt_ID == 11861), ],
#           fill = "black", alpha = 0.5) +
#   labs(title = paste0("GIFT region: ",
#                       geoentities[which(geoentities$entt_ID == 11861), ]$ge_ntty)) +
#   theme_void()
