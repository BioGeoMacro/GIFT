# 
# gift_report <- pkgnet::CreatePackageReport(pkg_name = "GIFT")
# 
# gift_report$FunctionReporter$pkg_graph
# 
# check_fct <- gift_report$FunctionReporter$pkg_graph$nodes$node
# check_fct <- check_fct[grep("check_", check_fct)]
# 
# gift_graph <- gift_report$FunctionReporter$pkg_graph$igraph
# gift_graph2 <- igraph::delete.vertices(gift_graph, c(check_fct, "make_box",
#                                                      "GIFT_versions"))
# 
# # Add links for GIFT_lists()
# gift_graph3 <- igraph::add.edges(gift_graph2, c(1,2, 1,12, 2,12))
# 
# igraph::V(gift_graph3)$type <- c(
#   "Regional checklists and species distributions", # GIFT_checklists
#   "Regional checklists and species distributions", # GIFT_checklists_conditional
#   "Regional checklists and species distributions", # GIFT_checklists_raw
#   "Spatial and environmental data", # GIFT_no_overlap
#   "Spatial and environmental data", # GIFT_spatial
#   "Species names, taxonomy and phylogeny", # GIFT_taxgroup
#   "Species names, taxonomy and phylogeny", # GIFT_taxonomy
#   "Utils", # GIFT_coverage
#   "Spatial and environmental data", # GIFT_env
#   "Spatial and environmental data", # GIFT_env_meta_misc
#   "Spatial and environmental data", # GIFT_env_meta_raster
#   "Regional checklists and species distributions", # GIFT_lists
#   "Spatial and environmental data", # GIFT_overlap
#   "Species names, taxonomy and phylogeny", # GIFT_phylogeny
#   "Species names, taxonomy and phylogeny", # GIFT_species
#   "Utils", # GIFT_references
#   "Spatial and environmental data", # GIFT_regions
#   "Utils", # GIFT_richness
#   "Spatial and environmental data", # GIFT_shapes
#   "Regional checklists and species distributions", # GIFT_species_distribution
#   "Species names, taxonomy and phylogeny", # GIFT_species_lookup
#   "Traits", # GIFT_traits
#   "Traits", # GIFT_traits_meta
#   "Traits", # GIFT_traits_raw
#   "Traits") # GIFT_traits_tax
# 
# colrs <- c("#e41a1c", "gray80", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")
# igraph::V(gift_graph3)$color <- colrs[as.factor(igraph::V(gift_graph3)$type)]
# 
# par(mar = rep(0, 4) + 0.1)
# set.seed(1)
# plot(gift_graph3,
#      edge.arrow.size = 0.2,
#      # vertex.color = "gold",
#      vertex.size = 15,
#      vertex.frame.color = "black", vertex.label.color = "black",
#      pt.cex = 20,
#      vertex.label.cex = 2, vertex.label.dist = 2, edge.curved = 0.2)
# legend(x = 1.0, y = -0.5,
#        c("Regional checklists and species distributions",
#          "Spatial and environmental data",
#          "Species names, taxonomy and phylogeny",
#          "Traits", "Utils"),
#        pch = 21, col = "#777777", pt.bg = colrs, pt.cex = 2, cex = 0.8,
#        bty = "n", ncol = 1)
# width 1600 height 1100
