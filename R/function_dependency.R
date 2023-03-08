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
# igraph::V(gift_graph2)$type <- c(
#   "Checklists/distribution", # GIFT_checklists
#   "Checklists/distribution", # GIFT_checklists_conditional
#   "Checklists/distribution", # GIFT_checklists_raw
#   "Spatial", # GIFT_no_overlap
#   "Spatial", # GIFT_spatial
#   "Species taxonomy/phylogeny", # GIFT_taxgroup
#   "Species taxonomy/phylogeny", # GIFT_taxonomy
#   "Traits", # GIFT_coverage
#   "Spatial", # GIFT_env
#   "Spatial", # GIFT_env_meta_misc
#   "Spatial", # GIFT_env_meta_raster
#   "Metadata", # GIFT_lists
#   "Spatial", # GIFT_overlap
#   "Phylogeny", # GIFT_phylogeny
#   "Species taxonomy/phylogeny", # GIFT_species
#   "Metadata", # GIFT_references
#   "Spatial", # GIFT_regions
#   "Metadata", # GIFT_richness
#   "Spatial", # GIFT_shapes
#   "Checklists/distribution", # GIFT_species_distribution
#   "Species taxonomy/phylogeny", # GIFT_species_lookup
#   "Traits", # GIFT_traits
#   "Traits", # GIFT_traits_meta
#   "Traits", # GIFT_traits_raw
#   "Traits") # GIFT_traits_tax
# 
# colrs <- c("#e41a1c", "gray80", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")
# igraph::V(gift_graph2)$color <- colrs[as.factor(igraph::V(gift_graph2)$type)]
# 
# par(mar = rep(0, 4) + 0.1)
# set.seed(1)
# plot(gift_graph2,
#      edge.arrow.size = 0.2,
#      # vertex.color = "gold",
#      vertex.size = 15,
#      vertex.frame.color = "black", vertex.label.color = "black",
#      pt.cex = 20,
#      vertex.label.cex = 1, vertex.label.dist = 2, edge.curved = 0.2)
# legend(x = 1.0, y = -0.5,
#        c("Checklists/distribution", "Metadata", "Phylogeny", "Spatial data",
#          "Species taxonomy/phylogeny", "Traits"),
#        pch = 21, col = "#777777", pt.bg = colrs, pt.cex = 2, cex = 0.8,
#        bty = "n", ncol = 1)
# width 1500 height 1000
