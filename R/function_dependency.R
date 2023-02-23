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
#                                                      "GIFT_version"))
# 
# igraph::V(gift_graph2)$type <- c(
#   "Checklists", # GIFT_checklist
#   "Checklists", # GIFT_checklist_conditional
#   "Checklists", # GIFT_checklist_raw
#   "Spatial", # GIFT_no_overlap
#   "Spatial", # GIFT_spatial
#   "Species distribution", # GIFT_taxgroup
#   "Species distribution", # GIFT_taxonomy
#   "Traits", # GIFT_coverage
#   "Spatial", # GIFT_env
#   "Spatial", # GIFT_env_meta_misc
#   "Spatial", # GIFT_env_meta_raster
#   "Metadata", # GIFT_lists
#   "Spatial", # GIFT_overlap
#   "Phylogeny", # GIFT_phylogeny
#   "Species distribution", # GIFT_species
#   "Metadata", # GIFT_references
#   "Spatial", # GIFT_regions
#   "Metadata", # GIFT_richness
#   "Spatial", # GIFT_shape
#   "Species distribution", # GIFT_species_distribution
#   "Species distribution", # GIFT_species_lookup
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
# legend(x = 1.5, y = -0.5,
#        c("Checklists", "Metadata", "Phylogeny", "Spatial data",
#          "Species distribution", "Traits"),
#        pch = 21, col = "#777777", pt.bg = colrs, pt.cex = 2, cex = 0.8,
#        bty = "n", ncol = 1)
