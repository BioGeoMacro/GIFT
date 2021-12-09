

library(sf)
# Get example shape
load("M:/joint_projects/Checklist DB/Geodata/Source_Layers/tdwg_lv2.RData")
plot(tdwg_lv2)
View(tdwg_lv2@data)

med <- st_read("misc/Mediterranean.shp")

med
plot(med)

save(med, file="data/med.rda")


?raster::extent




poly_test <- st_polygon(list(matrix(c(0,1,
                                 0,0,
                                 1,0,
                                 1,1,
                                 0,1), ncol = 2, byrow = TRUE)))

# extent <- c(xmin, xmax, ymin, ymax)
extent <- c(0,1,0,2)

c(extent[1],extent[3],
extent[2],extent[3],
extent[2],extent[4],
extent[1],extent[4],
extent[1],extent[3])


poly_test <- st_polygon(list(matrix(c(0,0,
                                      1,0,
                                      1,2,
                                      0,2,
                                      0,0), ncol = 2, byrow = TRUE)))

poly_test <- st_polygon(list(matrix(c(extent[1],extent[3],
                                      extent[2],extent[3],
                                      extent[2],extent[4],
                                      extent[1],extent[4],
                                      extent[1],extent[3]), ncol = 2, byrow = TRUE)))

str(poly_test)
plot(poly_test)
