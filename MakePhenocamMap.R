
library(ggmap)
library(sp)

#phenocam points
pheno_coords <- read.csv("~/Documents/Project_ZombieCreosote/PhenoCamZombieSurvey.csv", strip.white=T, skip=3)

pheno_coords.pts <- SpatialPointsDataFrame(pheno_coords[,c("Lat", "Lon")], pheno_coords)

#plot the  hybrid Google Maps basemap
map <- qmap("center", zoom = 10, maptype = 'hybrid')
#plot the crime points on top
map <- map + geom_point(data = pheno_coords, aes(x = Lon, y = Lat), color="red", alpha=0.5)
map
