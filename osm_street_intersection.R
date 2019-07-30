# ---
#
# verify type of street related to the collision
#
# ---
library(sf)
rm(list=ls())
setwd("E:/Documents/CICLO/Projetos/Analise de Colisoes/")

bateu <- read.csv("colisoes/sistema-bateu/dados/csv/edited/Import_coordinates_bicicleta.csv")
bici <- read.csv("colisoes/sistema-bateu/dados/csv/Import_coordinates.csv")
head(bateu)
road <- read_sf("C:/Users/João Bazzo/Dropbox/Infraestrutura Cicloviária no Estado do Paraná/Joao/roads/shp/cwb.shp")
head(road)
# ---
# manipulation
# ---
crs = st_crs(4326)
aux <- list()
for(i in 1:length(bateu$lat)){aux[[i]] <- st_point(c(bateu$lat[i],bateu$long[i]))}
aux1 <- st_sfc(aux)
shp_bateu <- st_sf(geom=aux1,crs=crs)
# road

# ---
# intersection
# ---
st_intersection(road$geometry,shp_bateu[1]) 

a0 <- as.matrix(data.frame(bateu$lat[1:5],bateu$long[1:5]))
a1 <- st_multipoint(a0)
a2 <- st_sfc(a1)
a3 <- st_sf(b=1, geom = a2, crs = crs)
a3

pts = matrix(1:15, , 3)
(mp2 = st_multipoint(pts))
(mp3 = st_multipoint(pts, "XYM"))
