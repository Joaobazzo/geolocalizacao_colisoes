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
# manipulation ####
# ---
crs = st_crs(4326)
aux <- list()
for(i in 1:length(bateu$lat)){aux[[i]] <- st_point(c(bateu$long[i],bateu$lat[i]))}
aux1 <- st_sfc(aux)
shp_bateu <- st_sf(bateu,geom=aux1,crs=crs)
# road

# ---
# intersection ####
# ---
a <- road$geometry; b <- shp_bateu
shp_bateu$name <- c()
shp_bateu$fclass <- c()
shp_bateu$oneway <- c()
shp_bateu$maxspeed <- c()
for(i in 1:length(shp_bateu$Tipo.Registro)){ # 
  d <- st_nearest_feature(x=b[i,],y=a)
  # road
  aux <- road[d,c("name","fclass","oneway","maxspeed")]
  aux <- as.data.frame(aux)[c(1:4)]
  shp_bateu$name[i] <- aux$name
  shp_bateu$fclass[i] <- aux$fclass
  shp_bateu$oneway[i] <- aux$oneway
  shp_bateu$maxspeed[i] <- aux$maxspeed
}
# --
# saving ####
# --
write_sf(shp_bateu,
         "geolocation_github-repo/geolocalizacao_colisoes/pre-processed_data/shp/bicycle_crash_osmdata-merged.shp")
break()