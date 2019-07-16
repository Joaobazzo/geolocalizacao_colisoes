#
#
#          analise de dados do sistema BATEU, filtra e dispoe os dados de atropelamentos
#          de ciclistas em coordenadas
#
###################################################################################################
#
# versoes
#
# v.1: mar/2017 - criacao do arquivo
#
#
#
#
###################################################################################################
#
# pacotes utilizados
#
library(openxlsx)
library(stringr)
library(RColorBrewer)
library("rvest")
library(sp)
library(rgdal)
library(ggmap)
library(maptools)
library(maps)
library(ggplot2)
#
# define working directory
#
setwd("/Users/Joao Vieira/Google Drive/CICLO/04. TECNICO/a1. Análise de Colisões/sistema-bateu/dados/")
#
# abre arquivo 1
#
source_file1 <- paste0("xlsx/cut/2011_2013_v1.xlsx")
file_2011 <- read.xlsx(source_file1,sheet = 1, startRow = 1, colNames = T)
file_2012 <- read.xlsx(source_file1,sheet = 2, startRow = 1, colNames = T)
file_2013 <- read.xlsx(source_file1,sheet = 3, startRow = 1, colNames = T)
#
# abre arquivo 2
#
source_file <- paste0("xlsx/cut/2014_2016_v2.xlsx")
file_2014 <- read.xlsx(source_file,sheet = 1, startRow = 1, colNames = T)
file_2015 <- read.xlsx(source_file,sheet = 2, startRow = 1, colNames = T)
file_2016 <- read.xlsx(source_file,sheet = 3, startRow = 1, colNames = T)
#
# concatena
#
file_all <- rbind(file_2011,file_2012,file_2013,file_2014,file_2015,file_2016)
#
###################################################################################
#
# Funcao
#
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +south +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

#############################################################################
#
#  limpeza de dados
#
# a) apenas BICICLETA
file_bicicleta_v1 <- file_all[which(file_all$Tipo.de.Veículo=="BICICLETA"),]
# b) numero de protocolo igual
file_bicicleta_v2 <- file_bicicleta_v1[duplicated(file_bicicleta_v1$Protocolo.BATEU)==F,]
# c) sem info de Via Primária
file_bicicleta_v3 <- file_bicicleta_v2[-which(file_bicicleta_v2$Via.Primária=="-"),]
# d) arrumar caracteres dos numeros das Vias Primarias
file_bicicleta_v3$`Número.(KM)` <- gsub("\\.","",x = file_bicicleta_v3$`Número.(KM)`)
#View(data.frame(file_bicicleta_v3$`Número.(KM)`,gsub("\\.","",x = file_bicicleta_v3$`Número.(KM)`)))
# e) acentos circunflexos, tremas e tios
file_bicicleta_v3$Via.Primária <- str_replace(file_bicicleta_v3$Via.Primária,"Â","A")
file_bicicleta_v3$Via.Secundária <- str_replace(file_bicicleta_v3$Via.Secundária,"Â","A")
file_bicicleta_v3$Cruzamento <- str_replace(file_bicicleta_v3$Cruzamento,"Â","A")

file_bicicleta_v3$Via.Primária <- str_replace(file_bicicleta_v3$Via.Primária,"ã","a")
file_bicicleta_v3$Via.Secundária <- str_replace(file_bicicleta_v3$Via.Secundária,"ã","a")
file_bicicleta_v3$Cruzamento <- str_replace(file_bicicleta_v3$Cruzamento,"ã","a")

file_bicicleta_v3$Via.Primária <- str_replace(file_bicicleta_v3$Via.Primária,"â","a")
file_bicicleta_v3$Via.Secundária <- str_replace(file_bicicleta_v3$Via.Secundária,"â","a")
file_bicicleta_v3$Cruzamento <- str_replace(file_bicicleta_v3$Cruzamento,"â","a")

file_bicicleta_v3$Via.Primária <- str_replace(file_bicicleta_v3$Via.Primária,"ê","e")
file_bicicleta_v3$Via.Secundária <- str_replace(file_bicicleta_v3$Via.Secundária,"ê","e")
file_bicicleta_v3$Cruzamento <- str_replace(file_bicicleta_v3$Cruzamento,"ê","e")

file_bicicleta_v3$Via.Primária <- str_replace(file_bicicleta_v3$Via.Primária,"Ê","E")
file_bicicleta_v3$Via.Secundária <- str_replace(file_bicicleta_v3$Via.Secundária,"Ê","E")
file_bicicleta_v3$Cruzamento <- str_replace(file_bicicleta_v3$Cruzamento,"Ê","E")

file_bicicleta_v3$Via.Primária <- str_replace(file_bicicleta_v3$Via.Primária,"ô","o")
file_bicicleta_v3$Via.Secundária <- str_replace(file_bicicleta_v3$Via.Secundária,"ô","o")
file_bicicleta_v3$Cruzamento <- str_replace(file_bicicleta_v3$Cruzamento,"ô","o")

file_bicicleta_v3$Via.Primária <- str_replace(file_bicicleta_v3$Via.Primária,"Ô","O")
file_bicicleta_v3$Via.Secundária <- str_replace(file_bicicleta_v3$Via.Secundária,"Ô","O")
file_bicicleta_v3$Cruzamento <- str_replace(file_bicicleta_v3$Cruzamento,"Ô","O")

file_bicicleta_v3$Via.Primária <- str_replace(file_bicicleta_v3$Via.Primária,"ç","c")
file_bicicleta_v3$Via.Secundária <- str_replace(file_bicicleta_v3$Via.Secundária,"ç","c")
file_bicicleta_v3$Cruzamento <- str_replace(file_bicicleta_v3$Cruzamento,"ç","c")

file_bicicleta_v3$Via.Primária <- str_replace(file_bicicleta_v3$Via.Primária,"É","E")
file_bicicleta_v3$Via.Secundária <- str_replace(file_bicicleta_v3$Via.Secundária,"É","E")
file_bicicleta_v3$Cruzamento <- str_replace(file_bicicleta_v3$Cruzamento,"É","E")

file_bicicleta_v3$Via.Primária <- str_replace(file_bicicleta_v3$Via.Primária,"Á","A")
file_bicicleta_v3$Via.Secundária <- str_replace(file_bicicleta_v3$Via.Secundária,"Á","A")
file_bicicleta_v3$Cruzamento <- str_replace(file_bicicleta_v3$Cruzamento,"Á","A")

file_bicicleta_v3$Via.Primária <- str_replace(file_bicicleta_v3$Via.Primária,"Á","A")
file_bicicleta_v3$Via.Secundária <- str_replace(file_bicicleta_v3$Via.Secundária,"Á","A")
file_bicicleta_v3$Cruzamento <- str_replace(file_bicicleta_v3$Cruzamento,"Á","A")
# f) Cria aba "Adress" - tanto para Cruzamento, quanto para Rua
vec_ad <- c()
for (i in (1:dim(file_bicicleta_v3)[1])){
  if(file_bicicleta_v3$`Número.(KM)`[i]%in%c("-","0")){vec_ad <- append(vec_ad,file_bicicleta_v3$Cruzamento[i])}
  else{
    vec_ad <- append(vec_ad,paste(file_bicicleta_v3$Via.Primária[i],file_bicicleta_v3$`Número.(KM)`[i]))}
  print(i)
}
file_bicicleta_v3$address <- vec_ad # nova coluna endereco
#
##############################################################################
#
# 3) Obtencao das coordenadas
#
# 
lat <- c()
long <- c()
for (i in (1:dim(file_bicicleta_v3)[1])){
  # 
  # 3.a) CRUZAMENTO
  # 
  if(file_bicicleta_v3$`Número.(KM)`[i]%in%c("-","0")){
    #
    # arruma nome cruzamento
    #
    street_c1 <- str_replace_all(file_bicicleta_v3$Via.Primária[i]," ","%20")
    street_c2 <- str_replace_all(file_bicicleta_v3$Via.Secundária[i]," ","%20")
    cruz <- paste0(file_bicicleta_v3$Via.Primária[i]," X ",file_bicicleta_v3$Via.Secundária[i])
    #
    # google
    #
    #ll.visited <- geocode(cruz)
    #visit.x <- ll.visited$lon
    #visit.y <- ll.visited$lat
    #
    # html
    #
    link_c = paste0("https://geocoder.cit.api.here.com/6.2/geocode.json?city=Curitiba&street=",
                    street_c1,"%20%40%20",street_c2,"&app_id=%20zp1A511aiJ5g0CPGeXUN&app_code=%20JiQl0fTzioxqQiItVoPAsg&gen=8")
    tt_c <- read_html(link_c)
    #
    # coordenadas
    #
    tt1_c <- str_split(tt_c,"DisplayPosition")[[1]][2]
    tt2_c <- regmatches(tt1_c,gregexpr('[0-9]+',tt1_c))[[1]]
    lat_cr <- as.numeric(paste0("-",tt2_c[1],".",tt2_c[2]))
    long_cr <- as.numeric(paste0("-",tt2_c[3],".",tt2_c[4]))
    #
    # acumula valores
    #
    lat <- append(lat,lat_cr)
    long <- append(long,long_cr)
    print(paste0("Cruzamento_",i,"_____Lat=",lat_cr,"____Long=",long_cr))}
  else{
    # 
    # 3.b) RUA NORMAL
    # 
    #
    # arruma nome rua
    #
    rua <- file_bicicleta_v3$address[i]
    rua_sn <- str_trim(str_sub(rua,1,str_count(rua)-str_count(word(rua,-1))),"right")
    street_r <- paste0(str_replace_all(rua_sn," ","%20"),"%2C%20",word(rua,-1))
    #
    # html
    #
    link_r = paste0("https://geocoder.cit.api.here.com/6.2/geocode.json?city=Curitiba&street=",
                    street_r,"&app_id=zp1A511aiJ5g0CPGeXUN&app_code=%20JiQl0fTzioxqQiItVoPAsg&gen=8")
    tt_r <- read_html(link_r)
    #
    # coordenadas
    #
    tt1_r <- str_split(tt_r,"DisplayPosition")[[1]][2]
    tt2_r <- regmatches(tt1_r,gregexpr('[0-9]+',tt1_r))[[1]]
    lat_r <- as.numeric(paste0("-",tt2_r[1],".",tt2_r[2]))
    long_r <- as.numeric(paste0("-",tt2_r[3],".",tt2_r[4]))
    #
    # acumula
    #
    lat <- append(lat,lat_r)
    long <- append(long,long_r)
    #
    #
    print(paste0("Endereco_",i,"_____Lat=",lat_r,"____Long=",long_r))}
}
#######################################################################
# 
# 4) Transforma Coordenadas e Filtra
# 
file_bicicleta_v3$lat <- lat
file_bicicleta_v3$long <- long
#
long[is.na(long)] <- 0
lat[is.na(lat)] <- 0
file_bicicleta_v3$UTM_x <- LongLatToUTM(long,lat,22)$X
file_bicicleta_v3$UTM_y <- LongLatToUTM(long,lat,22)$Y
#
file_bicicleta_v3 <- file_bicicleta_v3[-which(file_bicicleta_v3$UTM_y>8000000),]
#
# 5) Exporta arquivo .kml
#
file_bicicleta_v4 <- data.frame(UTM_x=file_bicicleta_v3$UTM_x,UTM_y=file_bicicleta_v3$UTM_y,
                               name=file_bicicleta_v3$address)
coordinates(file_bicicleta_v4) <- c("UTM_x", "UTM_y")
proj4string(file_bicicleta_v4) <- CRS("+init=epsg:32722")
meuse_jp <- spTransform(file_bicicleta_v4, CRS("+proj=longlat +datum=WGS84"))
writeOGR(obj = meuse_jp["name"], "teste.kml", layer="name", driver="KML") 
# 
# 6) Ordena datas e Exporta arquivo .csv
# 
sv_file <- write.csv(file_bicicleta_v3,"csv/Import_coordinates_bicicleta.csv",row.names = F,quote = F,fileEncoding = "UTF-8")
# 
# 7) Salva por intervalos de tempo
# 
#file_bicicleta_v3$Data <- as.Date(file_bicicleta_v3$Data,"%d/%m/%Y")
#range_old <- seq(as.Date("13/11/2014","%d/%m/%Y"),as.Date("c","%d/%m/%Y"),by=1)
#range_new <- seq(as.Date("13/12/2015","%d/%m/%Y"),as.Date("12/11/2016","%d/%m/%Y"),by=1)
#
# divida datas (antigas/atuais) conforme insercao acima
# nas indices 'range_old', 'range_new'
#
#data_old <- file_bicicleta_v3[which(file_bicicleta_v3$Data%in%range_old),]
#data_new <- file_bicicleta_v3[which(file_bicicleta_v3$Data%in%range_new),]
#
# salva arquive de indices antigos, e atuais
#
#folder_ext <- c("/Users/Joao Vieira/Google Drive/CICLO/04. TECNICO/Análise de Acidentes/area calma/")
#sv_file_old<- write.csv(data_old,paste0(folder_ext,"dados/csv/pedestre_area_calma_old.csv"),
#                        row.names = F,quote = F,fileEncoding = "UTF-8")
#sv_file_new <- write.csv(data_new,paste0(folder_ext,"dados/csv/pedestre_area_calma_new.csv"),
#                         row.names = F,quote = F,fileEncoding = "UTF-8")
