library(ggplot2)
library(openxlsx)
setwd("/Users/Joao Vieira/Dropbox/Ciclo/br-277/who/")
#
#
# transito
trans <- read.csv("data.csv",header = F,skip=2)
colnames(trans) <- c("pais","total","ob_per_pop")
#
#
# corrupcao
corp <- read.xlsx("CPI2016_FullDataSetWithRegionalTables.xlsx")
#
#
# corrupcao in transito
ind_ct <- which(corp$Country%in%trans$pais)
obito <- c()
for(i in (1:length(ind_ct))){
  alfa <- which(trans$pais%in%corp$Country[ind_ct[i]])
  if(length(alfa)>0){
    obito[i] <- trans$ob_per_pop[alfa]}else{obito[i] <- "-"}
 # print(i)
}
pais <- corp$Country[ind_ct]
corr <- 100 - corp$CPI2016[ind_ct]
cpt <- data.frame(pais,
                  corr,obito)
colnames(cpt) <- c("pais","score_cpt","score_ob")
View(cpt)
head(cpt,1) 
head(trans,1)


