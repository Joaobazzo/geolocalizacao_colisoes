require(openxlsx)
require(stringr)
require(RColorBrewer)

#Sys.setlocale("LC_TIME","Brazil")
setwd("/Users/Joao Vieira/Google Drive/CICLO/04. TECNICO/Análise de Acidentes/MAPAS/")
# ------------
# Le arquivo
# -----------
est <- read.xlsx("CSV/Análise dos Dados da Setran.v1.xlsx",sheet = 6, startRow = 7, colNames = T)
#View(est)

# Dados dos acidentes
# ------------------------
Data_cp <- as.Date(est$Data_AC[c(-which(est$Dia=="NI"),-which(complete.cases(est$Dia)==F))],"%d/%m/%Y")
#Data_cp
monthvec <- c(rep(paste0("0",as.character(1:9))),as.character(10:12))

vec_2012 <- c()
vec_2013 <- c()
vec_2014 <- c()
vec_2015 <- c()
vec_2016 <- c()
vec_total <- c()

for (m in (1:12)){
  vec_2012 <- append(vec_2012,length(which(format(Data_cp,"%Y")=="2012"&format(Data_cp,"%m")==monthvec[m])))
  vec_2013 <- append(vec_2013,length(which(format(Data_cp,"%Y")=="2013"&format(Data_cp,"%m")==monthvec[m])))
  vec_2014 <- append(vec_2014,length(which(format(Data_cp,"%Y")=="2014"&format(Data_cp,"%m")==monthvec[m])))
  vec_2015 <- append(vec_2015,length(which(format(Data_cp,"%Y")=="2015"&format(Data_cp,"%m")==monthvec[m])))
  vec_2016 <- append(vec_2016,length(which(format(Data_cp,"%Y")=="2016"&format(Data_cp,"%m")==monthvec[m])))
  vec_total <- append(vec_total,sum(vec_2012[m],vec_2013[m],vec_2014[m],vec_2015[m],vec_2016[m]))
}
# --------------------
TOTAL <- data.frame(vec_2012,vec_2013,vec_2014,vec_2015,vec_2016,vec_total)
colnames(TOTAL) <- c("2012","2013","2014","2015","2016")

grafs <- paste("JPG/datas_mes.jpg",sep="")
jpeg(grafs,width = 25,height=10,pointsize=11,res = 200,units = "cm")
par(mar=c(3.1,5.1,5.1,1.1),mfrow=c(1,1),mex=0.65)

colfunc <- c("#8B3E2F",brewer.pal(11,'RdYlBu'))

barplot(as.matrix(TOTAL), main="Mortes de Ciclistas por mês",ylab="Número de Mortes",cex.axis=1.4, 
        beside=TRUE, col=colfunc) 
legend("topleft", c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"),
       ncol = 3,cex=1, bty="n", fill=colfunc)

dev.off()
 # -------------------------------------------------------------------
grafs <- paste("JPG/datas_mes1_v1.jpg",sep="")
jpeg(grafs,width = 15,height=25,pointsize=11,res = 200,units = "cm")
par(mar=c(5.1,8.1,5.1,1.1),mfrow=c(5,1),mex=0.65)

colfunc <- c("#8B3E2F",brewer.pal(11,'RdYlBu'))

barplot(vec_2012,cex.axis=1.4,cex.names=1.4,yaxt="n",
        beside=TRUE, col=colfunc[4],las=1,
        names.arg=c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"))
axis(side = 2,at = c(0,1,2),las=2,cex.lab=3)
mtext(side = 2, line =  4, "2012")
barplot(vec_2013,cex.axis=1.4,cex.names=1.4,yaxt="n",
        beside=TRUE, col=colfunc[4],las=1,
        names.arg=c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"))
mtext(side = 2, line =  4, "2013")
axis(side = 2,at = 0:4,las=2,cex.lab=3)
barplot(vec_2014,cex.axis=1.4,cex.names=1.4,yaxt="n", 
        beside=TRUE, col=colfunc[4],las=1,
        names.arg=c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"))
mtext(side = 2, line =  4, "2014")
axis(side = 2,at = 0:3,las=2,cex.lab=3)
barplot(vec_2015,cex.axis=1.4,cex.names=1.4,yaxt="n",  
        beside=TRUE, col=colfunc[4],las=1,
        names.arg=c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"))
mtext(side = 2, line =  4, "2015")
axis(side = 2,at = 0:5,las=2,cex.lab=3)
barplot(vec_2016,cex.axis=1.4,cex.names=1.4,yaxt="n",
        beside=TRUE, col=colfunc[4],las=1,
        names.arg=c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"))
mtext(side = 2, line =  4, "2016")
axis(side = 2,at = 0:2,las=2,cex.lab=3)
dev.off()

