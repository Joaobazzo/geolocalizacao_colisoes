require(openxlsx)
require(stringr)
require(RColorBrewer)

#Sys.setlocale("LC_TIME","Brazil")
setwd("/Users/Joao Vieira/Google Drive/CICLO/04. TECNICO/Análise de Acidentes/MAPAS/")
# ------------
# Le arquivo
# -----------
est <- read.xlsx("CSV/Análise dos Dados da Setran.v1.xlsx",sheet = 7, startRow = 7, colNames = T)
#View(est)
# --------------

# --------------------------------
# Tratamento de dados
# --------------------------------

acumula <- function(acum){
  ac <- c()
  cc <- 0
  for (i in (1:length(acum))){
    cc = cc + acum[,i]
    ac <- append(ac,cc)
  }
  dtexp <- t(as.data.frame(ac))
  colnames(dtexp) <-c("SANTA FELICIDADE","PORTÃO","MATRIZ","BAIRRO NOVO",
                      "PINHEIRINHO","CAJURU","TATUQUARA","BOA VISTA","BOQUEIRÃO","CIC")
  return(dtexp)
}


Local <- est$REGIAO[which(complete.cases(est$REGIAO)==T)]
Lista <- Local[duplicated(Local)==F]


it1 <- length(which(Local==Lista[1]))
it2 <- length(which(Local==Lista[2]))
it3 <- length(which(Local==Lista[3]))
it4 <- length(which(Local==Lista[4]))
it5 <- length(which(Local==Lista[5]))
it6 <- length(which(Local==Lista[6]))
it7 <- length(which(Local==Lista[7]))
it8 <- length(which(Local==Lista[8]))
it9 <- length(which(Local==Lista[9]))
it10 <- length(which(Local==Lista[10]))

totalp <- data.frame(it1,it2,it3,it4,it5,it6,it7,it8,it9,it10)
colnames(totalp) <- Lista
total <- round(totalp[order(totalp)]*100/sum(totalp),1)
t_acum <- acumula(total)
nomes <- c("ST. FELICIDADE","PORTÃO","MATRIZ","BAIRRO NOVO",
           "PINHEIRINHO","CAJURU","TATUQUARA","BOA VISTA","BOQUEIRÃO","CIC")
# --------------------------
# Plot (regiao acumulada)
# -----------------------------
grafs <- paste("JPG/regiao_acumulada_v2.jpg",sep="")
jpeg(grafs,width = 25,height=20,pointsize=11,res = 200,units = "cm")
par(mar=c(10.1,10.1,5.1,10.1),mfrow=c(2,1),mex=0.65)

colfunc <- brewer.pal(10,'RdYlBu')

barplot(as.matrix(total[order(total,decreasing = T)]),  
        names.arg=nomes[order(total,decreasing = T)],main="Registros por área (%)",
        cex.names=0.65, beside=T, col=colfunc,font=1, las=1) 

barplot(as.matrix(t_acum[order(t_acum,decreasing = T)]),  
        names.arg=nomes[order(total,decreasing = T)],space = c(0.7),
        cex.names=0.65, beside=T, col=colfunc,font=1, las=1) 
#axis(side = 4,labels = c(0,3,6,9,12,15),at = seq(0,100,100/5),las=1)
#mtext(side = 4, line = 3, "Registros por área (%)")
mtext(side = 2, line = 4.5, "Registros acumulados por área (%)",)


dev.off()
