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
# --------------
# Filtro
# -------------
Hora_cp1 <- est$Hora[which(complete.cases(est$Dia)==T)]
Hora_cp2 <- Hora_cp1[-c(which(Hora_cp1=="IG"),which(Hora_cp1=="IGN"),which(est$Dia=="NI"))]
#Hora_cp <- str_sub(Hora_cp2,12,16)
Hora <- as.POSIXct(Hora_cp2, format="%H:%M")
# ---------------
# 1) Plot horario (pontual) | Vetor Hora
# --------------
Hora_leg <- paste0(seq(1,24,by=2),":00")
Hora_plot <- as.POSIXct(Hora_leg, format="%H:%M")
# -----------------#
grafs <- paste("JPG/horario.jpg",sep="")
jpeg(grafs,width = 25,height=8,pointsize=11,res = 200,units = "cm")
par(mar=c(5.1,1.1,1,1.1),mfrow=c(1,1),mex=0.65)
plot(Hora,rep(0.5,length(Hora)),ann=FALSE,yaxt="n",xaxt="n",
     ylim = c(0,0.65),cex=0.75,cex.axis=1,type="p",
     lty= 5 ,pch=16,col="black",xaxs = "r", yaxs = "i",las=1)
grid()
axis(side = 1,at = Hora_plot,labels = Hora_leg,par(cex=0.75,srt=30))
ll <- length(Hora)
for(i in (1:ll)){lines(rep(Hora[i],2),y = c(0,0.5),ylim=c(0,0.5),col="black")}
abline(h=0)
dev.off()
# --------------------------------------
#  2) Plot2 - grafico por barras
# -------------------------------------
aa <- as.POSIXct(paste0(seq(1,24,by=1),":00"), format="%H:%M")
it1 <- length(which(Hora<aa[2]))
it2 <- length(which(Hora>=aa[2]&Hora<aa[4]))
it3 <- length(which(Hora>=aa[4]&Hora<aa[6]))
it4 <- length(which(Hora>=aa[6]&Hora<aa[8]))
it5 <- length(which(Hora>=aa[8]&Hora<aa[10]))
it6 <- length(which(Hora>=aa[10]&Hora<aa[12]))
it7 <- length(which(Hora>=aa[12]&Hora<aa[14]))
it8 <- length(which(Hora>=aa[14]&Hora<aa[16]))
it9 <- length(which(Hora>=aa[16]&Hora<aa[18]))
it10 <- length(which(Hora>=aa[18]&Hora<aa[20]))
it11 <- length(which(Hora>=aa[20]&Hora<aa[22]))
it12 <- length(which(Hora>=aa[22]&Hora<aa[24]))

TOTAL <- data.frame(it1,it2,it3,it4,it5,it6,it7,it8,it9,it10,it11,it12)
colnames(TOTAL) <- c("00:00 - 1:59","02:00 - 03:59","04:00 - 05:59",
                     "06:00 - 07:59","08:00 - 09:59","10:00 - 11:59",
                     "12:00 - 13:59","14:00 - 15:59","16:00 - 17:59",
                     "18:00 - 19:59","20:00 - 21:59","22:00 - 23:59")
# -------- #
grafs <- paste("JPG/horario_bar_v1.jpg",sep="")
jpeg(grafs,width = 25,height=10,pointsize=11,res = 200,units = "cm")
par(mar=c(10.1,5.1,5.1,1.1),mfrow=c(1,1),mex=0.65)

colfunc <- c("#8B3E2F",brewer.pal(11,'RdYlBu'))

barplot(as.matrix(TOTAL), main="Mortes de Ciclistas por horário",ylab="Número de Mortes",
        cex.names=0.9, beside=TRUE, col=colfunc,font=2, las=2) 

#legend("topleft", c("00:00 - 05:00","05:00 - 07:00","07:00 - 09:00",
#                    "09:00 - 11:00","11:00 - 13:00","13:00 - 15:00",
#                    "15:00 - 17:00","17:00 - 19:00","19:00 - 21:00",
#                    "21:00 - 23:00","23:00 - 00:00"),
#       ncol = 3,cex=1, bty="n", fill=colfunc[4])

dev.off()

