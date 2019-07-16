require(XLConnect)
require(stringr)
require(RColorBrewer)

Sys.setlocale("LC_TIME","Brazil")
setwd("/Users/Joao Vieira/Google Drive/CICLO/PROJETOS_INTERNOS/ANALISE_ACIDENTES/MAPAS/")

source_file <- paste0("CSV/Análise dos Dados da Setran.v1.xlsx")
file <- loadWorkbook(source_file)
est <- readWorksheet(file,sheet = "TOTAL_COORD.",startRow = 7)
#View(est)
Hora_cp1 <- est$Hora[which(complete.cases(est$Hora)==T)]
Hora_cp2 <- Hora_cp1[-c(which(Hora_cp1=="IG"),which(Hora_cp1=="IGN"))]
Hora_cp <- str_sub(Hora_cp2,12,16)
Hora <- as.POSIXct(Hora_cp, format="%H:%M")

Hora_leg <- paste0(seq(5,24,by=2),":00")
Hora_plot <- as.POSIXct(Hora_leg, format="%H:%M")
 # -------------------------------------
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
# -------------------------------------
aa <- as.POSIXct(paste0(seq(1,24,by=1),":00"), format="%H:%M")
it1 <- length(which(Hora>=aa[1]&Hora<=aa[5]))
it2 <- length(which(Hora>aa[5]&Hora<=aa[7]))
it3 <- length(which(Hora>aa[7]&Hora<=aa[9]))
it4 <- length(which(Hora>aa[9]&Hora<=aa[11]))
it5 <- length(which(Hora>aa[11]&Hora<=aa[13]))
it6 <- length(which(Hora>aa[13]&Hora<=aa[15]))
it7 <- length(which(Hora>aa[15]&Hora<=aa[17]))
it8 <- length(which(Hora>aa[17]&Hora<=aa[19]))
it9 <- length(which(Hora>aa[19]&Hora<=aa[21]))
it10 <- length(which(Hora>aa[21]&Hora<=aa[23]))
it11 <- length(which(Hora>aa[23]&Hora<=aa[24]))

TOTAL <- data.frame(it1,it2,it3,it4,it5,it6,it7,it8,it9,it10,it11)
colnames(TOTAL) <- c("00:00 - 05:00","05:00 - 07:00","07:00 - 09:00",
                     "09:00 - 11:00","11:00 - 13:00","13:00 - 15:00",
                     "15:00 - 17:00","17:00 - 19:00","19:00 - 21:00",
                     "21:00 - 23:00","23:00 - 00:00")

grafs <- paste("JPG/horario_bar.jpg",sep="")
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

