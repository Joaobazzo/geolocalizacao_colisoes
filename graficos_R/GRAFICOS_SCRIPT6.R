require(XLConnect)
require(stringr)
require(RColorBrewer)

Sys.setlocale("LC_TIME","Brazil")
setwd("/Users/Joao Vieira/Google Drive/CICLO/05. PROJETOS_INTERNOS/ANALISE_ACIDENTES/MAPAS/")

source_file <- paste0("CSV/Análise dos Dados da Setran.v1.xlsx")
file <- loadWorkbook(source_file)
est <- readWorksheet(file,sheet = "TOTAL_COORD.",startRow = 7)
# ------------------------ FILTRO DIA
estnew <- est[which(complete.cases(est$Data_AC)==T),]
estnew$Data_AC <- as.Date(str_sub(estnew$Data_AC,0,12),"%Y-%m-%d")
monthvec <- c(rep(paste0("0",as.character(1:9))),as.character(10:12))

estnew$Data_pt <- c("Domingo", "Segunda", "Terça", "Quarta", "Quinta", 
             "Sexta", "Sábado")[as.POSIXlt(estnew$Data_AC)$wday + 1]
weekday <- c(which(estnew$Data_pt=="Segunda"),
             which(estnew$Data_pt=="Terça"),
             which(estnew$Data_pt=="Quarta"),
             which(estnew$Data_pt=="Quinta"),
             which(estnew$Data_pt=="Sexta"))
weekend <- c(which(estnew$Data_pt=="Sábado"),
            which(estnew$Data_pt=="Domingo"))
# Define período
estnew <- estnew[weekday,]

# -------------------------- FILTRO HORA
estnew <- estnew[which(complete.cases(estnew$Hora)==T),]
estnew <- estnew[-c(which(estnew$Hora=="IG"),which(estnew$Hora=="IGN"),which(estnew$Hora=="")),]
estnew$Hora <- str_sub(estnew$Hora,12,16)
estnew$Hora <- as.POSIXct(estnew$Hora, format="%H:%M")

Hora_leg <- paste0(seq(5,24,by=2),":00")
Hora_plot <- as.POSIXct(Hora_leg, format="%H:%M")

# -------------------------------------
aa <- as.POSIXct(paste0(seq(1,24,by=1),":00"), format="%H:%M")
it1 <- length(which(estnew$Hora>=aa[1] & estnew$Hora<=aa[5]))
it2 <- length(which(estnew$Hora>aa[5]  & estnew$Hora<=aa[7]))
it3 <- length(which(estnew$Hora>aa[7]  & estnew$Hora<=aa[9]))
it4 <- length(which(estnew$Hora>aa[9]  & estnew$Hora<=aa[11]))
it5 <- length(which(estnew$Hora>aa[11] & estnew$Hora<=aa[13]))
it6 <- length(which(estnew$Hora>aa[13] & estnew$Hora<=aa[15]))
it7 <- length(which(estnew$Hora>aa[15] & estnew$Hora<=aa[17]))
it8 <- length(which(estnew$Hora>aa[17] & estnew$Hora<=aa[19]))
it9 <- length(which(estnew$Hora>aa[19] & estnew$ora<=aa[21]))
it10 <- length(which(estnew$Hora>aa[21]& estnew$Hora<=aa[23]))
it11 <- length(which(estnew$Hora>aa[23]& estnew$Hora<=aa[24]))

TOTAL <- data.frame(it1,it2,it3,it4,it5,it6,it7,it8,it9,it10,it11)
colnames(TOTAL) <- c("00:00 - 05:00","05:00 - 07:00","07:00 - 09:00",
                     "09:00 - 11:00","11:00 - 13:00","13:00 - 15:00",
                     "15:00 - 17:00","17:00 - 19:00","19:00 - 21:00",
                     "21:00 - 23:00","23:00 - 00:00")

grafs <- paste("JPG/horario_bar_dia-de-semana.jpg",sep="")
jpeg(grafs,width = 25,height=10,pointsize=11,res = 200,units = "cm")
par(mar=c(10.1,5.1,5.1,1.1),mfrow=c(1,1),mex=0.65)

colfunc <- c("#8B3E2F",brewer.pal(11,'RdYlBu'))

barplot(as.matrix(TOTAL), main="Mortes por horário - dias de semana",ylab="Número de Mortes",
        cex.names=0.9, beside=TRUE, col=colfunc,font=2, las=2) 

#legend("topleft", c("00:00 - 05:00","05:00 - 07:00","07:00 - 09:00",
#                    "09:00 - 11:00","11:00 - 13:00","13:00 - 15:00",
#                    "15:00 - 17:00","17:00 - 19:00","19:00 - 21:00",
#                    "21:00 - 23:00","23:00 - 00:00"),
#       ncol = 3,cex=1, bty="n", fill=colfunc[4])

dev.off()

