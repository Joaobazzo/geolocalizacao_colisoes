require(XLConnect)
require(stringr)

setwd("/Users/Joao Vieira/Google Drive/CICLO/PROJETOS_INTERNOS/ANALISE_ACIDENTES/MAPAS/")

source_file <- paste0("CSV/Análise dos Dados da Setran.v1.xlsx")
file <- loadWorkbook(source_file)
est <- readWorksheet(file,sheet = "TOTAL_COORD.",startRow = 7)
#View(est)

# Dados dos acidentes
# ------------------------
monthvec <- c(rep(paste0("01/0",as.character(1:9))),paste0("01/",as.character(10:12)))
yearvec <- paste0(monthvec,"/",rep(as.character(12:16),12))
totvec <- yearvec[-which(yearvec%in%c("01/06/16","01/07/16","01/08/16",
                                      "01/09/16","01/10/16","01/11/16","01/12/16"))]

data_caracp <- as.Date(totvec,"%d/%m/%y")
data_carac <- format(data_caracp[order(data_caracp)],"%m/%y")

data_datp <- as.Date(totvec,"%d/%m/%y")
data_date <- data_datp[order(data_datp)]

Data_cp <- as.Date(est$Data_AC[which(complete.cases(est$Data_AC)==T)])
Data_cc <- Data_cp[order(Data_cp)]
# ------------------------
grafs <- paste("JPG/datas.jpg",sep="")
jpeg(grafs,width = 25,height=10,pointsize=11,res = 200,units = "cm")
par(mar=c(5.1,1.1,1,1.1),mfrow=c(2,1),mex=0.65)
plot(Data_cc,rep(0.5,length(Data_cc)),ann=FALSE,yaxt="n",xaxt="n",
     ylim = c(0,0.65),xlim = c(data_date[1],data_date[26]),cex=0.75,cex.axis=1,type="p",
     lty= 5 ,pch=16,col="black",xaxs = "r", yaxs = "i",las=1)
grid()

axis(side = 1,at = data_date,labels = data_carac,par(cex=0.75,srt=30),las=2)
ll <- length(Data_cc)
for(i in (1:ll)){lines(rep(Data_cc[i],2),y = c(0,0.5),ylim=c(0,0.5),col="black")}
# ---------------------------------------------------------------------------
plot(Data_cc,rep(0.5,length(Data_cc)),ann=FALSE,yaxt="n",xaxt="n",
     ylim = c(0,0.65),xlim = c(data_date[27],data_date[53]),cex=1,cex.axis=1,type="p",
     lty= 5 ,pch=16,col="black",xaxs = "r", yaxs = "i",las=1)
grid()

axis(side = 1,at = data_date,labels = data_carac,par(cex=0.75,srt=30),las=2)
ll <- length(Data_cc)
for(i in (1:ll)){lines(rep(Data_cc[i],2),y = c(0,0.5),ylim=c(0,0.5),col="black")}
dev.off()