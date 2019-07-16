require(XLConnect)
require(stringr)
require(RColorBrewer)

Sys.setlocale("LC_TIME","Brazil")
setwd("/Users/Joao Vieira/Google Drive/CICLO/PROJETOS_INTERNOS/ANALISE_ACIDENTES/MAPAS/")

source_file <- paste0("CSV/Análise dos Dados da Setran.v1.xlsx")
file <- loadWorkbook(source_file)
est <- readWorksheet(file,sheet = "TOTAL_COORD.",startRow = 7)
#View(est)

# Dados dos acidentes
# ------------------------
Data_cp <- as.Date(est$Data_AC[which(complete.cases(est$Data_AC)==T)])
monthvec <- c(rep(paste0("0",as.character(1:9))),as.character(10:12))

Data_pt <- c("Domingo", "Segunda", "Terça", "Quarta", "Quinta", 
  "Sexta", "Sábado")[as.POSIXlt(Data_cp)$wday + 1]

data_resumo <- data.frame(length(which(Data_pt=="Domingo")),
                          length(which(Data_pt=="Segunda")),
                          length(which(Data_pt=="Terça")),
                          length(which(Data_pt=="Quarta")),
                          length(which(Data_pt=="Quinta")),
                          length(which(Data_pt=="Sexta")),
                          length(which(Data_pt=="Sábado")))
colnames(data_resumo) <- c("Domingo", "Segunda", "Terça", "Quarta", "Quinta", 
                           "Sexta", "Sábado")

colfunc <- c(brewer.pal(7,'RdYlBu'))

grafs <- paste("JPG/dias_semana.jpg",sep="")
jpeg(grafs,width = 25,height=10,pointsize=11,res = 200,units = "cm")
par(mar=c(5.1,7.1,1.2,1.1),mfrow=c(1,1),mex=0.65)

barplot(as.vector(t(data_resumo)),ylab="Número de mortes",cex.axis=1.4, 
        beside=TRUE, col=colfunc,
        names.arg=c("Domingo", "Segunda", "Terça", "Quarta", "Quinta", 
                    "Sexta", "Sábado"))
dev.off()