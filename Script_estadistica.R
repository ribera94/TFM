install.packages("rvest")
library(rvest)
install.packages("reshape2")
library(reshape2)
library(stringr)
library(tidyr)
library(readxl)
install.packages("Rcpp")
library(Rcpp)
library(sqldf)
update.packages(c("Rcpp"))
#Funcio 
extraccio_estadistica <- function (df){
  indicador<-df$indicador
  web<-df$web
  html <- read_html(web)
  taula <- html %>% html_table(header=TRUE)
  
  first_table <- taula[[4]]
  
  data<- as.data.frame(first_table)
  data2<-subset(data,data[,1]!='')
  data3<-data2[, colSums(is.na(data2)) != nrow(data2)]
  
  
  columna1<- colnames(data3[1])
  transposada<-melt(data3, id.var=c(columna1))
  transposada$indicador <- indicador
  transposada[,1]<- sub(paste(indicador,"", sep=" "),"", transposada[,1])
  transposada2<-separate(transposada, col = columna1, into = c("Desagregat1","Codi_serie"),sep = "(?=\\([0-9]+)") 
  transposada3<<-transposada2[,c(5,1,2,3,4)]
  transposada4<<-rbind(transposada4,transposada3)
}


#Carrega excel
webs<-read_excel("C:/Users/joan.ribera/Desktop/Joan/Màster, TFM/Projecte/extraccio_estadistica.xlsx")


transposada4<- data.frame(indicador=character(), Desagregat1=character(), Codi_serie=character(), variable=factor(),value=character())

for (i in 1:nrow(webs)){
  webs2<<-webs[i,]
  extraccio_estadistica(webs2)
}


library(writexl)
write_xlsx(transposada4,"C:/Users/joan.ribera/Desktop/Joan/Màster, TFM/Projecte/taula_final_andorra.xlsx")

model_and <-read_excel("C:/Users/joan.ribera/Desktop/Joan/Màster, TFM/Projecte/model_estadistica_andorra.xlsx")
transposada4<-read_excel("C:/Users/joan.ribera/Desktop/Joan/Màster, TFM/Projecte/taula_final_andorra.xlsx")

vector<-as.vector(model_and$Desagregat1)
vector2<-as.vector(transposada4$Desagregat1)
dades_model_and<-subset(transposada4,transposada4$Desagregat1 %in% vector)

dades_model_and <- subset(dades_model_and, dades_model_and$indicador!="Nombre d'excursionistes.")
dades_model_and$Desagregat1[dades_model_and$Desagregat1 == "Total"] <- "Nombre d'assalariats."
write_xlsx(dades_model_and,"C:/Users/joan.ribera/Desktop/Joan/Màster, TFM/Projecte/dades_model_and.xlsx")



print(vector)
print(unique(vector2))
vector %in% vector2



#Instal.lació paquet

#install.packages("DBI")
library(DBI)
#install.packages("odbc")
library(odbc)
#install.packages("RODBC")
library(RODBC)
library(readxl)

#sort(unique(odbcListDrivers()[[1]]))

#Connexió amb la BBDD PBI. Modificar usuari!!
con <- dbConnect(odbc::odbc(), 
                 Driver = "SQL Server", 
                 Server = "SRVSQL01\\PROCESSOS",
                 Database = "PBI",
                 user = "INAF\\joan.ribera", 
                 Trusted_Connection = "True",
                 encoding = "latin1") 
dbWriteTable(con, "JR_dades_indicadors_andorra", transposada4, append=F, overwrite=T) # 1r nom taula BBDD, nom data.frame R
dbWriteTable(con, "JR_dades_model_andorra", dades_model_and, append=F, overwrite=T) # 1r nom taula BBDD, nom data.frame R


classificacio_paisos<-read_excel("C:/Users/joan.ribera/Desktop/Joan/Màster, TFM/Projecte/classificacio_paisos.xlsx")
dbWriteTable(con, "JR_classificacio_paisos", classificacio_paisos, append=F, overwrite=T) # 1r nom taula BBDD, nom data.frame R

#dades_model_and
#dades_model_and<-subset(dades_model_and,dades_model_and$indicador != 'Nombre de visitants per nacionalitat')
#dades_model_and$variable<-str_sub(dades_model_and$variable,-4)
#dades_model_and2<-sqldf('select distinct indicador, Desagregat1, Codi_serie, variable, sum(value) as value from dades_model_and 
                        #group by indicador, Desagregat1, Codi_serie, variable')
write_xlsx(dades_model_and,"C:/Users/joan.ribera/Desktop/Joan/Màster, TFM/Projecte/dades_model_and.xlsx")

