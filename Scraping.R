install.packages("rvest")
library(rvest)
install.packages("reshape2")
library(reshape2)
library(stringr)
install.packages("tidyr")
library(tidyr)
library(readxl)
install.packages("Rtools")

install.packages("ellipsis")
library(ellipsis)

install.packages("RSelenium")
library(RSelenium)
library(sqldf)
install.packages("dplyr")
library(dplyr)




#Extracció rating
html <- read_html('https://tradingeconomics.com/country-list/rating')
taula <- html %>% html_table(header=TRUE)
print(taula)
taula_ratings_actual <- taula[[1]] #dades actuals
taula_model_ratings <- taula[[2]] #dades ràting equivalència numèric


taula_ratings_paisos <- data.frame()
replacement <- c('andorra','france','italy','germany','spain','portugal','united-states','japan','china','liechtenstein','luxembourg','montenegro','malta','cyprus')
for (i in 1:length(replacement)){
  #html <- read_html(paste('https://tradingeconomics.com/',replacement[i],'/rating'))
  #print(html)
  html <- read_html(paste('https://tradingeconomics.com/',replacement[i],'/rating',sep = ""))
  taula <- html %>% html_table(header=TRUE)
  if (i==1){
    taula_ratings_paisos <- taula[[1]]
    taula_ratings_paisos$pais <- replacement[[i]]
    }
  else {
    taula_prov<- taula[[1]]
    taula_prov$pais <- replacement[[i]]
    taula_ratings_paisos <- rbind(taula_ratings_paisos,taula_prov)
    }
}

library(reshape2)
update.packages()
install.packages("Rccp")
library(Rcpp)
taula_transposada_ratings = melt(taula_model_ratings, id.vars = c("TE"),measure.vars = c("S&P","Moody's","Fitch"))

taula_ratings_paisos_numerics<- sqldf('select * from taula_ratings_paisos t1 left join taula_transposada_ratings t2
                                      on t1.Agency=t2.variable and t1.Rating = t2.value
                                                                            ')
taula_ratings_paisos_numerics2<- sqldf('select * from taula_ratings_paisos t1 left join taula_transposada_ratings t2
                                      on t1.Agency=t2.variable and t1.Rating = t2.value
                                                                            ')

taula_ratings_paisos_numerics$any<-str_sub(taula_ratings_paisos_numerics$Date,-4)

#Taula per fer left join
taula_paisos_anys<- merge(taula_ratings_paisos_numerics$pais,taula_ratings_paisos_numerics$any, all.x=TRUE)
taula_paisos_anys<- sqldf('select distinct x as pais, y as any from taula_paisos_anys')

taula_ratings_paisos_numerics<- sqldf('select distinct t1.pais, avg(t2.TE) as avgTE, t1.any 
                                        from taula_paisos_anys t1 left join taula_ratings_paisos_numerics t2 on t1.pais=t2.pais and t1.any=t2.any
                                        group by t1.pais, t1.any
                                      
                                      
                                      ')


library(zoo)

taula_ratings_paisos_numerics$avgTE<-na.locf(taula_ratings_paisos_numerics$avgTE, na.rm=FALSE)

taula_ratings_paisos_numerics<-subset(taula_ratings_paisos_numerics,taula_ratings_paisos_numerics$any>2000)

index<-read_excel("C:/Users/joan.ribera/Desktop/joan/Màster, TFM/Projecte/Borsa.xlsx")
index[1,2]

install.packages("quantmod", repos="http://R-Forge.R-project.org")
install.packages('quantmod')
library(quantmod)


symbols <- c("^IBEX","^FCHI","PSI20.LS","FTSEMIB.MI","^GDAXI","^STOXX50E","^DJI","^GSPC","^IXIC","^N225","000001.SS","EURUSD=X","EUR=X","CNY=X","JPY=X")
getSymbols(symbols, from="1997-12-31", src='yahoo',periodicity='monthly')

dfNew <- data.frame(Company = character(),
                    Date=as.Date(character()),
                    Close=numeric(),
                    Volume=numeric(), 
                    stringsAsFactors=FALSE) 

tickers <- c("IBEX","FCHI","PSI20.LS","FTSEMIB.MI","GDAXI","STOXX50E","DJI","GSPC","IXIC","N225","000001.SS","EURUSD=X","EUR=X","CNY=X","JPY=X")

for(i in 1:length(tickers)){
  dfSym <- cbind(Company=rep(tickers[i],nrow(get(tickers[i]))),fortify.zoo(get(tickers[i])[,4]))
  names(dfSym)[2:3] <- c('Date','Close')
  dfNew <- rbind(dfNew,dfSym)
}

taula_index_borsaris <- dfNew
taula_index_borsaris$any <- substr(taula_index_borsaris$Date,1,4)
taula_index_borsaris$mes <- str_sub(taula_index_borsaris$Date,6,7)

taula_index_borsaris <- subset(taula_index_borsaris,taula_index_borsaris$mes=='01')
as.numeric(taula_index_borsaris$any)
taula_index_borsaris$any<-as.numeric(taula_index_borsaris$any)-1

index2 <- as.data.frame(index)

taula_index_def<-sqldf("select t1.pais, t1.valor_taula, t1.tipus, t1.pais_divisa, t2.Close, t2.any from index2 t1 left join taula_index_borsaris t2 on t1.valor_taula=t2.Company order by t1.pais,t1.valor_taula, t2.any")

f_yoy <- function(x) c(NA, 100*diff(x)/x[-length(x)])


taula_index_def2 <- taula_index_def%>%
  group_by(valor_taula) %>%
  arrange(any) %>%
  mutate(pct.chg = (Close - lag(Close))/lag(Close))


taula_index_borsaris_final <- sqldf('select * from taula_index_def2 where tipus="index-borsa" ')
taula_divises_final<- sqldf('select * from taula_index_def2 where tipus="divisa" ')








install.packages("WDI")
library(WDI)
library(ggplot2)

dat = WDI(indicator=c('NY.GDP.MKTP.KD.ZG','EN.ATM.CO2E.PC','TM.VAL.MRCH.XD.WD','NY.GDP.DEFL.KD.ZG','TX.VAL.MRCH.XD.WD','NY.GDP.PCAP.KD.ZG'), country=c('AND','CHN','CYP','DEU','ESP','FRA','ITA','JPN','LIE','LUX','MCO','MNE','PRT','USA'), start=1960, end=2021)
data_worldbank<- subset(dat, dat$year>2000)
data_worldbank$country<- tolower(data_worldbank$country)




#Ajuntar 3 fonts

taula_ratings_paisos_numerics$pais_borsa <- taula_ratings_paisos_numerics$pais
taula_ratings_paisos_numerics$pais_borsa[taula_ratings_paisos_numerics$pais %in% c('andorra','cyprus','liechtenstein','luxembourg','malta','montenegro')] <- 'europe'
taula_ratings_paisos_numerics$pais_divisa <- taula_ratings_paisos_numerics$pais
taula_ratings_paisos_numerics$pais_divisa[taula_ratings_paisos_numerics$pais %in% c('andorra','cyprus','liechtenstein','luxembourg','malta','montenegro','germany','spain','france','portugal','italy')] <- 'europe'



taula_final<- sqldf('select t1.pais, t1.any, t1.avgTE, t2.valor_taula, t2."pct.chg" as variacio_borsa ,t1.pais_divisa
                    
                    from taula_ratings_paisos_numerics t1 left join taula_index_borsaris_final t2 on t1.pais_borsa=t2.pais and t1.any=t2.any
                   ')


taula_final2<- sqldf('select t1.*, t2."pct.chg" as variacio_divisa
                    from taula_final t1 left join taula_divises_final t2 on t1.pais_divisa=t2.pais_divisa and t1.any=t2.any
                   ')
data_worldbank$country[data_worldbank$country == "united states"] <- 'united-states'


taula_final3<- sqldf('select * 
                    from taula_final2 t1 left join data_worldbank t2 on t1.pais=t2.country and t1.any=t2.year
                   ')



con <- dbConnect(odbc::odbc(), 
                 Driver = "SQL Server", 
                 Server = "SRVSQL01\\PROCESSOS",
                 Database = "PBI",
                 user = "INAF\\joan.ribera", 
                 Trusted_Connection = "True",
                 encoding = "latin1") 


library(Hmisc)
taula_ratings_paisos$pais<-capitalize(taula_ratings_paisos$pais)
taula_ratings_paisos[with(taula_ratings_paisos,order(pais)),]
taula_ratings_paisos<-sqldf('select pais, Date, Agency, Rating, Outlook from taula_ratings_paisos')


taula_ratings_paisos_numerics3<-taula_ratings_paisos_numerics2 %>%
  arrange(Agency,pais) %>% 
  group_by(Agency,pais) %>% 
  summarise_all(first)

dbWriteTable(con, "JR_taula_ratings", taula_ratings_paisos_numerics3, append=F, overwrite=T) # 1r nom taula BBDD, nom data.frame R





dbWriteTable(con, "JR_taula_model_ratings", taula_model_ratings, append=F, overwrite=T) # 1r nom taula BBDD, nom data.frame R

taula_ratings_paisos_numerics$pais<-capitalize(taula_ratings_paisos_numerics$pais)
dbWriteTable(con, "JR_taula_ratings_numerics", taula_ratings_paisos_numerics, append=F, overwrite=T) # 1r nom taula BBDD, nom data.frame R
taula_index_def2$pais<-capitalize(taula_index_def2$pais)
dbWriteTable(con, "JR_index_borsa", taula_index_def2, append=F, overwrite=T) # 1r nom taula BBDD, nom data.frame R
dbWriteTable(con, "JR_data_worldbank", data_worldbank, append=F, overwrite=T) # 1r nom taula BBDD, nom data.frame R
dbWriteTable(con, "JR_model_multipais", taula_final3, append=F, overwrite=T) # 1r nom taula BBDD, nom data.frame R


write.csv(taula_final3,"C:/Users/joan.ribera/Desktop/Joan/Màster, TFM/Projecte/dades_model_multipais.csv")




























