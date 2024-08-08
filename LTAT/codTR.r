require(survival)
require(Hmisc)
require(lattice)
require(epitools)
require(ggplot2)
require(qcc)
require(doBy)

rm=list(ls())
df <- read.csv('trisp2013.csv',sep=';', header=T)
df$dtprel <- as.POSIXct(strptime(as.character(df$dtprel), "%d/%m/%Y" ))
df$dtreg <- as.POSIXct(strptime(as.character(df$dtreg), "%d/%m/%Y" ))
df$dtconf <- as.POSIXct(strptime(as.character(df$dtconf), "%d/%m/%Y" ))
df$dtini <- as.POSIXct(strptime(as.character(df$dtini), "%d/%m/%Y" ))
df$dtfine <- as.POSIXct(strptime(as.character(df$dtfine), "%d/%m/%Y" ))
dtprel <- epidate(df$dtprel)
dtreg <- epidate(df$dtreg)
dtconf <- epidate(df$dtconf)
dtini <- epidate(df$dtini)
dtfine <- epidate(df$dtfine)

#TEMPI DI ATTESA
Trisp <- (dtfine$dates-dtconf$dates)+1
Tcirc <- (dtini$dates-dtconf$dates)+1
df$Trisp <- as.numeric(Trisp)
df$Tcirc <- as.numeric(Tcirc)
Trisp<- aggregate(df$Trisp, list('laboratorio'=df$laboratorio, 'prova'=df$prova, 'VN+MP'=df$code),function(x){quantile(x, 0.8)})
Tcirc <-aggregate(df$Tcirc, list('laboratorio'=df$laboratorio, 'prova'=df$prova, 'VN+MP'=df$code),function(x){quantile(x, 0.8, na.rm=TRUE)})
nesam<- aggregate(df$Trisp, list('laboratorio'=df$laboratorio, 'prova'=df$prova, 'VN+MP'=df$code),length)
tt <- cbind(nesam, Trisp$x, Tcirc$x)
write.table(tt, file='tt.xls', dec=',')
write.table(df, file='tt1.xls', dec=',')



