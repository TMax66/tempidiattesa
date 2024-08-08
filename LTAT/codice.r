require(survival)
require(Hmisc)
require(lattice)
require(epitools)
require(ggplot2)
require(qcc)

rm=list(ls())
df <- read.csv('tatfdc.csv',sep=';', header=T)

names(df)<-c('nconf','dtprel','dtacc','dtreg','dtcarico','dtinan','dtfine','dtrdp','ncamp', 'nesam', 'annoacc', 'trireg', 'mreg', 'annoreg', 'triacc', 'macc', 'annopre','tripre', 'mpre')


df$dtprel <- as.POSIXct(strptime(as.character(df$dtprel), "%d/%m/%Y %H.%M.%S" ))
df$dtreg <- as.POSIXct(strptime(as.character(df$dtreg), "%d/%m/%Y %H.%M.%S" ))
df$dtacc <- as.POSIXct(strptime(as.character(df$dtacc), "%d/%m/%Y %H.%M.%S" ))
df$dtcarico <- as.POSIXct(strptime(as.character(df$dtcarico), "%d/%m/%Y %H.%M.%S" ))
df$dtinan <- as.POSIXct(strptime(as.character(df$dtinan), "%d/%m/%Y %H.%M.%S" ))
df$dtfine <- as.POSIXct(strptime(as.character(df$dtfine), "%d/%m/%Y %H.%M.%S" ))
df$dtrdp <- as.POSIXct(strptime(as.character(df$dtrdp), "%d/%m/%Y %H.%M.%S" ))


df <- subset(df,df$annoacc==2012)


dtprel <- epidate(df$dtprel)
dtreg <- epidate(df$dtreg)
dtacc <- epidate(df$dtacc)
dtinan <- epidate(df$dtinan)
dtfine <- epidate(df$dtfine)
dtrdp <- epidate(df$dtrdp)
dtcarico <- epidate(df$dtcarico)






#TEMPI DI ATTESA
TAr <- dtrdp$dates-dtacc$dates ###tempo reale di attesa
TAp <- dtrdp$dates-dtprel$dates###tempo percepito di attesa
TAn <- dtfine$dates-dtinan$dates###tempo di analisi reale
Tcirc <- dtinan$dates-dtacc$dates###tenpo di circolazione interna del campione
Tciris <- dtrdp$dates-dtfine$dates###tempo di circolazione del risultato




week


z <- aggregate(TAr, list(week), median, length)
names(z) <- c('settimana', 'tempo.mediano')





mod <- lm(tattesa~df$prova)

dfu <- df[!duplicated(df$nconf),]
dtrdp <- epidate(dfu$dtrdp)
dtacc1 <- epidate(dfu$dtacc)
tesiti <- dtrdp$dates-dtacc1$dates

week<-dtacc1$week
month <- as.factor(dtacc1$month)

z <- aggregate(tesiti, list(week), length)
names(z) <- c('settimana', 'tempo.mediano')



df$c1 <- ifelse(df$dtprel<= df$dtreg, 1, 0)
df$c2 <- ifelse(df$dtacc <= df$dtinan, 1, 0)



plot(survfit(Surv(tesiti)~1),xlab="giorni dall'accettazione",
     ylab="probabilità di emissione rdp", xlim=c(0,10))

legend("topright", bty="n", col=1:12, lty=1, leg=levels(month))


survfit(Surv(TAr)~1)


#####################poi cancella.....codici per lezione su accettazione c'è l'analisi dei tempi di circolazione interni#####################

require(survival)
require(Hmisc)
require(lattice)
require(epitools)
require(ggplot2)

rm=list(ls())
df <- read.csv('acc2012.csv',sep=';', header=T)

names(df)<-c('nconf','matrice','dtprel','dtreg','dtconf','dtrdp','settore','fin','categ', 'strutt', 'dtini', 'dtfin')


df$dtprel <- as.POSIXct(strptime(as.character(df$dtprel), "%d/%m/%Y %H.%M.%S" ))
df$dtreg <- as.POSIXct(strptime(as.character(df$dtreg), "%d/%m/%Y %H.%M.%S" ))
df$dtacc <- as.POSIXct(strptime(as.character(df$dtconf), "%d/%m/%Y %H.%M.%S" ))
df$dtinan <- as.POSIXct(strptime(as.character(df$dtini), "%d/%m/%Y %H.%M.%S" ))
df$dtfine <- as.POSIXct(strptime(as.character(df$dtfin), "%d/%m/%Y %H.%M.%S" ))
df$dtrdp <- as.POSIXct(strptime(as.character(df$dtrdp), "%d/%m/%Y %H.%M.%S" ))





dtprel <- epidate(df$dtprel)
dtreg <- epidate(df$dtreg)
dtacc <- epidate(df$dtacc)
dtinan <- epidate(df$dtinan)
dtfine <- epidate(df$dtfine)
dtrdp <- epidate(df$dtrdp)
dtcarico <- epidate(df$dtcarico)






#TEMPI DI ATTESA
TAr <- dtrdp$dates-dtacc$dates ###tempo reale di attesa
TAp <- dtrdp$dates-dtprel$dates###tempo percepito di attesa
TAn <- dtfine$dates-dtinan$dates###tempo di analisi reale

Tcirc <- dtinan$dates-dtacc$dates###tenpo di circolazione interna del campione
Tciris <- dtrdp$dates-dtfine$dates###tempo di circolazione del risultat


df$Tcirc <- as.vector(Tcirc)
x <- as.data.frame(table('Tempo circolazione campioni'=df$Tcirc,'Settore'=df$settore))
names(x) <- c('Tempo', 'Settore', 'Nconf')
x<- subset(x, levels(x$Tempo)==levels(x$Tempo)[1:15])
levels(x$Settore)<-c("Alimenti Uomo", "Alimenti Zootecnici", "Controllo Qualità", "Sanità Animale")
x <- x[order(x$Nconf, decreasing=TRUE),]
x$Nconf <- as.numeric(x$Nconf)
x$c <- cumsum(x$Nconf)
x$cc <- by(x$Settore, cumsum(x$Nconf))
x$PC <- x$c/sum(x$Nconf)
x$PC <- x$PC*100


png('tcirc.png', width=1000, height=800)
p <- ggplot(x, aes(x=Tempo))
p+geom_bar(aes(y=Nconf),stat="identity")+labs(x="Giorni (Tempo di circolazione del campione)", y="Numero conferimenti")
dev.off()

png('tcirc2.png', width=1000, height=600)
p <- ggplot(x, aes(Tempo,Nconf))+  facet_wrap(~ Settore)
p+geom_bar()+labs(x="Giorni (Tempo di circolazione del campione)", y="Numero conferimenti")
dev.off()




y <- as.data.frame(table('Tempo circolazione campioni'=df$Tcirc))


########################################################
##########AFLA###########################################

rm(list=ls())
df <- read.csv('afla.csv',sep=";", header=T)



df$dreg <- as.POSIXct(strptime(as.character(df$dreg), "%d/%m/%Y %H.%M.%S" ))
df$dtacc <- as.POSIXct(strptime(as.character(df$dtacc), "%d/%m/%Y %H.%M.%S" ))
df$dtfine <- as.POSIXct(strptime(as.character(df$dtfine), "%d/%m/%Y %H.%M.%S"))

dreg <- epidate(df$dreg)
dtacc <- epidate(df$dtacc)
dtfine <- epidate(df$dtfine)

Tr <- dtfine$dates-dtacc$dates ###tempo di risposta
