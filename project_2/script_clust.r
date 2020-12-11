# pulizia RStudio
cat("\014")
rm(list = ls())

# importazione librerie da utilizzare
library(corrplot)
library(caret)
library(MASS)
library(cluster)

################################################################################
####################    IMPORTAZIONE E PULIZIA DEI DATI    #####################
################################################################################

# caricamento dei dati dal file csv
data = read.csv('tabella.csv', header = TRUE, sep = ";")

# rimozione colonne non utilizzate: colonne che non servono ai fini della
# classificazione dei computer (Ranking TOP500, Nome), colonne che presentano
# valore costante per tutte le osservazioni (OS.Family)
data$Rank <- NULL
data$Previous.Rank <- NULL
data$First.Appearance <- NULL
data$First.Rank <- NULL
data$Name <- NULL
data$Computer <- NULL
data$Power.Source <- NULL
data$OS.Family <- NULL
data$Site.ID <- NULL
data$System.ID <- NULL

# controllo delle colonne rimanenti per valori mancanti
sum(is.na(data$Site))
sum(is.na(data$Manufacturer))
sum(is.na(data$Country))
sum(is.na(data$Year))
sum(is.na(data$Segment))
sum(is.na(data$Total.Cores))
sum(is.na(data$Accelerator.Co.Processor.Cores))
sum(is.na(data$Rmax..TFlop.s.))
sum(is.na(data$Rpeak..TFlop.s.))
sum(is.na(data$Nmax))
sum(is.na(data$Nhalf))
sum(is.na(data$HPCG..TFlop.s.))
sum(is.na(data$Power..kW.))
sum(is.na(data$Power.Efficiency..GFlops.Watts.))
sum(is.na(data$Architecture))
sum(is.na(data$Processor))
sum(is.na(data$Processor.Technology))
sum(is.na(data$Processor.Speed..MHz.))
sum(is.na(data$Operating.System))
sum(is.na(data$Accelerator.Co.Processor))
sum(is.na(data$Cores.per.Socket))
sum(is.na(data$Processor.Generation))
sum(is.na(data$System.Model))
sum(is.na(data$System.Family))
sum(is.na(data$Interconnect.Family))
sum(is.na(data$Interconnect))
sum(is.na(data$Continent))

# notiamo che ci sono un numero non indifferente di NA in alcune colonne,
# eliminiamo tali colonne
data$Accelerator.Co.Processor.Cores <- NULL
data$Nhalf <- NULL
data$HPCG..TFlop.s. <- NULL
data$Power..kW. <- NULL
data$Power.Efficiency..GFlops.Watts. <- NULL

# assegniamo nomi più leggibili alle colonne rimanenti della tabella che saranno
# oggetto della nostra analisi

# stampa sommario struttura iniziale: prime osservazioni
summary(data)
str(data)
with(data, table(Segment))

# convertiamo i dati di tipo Factor in numerico
data$Site <- as.numeric(data$Site)
data$Manufacturer <- as.numeric(data$Manufacturer)
data$Country <- as.numeric(data$Country)
data$Segment <- as.numeric(data$Segment)
data$Architecture <- as.numeric(data$Segment)
data$Processor <- as.numeric(data$Processor)
data$Processor.Technology <- as.numeric(data$Processor.Technology)
data$Operating.System  <- as.numeric(data$Operating.System )
data$Accelerator.Co.Processor <- as.numeric(data$Accelerator.Co.Processor)
data$Processor.Generation <- as.numeric(data$Processor.Generation)
data$System.Model <- as.numeric(data$System.Model)
data$System.Family <- as.numeric(data$System.Family)
data$Interconnect.Family <- as.numeric(data$Interconnect.Family)
data$Interconnect <- as.numeric(data$Interconnect)
data$Continent <- as.numeric(data$Continent)

# eliminiamo eventuali NA, NaN generati durante la conversione dal tipo Factor
# al tipo numeric
sum(is.na(data))
data<-na.omit(data)

# controlliamo la nuova struttura dei dati
str(data)

plot(data)

################################################################################
#####################                  PCA                 #####################
################################################################################
pca = princomp(scale(data))
summary(pca)
biplot(pca)
loadings(pca)

################################################################################
#####################          CLUSTERING K-MEANS          #####################
################################################################################
km = kmeans(data, 6, nstart=20)
km.clusters = km$cluster
plot(pca$scores[,1:22], col=km.clusters, pch=20)

################################################################################
#####################            CLUSTERING PAM            #####################
################################################################################
pm = pam(data, 6)
pm.clusters = pm$cluster
plot(data, col=pm$cluster, pch=20)
layout(t(1:2))
plot(pm)
