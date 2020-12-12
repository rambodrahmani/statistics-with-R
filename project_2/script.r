# pulizia RStudio
cat("\014")
rm(list = ls())

# importazione librerie da utilizzare
library(corrplot)
library(caret)
library(MASS)
library(cluster)
library(ggbiplot)

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
colnames(data) <- c("Site", "Manufacturer", "Country", "Year", "Segment",
                    "TotalCores", "Rmax", "Rpeak", "Nmax", "Architecture",
                    "Processor", "ProcessorTechnology", "ProcessorSpeed",
                    "OperatingSystem", "CoProcessor", "CoresPerSocket",
                    "ProcessorGeneration", "SystemModel", "SystemFamily",
                    "InterconnectFamily", "Interconnect", "Continent")

# stampa sommario struttura iniziale: prime osservazioni
summary(data)
str(data)
with(data, table(Segment))

# convertiamo i dati di tipo Factor in numeric in modo da non avere problemi in
# seguito per i differenti tipi di analisi
data$Site <- as.numeric(data$Site)
data$Manufacturer <- as.numeric(data$Manufacturer)
data$Country <- as.numeric(data$Country)
data$Segment <- as.numeric(data$Segment)
data$Architecture <- as.numeric(data$Architecture)
data$Processor <- as.numeric(data$Processor)
data$ProcessorTechnology <- as.numeric(data$ProcessorTechnology)
data$OperatingSystem  <- as.numeric(data$OperatingSystem)
data$CoProcessor <- as.numeric(data$CoProcessor)
data$ProcessorGeneration <- as.numeric(data$ProcessorGeneration)
data$SystemModel <- as.numeric(data$SystemModel)
data$SystemFamily <- as.numeric(data$SystemFamily)
data$InterconnectFamily <- as.numeric(data$InterconnectFamily)
data$Interconnect <- as.numeric(data$Interconnect)
data$Continent <- as.numeric(data$Continent)

# eliminiamo eventuali NA, NaN generati durante la conversione dal tipo Factor
# al tipo numeric
sum(is.na(data))
data<-na.omit(data)

# controlliamo la nuova struttura dei dati
str(data)

# costruzione modello LDA preliminare
lda = lda(Segment~., data=data)
plot(lda, col = 1 + as.numeric(data$Segment))
lda.values = predict(lda)
plot(lda.values$x, pch=20, col = as.numeric(data$Segment)+1)

# valutiamo l’accuratezza
sum(data$Segment == lda.values$class)
sum(data$Segment == lda.values$class)/length(data$Segment)

# vengono classificati correttamente (nel segment di mercato di appartenenza)
# 363 supercomputer su 500, accuratezza del 73.33%

# valutiamo l’errore
sum(data$Segment != lda.values$class)
sum(data$Segment != lda.values$class)/length(data$Segment)

# vengono classificati erroneamente solamente 132 supercomputer su 500, errore
# del 26.67%

# non è stata effettuata alcuna suddivisione dei dati in training set e test
# set, e si ottengono risultati non soddisfacenti inferiori all'80% di
# precisione: si procede con una analisi di tipo PCA per cercare di ridurre
# il numero di fattori in gioco e, soprattutto, catturare quelle componenti
# che meglio descrivono il fattore Segment

# modello per analisi discriminante quadratica
qda = qda(Segment~., data=data)

# Error in qda.default(x, grouping, ...) : 
#   some group is too small for 'qda'
# la QDA non funzione nemmeno:
# The problem seems to be that you have more variables (columns) than the
# smallest of your classes. You need to reduce the number of variables or use a
# LDA instead.

################################################################################
#####################                  PCA                 #####################
################################################################################

# primo modello PCA senza usare l'informazione sulla classe
pca = princomp(scale(subset(data, select=-Segment)))
data.pca<-as.data.frame(pca$scores[,1:10])
data.pca<-cbind(data.pca, data$Segment)
colnames(data.pca)[11] <- "Segment"
lda = lda(Segment~., data=data.pca)
plot(lda, col = 1 + as.numeric(data.pca$Segment))
lda.values=predict(lda)
plot(lda.values$x, pch=20, col = as.numeric(data.pca$Segment)+1)
sum(data$Segment == lda.values$class)
sum(data$Segment == lda.values$class)/length(data.pca$Segment)
sum(data$Segment != lda.values$class)
sum(data$Segment != lda.values$class)/length(data$Segment)
# senza usare l'informazione sulla classe otteniamo informazioni anche peggiori
# rispetto al modello LDA originario di partenza: 337 classificazioni corrette
# su 500, accuratezza del 68.08%, e margine di errore pari a 158 su 500, in
# percentuale 31.91%

# secondo modello PCA utilizzando l'informazione sulla classe
pca = princomp(scale(data))
summary(pca)
loadings(pca)
ggbiplot(pca, choices = c(1,2))

plot(cumsum(pca$sdev^2)/sum(pca$sdev^2), type="b", ylim=c(0,1),
     ylab = "Varianza Spiegata", xlab = "Componenti Principali")
segments(1, 0.8, 23, 0.8, col="red")

# il numero iniziale di fattori è certamente elevato, e da quanto riusciamo a
# vedere dalle varianze cumulate del modello PCA, abbiamo bisogno di almeno le
# prime 10 componenti principali per catturare un buon 84% della struttura
# originale, questo risultato non è fantastico certamente ma comunque meglio
# della situazione originale, la speranza è che così facendo la QDA sia almeno
# possibile da eseguire e che la LDA ci fornisca risultati migliori

# costruiamo il data frame con il minor numero di componenti che megglio
# catturano la variabilità del fattore originale Segment
data.pca<-as.data.frame(pca$scores[,c(1, 6, 7, 8, 10, 11, 12, 15, 17)])
data.pca<-cbind(data.pca, data$Segment)
colnames(data.pca)[10] <- "Segment"

################################################################################
####################     ANALISI DISCRIMINANTE LINEARE     #####################
################################################################################
lda = lda(Segment~., data=data.pca)
plot(lda, col = 1 + as.numeric(data.pca$Segment))

lda.values=predict(lda)
plot(lda.values$x, pch=20, col = as.numeric(data.pca$Segment)+1)

# valutiamo l’accuratezza
sum(data$Segment == lda.values$class)
sum(data$Segment == lda.values$class)/length(data.pca$Segment)

# vengono classificati correttamente (nel segment di mercato di appartenenza)
# 490 supercomputer su 500, accuratezza del 98.98%

# valutiamo l’errore
sum(data$Segment != lda.values$class)
sum(data$Segment != lda.values$class)/length(data$Segment)

# vengono classificati erroneamente solamente 5 supercomputer su 500, errore
# del 1.02%

# Autovaluazione Analisi Discriminante Lineare
acc = rep(0, 30)
l = nrow(data)
for(i in 1:30) {
  idx = sample(l, 30)
  train = data.pca[-idx,]
  test = data.pca[idx,]
  train.lda = lda(Segment~., data=train)
  test.pt = predict(train.lda, newdata=test)$class
  acc[i] = sum(test.pt == data$Segment[idx])/30
}
mean(acc)
sd(acc)
hist(acc)

################################################################################
####################   ANALISI DISCRIMINANTE QUADRATICA    #####################
################################################################################

# costruiamo il data frame con le osservazioni secondo la PCA: non possiamo
# usare più di fattori per evitare di avere l'errore ottenuto nel modello
# originario di partenza
data.pca<-as.data.frame(pca$scores[,c(1, 6, 7, 8, 10)])
data.pca<-cbind(data.pca, data$Segment)
colnames(data.pca)[6] <- "Segment"

# modello per analisi discriminante quadratica
qda = qda(Segment~., data=data.pca)

# valutiamo l’accuratezza
sum(data$Segment == predict(qda)$class)
sum(data$Segment == predict(qda)$class)/length(data$Segment)

# vengono classificati correttamente (nel segment di mercato di appartenenza)
# 498 supercomputer su 500, accuratezza del 99.6%

# valutiamo l’errore
sum(data$Segment != predict(qda)$class)
sum(data$Segment != predict(qda)$class)/length(data$Segment)

# vengono classificati erroneamente solamente 2 supercomputer su 500, errore del
# 0.4%

# Autovaluazione Analisi Discriminante Quadratica
acc = rep(0, 25)
l = nrow(data)
for(i in 1:30) {
  idx = sample(l, 25)
  train = data.pca[-idx,]
  test = data.pca[idx,]
  train.qda = qda(Segment~., data=train)
  test.pt = predict(train.qda, newdata=test)$class
  acc[i] = sum(test.pt == data$Segment[idx])/25
}
mean(acc)
sd(acc)
hist(acc)

################################################################################
#####################         REGRESSIONE LOGISTICA        #####################
################################################################################

# costruiamo il data frame con le osservazioni secondo la PCA
data.pca<-as.data.frame(pca$scores[,1:9])
data.pca<-cbind(data.pca, data$Segment)
colnames(data.pca)[10] <- "Segment"

# regressione logistica per classificare Academic
dt = data.pca[,1:9]
dt$class<-as.numeric(data.pca$Segment == "Academic")
academic.glm = glm(class~., family = binomial, data = dt)
academic.p = predict(academic.glm, type="response")

# costruiamo il data frame con le osservazioni secondo la PCA
data.pca<-as.data.frame(pca$scores[,1:12])
data.pca<-cbind(data.pca, data$Segment)
colnames(data.pca)[13] <- "Segment"

# regressione logistica per classificare Government
dt = data.pca[,1:12]
dt$class<-as.numeric(data.pca$Segment == "Government")
government.glm = glm(class~., family = binomial, data = dt)
government.p = predict(government.glm, type="response")

# costruiamo il data frame con le osservazioni secondo la PCA
data.pca<-as.data.frame(pca$scores[,1:12])
data.pca<-cbind(data.pca, data$Segment)
colnames(data.pca)[13] <- "Segment"

# regressione logistica per classificare Industry
dt = data.pca[,1:12]
dt$class<-as.numeric(data.pca$Segment == "Industry")
industry.glm = glm(class~., family = binomial, data = dt)
industry.p = predict(industry.glm, type="response")

# costruiamo il data frame con le osservazioni secondo la PCA
data.pca<-as.data.frame(pca$scores[,1:12])
data.pca<-cbind(data.pca, data$Segment)
colnames(data.pca)[13] <- "Segment"

# regressione logistica per classificare Others
dt = data.pca[,1:12]
dt$class<-as.numeric(data.pca$Segment == "Others")
others.glm = glm(class~., family = binomial, data = dt)
others.p = predict(others.glm, type="response")

# costruiamo il data frame con le osservazioni secondo la PCA
data.pca<-as.data.frame(pca$scores[,1:12])
data.pca<-cbind(data.pca, data$Segment)
colnames(data.pca)[13] <- "Segment"

# regressione logistica per classificare Research
dt = data.pca[,1:12]
dt$class<-as.numeric(data.pca$Segment == "Research")
research.glm = glm(class~., family = binomial, data = dt)
research.p = predict(research.glm, type="response")

# costruiamo il data frame con le osservazioni secondo la PCA
data.pca<-as.data.frame(pca$scores[,1:9])
data.pca<-cbind(data.pca, data$Segment)
colnames(data.pca)[10] <- "Segment"

# regressione logistica per classificare Vendor
dt = data.pca[,1:9]
dt$class<-as.numeric(data.pca$Segment == "Vendor")
vendor.glm = glm(class~., family = binomial, data = dt)
vendor.p = predict(vendor.glm, type="response")

# Calcoliamo la risposta finale e l’accuratezza.
probs = cbind(academic.p, government.p, industry.p, others.p, research.p, vendor.p)
l = length(data.pca$Segment)
res = rep(0, l)
for(i in 1:l) {
  res[i] = which.max(probs[i,])
}
res <- factor(res)
levels(res) <- c("Academic", "Government", "Industry", "Others", "Research", "Vendor")
sum(data.pca$Segment == res)
sum(data.pca$Segment == res)/l