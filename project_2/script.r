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
#data$Segment <- as.numeric(data$Segment)
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

################################################################################
#####################                  PCA                 #####################
################################################################################
pca = princomp(scale(subset(data, select=-Segment)))
summary(pca)
biplot(pca)
loadings(pca)

# costruiamo il data frame con le osservazioni secondo la PCA
data.pca<-as.data.frame(pca$scores[,1:10])
data.pca<-cbind(data.pca, data$Segment)
colnames(data.pca)[11] <- "Segment"

################################################################################
####################     ANALISI DISCRIMINANTE LINEARE     #####################
################################################################################
lda = lda(Segment~., data=data.pca)
plot(lda, col = 1 + as.numeric(data.pca$Segment))

lda.values=predict(lda)
plot(lda.values$x, pch=20, col = as.numeric(data$Segment)+1)

# valutiamo l’accuratezza
sum(data$Segment == lda.values$class)
sum(data$Segment == lda.values$class)/length(data$Segment)

# vengono classificati correttamente (nel segment di mercato di appartenenza)
# 498 supercomputer su 500, accuratezza del 99.6%

# valutiamo l’errore
sum(data$Segment != lda.values$class)
sum(data$Segment != lda.values$class)/length(data$Segment)

# vengono classificati erroneamente solamente 2 supercomputer su 500, errore del
# 0.4%

# sulla tabella sulla quale è stato addrestrato, il modello fa parecchio bene
# anche se ovviamente questo non è un risultato sorprendente, anzi, ci sarebbe
# stato da preoccuparsi fosse stato il contrario

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

# costruiamo il data frame con le osservazioni secondo la PCA
data.pca<-as.data.frame(pca$scores[,1:7])
data.pca<-cbind(data.pca, data$Segment)
colnames(data.pca)[8] <- "Segment"

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

# sulla tabella sulla quale è stato addrestrato, il modello fa parecchio bene
# anche se ovviamente questo non è un risultato sorprendente, anzi, ci sarebbe
# stato da preoccuparsi fosse stato il contrario

# Autovaluazione Analisi Discriminante Lineare
acc = rep(0, 1)
l = nrow(data)
for(i in 1:30) {
  idx = sample(l, 1)
  train = data.pca[-idx,]
  test = data.pca[idx,]
  train.qda = qda(Segment~., data=train)
  test.pt = predict(train.qda, newdata=test)$class
  acc[i] = sum(test.pt == data$Segment[idx])/1
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