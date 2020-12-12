# pulizia RStudio
cat("\014")
rm(list = ls())

# importazione librerie da utilizzare
library(corrplot)
library(caret)
library(MASS)
library(cluster)
library(ggbiplot)
library(pROC)
require(multiROC)
require(ggplot2)

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

# Segment
#    Academic Government   Industry     Others   Research     Vendor 
#          67         34        273         14        103          9

# eliminiamo eventuali punti contenenti valori NA o NaN
sum(is.na(data))
data <- na.omit(data)

# convertiamo i dati di tipo Factor in numeric in modo da non avere problemi in
# seguito per i differenti tipi di analisi
data$Site <- as.numeric(data$Site)
data$Manufacturer <- as.numeric(data$Manufacturer)
data$Country <- as.numeric(data$Country)
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

# controlliamo la nuova struttura dei dati
str(data)

# costruzione modello LDA preliminare
lda = lda(Segment~., data=data)
plot(lda, col = 1 + as.numeric(data$Segment))
lda.values = predict(lda)
plot(lda.values$x, pch=20, col = data$Segment)
legend("bottomright",
       inset = 0.02,
       levels(data$Segment),
       col = c("black", "red", "green3", "blue", "cyan", "magenta"),
       pch = 19,
       bg = "gray")

# valutiamo l’accuratezza del modello LDA preliminare
sum(data$Segment == lda.values$class)
sum(data$Segment == lda.values$class)/length(data$Segment)

# vengono classificati correttamente (nel segment di mercato di appartenenza)
# 363 supercomputer su 495, accuratezza del 73.33%

# valutiamo l’errore
sum(data$Segment != lda.values$class)
sum(data$Segment != lda.values$class)/length(data$Segment)

# vengono classificati erroneamente solamente 132 supercomputer su 495, errore
# del 26.67%

# visualizziamo la matrice di confusione
confusionMatrix(as.factor(lda.values$class), as.factor(data$Segment))

# non è stata effettuata alcuna suddivisione dei dati in training set e test
# set, e si ottengono risultati non soddisfacenti inferiori all'80% di
# precisione: si procede con una analisi di tipo PCA per cercare di ridurre
# il numero di fattori in gioco e, soprattutto, catturare quelle componenti
# che meglio descrivono il fattore Segment

# costruzione modello QDA preliminare
qda = qda(Segment~., data=data)

# Error in qda.default(x, grouping, ...) : 
#   some group is too small for 'qda'
# la QDA non funzione nemmeno:
# The problem seems to be that you have more variables (columns) than the
# smallest of your classes. You need to reduce the number of variables or use a
# LDA instead.

# salviamo la colonna dei fattori Segment per utilizzarla più avanti
Segments = data$Segment
data$Segment <- as.numeric(data$Segment)

################################################################################
#####################                  PCA                 #####################
################################################################################

# primo modello PCA senza usare l'informazione sulla classe
pca = princomp(scale(subset(data, select=-Segment)))
data.pca <- as.data.frame(pca$scores[,1:10])
data.pca <- cbind(data.pca, Segments)
colnames(data.pca)[11] <- "Segment"
lda = lda(Segment~., data=data.pca)
plot(lda, col = 1 + as.numeric(data.pca$Segment))
lda.values=predict(lda)
plot(lda.values$x, pch=20, col = as.numeric(data.pca$Segment)+1)
legend("bottomright",
       inset = 0.02,
       levels(Segments),
       col = c("black", "red", "green3", "blue", "cyan", "magenta"),
       pch = 19,
       bg = "gray")
sum(data.pca$Segment == lda.values$class)
sum(data.pca$Segment == lda.values$class)/length(data.pca$Segment)
sum(data.pca$Segment != lda.values$class)
sum(data.pca$Segment != lda.values$class)/length(data$Segment)
# senza usare l'informazione sulla classe otteniamo informazioni anche peggiori
# rispetto al modello LDA originario di partenza: 337 classificazioni corrette
# su 495, accuratezza del 68.08%, e margine di errore pari a 158 su 495, in
# percentuale 31.91%

# secondo modello PCA utilizzando l'informazione sulla classe
pca = princomp(scale(data))
summary(pca)
loadings(pca)

# biplot
ggbiplot(pca, choices = c(10,7))

plot(cumsum(pca$sdev^2)/sum(pca$sdev^2), type="b", ylim=c(0,1),
     ylab = "Varianza Spiegata Cumulata", xlab = "Componenti Principali")
segments(1, 0.8, 23, 0.8, col="red", lwd = 2)

# il numero iniziale di fattori è certamente elevato, e da quanto riusciamo a
# vedere dalle varianze cumulate del modello PCA, abbiamo bisogno di almeno le
# prime 10 componenti principali per catturare un buon 84% della struttura
# originale, questo risultato non è fantastico certamente ma comunque meglio
# della situazione originale, la speranza è che così facendo la QDA sia almeno
# possibile da eseguire e che la LDA ci fornisca risultati migliori

# costruiamo il data frame con il minor numero di componenti che megglio
# catturano la variabilità del fattore originale Segment: le componenti principali
# che sono maggiormente correlate con Segment sono Comp.10, Comp.7, Comp.6,
# Comp.8, Comp.12, Comp.11, Comp.15, Comp.1, Comp.17
data.pca <- as.data.frame(pca$scores[,c(10, 7, 6, 8, 12)])
data.pca <- cbind(data.pca, Segments)
colnames(data.pca)[6] <- "Segment"

################################################################################
####################     ANALISI DISCRIMINANTE LINEARE     #####################
################################################################################
lda = lda(Segment~., data=data.pca)
plot(lda, col = 1 + as.numeric(data.pca$Segment))

# visualizzazione grafica dei risultati della classificazione effettuata
lda.values = predict(lda)
plot(lda.values$x, pch=20, col = data.pca$Segment, main = "Classificazione Multiclasse tramite Analisi Driscriminante Lineare")
legend("bottomright",
       inset = 0.02,
       levels(Segments),
       col = c("black", "red", "green3", "blue", "cyan", "magenta"),
       pch = 19,
       bg = "gray")

# valutiamo l’accuratezza
sum(data.pca$Segment == lda.values$class)
sum(data.pca$Segment == lda.values$class)/length(data.pca$Segment)

# vengono classificati correttamente (nel segment di mercato di appartenenza)
# 442 supercomputer su 495, accuratezza del 89.29%

# valutiamo l’errore
sum(data.pca$Segment != lda.values$class)
sum(data.pca$Segment != lda.values$class)/length(data$Segment)

# vengono classificati erroneamente solamente 53 supercomputer su 495, errore
# del 10.70%

# visualizziamo la matrice di confusione
confusionMatrix(as.factor(lda.values$class), as.factor(Segments))

# visualizziamo curva ROC multiclasse
lda_plot_data <- as.data.frame(as.numeric(data.pca$Segment == "Academic"))
lda_plot_data <- cbind(lda_plot_data, as.numeric(data.pca$Segment == "Government"))
lda_plot_data <- cbind(lda_plot_data, as.numeric(data.pca$Segment == "Industry"))
lda_plot_data <- cbind(lda_plot_data, as.numeric(data.pca$Segment == "Others"))
lda_plot_data <- cbind(lda_plot_data, as.numeric(data.pca$Segment == "Research"))
lda_plot_data <- cbind(lda_plot_data, as.numeric(data.pca$Segment == "Vendor"))
lda_plot_data <- cbind(lda_plot_data, lda.values$posterior)
colnames(lda_plot_data) <- c("Academic_true",
                             "Government_true",
                             "Industry_true",
                             "Others_true",
                             "Research_true",
                             "Vendor_true",
                             "Academic_pred_LDA",
                             "Government_pred_LDA",
                             "Industry_pred_LDA",
                             "Others_pred_LDA",
                             "Research_pred_LDA",
                             "Vendor_pred_LDA")

roc_res <- multi_roc(lda_plot_data)
roc_res_df <- plot_roc_data(roc_res)

ggplot(roc_res_df, aes(x = 1-Specificity, y=Sensitivity)) +
  ggtitle("Curva ROC Classificazione Multiclasse tramite Analisi Driscriminante Lineare") +
  geom_path(aes(color = Group, linetype=Method), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), colour='grey', linetype = 'dotdash') +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, linetype="solid", colour ="black"))

# Autovaluazione Analisi Discriminante Lineare
acc = rep(0, 30)
l = nrow(data)
for(i in 1:30) {
  idx = sample(l, 30)
  train = data.pca[-idx,]
  test = data.pca[idx,]
  train.lda = lda(Segment~., data=train)
  test.pt = predict(train.lda, newdata=test)$class
  acc[i] = sum(test.pt == data.pca$Segment[idx])/30
}
mean(acc)
sd(acc)
hist(acc, 10, main="Accuratezza della Classificazione per mezzo di LDA",
     col = c("blue", "red", "gray", "green"), xlab = "Accuratezza")

################################################################################
####################   ANALISI DISCRIMINANTE QUADRATICA    #####################
################################################################################

# costruiamo il data frame con le osservazioni secondo la PCA: non possiamo
# usare più di fattori per evitare di avere l'errore ottenuto nel modello
# originario di partenza
data.pca <- as.data.frame(pca$scores[,c(10, 7, 6, 8, 12)])
data.pca <- cbind(data.pca, Segments)
colnames(data.pca)[6] <- "Segment"

# modello per analisi discriminante quadratica
qda = qda(Segment~., data=data.pca)
qda.values = predict(qda)
qda.post = qda.values$posterior

# visualizzazione grafica dei risultati della classificazione effettuata
plot(qda.post, pch=20, col = data.pca$Segment)
legend("topright",
       inset = 0.02,
       levels(Segments),
       col = c("black", "red", "green3", "blue", "cyan", "magenta"),
       pch = 19,
       bg = "gray")

# valutiamo l’accuratezza
sum(data.pca$Segment == qda.values$class)
sum(data.pca$Segment == qda.values$class)/length(data$Segment)

# vengono classificati correttamente (nel segment di mercato di appartenenza)
# 458 supercomputer su 495, accuratezza del 92.52%

# valutiamo l’errore
sum(data.pca$Segment != qda.values$class)
sum(data.pca$Segment != qda.values$class)/length(data$Segment)

# vengono classificati erroneamente solamente 37 supercomputer su 495, errore del
# 7.47%

# visualizziamo la matrice di confusione
confusionMatrix(as.factor(qda.values$class), as.factor(Segments))

# visualizziamo curva ROC multiclasse
qda_plot_data <- as.data.frame(as.numeric(data.pca$Segment == "Academic"))
qda_plot_data <- cbind(qda_plot_data, as.numeric(data.pca$Segment == "Government"))
qda_plot_data <- cbind(qda_plot_data, as.numeric(data.pca$Segment == "Industry"))
qda_plot_data <- cbind(qda_plot_data, as.numeric(data.pca$Segment == "Others"))
qda_plot_data <- cbind(qda_plot_data, as.numeric(data.pca$Segment == "Research"))
qda_plot_data <- cbind(qda_plot_data, as.numeric(data.pca$Segment == "Vendor"))
qda_plot_data <- cbind(qda_plot_data, qda.post)
colnames(qda_plot_data) <- c("Academic_true",
                             "Government_true",
                             "Industry_true",
                             "Others_true",
                             "Research_true",
                             "Vendor_true",
                             "Academic_pred_QDA",
                             "Government_pred_QDA",
                             "Industry_pred_QDA",
                             "Others_pred_QDA",
                             "Research_pred_QDA",
                             "Vendor_pred_QDA")

roc_res <- multi_roc(qda_plot_data)
roc_res_df <- plot_roc_data(roc_res)

ggplot(roc_res_df, aes(x = 1-Specificity, y=Sensitivity)) +
  geom_path(aes(color = Group, linetype=Method), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))

# Autovaluazione Analisi Discriminante Quadratica
acc = rep(0, 30)
l = nrow(data)
for(i in 1:30) {
  idx = sample(l, 30)
  train = data.pca[-idx,]
  test = data.pca[idx,]
  train.qda = qda(Segment~., data=train)
  test.pt = predict(train.qda, newdata=test)$class
  acc[i] = sum(test.pt == data.pca$Segment[idx])/30
}
mean(acc)
sd(acc)
hist(acc, 10, main="Accuratezza della Classificazione per mezzo di QDA",
     col = c("blue", "red", "gray", "green"), xlab = "Accuratezza")

################################################################################
#####################         REGRESSIONE LOGISTICA        #####################
################################################################################

# costruiamo il data frame con le osservazioni secondo la PCA: in questo caso
# dobbiamo tenere a mente anche il fatto che la nostra scelta è influenzata
# anche da risultati che otteniamo, in termini di convergenza, dal modello
# di regressione logistica
data.pca <- as.data.frame(pca$scores[,c(10, 7, 6, 8, 12)])
data.pca <- cbind(data.pca, Segments)
colnames(data.pca)[6] <- "Segment"

# regressione logistica per classificare Academic
dt = data.pca[,1:5]
dt$class<-as.numeric(data.pca$Segment == "Academic")
academic.glm = glm(class~., family = binomial, data = dt)
academic.p = predict(academic.glm, type="response")

# regressione logistica per classificare Government
dt = data.pca[,1:5]
dt$class<-as.numeric(data.pca$Segment == "Government")
government.glm = glm(class~., family = binomial, data = dt)
government.p = predict(government.glm, type="response")

# regressione logistica per classificare Industry
dt = data.pca[,1:5]
dt$class<-as.numeric(data.pca$Segment == "Industry")
industry.glm = glm(class~., family = binomial, data = dt)
industry.p = predict(industry.glm, type="response")

# regressione logistica per classificare Others
dt = data.pca[,1:5]
dt$class<-as.numeric(data.pca$Segment == "Others")
others.glm = glm(class~., family = binomial, data = dt)
others.p = predict(others.glm, type="response")

# regressione logistica per classificare Research
dt = data.pca[,1:5]
dt$class<-as.numeric(data.pca$Segment == "Research")
research.glm = glm(class~., family = binomial, data = dt)
research.p = predict(research.glm, type="response")

# regressione logistica per classificare Vendor
dt = data.pca[,1:5]
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

# valutiamo l’accuratezza
sum(data.pca$Segment == res)
sum(data.pca$Segment == res)/l

# valutiamo l’errore
sum(data.pca$Segment != res)
sum(data.pca$Segment != res)/l

# dovendo abbassare il numero di fattori presi in considerazione per far si che
# l'algoritmo converga per tutti i modelli di regressione logistica costruiti
# la precisione che otteniamo non è proprio soddisfacente rispetto ai precedenti
# due modelli analizzati: 343 classificazioni corrette e 152 errate, accuratezza
# del 69.29% con un margine di errore pari al 30.70%

# visualizziamo curva ROC multiclasse
lda_plot_data <- as.data.frame(as.numeric(data.pca$Segment == "Academic"))
lda_plot_data <- cbind(lda_plot_data, as.numeric(data.pca$Segment == "Government"))
lda_plot_data <- cbind(lda_plot_data, as.numeric(data.pca$Segment == "Industry"))
lda_plot_data <- cbind(lda_plot_data, as.numeric(data.pca$Segment == "Others"))
lda_plot_data <- cbind(lda_plot_data, as.numeric(data.pca$Segment == "Research"))
lda_plot_data <- cbind(lda_plot_data, as.numeric(data.pca$Segment == "Vendor"))
lda_plot_data <- cbind(lda_plot_data, academic.p)
lda_plot_data <- cbind(lda_plot_data, government.p)
lda_plot_data <- cbind(lda_plot_data, industry.p)
lda_plot_data <- cbind(lda_plot_data, others.p)
lda_plot_data <- cbind(lda_plot_data, research.p)
lda_plot_data <- cbind(lda_plot_data, vendor.p)
colnames(lda_plot_data) <- c("Academic_true",
                             "Government_true",
                             "Industry_true",
                             "Others_true",
                             "Research_true",
                             "Vendor_true",
                             "Academic_pred_GLM",
                             "Government_pred_GLM",
                             "Industry_pred_GLM",
                             "Others_pred_GLM",
                             "Research_pred_GLM",
                             "Vendor_pred_GLM")

roc_res <- multi_roc(lda_plot_data)
roc_res_df <- plot_roc_data(roc_res)

ggplot(roc_res_df, aes(x = 1-Specificity, y=Sensitivity)) +
  geom_path(aes(color = Group, linetype=Method), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))