# pulizia RStudio
cat("\014")
rm(list = ls())

# importazione librerie da utilizzare
library(corrplot)

# caricamento dei dati dal file csv
data = read.csv('data.csv', header = TRUE, sep = ";")

# rimozione colonne non utilizzate
data$Rank <- NULL
data$Previous.Rank <- NULL
data$First.Appearance <- NULL
data$First.Rank <- NULL
data$Name <- NULL
data$Computer <- NULL
data$Site <- NULL
data$Manufacturer <- NULL
data$Country <- NULL
data$Year <- NULL
data$Segment <- NULL
data$Nmax <- NULL
data$Nhalf <- NULL
data$Power.Source <- NULL
data$Power.Efficiency..GFlops.Watts. <- NULL
data$Architecture <- NULL
data$Processor <- NULL
data$Processor.Technology <- NULL
data$Operating.System <- NULL
data$OS.Family <- NULL
data$Accelerator.Co.Processor <- NULL
data$Processor.Generation <- NULL
data$System.Model <- NULL
data$System.Family <- NULL
data$Interconnect.Family <- NULL
data$Interconnect <- NULL
data$Continent <- NULL
data$Site.ID <- NULL
data$System.ID <- NULL

# stampa sommario struttura iniziale: prime osservazioni
summary(data)
str(data)
plot(data)

# controllo delle colonne per valori mancanti
sum(is.na(data$Total.Cores))
sum(is.na(data$Accelerator.Co.Processor.Cores))
sum(is.na(data$Rmax..TFlop.s.))
sum(is.na(data$Rpeak..TFlop.s.))
sum(is.na(data$HPCG..TFlop.s.))
sum(is.na(data$Power..kW.))
sum(is.na(data$Processor.Speed..MHz.))
sum(is.na(data$Cores.per.Socket))

# notiamo che ci sono un numero non indifferente di NA in alcune colonne,
# eliminiamo tali colonne
data$Accelerator.Co.Processor.Cores <- NULL
data$HPCG..TFlop.s. <- NULL
data$Power..kW. <- NULL

# assegniamo nomi più leggibili alle colonne della tabella
colnames(data) <- c("TotalCores", "Rmax", "Rpeak", "ProcessorSpeed", "CoresPerSocket")

# standardizzazione tabella
st_data = data.frame(scale(data))

# visualizzazione tabella standardizzata
# la standardizzazione non varia il grafico di dispersione
# e non è stata quindi considerata nel seguito dell'analisi
plot(st_data)

# nuova stampa dei dati prima di iniziare l'analisi
summary(data)
str(data)
head(data)
plot(data)
corrplot(cor(data), method = "number")

# costruzione modello di regressione lineare
r = matrix(ncol = 2, nrow = 4)

lm.1<-lm(Rmax~Rpeak+TotalCores+CoresPerSocket+ProcessorSpeed, data = data)
summary(lm.1)
r[1,]= c(summary(lm.1)$r.squared, summary(lm.1)$adj.r.squared)

lm.2<-lm(Rmax~Rpeak+TotalCores+CoresPerSocket, data = data)
summary(lm.2)
r[2,]= c(summary(lm.2)$r.squared, summary(lm.2)$adj.r.squared)

lm.3<-lm(Rmax~Rpeak+TotalCores, data = data)
summary(lm.3)
r[3,]= c(summary(lm.3)$r.squared, summary(lm.3)$adj.r.squared)

lm.4<-lm(Rmax~Rpeak, data = data)
summary(lm.4)
r[4,]= c(summary(lm.4)$r.squared, summary(lm.4)$adj.r.squared)

# traccio il grafico di comparazione degli R^2 e R^2 corretti
ymin = min(r)
ymax = max(r)
xl <- expression(R^2 ~ e ~ R^2 ~ Corretto ~ dei ~ 4 ~ Modelli ~ di ~ Regressione ~ Lineare ~ Valutati)
plot(r[,1], pch = 19, type = "b", col = "red", xaxt="n", ylab = "", ylim = c(ymin,ymax), xlab = xl, cex = 1.5, cex.axis = 1.5, cex.lab = 1.5)
axis(1 , at = 0:4, labels = 0:4)
lines(r[,2], pch = 19, type = "b", col = "blue")

# modello di regressione lineare
lm = lm.3
summary(lm)

# analisi dei residui del modello di regressione lineare
lm.resid = residuals(lm)
plot(predict(lm), lm.resid)
par(mfrow=c(1, 2))
hist(lm.resid, 100, freq = FALSE)
lines(sort(lm.resid), dnorm(sort(lm.resid), mean(lm.resid), sd(lm.resid)), col="red", lwd=2)
qqnorm(lm.resid)
qqline(lm.resid, col="red", lwd=2)
skewness = mean(((lm.resid - mean(lm.resid)) / sd(lm.resid))^3)
skewness
kurtosi = mean(((lm.resid - mean(lm.resid)) / sd(lm.resid))^4) - 3
kurtosi
shapiro.test(lm.resid)

# approfondimento analisi dei residui: calcolo della correlazione tra i
# fattori di ingresso e tra i fattori di ingresso e i residui
cor(data$Rpeak, data$TotalCores)
cor(data$Rpeak, lm.resid)
cor(data$TotalCores, lm.resid)

# approfondimento analisi dei residui: valutazione modello di regressione
# lineare con tutti i fattori
lm.1.resid = residuals(lm.1)
shapiro.test(lm.1.resid)

# approfondimento analisi dei residui: valutazione modello di regressione
# lineare semplice
lm.4.resid = residuals(lm.4)
shapiro.test(lm.4.resid)

# approfondimento analisi dei residui: rimozione residui relativi agli outliers
boxplot(lm.resid, main="Boxplot residui iniziali", col=(c("gold","darkgreen")), outcol="red")
Residuals_outliers <- boxplot(lm.resid, plot=FALSE)$out
Residuals_outliers <- rev(sort(Residuals_outliers))
Residuals_outliers
lm.resid<-lm.resid[-which(lm.resid %in% Residuals_outliers[1:length(Residuals_outliers)])]

# plot residui: non ho usato predict(lm) dato che avendo eliminato alcuni dei
# residui ora ho dimensioni differenti
plot(lm.resid)

# visualizziamo nuovamente i grafici relativi ai residui per vedere se la loro
# distribuzione sia migliorata o meno
par(mfrow=c(1, 2))
hist(lm.resid, 100, freq = FALSE)
lines(sort(lm.resid), dnorm(sort(lm.resid), mean(lm.resid), sd(lm.resid)), col="red", lwd=2)
qqnorm(lm.resid)
qqline(lm.resid, col="red", lwd=2)
skewness = mean(((lm.resid - mean(lm.resid)) / sd(lm.resid))^3)
skewness
kurtosi = mean(((lm.resid - mean(lm.resid)) / sd(lm.resid))^4) - 3
kurtosi
shapiro.test(lm.resid)

# approfondimento analisi dei residui: rimozione dei residui che causano la
# deviazione dal un modello Gaussiano
lm.resid.ord = sort(lm.resid)
lm.resid<-lm.resid[-which(lm.resid %in% lm.resid.ord[1:10])]
lm.resid<-lm.resid[-which(lm.resid %in% lm.resid.ord[400:467])]
boxplot(lm.resid, main="Boxplot residui dopo la rimozione degli outliers", col=(c("gold","darkgreen")), outcol="red")

# plot residui: non ho usato predict(lm) dato che avendo eliminato alcuni dei
# residui ora ho dimensioni differenti
plot(lm.resid)

# visualizziamo nuovamente i grafici relativi ai residui per vedere se la loro
# distribuzione sia migliorata o meno
par(mfrow=c(1, 2))
hist(lm.resid, 100, freq = FALSE)
lines(sort(lm.resid), dnorm(sort(lm.resid), mean(lm.resid), sd(lm.resid)), col="red", lwd=2)
qqnorm(lm.resid)
qqline(lm.resid, col="red", lwd=2)
skewness = mean(((lm.resid - mean(lm.resid)) / sd(lm.resid))^3)
skewness
kurtosi = mean(((lm.resid - mean(lm.resid)) / sd(lm.resid))^4) - 3
kurtosi
shapiro.test(lm.resid)

# costruzione modello regressione esponenziale
lm.2<-lm(log(Rmax)~log(TotalCores)+log(Rpeak)+log(ProcessorSpeed)+log(CoresPerSocket), data = data)
summary(lm.2)
lm.2.resid = residuals(lm.2)
plot(predict(lm.2), lm.2.resid)
hist(lm.2.resid, 100, freq = FALSE)
lines(sort(lm.2.resid), dnorm(sort(lm.2.resid), mean(lm.2.resid), sd(lm.2.resid)), col="red")
qqnorm(lm.2.resid)
qqline(lm.2.resid, col="red")
shapiro.test(lm.2.resid)
