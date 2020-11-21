# pulizia RStudio
cat("\014")
rm(list = ls())

# importazione librerie da utilizzare
library(corrplot)
library(scatterplot3d)

################################################################################
####################    IMPORTAZIONE E PULIZIA DEI DATI    #####################
################################################################################

# caricamento dei dati dal file csv
data = read.csv('tabella.csv', header = TRUE, sep = ";")

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

################################################################################
####################          REGRESSIONE LINEARE           ####################
################################################################################

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

#################################################################################################
#################    MANCA DA FARE L'ANALISI DI UN EVENTUALE ALLINEAMENTO      ##################
#################################################################################################
lm.5<-lm(Rmax~TotalCores, data = data)
summary(lm.5)

# traccio il grafico di comparazione degli R^2 e R^2 corretti
ymin = min(r)
ymax = max(r)
r_squared <- expression(R^2)
r_squared_adj <- expression(R^2 ~ Corretto)
xl <- expression(R^2 ~ e ~ R^2 ~ Corretto ~ dei ~ 4 ~ Modelli ~ di ~ Regressione ~ Lineare ~ Valutati)
plot(r[,1], pch = 19, type = "b", col = "red", xaxt="n", ylab = "", ylim = c(ymin,ymax), xlab = xl, cex = 1.5, cex.axis = 1.5, cex.lab = 1.5)
axis(1 , at = 0:4, labels = 0:4)
lines(r[,2], pch = 19, type = "b", col = "blue")
legend(3.53, 0.9696, legend=c(r_squared, r_squared_adj), col=c("red", "blue"), lty=1:1)

# modello di regressione lineare scelto
lm = lm.3
summary(lm)

# scatterplot 3d del modello di regressione lineare
s3d<-scatterplot3d(data$Rpeak, data$TotalCores , data$Rmax , main="Scatterplot 3D Modello di Regressione Lineare", pch=16, highlight.3d=TRUE, type="p", grid = T, xlab = "Rpeak", ylab = "TotalCores", zlab = "Rmax")
s3d$plane3d(lm, draw_lines = F, draw_polygon = F)

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
# lineare con tutti i fattori per assicurarmi che non ci sia qualche predittore
# che spiega quella parte di struttura che ho nei residui
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
lm.resid<-lm.resid[-which(lm.resid %in% lm.resid.ord[360])]
lm.resid<-lm.resid[-which(lm.resid %in% lm.resid.ord[length(lm.resid.ord)])]
boxplot(lm.resid, main="Boxplot residui dopo la rimozione degli outliers", col=(c("gold","darkgreen")), outcol="red")

# plot residui: non ho usato predict(lm) dato che avendo eliminato alcuni dei
# residui ora ho dimensioni differenti
plot(lm.resid)

# visualizziamo nuovamente i grafici relativi ai residui per vedere se la loro
# distribuzione sia migliorata o meno
par(mfrow=c(1, 2))
hist(lm.resid, 100, freq = FALSE)
lines(sort(lm.resid), dnorm(sort(lm.resid), mean(lm.resid), sd(lm.resid)), col="red", lwd=2)
lines(density(lm.resid), col="blue", lwd=2)
qqnorm(lm.resid)
qqline(lm.resid, col="red", lwd=2)
skewness = mean(((lm.resid - mean(lm.resid)) / sd(lm.resid))^3)
skewness
kurtosi = mean(((lm.resid - mean(lm.resid)) / sd(lm.resid))^4) - 3
kurtosi
shapiro.test(lm.resid)

# modello lineare definitivo
linearModel = lm

################################################################################
####################        REGRESSIONE ESPONENZIALE        ####################
################################################################################

# costruzione modello di regressione esponenziale
r = matrix(ncol = 2, nrow = 4)

lm.1<-lm(log(Rmax)~log(Rpeak)+log(TotalCores)+log(CoresPerSocket)+log(ProcessorSpeed), data = data)
summary(lm.1)
r[1,]= c(summary(lm.1)$r.squared, summary(lm.1)$adj.r.squared)

lm.2<-lm(log(Rmax)~log(Rpeak)+log(TotalCores)+log(CoresPerSocket), data = data)
summary(lm.2)
r[2,]= c(summary(lm.2)$r.squared, summary(lm.2)$adj.r.squared)

lm.3<-lm(log(Rmax)~log(Rpeak)+log(TotalCores), data = data)
summary(lm.3)
r[3,]= c(summary(lm.3)$r.squared, summary(lm.3)$adj.r.squared)

lm.4<-lm(log(Rmax)~log(Rpeak), data = data)
summary(lm.4)
r[4,]= c(summary(lm.4)$r.squared, summary(lm.4)$adj.r.squared)

# traccio il grafico di comparazione degli R^2 e R^2 corretti
par(mfrow=c(1, 1))
ymin = min(r)
ymax = max(r)
r_squared <- expression(R^2)
r_squared_adj <- expression(R^2 ~ Corretto)
xl <- expression(R^2 ~ e ~ R^2 ~ Corretto ~ dei ~ 4 ~ Modelli ~ di ~ Regressione ~ Esponenziale ~ Valutati)
plot(r[,1], pch = 19, type = "b", col = "red", xaxt="n", ylab = "", ylim = c(ymin,ymax), xlab = xl, cex = 1.5, cex.axis = 1.5, cex.lab = 1.5)
axis(1 , at = 0:4, labels = 0:4)
lines(r[,2], pch = 19, type = "b", col = "blue")
legend(3.53, 0.8496, legend=c(r_squared, r_squared_adj), col=c("red", "blue"), lty=1:1)

# modello di regressione esponenziale scelto
lm = lm.3
summary(lm)

# scatterplot 3d del modello di regressione esponenziale
s3d<-scatterplot3d(log(data$Rpeak), log(data$TotalCores), log(data$Rmax) , main="Scatterplot 3D Modello di Regressione Esponenziale", pch=16, highlight.3d=TRUE, type="p", grid = T, xlab = "log(Rpeak)", ylab = "log(TotalCores)", zlab = "log(Rmax)")
s3d$plane3d(lm, draw_lines = F, draw_polygon = F)

# analisi dei residui del modello di regressione esponenziale
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
# lineare con tutti i fattori per assicurarmi che non ci sia qualche predittore
# che spiega quella parte di struttura che ho nei residui
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
lm.resid<-lm.resid[-which(lm.resid %in% lm.resid.ord[1:11])]
lm.resid<-lm.resid[-which(lm.resid %in% lm.resid.ord[279:279])]
boxplot(lm.resid, main="Boxplot residui dopo la rimozione degli outliers", col=(c("gold","darkgreen")), outcol="red")

# plot residui: non ho usato predict(lm) dato che avendo eliminato alcuni dei
# residui ora ho dimensioni differenti
plot(lm.resid)

# visualizziamo nuovamente i grafici relativi ai residui per vedere se la loro
# distribuzione sia migliorata o meno
par(mfrow=c(1, 2))
hist(lm.resid, 100, freq = FALSE)
lines(sort(lm.resid), dnorm(sort(lm.resid), mean(lm.resid), sd(lm.resid)), col="red", lwd=2)
lines(density(lm.resid), col="blue", lwd=2)
qqnorm(lm.resid)
qqline(lm.resid, col="red", lwd=2)
skewness = mean(((lm.resid - mean(lm.resid)) / sd(lm.resid))^3)
skewness
kurtosi = mean(((lm.resid - mean(lm.resid)) / sd(lm.resid))^4) - 3
kurtosi
shapiro.test(lm.resid)

# modello esponenziale definitivo
exponentialModel = lm

# comparazione scatterplot 3d dei due modelli ottenuti
par(mfrow=c(1, 2))
# scatterplot 3d del modello di regressione lineare
s3d<-scatterplot3d(data$Rpeak, data$TotalCores , data$Rmax , main="Regressione Lineare", pch=16, highlight.3d=TRUE, type="p", grid = T, xlab = "Rpeak", ylab = "TotalCores", zlab = "Rmax")
s3d$plane3d(linearModel, draw_lines = F, draw_polygon = F)
# scatterplot 3d del modello di regressione esponenziale
s3d<-scatterplot3d(log(data$Rpeak), log(data$TotalCores), log(data$Rmax) , main="Regressione Esponenziale", pch=16, highlight.3d=TRUE, type="p", grid = T, xlab = "log(Rpeak)", ylab = "log(TotalCores)", zlab = "log(Rmax)")
s3d$plane3d(exponentialModel, draw_lines = F, draw_polygon = F)

################################################################################
####################               PREDIZIONE               ####################
################################################################################

# dati in scala logaritmica
ldata = log(data)

# creazione sottoinsiemi di training e di test
testset = sort(sample(500,50))
data_train = data[-testset,]
data_test = data[testset,]
ldata_train = ldata[-testset,]
ldata_test = ldata[testset,]

# costruzione modelli lineare e modello esponenziale
data_train.lm = lm(Rmax~Rpeak+TotalCores, data=data_train)
ldata_train.lm = lm(Rmax~Rpeak+TotalCores, data=ldata_train)
summary(data_train.lm)$r.squared
summary(ldata_train.lm)$r.squared

# calcoliamo l’errore per i due modelli.
data_train.lm.p = predict(data_train.lm, data_test)
ldata_train.lm.p = predict(ldata_train.lm, ldata_test)
sqrt(mean((data_train.lm.p - data_train$Rmax)^2))
sqrt(mean((exp(ldata_train.lm.p) - ldata_train$Rmax)^2))

par(mfrow=c(1, 1))
gmin = min(data_train.lm.p, exp(ldata_train.lm.p), data_test$Rmax)
gmax = max(data_train.lm.p, exp(ldata_train.lm.p), ldata_test$Rmax)
plot(data_test$Rmax, pch = 20, ylim = c(gmin, gmax))
points(data_train.lm.p, col = "blue", pch = 20)
points(exp(ldata_train.lm.p), col = "red", pch = 20) 
legend("topright",inset=0.02, c("dati","modello lineare",
                              "modello logaritmico"),col=c("black","blue","red"),
       pch=c(19,19), bg="gray", cex=.8)


################################################################################
n = 100
err_lin = rep(0,n)
err_log = rep(0,n)
for(i in 1:n){
  # creazione sottoinsiemi di training e di test
  testset = sort(sample(500,50))
  data_train = data[-testset,]
  data_test = data[testset,]
  ldata_train = ldata[-testset,]
  ldata_test = ldata[testset,]
  
  # costruzione modelli lineare e modello esponenziale
  data_train.lm = lm(Rmax~Rpeak+TotalCores, data=data_train)
  ldata_train.lm = lm(Rmax~Rpeak+TotalCores, data=ldata_train)
  
  # calcoliamo l’errore per i due modelli.
  data_train.lm.p = predict(data_train.lm, data_test)
  ldata_train.lm.p = predict(ldata_train.lm, ldata_test)
  
  err_lin[i] = sqrt(mean((data_train.lm.p - data_train$Rmax)^2))
  err_log[i] = sqrt(mean((exp(ldata_train.lm.p) - ldata_train$Rmax)^2))
}

# stampa media errori
mean(err_lin)
mean(err_log)

# stampa deviazione standard errori
sd(err_lin)
sd(err_log)

# rappresentazione grafica
gmin = min(err_lin, err_log)
gmax = max(err_lin, err_log)

plot(err_lin, type="b", pch=20, col="blue", ylim=c(gmin,gmax))
points(err_log, type="b", pch=20, col="red")