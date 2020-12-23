# pulizia RStudio
cat("\014")
rm(list = ls())

# importazione librerie da utilizzare

# caricamento dei dati dal file csv
data = read.csv('tabella.csv', header = TRUE, sep = ",", stringsAsFactors = F)

# esplorazione preliminare della struttura dei dati
str(data)

# mi assicuro che i dati sia consistenti con quanto visto su github.com
# https://github.com/torvalds/linux: 981,442 commits
sum(data$commits)

# convertiamo in serie storica
data_ts <- ts(data$commits, frequency = 12, start = c(2001, 9), end = c(2020, 12))

# visualizzazione grafica della serie storica
ts.plot(data_ts, xlab="Anni", ylab="Numero di Commits")

# facciamo partire la serie storica dal gennaio 2005
data <- data[41:232,]
data_ts <- ts(data$commits, frequency = 12, start = c(2005, 1), end = c(2020, 12))

# visualizzazione grafica della nuova serie storica
ts.plot(data_ts, xlab="Anni", ylab="Numero di Commits")

# parametri della serie storica
start(data_ts)
end(data_ts)
frequency(data_ts)

# funzione di autocorrelazione: abbiamo stagionalita' annuale
acf(data_ts, 30)
acf(data_ts, 60)

# indaghiamo sulla presenza di stagionalita'
plot(diff(data_ts), xlab="Anni", ylab="Numero di Commits al netto del Trend")
acf(diff(data_ts), 30)
acf(diff(data_ts), 60)

# non emerge presenza di stagionalita'

# proviamo a confrontare i grafici dei periodi
m_data = matrix(data[, 2], 12, 16, byrow = F)
par(bg = "gray24")
ts.plot(m_data, col = heat.colors(12))
lines(rowMeans(m_data), lwd = 3, col = "white")
ts.plot(scale(m_data, scale = F), col = heat.colors(12), main="Numero di commits mensili dal 2005 al 2020 (andamento centrato a media nulla)")
lines(rowMeans(scale(m_data, scale = F)), lwd = 3, col = "blue")
legend("bottomleft",
       inset = 0.02,
       c("Andamento medio"),
       col = c("blue"),
       pch = c(19),
       bg = "gray",
       cex = 0.8)
par(bg = "white")

# visualizziamo le bande di confidenza empiriche, a una deviazione standard
sd = vector("numeric", 12)
for (i in 1:12) {
  sd[i] = sd(m_data[i, ])
}
m = rowMeans(m_data)
plot(m, pch = 20, type = "b", ylim = range(c(m - 2 * sd, m + 2 * sd)), xlab="Mese",
     main="Bande di confidenza empiriche per il numero mensile di commits")
arrows(1:12, m - sd, 1:12, m + sd, length = 0.02, angle = 90, code = 3, col = "green3")
points(m + sd, type = "b", pch = 20, col = "gray")
points(m - sd, type = "b", pch = 20, col = "gray")

# non c'e' ragione di decomporre la serie: togliamoci ogni dubbio
data_ts.da = decompose(data_ts, type = "additive")
plot(data_ts.da)

# confrontiamo le scale tra la componente stagionale e quella di rumore
plot(data_ts.da$random, col = "blue")
lines(data_ts.da$seasonal, col = "red")
legend("topright",
       inset = 0.02,
       c("Componente di Errore",
         "Componente Stagionale"),
       col = c("blue", "red"),
       pch = c(19,19),
       bg = "gray",
       cex = 0.8)

# analisi dei residui decomposizione additiva
layout(t(1:3))
data_ts.da.r = as.vector(window(data_ts.da$random, c(2005, 7), c(2020, 6)))
plot(data_ts.da.r, pch = 20, main="Residui Decomposizione Additiva")
#acf(data_ts.da.r)

# controllo residui gaussiani
hist(data_ts.da.r, 41, freq = F, main="Istogramma Residui Decomposizione Additiva")
lines(density(data_ts.da.r), col = "blue")
lines(sort(data_ts.da.r), dnorm(sort(data_ts.da.r), mean(data_ts.da.r), sd(data_ts.da.r)), col = "red")
legend("topright",
       inset = 0.02,
       c("Densità Empirica",
         "Densità Gaussiana"),
       col = c("blue", "red"),
       pch = c(19, 19),
       bg = "gray",
       cex = 0.8)
qqnorm(data_ts.da.r, main="Q-Q Plot Residui Decomposizione Additiva")
qqline(data_ts.da.r, col="red", lwd=2)
layout(1)

# decomposizione moltiplicativa
acf(log(data_ts), 30)
acf(diff(log(data_ts)), 60)
data_ts.dm = decompose(data_ts, type = "multiplicative")
plot(data_ts.dm)

# analisi dei residui decomposizione moltiplicativa
layout(t(1:3))
data_ts.dm.r = as.vector(window(data_ts.dm$random, c(2005, 7), c(2020, 6)))
plot(log(data_ts.dm.r), pch = 20, main="Residui Decomposizione Moltiplicativa")
#acf(data_ts.dm.r)

# controllo residui gaussiani
data_ts.dm.rl = log(data_ts.dm.r)
hist(data_ts.dm.rl, 41, freq = F, main="Istogramma Residui Decomposizione Moltiplicativa")
lines(density(data_ts.dm.rl), col = "blue")
lines(sort(data_ts.dm.rl), dnorm(sort(data_ts.dm.rl), mean(data_ts.dm.rl), sd(data_ts.dm.rl)), col = "red")
legend("topright",
       inset = 0.02,
       c("Densità Empirica",
         "Densità Gaussiana"),
       col = c("blue", "red"),
       pch = c(19, 19),
       bg = "gray",
       cex = 0.8)
qqnorm(data_ts.dm.rl, main="Q-Q Plot Residui Decomposizione Moltiplicativa")
qqline(data_ts.dm.rl, col="red", lwd=2)
layout(1)

# stagionalita' multipla con seasonal window ridotta
data_ts.stl = stl(data_ts, s.window = 3)
plot(data_ts.stl, main = "Decomposizione con Stagionalità non uniforme")

# modello a smorzamento esponenziale
data_ts.se = HoltWinters(data_ts, beta = F, gamma = F)
data_ts.se
plot(data_ts.se)

# modello a smorzamento esponenziale con Trend
data_ts.set = HoltWinters(data_ts, gamma = F)
data_ts.set
plot(data_ts.set, main="Holt-Winters filtering with Trend")

# modello a smorzamento esponenziale con Trend e Stagionalita'
data_ts.sets = HoltWinters(data_ts)
data_ts.sets
plot(data_ts.sets, main="Holt-Winters filtering with Trend and Seasonality")

# comparazione grafica SE, SET e SETS
m = 24
l = length(data_ts)

pred.se = rep(0, m)
pred.set = rep(0, m)
pred.sets = rep(0, m)

res.se = rep(0, m)
res.set = rep(0, m)
res.sets = rep(0, m)

j = 1
for (i in (l - m):(l - 1)) {
  data_ts = ts(data[1:i, 2], frequency = 12, start = 2005)
  data_ts.se = HoltWinters(data_ts, beta = F, gamma = F)
  data_ts.set = HoltWinters(data_ts, gamma = F)
  data_ts.sets = HoltWinters(data_ts)
  
  pred.se[j] = predict(data_ts.se, n.ahead = 1, se.fit = F)
  pred.set[j] = predict(data_ts.set, n.ahead = 1, se.fit = F)
  pred.sets[j] = predict(data_ts.sets, n.ahead = 1, se.fit = F)
  
  res.se[j] = data[i + 1, 2] - predict(data_ts.se, n.ahead = 1, se.fit = F)
  res.set[j] = data[i + 1, 2] - predict(data_ts.set, n.ahead = 1, se.fit = F)
  res.sets[j] = data[i + 1, 2] - predict(data_ts.sets, n.ahead = 1, se.fit = F)
  
  j = j + 1
}

plot(data[(l - m + 1):l, 2], type = "b", col = "black", pch = 20,
     main="Predizione modelli Holt-Winters",
     ylab = "Numero di Commits", xlab = "Mesi", xaxt="n")
axis(1, at = seq(1, 24, by = 1), las=2)
points(pred.se, type = "b", col = "blue", pch = 20)
points(pred.set, type = "b", col = "green3", pch = 20)
points(pred.sets, type = "b", col = "red", pch = 20)
legend("bottomleft",
       inset = 0.02,
       c("Valore Effettivo",
         "Previsione SE",
         "Previsione SET",
         "Previsione SETS"),
       col = c("black", "blue", "green3", "red"),
       pch = c(19, 19, 19, 19),
       bg = "gray",
       cex = 0.8)

plot(res.se, type = "b", col = "blue", pch = 20,
     main="Errore modelli Holt-Winters",
     ylab = "Errore", xlab = "Mesi", xaxt="n")
axis(1, at = seq(1, 24, by = 1), las=2)
points(res.set, type = "b", col = "green3", pch = 20)
points(res.sets, type = "b", col = "red", pch = 20)
legend("bottomleft",
       inset = 0.02,
       c("Errore SE [RMS = 1270.249, SD = 1296.827]",
         "Errore SET [RMS = 1277.189, SD = 1301.629]",
         "Errore SETS [RMS = 1141.315, SD = 1165.146]"),
       col = c("blue", "green3", "red"),
       pch = c(19, 19, 19),
       bg = "gray",
       cex = 0.8)

# comparazione numerica SE, SET e SETS
sqrt(mean(res.se^2))
sd(res.se)
sqrt(mean(res.set^2))
sd(res.set)
sqrt(mean(res.sets^2))
sd(res.sets)

# controllo residui gaussiani modello a smorzamento esponenziale con Trend
layout(t(1:3))
data_ts.set.r = as.vector(residuals(data_ts.set))
plot(data_ts.set.r, pch = 20, main="Residui Modello a Smorzamento Esponenziale con Trend")
hist(data_ts.set.r, 40, freq = F, main="Istogramma Residui Modello a Smorzamento Esponenziale con Trend")
lines(density(data_ts.set.r), col = "blue")
lines(sort(data_ts.set.r), dnorm(sort(data_ts.set.r), mean(data_ts.set.r), sd(data_ts.set.r)), col = "red")
legend("topright",
       inset = 0.02,
       c("Densità Empirica",
         "Densità Gaussiana"),
       col = c("blue", "red"),
       pch = c(19, 19),
       bg = "gray",
       cex = 0.8)
qqnorm(data_ts.set.r, main="Q-Q Plot Residui Modello a Smorzamento Esponenziale con Trend")
qqline(data_ts.set.r, col="red", lwd=2)
shapiro.test(data_ts.set.r)
layout(1)

# visualizziamo le incertezze al 90%
plot(data_ts.set, predict(data_ts.set, 12))
lines(predict(data_ts.set, 12) + qnorm(0.05, mean(data_ts.set.r), sd(data_ts.set.r)), col = "blue")
lines(predict(data_ts.set, 12) + qnorm(0.95, mean(data_ts.set.r), sd(data_ts.set.r)), col = "blue")
lines(predict(data_ts.set, 12) + quantile(data_ts.set.r, 0.05), col = "green3")
lines(predict(data_ts.set, 12) + quantile(data_ts.set.r, 0.95), col = "green3")

# pulizia
data_ts <- ts(data$commits, frequency = 12, start = c(2005, 1), end = c(2020, 11))
data_ts.set = HoltWinters(data_ts, gamma = F)

# valutazione della natura autoregressiva
layout(t(1:2))
acf(data_ts, 80)
pacf(data_ts)
layout(1)

# Autoregressione con il metodo dei minimi quadrati
data_ts.ls = ar(data_ts, method = "ols")
o = data_ts.ls$order
a = data_ts.ls$ar
b = data_ts.ls$x.intercept
data_ts.ls.an = data_ts
for (i in (o + 1):length(data_ts)) {
  data_ts.ls.an[i] = sum(rev(a) * data_ts[(i - o):(i - 1)]) + mean(data_ts) * (1 - sum(a)) + b
}
ts.plot(data_ts, data_ts - data_ts.ls$resid, col = c("black", "red"),
        main="Modello di Autoregressione con il metodo dei minimi quadrati")

# analisi dei residui
layout(t(1:3))
data_ts.ls.r = as.double(na.omit(data_ts.ls$resid))
data_ts.ls.fitted = as.double(na.omit(data_ts - data_ts.ls$resid))
#plot(data_ts.ls.r, pch = 20)
plot(data_ts.ls.fitted, data_ts.ls.r, pch = 20,
     main="Residui Modello Autoregressione con il metodo dei minimi quadrati")
var(data_ts.ls.r)/var(data_ts.ls.r + data_ts.ls.fitted)
#acf(data_ts.ls.r)
#pacf(data_ts.ls.r)
hist(data_ts.ls.r, 20, freq = F,
     main="Istogramma Residui")
lines(density(data_ts.ls.r), col = "red")
lines(sort(data_ts.ls.r), dnorm(sort(data_ts.ls.r), mean(data_ts.ls.r), sd(data_ts.ls.r)), col = "blue")
qqnorm(data_ts.ls.r, pch = 20)
qqline(data_ts.ls.r, col="red")
shapiro.test(data_ts.ls.r)
layout(1)

# previsione con il modello autoregressivo
data_ts.ls.pt = predict(data_ts.ls, n.ahead = 12, se.fit = FALSE)
y.max = max(data_ts.ls.pt + quantile(data_ts.ls.r, 0.975))
y.min = min(data_ts.ls.pt + quantile(data_ts.ls.r, 0.025))
ts.plot(window(data_ts, 2020), window(data_ts - data_ts.ls$resid, 2020),
        data_ts.ls.pt, col = c("black", "blue", "red"), lwd = c(1, 1, 2),
        ylim = c(y.min, y.max))

# stima empirica dell'incertezza
lines(data_ts.ls.pt + quantile(data_ts.ls.r, 0.975), col = "green3")
lines(data_ts.ls.pt + quantile(data_ts.ls.r, 0.025), col = "green3")

# stima parametrica dell'incertezza
lines(data_ts.ls.pt + qnorm(0.975, mean(data_ts.ls.r), sd(data_ts.ls.r)), col = 'brown')
lines(data_ts.ls.pt + qnorm(0.025, mean(data_ts.ls.r), sd(data_ts.ls.r)), col = 'brown')

# confrontiamo analisi e previsioni con Holt-Winters

# analisi
ts.plot(data_ts, data_ts - data_ts.ls$resid, data_ts.set$fitted[, 1],
        col = c("black", "blue", "red"))

# previsioni
ts.plot(data_ts.ls.pt, pred.set[13:24], col = c("blue", "green3"))
lines(data_ts.ls.pt + quantile(data_ts.ls.r, 0.975), col = "lightblue")
lines(data_ts.ls.pt + quantile(data_ts.ls.r, 0.025), col = "lightblue")

# confrontiamo i due metodi con l’autovalutazione
train = window(data_ts, end = c(2019, 12))
test = window(data_ts, c(2020, 1))
data_ts.ls.p = predict(ar(train, method = "ols"), n.ahead = 11, se.fit = FALSE)
ts.plot(test, pred.set[13:23], data_ts.ls.p, col = c("black", "red", "blue"), type="b",
        main="Confronto Previsioni SET e AR")
sqrt(mean((pred.set[13:23] - test)^2))
sqrt(mean((data_ts.ls.p - test)^2))
legend("topleft",
       inset = 0.02,
       c("Valori reali",
         "Previsione modello SET",
         "Previsione modello AR"),
       col = c("black", "red", "blue"),
       pch = c(19, 19, 19),
       bg = "gray",
       cex = 0.8)
