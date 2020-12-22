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
plot(data_ts.da.r, pch = 20)
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
plot(log(data_ts.dm.r), pch = 20)
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

# smorzamento esponenziale
data_ts.se = HoltWinters(data_ts, beta = F, gamma = F)
data_ts.se
plot(data_ts.se)

# smorzamento esponenziale con Trend
data_ts.set = HoltWinters(data_ts, gamma = F)
data_ts.set
plot(data_ts.set, main="Holt-Winters filtering with Trend")

# modello smorzamento esponenziale con Trend e Stagionalita'
data_ts.sets = HoltWinters(data_ts)
data_ts.sets
plot(data_ts.sets, main="Holt-Winters filtering with Trend and Seasonality")

# comparazione grafica SE, SET e SETS
m = 12
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

plot(data[181:192, 2], type = "b", col = "black", pch = 20)
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

# plot(res.se, type = "b", col = "blue", pch = 20)
# points(res.set, type = "b", col = "green3", pch = 20)
# points(res.sets, type = "b", col = "red", pch = 20)
# legend("bottomleft",
#        inset = 0.02,
#        c("Errore SE",
#          "Errore SET",
#          "Errore SETS"),
#        col = c("blue", "green3", "red"),
#        pch = c(19, 19, 19),
#        bg = "gray",
#        cex = 0.8)

# comparazione numerica SE, SET e SETS
sqrt(mean(res.se^2))
sd(res.se)
sqrt(mean(res.set^2))
sd(res.set)
sqrt(mean(res.sets^2))
sd(res.sets)
