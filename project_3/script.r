# pulizia RStudio
cat("\014")
rm(list = ls())

# importazione librerie da utilizzare

# caricamento dei dati dal file csv
data = read.csv('tabella.csv', header = TRUE, sep = ",")

# esplorazione preliminare della struttura dei dati
str(data)

# diagramma a torta con suddivisione dei dati in base all'address space
table = table(data$address_space)
slices <- c(table[1], table[2], table[3], table[4])
lbls <- c("Non-Tor", "Non-VPN", "Tor", "VPN")
pie(slices, labels = lbls, main="Pie Chart of Countries")

# diagramma a torta con suddivisione dei dati in base all'application category
table(data$traffic_category)
table = table(data$traffic_category)
slices <- table
lbls <- c("Audio-Streaming", "AUDIO-STREAMING", "Browsing", "Chat", "Email", "File-transfer", "File-Transfer", "P2P", "Video-streaming", "Video-Streaming")
pie(slices, labels = lbls, main="Pie Chart of Countries")

# selezioniamo esclusivamente il traffico relativo alla darknet
# data <- data[data$address_space != "Non-Tor", ]
# data <- data[data$address_space != "NonVPN", ]

# aggiornamento tabella fattori
# data$address_space <- factor(data$address_space)
# table(data$address_space)

# eliminazione colonne non piu necessarie
data$address_space <- NULL
data$traffic_category <- NULL

# convert AM/PM to 24h format
multidate <- function(data, formats) {
  a<-list()
  for(i in 1:length(formats)){
    a[[i]] <- format(strptime(data, formats[i]), "%d/%m/%Y %H:%M:%S")
    a[[1]][!is.na(a[[i]])]<-a[[i]][!is.na(a[[i]])]
  }
  a[[1]]
}
data$timestamp <- multidate(data$timestamp, c("%d/%m/%Y %H:%M", "%d/%m/%Y %I:%M:%S %p"))

# convert strings to POSIXlt timestamp
data$timestamp_1 <- as.POSIXct(data$timestamp, format = "%d/%m/%Y %H:%M:%S")
data$timestamp_2 <- as.POSIXct(data$timestamp, format = "%d/%m/%Y %H:%M")
data$timestamp_3 <- as.POSIXct(data$timestamp, format = "%d/%m/%Y %H")
data$time <- format(strptime(data$timestamp, "%d/%m/%Y %H:%M:%S"), "%d/%m/%Y %-H")

# order data by timestamp
data <- data[order(data$timestamp_1), ]

# add date column
# data$date <- as.Date(data$timestamp_1)

data$timestamp <- NULL
data$timestamp_1 <- NULL
data$timestamp_2 <- NULL
data$timestamp_3 <- NULL

library(dplyr)
data<-data %>% 
  group_by_if(is.numeric %>% Negate) %>%
  summarize_all(sum)
