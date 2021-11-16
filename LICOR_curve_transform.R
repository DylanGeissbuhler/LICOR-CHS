rm(list = ls())
dev.off()
options(scipen = 999)

#load libraries & install packages if necessary-------------------------------------
load.lib = c("tidyverse", "DescTools", "plotly", "data.table", "purrr", "stringr", "pastecs","readxl","MASS", "ggplot2", "pracma", "signal","gridGraphics", "pdftools", "outliers", "lubridate")   
install.lib = load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib,require,character=TRUE)

fileName = file.choose() # when prompted, choose the appropriate file 

#Dataframe creation
data <- read.delim(fileName, header = T, skip = 1, col.names = c("Date","Time","CO2","H2O","TH2O","TCell","PCell","Abs1","Abs2","Volt","NA"))

#Conversion of elapsed time into seconds 
seconds <-  period_to_seconds(hms(data$Time))
data$SecTime = seconds[]-seconds[1]

# Graph to visualize the concentration peak
(CO2 <- plot_ly(data = data, x = ~SecTime, y = ~CO2, name = 'CO2', mode = 'lines', type = 'scatter') %>%
  layout(title = "CO2 Curve", xaxis = list(title="Time"), yaxis = list(title="CO2 [ppm]")))

# Calculation of the flux of C curve
flow <- 1  # in ml/sec
Vcell <- 14.5 # in ml

data$Cflux <- ((12.01/Vcell)*(data$PCell*flow)/(8.3145*(data$TCell+ 273.15)))*data$CO2 # Gives a curve of C in microgramms per sec

(Cflux <- plot_ly(data = data, x = ~SecTime, y = ~Cflux, name = 'C', mode = 'lines', type = 'scatter') %>%
  layout(title = "C Flux", xaxis = list(title="Time (s)"), yaxis = list(title="C [micrograms/s]")))

#Calculation of the total amount of emitted C
TotalC <-  AUC(data$SecTime, data$Cflux, method = "trapezoid")
