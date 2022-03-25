rm(list = ls())
dev.off()
options(scipen = 999)

#load libraries & install packages if necessary-------------------------------------
load.lib = c("tidyverse", "DescTools", "plotly", "data.table", "purrr", "stringr", "pastecs","readxl","MASS",
             "ggplot2", "pracma", "signal","gridGraphics", "pdftools", "outliers", "lubridate", "quantmod")   
install.lib = load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib,require,character=TRUE)

fileName = file.choose() # when prompted, choose the appropriate file 

#Dataframe creation
data <- read.delim(fileName, header = T, skip = 1, col.names = c("Date","Time","CO2","H2O","TH2O","TCell","PCell","Abs1","Abs2","Volt","ind"))
data[11] <- 1:nrow(data)

# Conversion of elapsed time into seconds 
seconds <-  period_to_seconds(hms(data$Time))
data$SecTime = seconds[]-seconds[1]

# Baseline correction
baseline <- 365
data$CO2 <- data$CO2 - baseline

# Graph to visualize the concentration peak
(CO2 <- plot_ly(data = data, x = ~ind, y = ~CO2, name = 'CO2', mode = 'lines', type = 'scatter') %>%
  layout(xaxis = list(title="Time (meas)"), yaxis = list(title="CO2 [ppm]")))


# Calculation of the flux of C curve
flow <- 55 # in ml/min
flowliter <- flow/(1000*60)  # in L/sec


# Curve of C in microgramms per sec (ideal gas law), from the ppm and flow
data$Cflux <- ((12.01)*(data$PCell*flow)/(8.3145*(data$TCell+ 273.15)))*data$CO2

(Cflux <- plot_ly(data = data, x = ~SecTime, y = ~Cflux, name = 'C', mode = 'lines', type = 'scatter') %>%
  layout(title = "C Flux", xaxis = list(title="Time (s)"), yaxis = list(title="C [micrograms/s]")))

# Manual method to detect peaks
# CO2 peaks
valuePeak <- 1800

max <- which(diff(sign(diff(data$CO2, lag = 2)))==-2)+1 # Indices at which local max or min are detected
peaksInd <- which(data$CO2[max]>valuePeak) # Indices of max which correspond to actual peaks 
maxInd <- max[peaksInd] # Indices of these in the actual data

peaksVal <- data$CO2[maxInd] # Value of the peaks [ppm]
peaksValFlux <- data$Cflux[maxInd] # Value of the peaks [micrograms]

# Integration of the CO2 peaks
delay <- 50 # Delay between start of peak and max [s]
tail <- 120 # Duration of tailing [s]

CarbonCO2 <-  data.frame(matrix(NA, nrow = length(maxInd)))
for (i in 1:length(maxInd))
  CarbonCO2[i,1] =  AUC(data$ind[(maxInd[i]-delay*2):(maxInd[i]+ tail*2)], data$CO2[(maxInd[i]-delay*2):(maxInd[i]+ tail*2)], method = "trapezoid")

# Integration  of C flux peaks
CarbonF <-  data.frame(matrix(NA, nrow = length(maxInd)))
for (i in 1:length(maxInd))
  CarbonF[i,1] =  AUC(data$SecTime[(maxInd[i]-delay):(maxInd[i]+ tail)], data$Cflux[(maxInd[i]-delay):(maxInd[i]+ tail)], method = "trapezoid")/2


