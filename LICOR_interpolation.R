rm(list = ls())
options(scipen = 999)

#load libraries & install packages if necessary-------------------------------------
load.lib = c("tidyverse", "DescTools", "plotly", "data.table", "purrr", "stringr", "pastecs","readxl","MASS",
             "ggplot2", "pracma", "signal","gridGraphics", "pdftools", "outliers", "lubridate", "quantmod")   
install.lib = load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib,require,character=TRUE)

fileName = file.choose() # when prompted, choose the appropriate file 

#Dataframe creation
data <- read.delim(fileName, header = T, skip = 1, col.names = c("Date","Time","CO2","H2O","TH2O","TCell","PCell",
                                                                 "Abs1","Abs2","Volt","datetime"))

# Parameters from the graph
delay <- 20 # Delay between start of peak and max [s]
tail <- 120 # Duration of tailing [s]
baseline <- 390 # Baseline level of CO2 between the peaks
threshold <- 2000

# Data reinterpolated as to have a value exactly for each second

seconds <-  period_to_seconds(hms(data$Time))
data$SecTime = seconds[]-seconds[1]

new_time <- seq(from = data$SecTime[1], to = data$SecTime[nrow(data)], by = 1)
new_data <- data.frame(Date = new_time, 
                       CO2 = approx(data$SecTime, data$CO2, new_time, method = "linear")$y,
                       PCell = approx(data$SecTime, data$PCell, new_time, method = "linear")$y,
                       TCell = approx(data$SecTime, data$TCell, new_time, method = "linear")$y,
                       H2O = approx(data$SecTime, data$H2O, new_time, method = "linear")$y)

# Manual method to detect peaks
max <- which(diff(sign(diff(new_data$CO2, lag = 2)))==-2)+1 # Indices at which local max or min are detected
peaksInd <- which(new_data$CO2[max]>threshold) # Indices of max which correspond to actual peaks 
maxInd <- max[peaksInd]# Indices of these in the actual data
len = length(maxInd)

peaksVal <- new_data$CO2[maxInd] # Value of the peaks [ppm]
peaksValFlux <- new_data$Cflux[maxInd] # Value of the peaks [micrograms]

new_data$CO2 <- new_data$CO2 - baseline

# Prepare lines to indicate the different peaks
line <- list(
  type = "line",
  line = list(color = "red"),
  xref = "x",
  yref = "y"
)

line_x1 <- list()
line_x2 <- list()
txt <- vector()
for(i in 1:len){
  line_x1[i] <- maxInd[i]-delay
  line_x2[i] <- maxInd[i]+tail
  txt[i] <- as.character(i)
}

for(i in 1:len){
  
}

line_x <- append(line_x1,line_x2)

lines <- list()
for (i in 1:(len*2)){
  line[c("x0","x1")] <- line_x[i]
  line[["y0"]] <- 0
  line[["y1"]] <- threshold
  lines <- c(lines, list(line))
}

# Graph to visualize the concentration peak (with baseline correction)
(CO2_base <- plot_ly(data = new_data, x = ~Date, y = ~CO2, name = 'CO2', mode = 'lines', type = 'scatter')%>%
    layout(yaxis = list(title="CO<sub>2 </sub>[ppm]"))%>%
    layout(shapes = lines)%>%
    add_text(showlegend = FALSE, x = maxInd, y = -100, text = txt)%>%
    add_annotations(text = "CO<sub>2 </sub>curve", x = 0, y = 1, yref = "paper",
                    xref = "paper", xanchor = "left", 
                    yanchor = "top", yshift = 25,showarrow = FALSE, font = list(size = 15)))

# Calculation of the flux of C curve
flow <- 55 # in ml/min
flowliter <- flow/(1000*60)  # in L/sec

# Curve of C in microgramms per sec (ideal gas law), from the ppm and flow, from new data time scale
new_data$Cflux <- ((12.01)*(new_data$PCell*flowliter)/(8.3145*(new_data$TCell+ 273.15)))*new_data$CO2

# Integration  of C flux peaks with new time scale
CarbonF_new <-  data.frame(matrix(NA, nrow = length(maxInd)))
for (i in 1:length(maxInd)){
  CarbonF_new[i,1] =  AUC(new_data$Date[(maxInd[i]-delay):(maxInd[i]+ tail)],
                          new_data$Cflux[(maxInd[i]-delay):(maxInd[i]+ tail)], 
                          method = "trapezoid")
}
