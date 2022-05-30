rm(list = ls())
options(scipen = 999)

#load libraries & install packages if necessary-------------------------------------
load.lib = c("tidyverse", "DescTools", "plotly", "data.table", "purrr", "stringr", "pastecs","readxl","MASS",
             "ggplot2", "pracma", "signal","gridGraphics", "pdftools", "outliers", "lubridate", "quantmod")   
install.lib = load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib,require,character=TRUE)

#fileName = file.choose() # when prompted, choose the appropriate file

pars <- function(fileName){

  data <- read.delim(fileName, header = T, skip = 1, col.names = c("Date","Time","CO2","H2O","TH2O","TCell","PCell",
                                                                 "Abs1","Abs2","Volt","ind"))
  data[11] <- 1:nrow(data)

  # Conversion of elapsed time into seconds 
  seconds <-  period_to_seconds(hms(data$Time))

  data$SecTime = seconds[]-seconds[1]
  
  # Solve the problem of the data having 2 measurements per seconds
  if (data$Time[1] != data$Time[2]){
    data <- data[-1,]
  
    for(i in seq(from = 2, to = nrow(data), by = 2)){
      data$SecTime[i] = data$SecTime[i] + 0.5
      i = i + 2
    }
  }

  return(data)
}


plot1 <- function(data, baseline = 400, threshold = 1000, maxInd) {
  # Baseline level of CO2 between the peaks
  data$CO2 <- data$CO2 - baseline
  
  max <- which(diff(sign(diff(data$CO2, lag = 2)))==-2)+1 # Indices at which local max or min are detected
  peaksInd <- which(data$CO2[max]>threshold) # Indices of max which correspond to actual peaks 
  maxInd <- max[peaksInd] # Indices of these in the actual data
  
  # Prepare lines to indicate the different peaks
  line <- list(
    type = "line",
    line = list(color = "red"),
    xref = "x",
    yref = "y"
  )
  
  lines <- list()
  txt <- vector()
  for (i in 1:length(maxInd)){
    line[c("x0", "x1")] <- maxInd[i]/2
    line[["y0"]] <- 0
    line[["y1"]] <- threshold
    lines <- c(lines, list(line))
    
    txt[i] <- as.character(i)
    
}
  
  # Graph to visualize the concentration peak (with baseline correction)
  CO2_base <- plot_ly(data = data, x = ~SecTime, y = ~CO2, name = 'CO2', mode = 'lines', type = 'scatter')%>%
    layout(yaxis = list(title="CO<sub>2 </sub>[ppm]"))%>%
    layout(shapes = lines)%>%
    add_text(showlegend = FALSE, x = maxInd/2 + 200, y = ~threshold, text = txt)%>%
    add_annotations(text = "CO<sub>2 </sub>curve", x = 0, y = 1, yref = "paper", xref = "paper", xanchor = "left", 
      yanchor = "top", yshift = 20,showarrow = FALSE, font = list(size = 15))
  
  return(CO2_base)
}

plot2 <- function(data, flow = 55, baseline = 400) {
  
  # Baseline level of CO2 between the peaks
  data$CO2 <- data$CO2 - baseline
  
  # Calculation of the flux of C curve
  flowliter <- flow/(1000*60)  # in L/sec
  
  # Curve of C in microgramms per sec (ideal gas law), from the ppm and flow
  data$Cflux <- ((12.01)*(data$PCell*flowliter)/(8.3145*(data$TCell+ 273.15)))*data$CO2
  
  Cflux <- plot_ly(data = data, x = ~SecTime, y = ~Cflux, name = 'C', mode = 'lines', type = 'scatter') %>%
    layout(xaxis = list(title="Time (s)"), yaxis = list(title="C [Î¼g/s]"))%>% 
    add_annotations(text = "Carbon flux curve", x = 0, y = 1, yref = "paper", xref = "paper", xanchor = "left", 
    yanchor = "top", yshift = 20,showarrow = FALSE, font = list(size = 15))%>%
    layout(showlegend = FALSE)
  
  return(Cflux)
}

integ <- function(data, delay = 20, tail = 120, threshold = 1000, flow = 55, baseline = 400){
  
  # Baseline level of CO2 between the peaks
  data$CO2 <- data$CO2 - baseline
  
  # Manual method to detect peaks
  max <- which(diff(sign(diff(data$CO2, lag = 2)))==-2)+1 # Indices at which local max or min are detected
  peaksInd <- which(data$CO2[max]>threshold) # Indices of max which correspond to actual peaks 
  maxInd <- max[peaksInd] # Indices of these in the actual data

  # Calculation of the flux of C curve
  flowliter <- flow/(1000*60)  # in L/sec

  # Curve of C in microgramms per sec (ideal gas law), from the ppm and flow
  data$Cflux <- ((12.01)*(data$PCell*flowliter)/(8.3145*(data$TCell+ 273.15)))*data$CO2

  peaksVal <- data$CO2[maxInd] # Value of the peaks [ppm]
  peaksValFlux <- data$Cflux[maxInd] # Value of the peaks [micrograms]

  # Integration of the CO2 peaks
  CarbonCO2 <-  data.frame(matrix(NA, nrow = length(maxInd)))
  for (i in 1:length(maxInd))
    CarbonCO2[i,1] =  AUC(data$ind[(maxInd[i]-delay*2):(maxInd[i]+ tail*2)], data$CO2[(maxInd[i]-delay*2):(maxInd[i]+ tail*2)], method = "trapezoid")

  # Integration  of C flux peaks
  CarbonF <-  data.frame(matrix(NA, nrow = length(maxInd)))
  for (i in 1:length(maxInd))
    CarbonF[i,1] =  AUC(data$SecTime[(maxInd[i]-delay):(maxInd[i]+ tail)], data$Cflux[(maxInd[i]-delay):(maxInd[i]+ tail)], method = "trapezoid")/2

  tab <- data.frame(1:nrow(CarbonF),CarbonF,peaksVal)
  colnames(tab) <- c("NÂ°","C [Î¼g]", "Peak Value [ppm]")

  return(tab)
}
