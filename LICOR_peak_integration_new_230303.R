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
  
  new_time <- seq(from = data$SecTime[1], to = data$SecTime[nrow(data)], by = 1)
  data <- data.frame(Date = new_time, 
                         CO2 = approx(data$SecTime, data$CO2, new_time, method = "linear")$y,
                         PCell = approx(data$SecTime, data$PCell, new_time, method = "linear")$y,
                         TCell = approx(data$SecTime, data$TCell, new_time, method = "linear")$y,
                         H2O = approx(data$SecTime, data$H2O, new_time, method = "linear")$y)

  return(data)
}


plot1 <- function(data, baseline = 400, threshold = 1000, maxInd, delay = 20, tail = 120) {
  # Baseline level of CO2 between the peaks
  #data$CO2 <- data$CO2 - baseline
  
  max <- which(diff(sign(diff(data$CO2, lag = 2)))==-2)+1 # Indices at which local max or min are detected
  peaksInd <- which(data$CO2[max]>threshold) # Indices of max which correspond to actual peaks 
  maxInd <- max[peaksInd] # Indices of these in the actual data
  len = length(maxInd)
  
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
    line_x1[i] <- maxInd[i]-delay*2
    line_x2[i] <- maxInd[i]+tail*2
    
    txt[i] <- as.character(i)
  }
  
  line_x <- append(line_x1, line_x2)
  
  lines <- list()
  for (i in 1:(len*2)){
    line[c("x0","x1")] <- line_x[i]
    line[["y0"]] <- 0
    line[["y1"]] <- threshold
    lines <- c(lines, list(line))
  }
  
  # Graph to visualize the concentration peak (with baseline correction)
  CO2 <- plot_ly(data = data, x = ~Date, y = ~CO2, name = 'CO2', mode = 'lines', type = 'scatter')%>%
      layout(yaxis = list(title="CO<sub>2 </sub>[ppm]"))%>%
      layout(shapes = lines)%>%
      add_text(showlegend = FALSE, x = maxInd, y = -800, text = txt)%>%
      add_annotations(text = "CO<sub>2 </sub>curve", x = 0, y = 1, yref = "paper", xref = "paper", xanchor = "left", 
                      yanchor = "top", yshift = 20,showarrow = FALSE, font = list(size = 15))

  
  return(CO2)
}

plot2 <- function(data) {
  
  
  H2O <- plot_ly(data = data, x = ~Date, y = ~H2O, name = 'H2O', mode = 'lines', type = 'scatter') %>%
    layout(xaxis = list(title="Time (s)"), yaxis = list(title="H2O [mmol/mol]"))%>% 
    add_annotations(text = "H20 content", x = 0, y = 1, yref = "paper", xref = "paper", xanchor = "left", 
    yanchor = "top", yshift = 20,showarrow = FALSE, font = list(size = 15))%>%
    layout(showlegend = FALSE)
  
  return(H2O)
}

plot3 <- function(data) {
  
  
  Pres <- plot_ly(data = data, x = ~Date, y = ~PCell, name = 'PCell', mode = 'lines', type = 'scatter') %>%
    layout(xaxis = list(title="Time (s)"), yaxis = list(title="PCell [kPa]"))%>%
    add_annotations(text = "Cell Pressure", x = 0, y = 1, yref = "paper", xref = "paper", xanchor = "left", 
    yanchor = "top", yshift = 20,showarrow = FALSE, font = list(size = 15))%>%
    layout(showlegend = FALSE)
  
  return(Pres)
}

integ <- function(data, delay, tail, threshold = 1000, flow = 55, baseline = 400){
  
  # Manual method to detect peaks
  max <- which(diff(sign(diff(data$CO2, lag = 2)))==-2)+1 # Indices at which local max or min are detected
  peaksInd <- which(data$CO2[max]>threshold) # Indices of max which correspond to actual peaks 
  maxInd <- max[peaksInd] # Indices of these in the actual data
  
  # Calculate individual linear baseline for each peak of CO2
  linbasCO2 <- matrix(nrow = length(maxInd), ncol = (tail*2 + delay*2)+1)
  for (i in 1:length(maxInd)){
    linbasCO2[i,] <- data$CO2[(maxInd[i]-delay*2):(maxInd[i]+tail*2)]-seq(from = data$CO2[(maxInd[i]-delay*2)],
                                                                          to = data$CO2[(maxInd[i]+tail*2)], length.out = (delay*2 + tail*2)+1)
  }

  # Calculation of the flux of C curve
  flowliter <- flow/(1000*60*2)  # in L/pt (the factor is there because we have 2 points per sec)

  # Curve of C in microgramms per sec (ideal gas law), from the ppm and flow
  data$Cflux <- ((12.01)*(data$PCell*flowliter)/(8.3145*(data$TCell+ 273.15)))*data$CO2

  peaksVal <- data$CO2[maxInd] # Value of the peaks [ppm]
  peaksValFlux <- data$Cflux[maxInd] # Value of the peaks [micrograms]
  
  # Calculate individual linear baseline for each peak of Cflux
  linbasCflux <- matrix(nrow = length(maxInd), ncol = (tail*2 + delay*2)+1)
  for (i in 1:length(maxInd)){
    linbasCflux[i,] <- data$Cflux[(maxInd[i]-delay*2):(maxInd[i]+tail*2)]-seq(from = data$Cflux[(maxInd[i]-delay*2)],
                                                                              to = data$Cflux[(maxInd[i]+tail*2)], length.out = (delay*2 + tail*2)+1)
  }
  # Integration  of C flux peaks with new time scale
  CarbonF <-  data.frame(matrix(NA, nrow = length(maxInd)))
  for (i in 1:length(maxInd)){
    CarbonF[i,1] =  AUC(data$Date[(maxInd[i]-delay*2):(maxInd[i]+ tail*2)],
                            linbasCflux[i,], 
                            method = "trapezoid")
  }

  tab <- data.frame(1:nrow(CarbonF),CarbonF,peaksVal)
  colnames(tab) <- c("N°","C [μg]", "Peak Value [ppm]")

  return(tab)
}
