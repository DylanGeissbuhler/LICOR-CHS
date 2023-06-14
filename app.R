load.lib = c("tidyverse", "DescTools", "plotly", "data.table", "purrr","readxl","MASS", "pracma", "lubridate","shiny","vtable")   
install.lib = load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib,require,character=TRUE)

options(shiny.maxRequestSize=30*1024^2)

ui <- fluidPage(
  
  # App title
  titlePanel("LICOR-CHS CO2 Peak integration"),

    sidebarPanel(
      numericInput("flow","Flowrate of CHS [mL/min] :", value = 55),
      numericInput("threshold","Value of threshold for peak [ppm] :", value = 2000),
      numericInput("delay","Delay between start of peak and top [s] :", value = 20),
      numericInput("tail", "Duration of tailing [s] :", value = 120),
      #Checkbox to know if restek flow file should be used
      checkboxInput("use_file_flow", "Use flow file", value = FALSE),
      tableOutput("carbon")
    ),
  
 # UI
 
splitLayout(cellWidths = c("40%", "30%"), 
            fileInput("file1","peak data .txt file (max. size 30MB)", multiple = FALSE),
            fileInput("fileFlow", "flow data .txt file", multiple = FALSE)),

#Button used to lauch processing of files
actionButton("flow_btn", "Process Files"), 
 
 
 mainPanel(
   
   plotlyOutput("CO2Plot", height = "1000px"),

   )
  )

# Define server logic 
server <- function(input, output, session) {
  
  source("LICOR_peak_integration_new_230614.R", encoding = "UTF-8", echo = TRUE)
  
observeEvent(input$flow_btn, {
  
  
  if(is.null(input$file1) && is.null(input$fileFlow)){
   
     #No file uploaded, do nothing
    print("No files uploaded")
    
  }else if(is.null(input$file1) && !is.null(input$fileFlow)){
    
    #Only the flow file is uploaded, tell the user to upload a CO2 data file
    print("Please upload a CO2 peak .txt file")
    
  }else if(!is.null(input$file1) && is.null(input$fileFlow)){
    
    if (input$use_file_flow){
      print("Please upload a flow file...")
    }else{
    data <- reactive(
      pars(input$file1$datapath)
    )
  output$CO2Plot <- renderPlotly({
    req(input$file1)
    
    subplot(list(plot1(data(), input$baseline, input$threshold, maxInd, input$delay, input$tail), 
                 plot2(data()),
                 plot3(data())), 
            nrows = 3, margin = 0.03, titleX = TRUE, titleY = TRUE,
            shareX = TRUE)
  })
  output$carbon <- renderTable({
    req(input$file1)
    integ(data(), input$delay, input$tail, input$threshold, input$flow, input$baseline)
  })
  }
  }else if(!is.null(input$file1) && !is.null(input$fileFlow)){
  
    if (input$use_file_flow){
      data <- reactive(
      parsFlow(input$file1$datapath, input$fileFlow$datapath)
      )
      output$CO2Plot <- renderPlotly({
        req(input$file1, input$fileFlow)
    
        subplot(list(plot1(data(), input$baseline, input$threshold, maxInd, input$delay, input$tail), 
                 plot2(data()),
                 plot3(data()),
                 plot4(data())), 
            nrows = 3, margin = 0.03, titleX = TRUE, titleY = TRUE,
            shareX = TRUE)
  })
  output$carbon <- renderTable({
    req(input$file1, input$fileFlow)
    integFlow(data(), input$delay, input$tail, input$threshold)
  })
    }else{
      print("Using fixed flow value...")
      
      data <- reactive(
        pars(input$file1$datapath)
      )
      output$CO2Plot <- renderPlotly({
        req(input$file1)
        
        subplot(list(plot1(data(), input$baseline, input$threshold, maxInd, input$delay, input$tail), 
                     plot2(data()),
                     plot3(data())), 
                nrows = 3, margin = 0.03, titleX = TRUE, titleY = TRUE,
                shareX = TRUE)
      })
      output$carbon <- renderTable({
        req(input$file1)
        integ(data(), input$delay, input$tail, input$threshold, input$flow, input$baseline)
      })
    }
    }
  })
 }

shinyApp(ui = ui, server = server)
