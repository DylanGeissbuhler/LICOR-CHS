load.lib = c("tidyverse", "DescTools", "plotly", "data.table", "purrr","readxl","MASS", "pracma", "lubridate","shiny","vtable")   
install.lib = load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib,require,character=TRUE)

options(shiny.maxRequestSize=30*1024^2)

ui <- fluidPage(
  
  # App title
  titlePanel("LICOR-CHS Sample CO2 Quantification"),

    sidebarPanel(
      numericInput("baseline","Enter a baseline value [ppm] :", value = 400),
      numericInput("flow","Flowrate of CHS [mL/min] :", value = 55),
      numericInput("threshold","Value of threshold for peak [ppm] :", value = 2000),
      numericInput("delay","Delay between start of peak and top [s] :", value = 20),
      numericInput("tail", "Duration of tailing [s] :", value = 120),
      tableOutput("carbon")
    ),
  
 # UI
 mainPanel(
   
   fileInput("file1","Choose a .txt file (max. size 30MB) :", multiple = FALSE),
  
   plotlyOutput("CO2Plot", height = "1000px"),

   )
  )

# Define server logic 
server <- function(input, output, session) {

  source("LICOR_integration.R", encoding = "UTF-8", echo = TRUE)
  
  data <- reactive(
    pars(input$file1$datapath)
  )
  
  output$contents <- renderTable({
    req(input$file1)
    data()
  })
  
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

shinyApp(ui = ui, server = server)

