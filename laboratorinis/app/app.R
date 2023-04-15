  library(shiny)
  library(tidyverse)
  library(readr)
  library(ggplot2)
  
  data = read.csv("../data/lab_sodra.csv")
  prafiltruota = data[data$ecoActCode == "471900",]
  
  # UI 
  ui = fluidPage(
    titlePanel("471900 Kita mažmeninė prekyba nespecializuotose parduotuvėse"),
    sidebarLayout(
      sidebarPanel(
        selectInput("company_code", label = "Company code:", choices = unique(prafiltruota$code)),
        actionButton("submit_button", "Submit")
      ),
      
      mainPanel(
        plotOutput("company_plot"),
        tableOutput("table")
      )
    )
  )
  
  #  Server logic 
  server <- function(input, output, session) {
    Graf = reactive({
      req(input$company_code)
      Filter %>% filter(code %in% input$company_code)
    })
    output$company_plot <- renderPlot({
      g = ggplot(Graf(), aes(x = month, y = avgWage, group = name, color = name))
      g + geom_line()
    })
    output$table = renderTable(Graf())
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
