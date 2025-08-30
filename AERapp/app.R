# from AER_app

library(AER)
library(tidyverse)
data("NMES1988")

NMES1988 <- NMES1988 |>
    sample_n(500)    # use smaller data so shiny app runs faster


ui <- fluidPage(
  titlePanel("Predictors of physician visits in NMES1988 data"),
  sidebarLayout(position = "left",
    sidebarPanel(
      selectInput("x", label = "x-axis variable:",
                  choices = c("Family income" = "income",
                              "Age (years/10)" = "age",
                              "Chronic conditions" = "chronic")),
      checkboxInput("type_check2","Include private insurance?",value=FALSE)
    ),
    mainPanel(
      plotOutput(outputId = "scatterPlot")
    )
  )
)

server <- function(input, output) {
      
  output$scatterPlot <- renderPlot({
     if (input$type_check2) 
        ggplot(NMES1988, aes(x = .data[[input$x]], y = visits, color = insurance)) +   
          scale_color_manual(values=c("orange","blue")) + 
          geom_jitter(size = 0.9, alpha = 0.4) 
     else if (!input$type_check2)      
        ggplot(NMES1988, aes(x = .data[[input$x]], y = visits)) +   
          scale_color_manual(values=c("orange","blue")) + 
          geom_jitter(size = 0.9, alpha = 0.4)
  })
}  

shinyApp(ui = ui, server = server)