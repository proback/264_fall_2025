# from ProfR_simple_app

# Description of variables in Ames housing data:
# https://ww2.amstat.org/publications/jse/v19n3/decock/DataDocumentation.txt


library(shiny)
library(tidyverse)
ames <- read_csv("data/AmesHousing.csv")
ames <- ames %>%
  rename(LotShape = `Lot Shape`,
         LandSlope = `Land Slope`,
         HouseStyle = `House Style`,
         PavedDrive = `Paved Drive`)
  
ui <- fluidPage(
  titlePanel("Cool Shiny App on Ames Housing Data"),
  sidebarLayout(position = "right",
    sidebarPanel(
      selectInput(inputId = "categ",
                  label = h3("Boxplot categories"),
                  choices = c("Lot Shape" = "LotShape", 
                              "Land Slope" = "LandSlope", 
                              "House Style" = "HouseStyle", 
                              "Foundation" = "Foundation", 
                              "Paved Drive" = "PavedDrive"),
                  selected = "Foundation"),
      numericInput(inputId = "cutoff", 
                   label = h3("Exclude prices above:"), 
                   value = 500000)
    ),
    mainPanel(
      plotOutput(outputId = "boxPlot")
    )
  )
)

server <- function(input, output) {
  
  selectedData <- reactive({
    ames %>%
      filter(SalePrice < input$cutoff) %>%
      select(input$categ, "SalePrice")
  })
   
  output$boxPlot <- renderPlot({
    ggplot(data = selectedData(), aes(x = .data[[input$categ]], y = SalePrice)) +
      geom_boxplot() +
      coord_flip() 
  })
}

shinyApp(ui = ui, server = server)