# from ProfR_app

# Description of variables in Ames housing data:
# https://ww2.amstat.org/publications/jse/v19n3/decock/DataDocumentation.txt


library(shiny)
library(tidyverse)
ames <- read_csv("data/AmesHousing.csv")
ames <- ames %>%
  rename(LotShape = `Lot Shape`,
         LandSlope = `Land Slope`,
         HouseStyle = `House Style`,
         PavedDrive = `Paved Drive`,
         LotArea = `Lot Area`,
         OverallQual = `Overall Qual`,
         YearBuilt = `Year Built`,
         SqFtAboveGrd = `Gr Liv Area`,
         Bedrooms = `Bedroom AbvGr`)


cat_predictors <- fluidPage(
    sidebarLayout(sidebarPanel(
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
    ))
)

num_predictors <- fluidPage(
  sidebarLayout(sidebarPanel(
    selectInput(inputId = "numer",
                label = h3("Numeric predictors"),
                choices = c("Lot Area" = "LotArea", 
                            "Overall Quality" = "OverallQual", 
                            "Year Built" = "YearBuilt", 
                            "Above Ground Sqft" = "SqFtAboveGrd", 
                            "Bedrooms" = "Bedrooms"),
                selected = "Above Ground Sqft"),
    numericInput(inputId = "cutoff2", 
                 label = h3("Exclude prices above:"), 
                 value = 500000)
  ),
  mainPanel(
    plotOutput(outputId = "scatterPlot")
  ))
)


ui <- tagList(navbarPage(title = "Cool Shiny App on Ames Housing Data",
                 tabPanel("Categorical Predictors",
                          cat_predictors),
                 tabPanel("Numeric Predictors",
                          num_predictors))
)


server <- function(input, output) {
  
  output$boxPlot <- renderPlot({
    datasetInput = reactive({
      ames %>%
        filter(SalePrice < input$cutoff) %>%
        select(input$categ, "SalePrice")
    })
    mydata1 = datasetInput()
    ggplot(mydata1, aes(x = .data[[input$categ]], y = SalePrice)) +
      geom_boxplot() +
      coord_flip() 
  })

  output$scatterPlot <- renderPlot({
    datasetInput = reactive({
      ames %>%
        filter(SalePrice < input$cutoff2) %>%
        select(input$numer, "SalePrice")
    })
    mydata2 = datasetInput()
    ggplot(mydata2, aes(x = .data[[input$numer]], y = SalePrice)) +
      geom_point() +
      geom_smooth() 
  })
}

shinyApp(ui = ui, server = server)