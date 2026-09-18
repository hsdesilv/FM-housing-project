library(shiny)
library(vetiver)

endpoint <- vetiver_endpoint(
  "http://127.0.0.1:8000/predict"
)
ui <- fluidPage(
  
  titlePanel("FM Housing Price Predictor"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      numericInput(
        "total_sq_ft",
        "Total Square Footage:",
        value = 2000,
        min = 0
      ),
      
      sliderInput(
        "year_built",
        "Year Built:",
        min = 1800,
        max = 2022,
        value = 2005,
        step = 1
      ),
      
      sliderInput(
        "total_bedrooms",
        "Number of Bedrooms:",
        min = 0,
        max = 6,
        value = 3,
        step = 1
      ),
      
      sliderInput(
        "total_bathrooms",
        "Number of Bathrooms:",
        min = 0,
        max = 6,
        value = 2,
        step = 1
      ),
      
      sliderInput(
        "garage_stalls",
        "Garage Stalls:",
        min = 0,
        max = 4,
        value = 2,
        step = 1
      ),
      
      numericInput(
        "lot_size_sq_ft",
        "Lot Size (sq ft):",
        value = 8000,
        min = 0
      ),
      
      selectInput(
        "city",
        "City:",
        choices = c(
          "Fargo",
          "Moorhead",
          "West Fargo"
        ),
        selected = "Fargo"
      ),
      
      selectInput(
        "high_school",
        "High School:",
        choices = c(
          "Fargo Davies",
          "Fargo North",
          "Fargo South",
          "Moorhead",
          "West Fargo",
          "West Fargo Sheyenne"
        ),
        selected = "Fargo Davies"
      ),
      
      selectInput(
        "style",
        "Home Style:",
        choices = c(
          "1 1/2 Stor",
          "1 Story",
          "2 Story",
          "3 Level",
          "3 Story",
          "4 Level",
          "Bi Level",
          "None"
        ),
        selected = "1 Story"
      )
      
    ),  
    
    mainPanel(
      h3("Predicted Sold Price"),
      
      actionButton(
        "predict_button",
        "Predict Price"
      ),
      
      
      div(
        style = "
      font-size: 36px;
      font-weight: bold;
      margin-top: 20px;
    ",
      
      textOutput("predicted_price")
    )
    
  )  
  
)
  
)


server <- function(input, output, session) {
  
  observeEvent(input$predict_button, {
    
    new_house <- data.frame(
      total_sq_ft = input$total_sq_ft,
      year_built = as.integer(input$year_built),
      total_bedrooms = as.integer(input$total_bedrooms),
      total_bathrooms = as.integer(input$total_bathrooms),
      garage_stalls = as.integer(input$garage_stalls),
      lot_size_sq_ft = input$lot_size_sq_ft,
      city = input$city,
      high_school = input$high_school,
      style = input$style
    )
    
    prediction <- predict(
      endpoint,
      new_house)
    
    output$predicted_price <- renderText({
      
      paste0(
        "$",
        format(
          round(prediction$.pred),
          big.mark = ","
        )
      )
      
    })
    
   
    
  })
  
}

shinyApp(ui = ui, server = server)
