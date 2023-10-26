# Libraries ----
library(shiny)

# Shiny app ----
ui <- fluidPage(
    titlePanel("Car Price Forecasting"),
    sidebarLayout(
        sidebarPanel(
            selectInput("car_make", "Car Make:", choices = unique(car_sales$car_make)),
            selectInput("car_model", "Car Model:", choices = unique(car_sales$car_model)),
            numericInput("car_year", "Car Year:", 
                         min = min(car_sales$year), 
                         max = max(car_sales$year), 
                         value = 2020),
            numericInput("com_earned", "Commission Earned:", 
                         min = min(car_sales$commission_earned), 
                         max = max(car_sales$commission_earned), 
                         value = 1000),
            numericInput("com_rate", "Commission Rate:", 
                         min = min(car_sales$commission_rate), 
                         max = max(car_sales$commission_rate), 
                         value = 0.1),
            
            actionButton("predictBtn", "Predict Price"),
            textOutput("predictedPrice")
        ),
        mainPanel()
    )
)

# Server ----
server <- function(input, output) {
    observeEvent(input$predictBtn, {
        new_data <- data.frame(
            car_make = input$car_make,
            car_model = input$car_model,
            year = input$car_year,
            commision_earned = input$com_earned,
            commission_rate = input$com_rate
        )
        
        # Encode categorical variables
        new_data <- dummy_vars(select = c(car_make, car_model), data = new_data)
        
        # Predict the car price
        predicted_price <- predict(lm_fit, new_data)
        
        output$predictedPrice <- renderText({
            paste("Predicted Price: $", round(predicted_price, 2))
        })
    })
}

# Run the App ----
shinyApp(ui, server)
