library(shiny)
library(ggplot2)

df <- read.csv('car details v4 edited.csv', header = T)
options(scipen=999)
library(tree)

ui <- fluidPage(
  titlePanel("Used Car Price Based On "),
  h4("This app uses used car auction data to create a model to help you predict a price of a car.
     It contains 2 histograms to help you get a better feeling of what a car should be worth based 
     on its specs. The left histogram will help give you the general look at what the trends in 
     car prices are and the right one will allow you to pick the general trends of what the car specs 
     that are being sold the most at. The tree at the bottom can help you make a general price of a 
     car based on the current specs of the car."),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Choose a Variable to Visualize for the Right side histogram:",
                  choices = c("Kilometer", "Max_Power", "Max_Torque", "Engine")),
      sliderInput("binwidth1", "Bin Width for Price Histogram",
                  min = 10, max = 50, value = 25),
      sliderInput("binwidth2", "Bin Width for Histogram on the Far Right",
                  min = 10, max = 50, value = 25),
      h3("Predict your price using the tree."),
      sliderInput("mileage", "Mileage", min = 0, max = 2000000, value = 50000),
      sliderInput("Max_Power", "Max_Power", min = 20, max = 800, value = 150),
      sliderInput("Max_Torque", "Max_Torque", min = 20, max = 800, value = 150),
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput("histPlot2")),
        column(6, plotOutput("histPlot1"))
      ),
      fluidRow(
        column(12, h2("Decision Tree for Used Car Price Prediction"), plotOutput("treeOutput"))
      ),
      h2("Predicted Price"),
      textOutput("predictedPrice")
    )
  )
)

server <- function(input, output) {
  output$histPlot1 <- renderPlot({
    ggplot(df, aes_string(x = input$variable)) +
      geom_histogram(fill = "blue", color = "black", bins = input$binwidth2) +
      ggtitle(paste("Histogram of", input$variable))+
      ylab("Number of Car Listings")
  })
  
  output$histPlot2 <- renderPlot({
    ggplot(df, aes(x = priceUSD)) +
      geom_histogram(fill = "grey", color = "black", bins = input$binwidth1) +
      ggtitle("Histogram of Price")+
      ylab("Number of Car Listings")
  })
  output$treeOutput <- renderPlot({
    df <- subset(df, select = -c(X0.012,Price.inr,Width,Year, Length, Height, Fuel.Tank.Capacity))
    set.seed(1)
    train = sample(1:nrow(df), nrow(df)/2)
    tree.df=tree(priceUSD~.,df,subset=train)
    
    summary(tree.df)
    plot(tree.df)
    text(tree.df,pretty=0)
  })
  output$predictedPrice <- renderText({
    # Create a new data frame with user-specified values
    new_data <- data.frame(
      Kilometer = input$mileage,
      Max_Power = input$Max_Power,
      Engine = 100,
      Max_Torque = input$Max_Torque,
      Year = 2020,
      Length = 2000,
      Width = 2000,
      Height = 2000,
      Fuel.Tank.Capacity = 30
    )
    
    # Make prediction using the decision tree model
    predicted_price <- predict(tree.df, newdata = new_data)
    
    # Display the predicted price
    paste("Predicted Price:", round(predicted_price, 2))
  })
}

shinyApp(ui = ui, server = server) 