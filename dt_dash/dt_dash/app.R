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
                  min = 10, max = 50, value = 25)
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput("histPlot2")),
        column(6, plotOutput("histPlot1"))
      ),
      fluidRow(
        column(12, h2("Decision Tree for Used Car Price Prediction"), plotOutput("treeOutput"))
      )
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
    df <- subset(df, select = -c(X0.012,Price.inr,Width,Year))
    set.seed(1)
    train = sample(1:nrow(df), nrow(df)/2)
    tree.df=tree(priceUSD~.,df,subset=train)
    
    summary(tree.df)
    plot(tree.df)
    text(tree.df,pretty=0)
  })
}

shinyApp(ui = ui, server = server) 