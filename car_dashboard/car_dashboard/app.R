library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(glmnet)
library(data.table) 

# Load and preprocess the data
usa_data <- read.csv("USA_CAR_DATASET.csv")  

usa_clean <- usa_data %>%
  select(price, year, brand, model, mileage, title_status) %>%
  filter(!is.na(price) & price > 0 & mileage > 0) %>%  # Remove rows with invalid prices or mileage
  mutate(
    year = as.numeric(year),
    mileage = as.numeric(mileage),
    brand = as.factor(brand),
    title_status = as.factor(title_status)
  ) %>%
  filter(brand != "Ford") %>%  # Remove Ford as it is an outlier
  drop_na()  # Drop rows with remaining NAs

# Remove brands with few entries
brand_counts <- usa_clean %>%
  count(brand) %>%
  arrange(n)

low_count_brands <- brand_counts %>%
  filter(n < 10)

usa_clean <- usa_clean %>%
  filter(!(brand %in% low_count_brands$brand))

# Detect and remove outliers based on IQR
price_iqr <- IQR(usa_clean$price)
price_q1 <- quantile(usa_clean$price, 0.25)
price_q3 <- quantile(usa_clean$price, 0.75)

mileage_iqr <- IQR(usa_clean$mileage)
mileage_q1 <- quantile(usa_clean$mileage, 0.25)
mileage_q3 <- quantile(usa_clean$mileage, 0.75)

usa_clean <- usa_clean %>%
  filter(
    price >= (price_q1 - 1.5 * price_iqr) & price <= (price_q3 + 1.5 * price_iqr),
    mileage >= (mileage_q1 - 1.5 * mileage_iqr) & mileage <= (mileage_q3 + 1.5 * mileage_iqr)
  )

usa_clean <- usa_clean %>%
  mutate(
    year_scaled = scale(year),
    mileage_scaled = scale(mileage),
    year_mileage_interaction = year_scaled * mileage_scaled,
    mileage_squared = mileage_scaled^2,
    year_squared = year_scaled^2,
    mileage_category = cut(mileage, breaks = c(-Inf, 50000, 100000, Inf), labels = c("Low", "Medium", "High"))
  )

# Split data into training and testing sets
set.seed(1)
n <- nrow(usa_clean)
train_indices <- sample(1:n, size = 0.7 * n)
train_data <- usa_clean[train_indices, ]
test_data <- usa_clean[-train_indices, ]

train_data <- train_data %>%
  mutate(log_price = log(price), log_mileage = log(mileage))

test_data <- test_data %>%
  mutate(log_price = log(price), log_mileage = log(mileage))

x_train <- model.matrix(log_price ~ year_scaled + log_mileage + year_mileage_interaction + mileage_squared + year_squared + brand + title_status + mileage_category, data = train_data)[, -1]
y_train <- train_data$log_price

x_test <- model.matrix(log_price ~ year_scaled + log_mileage + year_mileage_interaction + mileage_squared + year_squared + brand + title_status + mileage_category, data = test_data)[, -1]
y_test <- test_data$log_price

# Ridge Regression model
set.seed(1)
ridge_model <- cv.glmnet(x_train, y_train, alpha = 0, nfolds = 5)  # Reduce the number of folds for faster training

# Shiny UI
ui <- fluidPage(
  titlePanel("Car Price Prediction - Ridge Regression"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "brand", 
        "Select Brand:", 
        choices = unique(usa_clean$brand)
      ),
      helpText("Choose the car manufacturer from the list of available brands. Only brands with sufficient data are included."),
      
      sliderInput(
        "year", 
        "Car Year:", 
        min = 1990, 
        max = 2024, 
        value = 2015, 
        step = 1, 
        sep = ""
      ),
      helpText("Select the manufacturing year of the car. Older cars may generally have lower prices, depending on other factors."),
      
      sliderInput(
        "mileage", 
        "Mileage:", 
        min = 0, 
        max = 300000, 
        value = 50000, 
        step = 1000
      ),
      helpText("Set the total mileage of the car. Lower mileage typically indicates less wear and tear, which can result in higher prices."),
      
      selectInput(
        "title_status", 
        "Title Status:", 
        choices = unique(usa_clean$title_status)
      ),
      helpText("Select the car's title status (e.g., Clean, Salvage). Clean titles usually increase a car's value."),
      
      actionButton("predict_btn", "Predict Price"),
      textOutput("predicted_price")
    ),
    mainPanel(
      plotOutput("plot"),
      plotOutput("scatter_plot")
    )
  )
)

# Shiny server
server <- function(input, output) {
  predict_price <- eventReactive(input$predict_btn, {
    new_data <- data.frame(
      year_scaled = (input$year - mean(usa_clean$year)) / sd(usa_clean$year),
      mileage_scaled = (input$mileage - mean(usa_clean$mileage)) / sd(usa_clean$mileage),
      brand = factor(input$brand, levels = levels(usa_clean$brand)),
      title_status = factor(input$title_status, levels = levels(usa_clean$title_status)),
      year_mileage_interaction = (input$year - mean(usa_clean$year)) / sd(usa_clean$year) * (input$mileage - mean(usa_clean$mileage)) / sd(usa_clean$mileage),
      mileage_squared = ((input$mileage - mean(usa_clean$mileage)) / sd(usa_clean$mileage))^2,
      year_squared = ((input$year - mean(usa_clean$year)) / sd(usa_clean$year))^2,
      mileage_category = cut(input$mileage, breaks = c(-Inf, 50000, 100000, Inf), labels = c("Low", "Medium", "High"))
    )
    
    x_new <- model.matrix(~ year_scaled + mileage_scaled + year_mileage_interaction + mileage_squared + year_squared + brand + title_status + mileage_category, data = new_data)[, -1]
    log_price_pred <- predict(ridge_model, s = ridge_model$lambda.min, newx = x_new)
    exp(log_price_pred)
  })
  
  output$predicted_price <- renderText({
    req(predict_price())
    paste("Predicted Price: $", round(predict_price(), 2))
  })
  
  output$plot <- renderPlot({
    # Create a bar plot of average predicted prices per brand
    cached_predictions <- predict(ridge_model, s = ridge_model$lambda.min, newx = x_test)  # Cache predictions to avoid recalculating
    test_data_with_predictions <- test_data %>%
      mutate(predicted_price = exp(cached_predictions))
    
    avg_price_per_brand <- test_data_with_predictions %>%
      group_by(brand) %>%
      summarise(avg_predicted_price = mean(predicted_price, na.rm = TRUE))
    
    ggplot(data = avg_price_per_brand, aes(x = brand, y = avg_predicted_price, fill = brand)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      theme_minimal() +
      labs(title = "Average Predicted Price per Brand", x = "Brand", y = "Average Predicted Price") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14))
  })
  
  output$scatter_plot <- renderPlot({
    # Create a scatter plot of predicted vs actual prices
    cached_predictions <- predict(ridge_model, s = ridge_model$lambda.min, newx = x_test)
    test_data_with_predictions <- test_data %>%
      mutate(predicted_price = exp(cached_predictions))
    
    ggplot(data = test_data_with_predictions, aes(x = predicted_price, y = price)) +
      geom_point(alpha = 0.6, position = position_jitter(width = 1000, height = 1000)) +
      geom_point(alpha = 0.6) +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      theme_minimal() +
      labs(title = "Predicted vs Actual Prices", x = "Predicted Price", y = "Actual Price") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
