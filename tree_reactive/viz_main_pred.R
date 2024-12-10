library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(glmnet)
library(data.table)
library(tree)
library(ggcorrplot)


# prepare data
pdata <- read.csv("car_prices.csv")
cdata <- read.csv("Car Dataset 1945-2020.csv")

# make column names same
names(cdata) <- tolower(names(cdata))
names(pdata) <- tolower(names(pdata))


# avg the mmr's and sell prices
avg_data <- pdata %>%
  group_by(make, model, year) %>%
  summarise(
    avg_mmr = mean(mmr, na.rm = TRUE),
    avg_sellingprice = mean(sellingprice, na.rm = TRUE),
    .groups = 'drop'
  )

# year_from and year_to in cdata to numeric
cdata <- cdata %>%
  mutate(year_from = as.numeric(year_from), year_to = as.numeric(year_to))

# join and filter the data for the year range
cdata_with_avg <- cdata %>%
  left_join(avg_data, by = c("make", "modle" = "model")) %>%
  filter(year_from <= year & year <= year_to)


# filter out rows where avg_mmr or avg_sellingprice are NA
final_cdata_must_sell <- cdata_with_avg %>%
  filter(!is.na(avg_mmr) & !is.na(avg_sellingprice))

final_cdata <- cdata_with_avg

# convert character columns to factors in cdata
cdata_factored <- final_cdata %>% mutate(across(where(is.character), factor))


# adds dif between mmr and real sell price
clean_data <- final_cdata %>%
  mutate(diff = avg_mmr - avg_sellingprice)


clean_data$modle <- as.character(clean_data$modle)
clean_data$make <- as.character(clean_data$make)


clean_data[] <- lapply(clean_data, function(x) {
  if (is.character(x)) {
    (as.factor(x))  
  } 
  else {
    x  # Leave non-factor columns unchanged
  }
})
# hard coded, but not needed
clean_data$make <- as.character(clean_data$make)
clean_data$modle <- as.character(clean_data$modle)
clean_data$generation <- as.character(clean_data$generation)
clean_data$series <- as.character(clean_data$series)
clean_data$curb_weight_kg <- as.numeric(clean_data$curb_weight_kg)
clean_data$rear_track_mm <- as.numeric(clean_data$rear_track_mm)
clean_data$trailer_load_with_brakes_kg <- as.numeric(clean_data$trailer_load_with_brakes_kg)
clean_data$front_track_mm <- as.numeric(clean_data$front_track_mm)
clean_data$wheel_size_r14 <- as.numeric(clean_data$wheel_size_r14)
clean_data$cargo_volume_m3 <- as.numeric(clean_data$cargo_volume_m3)
clean_data$compression_ratio <- as.numeric(clean_data$compression_ratio)
clean_data$turnover_of_maximum_torque_rpm <- as.numeric(clean_data$turnover_of_maximum_torque_rpm)
clean_data$engine_hp_rpm <- as.numeric(clean_data$engine_hp_rpm)
clean_data$capacity_cm3 <- as.numeric(clean_data$capacity_cm3)
clean_data$engine_hp <- as.numeric(clean_data$engine_hp)
clean_data$range_km <- as.numeric(clean_data$range_km)
clean_data$max_trunk_capacity_l <- as.numeric(clean_data$max_trunk_capacity_l)
clean_data$max_speed_km_per_h <- as.numeric(clean_data$max_speed_km_per_h)
clean_data$back_suspension <- as.numeric(clean_data$back_suspension)
clean_data$front_suspension <- as.numeric(clean_data$front_suspension)
clean_data$trim <- as.numeric(clean_data$trim)
clean_data$fuel_tank_capacity_l <- as.numeric(clean_data$fuel_tank_capacity_l)
clean_data$cargo_compartment_length_width_height_mm <- as.numeric(clean_data$cargo_compartment_length_width_height_mm)
clean_data$front_rear_axle_load_kg <- as.numeric(clean_data$front_rear_axle_load_kg)
clean_data$length_mm <- as.numeric(clean_data$length_mm)
clean_data$width_mm <- as.numeric(clean_data$width_mm)
clean_data$wheelbase_mm <- as.numeric(clean_data$wheelbase_mm)
clean_data$maximum_torque_n_m <- as.numeric(clean_data$maximum_torque_n_m)
clean_data$clearance_mm <- as.numeric(clean_data$clearance_mm)
clean_data$load_height_mm <- as.numeric(clean_data$load_height_mm)
clean_data$number_of_seats <- as.numeric(clean_data$number_of_seats)
clean_data$ground_clearance_mm <- as.numeric(clean_data$trailer_load_with_brakes_kg)
clean_data$height_mm <- as.numeric(clean_data$height_mm)

clean_data$diff <- clean_data$avg_sellingprice - clean_data$avg_mmr


# gets rid of bad columns and rows
attach(clean_data)
clean_data <- clean_data %>%
  select(where(~ mean(is.na(.)) <= 0.3))


clean_data<- clean_data[rowSums(is.na(clean_data)) <= 1, ]

incomplete_clean_data <- clean_data
clean_data<- clean_data[rowSums(is.na(clean_data)) <= 0, ]
plot(clean_data$max_speed_km_per_h, clean_data$engine_hp, main = "max speed vs engine hp", xlab = "engine hp", ylab = "max speed km/h")
library(dplyr)

# Assuming df is your data frame
numeric_data <- clean_data %>% mutate_all(as.numeric)
numeric_data[is.na(numeric_data)] <- 0  # Replace NA with 0, or handle as required


#model1
#par(mar = c(0.1, 0.1, 0.1, 0.1))
set.seed(1)
train = sample(1:nrow(clean_data), nrow(clean_data)/2)

train = sample(1:nrow(numeric_data), nrow(numeric_data)/2)
#Experimental----------------------------------------------------- 
#wasnt able to invest as much time into these other ones
#tree.car=tree(max_speed_km_per_h ~., clean_data,subset=train)
# 
# summary(tree.car)
# plot(tree.car)
# text(tree.car,pretty=0)
# 
# # see if pruning gives us a better tree
# cv.car=cv.tree(tree.car)
# plot(cv.car$size,cv.car$dev,type='b')
# cv.car
# 
# # make predictions using the tree
# yhat=predict(tree.car,newdata=clean_data[-train,])
# car.test=clean_data[-train,"max_speed_km_per_h"]
# plot(yhat,car.test)
# abline(0,1)
# mean((yhat-car.test)^2)
# 
# # make a linear regression model using lstat, rm, and dis
# mod=lm(max_speed_km_per_h ~ acceleration_0_100_km.h_s+ minimum_trunk_capacity_l + height_mm + engine_hp + body_type
#        , data=clean_data, subset=train)
# yhat2=predict(mod,newdata=clean_data[-train,])
# plot(yhat2,car.test)
# abline(0,1)
# mean((yhat2-car.test)^2)

# #model 2
# numeric_data <- clean_data %>% select_if(is.numeric)
# cor(numeric_data, use = "complete.obs")[, "diff"]
# 
# 
# 
# 
# set.seed(1)
# train = sample(1:nrow(clean_data), nrow(clean_data)/2)
# tree.car=tree(diff~. -avg_mmr -avg_sellingprice -year_from -year_to -year, clean_data,subset=train)
# 
# summary(tree.car)
# plot(tree.car)
# text(tree.car,pretty=0)
# 
# # see if pruning gives us a better tree
# cv.car=cv.tree(tree.car)
# plot(cv.car$size,cv.car$dev,type='b')
# cv.car
# 
# # make predictions using the tree
# yhat=predict(tree.car,newdata=clean_data[-train,])
# car.test=clean_data[-train,"diff"]
# plot(yhat,car.test)
# abline(0,1)
# mean((yhat-car.test)^2)
# 
# # make a linear regression model using lstat, rm, and dis
# mod=lm(max_speed_km_per_h ~ minimum_trunk_capacity_l + height_mm + engine_hp + body_type + + wheel_size_r14 + ground_clearance_mm + number_of_cylinders + number_of_gears, data=clean_data, subset=train)
# yhat2=predict(mod,newdata=clean_data[-train,])
# plot(yhat2,car.test)
# abline(0,1)
# mean((yhat2-car.test)^2)
# 
# 
# #model3
# attach(numeric_data)
# numeric_data <- clean_data %>% 
#   select_if(is.numeric)
# set.seed(1)
# train = sample(1:nrow(numeric_data), nrow(numeric_data)/2)
# tree.car=tree(diff~. -avg_mmr -avg_sellingprice -year, numeric_data,subset=train)
# 
# summary(tree.car)
# plot(tree.car)
# text(tree.car,pretty=0)
# 
# # see if pruning gives us a better tree
# cv.car=cv.tree(tree.car)
# plot(cv.car$size,cv.car$dev,type='b')
# cv.car
# 
# # make predictions using the tree
# yhat=predict(tree.car,newdata=clean_data[-train,])
# car.test=numeric_data[-train,"diff"]
# plot(yhat,car.test)
# abline(0,1)
# mean((yhat-car.test)^2)
# #model 4
# attach(numeric_data)
# cor(range_km, numeric_data)
# set.seed(1)
# train = sample(1:nrow(numeric_data), nrow(numeric_data)/2)
# car.test=numeric_data[-train,] # make test set the complement of train
# 
# 
# lm.car <- lm(range_km ~ front_suspension +back_suspension +fuel_tank_capacity_l +number_of_gears +number_of_seats +number_of_cylinders +capacity_cm3 +full_weight_kg +number_of_cylinders +engine_hp +engine_hp_rpm +front_track_mm +rear_track_mm +wheel_size_r14 +wheelbase_mm +ground_clearance_mm +max_trunk_capacity_l +cargo_volume_m3 +max_speed_km_per_h +maximum_torque_n_m +acceleration_0_100_km.h_s, numeric_data[train,])
# plot(lm.car)
# coef(lm.car)
# 
# #prediction
# pred.car <- predict(lm.car, newdata = numeric_data[-train,] , interval = "confidence")
# #print(pred.car)

#
#Experimental----------------------------------------------------- 



# shiny car
library(shiny)
library(dplyr)

# Assuming clean_data and numeric_data are already loaded and processed
ui <- fluidPage(
  titlePanel("Car Specification Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("make", "Make:", choices = unique(clean_data$make), selected = NULL, multiple = TRUE),
      selectInput("modle", "Model:", choices = unique(clean_data$modle), selected = NULL, multiple = TRUE),
      sliderInput("number_of_seats", "Number of Seats:", min = min(clean_data$number_of_seats, na.rm = TRUE), max = max(clean_data$number_of_seats, na.rm = TRUE), value = min(clean_data$number_of_seats, na.rm = TRUE)),
      sliderInput("length_mm", "Length (mm):", min = min(clean_data$length_mm, na.rm = TRUE), max = max(clean_data$length_mm, na.rm = TRUE), value = min(clean_data$length_mm, na.rm = TRUE)),
      sliderInput("width_mm", "Width (mm):", min = min(clean_data$width_mm, na.rm = TRUE), max = max(clean_data$width_mm, na.rm = TRUE), value = min(clean_data$width_mm, na.rm = TRUE)),
      sliderInput("height_mm", "Height (mm):", min = min(clean_data$height_mm, na.rm = TRUE), max = max(clean_data$height_mm, na.rm = TRUE), value = min(clean_data$height_mm, na.rm = TRUE)),
      sliderInput("wheelbase_mm", "Wheelbase (mm):", min = min(clean_data$wheelbase_mm, na.rm = TRUE), max = max(clean_data$wheelbase_mm, na.rm = TRUE), value = min(clean_data$wheelbase_mm, na.rm = TRUE)),
      sliderInput("curb_weight_kg", "Curb Weight (kg):", min = min(clean_data$curb_weight_kg, na.rm = TRUE), max = max(clean_data$curb_weight_kg, na.rm = TRUE), value = min(clean_data$curb_weight_kg, na.rm = TRUE)),
      sliderInput("engine_hp", "Engine HP:", min = min(clean_data$engine_hp, na.rm = TRUE), max = max(clean_data$engine_hp, na.rm = TRUE), value = min(clean_data$engine_hp, na.rm = TRUE)),
      actionButton("predict", "Predict")
    ),
    mainPanel(
      textOutput("pred_max_speed"), 
      textOutput("pred_accel"), 
      textOutput("mse_max_speed"), 
      textOutput("mse_accel"), 
      textOutput("rmse_max_speed"), 
      textOutput("rmse_accel"), 
      plotOutput("max_speed_plot"), 
      plotOutput("accel_plot")
    )
  )
)




# server
server <- function(input, output) {
  observeEvent(input$predict, {
    print("Predict button clicked")  # Debugging line
    
    # Normalize input values for consistency
    input_make <- tolower(trimws(input$make))
    input_modle <- tolower(trimws(input$modle))
    
    # Normalize data values for consistency
    clean_data$make <- tolower(trimws(clean_data$make))
    clean_data$modle <- tolower(trimws(clean_data$modle))
    
    # Convert the specified features to numeric in the entire dataset
    numeric_data <- clean_data %>%
      mutate(across(c(number_of_seats, length_mm, width_mm, height_mm, wheelbase_mm, curb_weight_kg, engine_hp), as.numeric))
    
    # Print to debug
    print("Converted data:")
    print(numeric_data)
    
    # Build predictive models using the entire dataset
    model_max_speed <- lm(max_speed_km_per_h ~ number_of_seats + length_mm + width_mm + height_mm + wheelbase_mm + curb_weight_kg + engine_hp + make + modle, data = numeric_data)
    model_accel <- lm(acceleration_0_100_km.h_s ~ number_of_seats + length_mm + width_mm + height_mm + wheelbase_mm + curb_weight_kg + engine_hp + make + modle, data = numeric_data)
    
    # Print model summaries to debug
    print(summary(model_max_speed))
    print(summary(model_accel))
    
    # Calculate MSE for the models 
    mse_max_speed <- mean(residuals(model_max_speed)^2) 
    mse_accel <- mean(residuals(model_accel)^2) 
    
    # Calculate RMSE for the models 
    rmse_max_speed <- sqrt(mse_max_speed) 
    rmse_accel <- sqrt(mse_accel) 
    
    # Print MSE and RMSE values to debug 
    print(paste("MSE for Max Speed: ", mse_max_speed)) 
    print(paste("RMSE for Max Speed: ", rmse_max_speed)) 
    print(paste("MSE for Acceleration: ", mse_accel)) 
    print(paste("RMSE for Acceleration: ", rmse_accel))
    
    # Create a new data frame with the input values for prediction
    new_data <- data.frame(
      number_of_seats = as.numeric(input$number_of_seats),
      length_mm = as.numeric(input$length_mm),
      width_mm = as.numeric(input$width_mm),
      height_mm = as.numeric(input$height_mm),
      wheelbase_mm = as.numeric(input$wheelbase_mm),
      curb_weight_kg = as.numeric(input$curb_weight_kg),
      engine_hp = as.numeric(input$engine_hp),
      make = as.factor(input_make),
      modle = as.factor(input_modle)
    )
    
    # Print new data to debug
    print("New data for prediction:")
    print(new_data)
    
    # Predict using the models
    pred_max_speed <- predict(model_max_speed, newdata = new_data)
    pred_accel <- predict(model_accel, newdata = new_data)
    
    # Print predictions to debug
    print("Predictions:")
    print(pred_max_speed)
    print(pred_accel)
    
    # Output the predictions
    output$pred_max_speed <- renderText({ paste("Predicted Max Speed: ", round(pred_max_speed, 2), " km/h") })
    output$pred_accel <- renderText({ paste("Predicted Acceleration (0-100 km/h): ", round(pred_accel, 2), " s") })
    output$mse_max_speed <- renderText({ paste("MSE for Max Speed Model: ", round(mse_max_speed, 2)) })
    output$mse_accel <- renderText({ paste("MSE for Acceleration Model: ", round(mse_accel, 2)) })
    output$rmse_accel <- renderText({ paste("RMSE for Acceleration Model: ", round(rmse_accel, 2)) })
    output$rmse_max_speed <- renderText({ paste("RMSE for Max Speed Model: ", round(rmse_max_speed, 2)) })
  })
  
  # Generate plot for max speed
  output$max_speed_plot <- renderPlot({
    avg_max_speed <- clean_data %>%
      group_by(make) %>%
      summarise(avg_max_speed = mean(max_speed_km_per_h, na.rm = TRUE)) %>%
      arrange(desc(avg_max_speed))
    
    barplot(avg_max_speed$avg_max_speed, names.arg = paste(avg_max_speed$make),
            xlab = "Make", ylab = "Max Speed (km/h)",
            main = "Max Speed by Make",
            col = "blue", las = 2, cex.names = 0.7)
  })
  
  # Generate plot for acceleration
  output$accel_plot <- renderPlot({
    avg_accel <- clean_data %>%
      group_by(make) %>%
      summarise(avg_accel = mean(acceleration_0_100_km.h_s, na.rm = TRUE)) %>%
      arrange(avg_accel)
    
    barplot(avg_accel$avg_accel, names.arg = paste(avg_accel$make),
            xlab = "Make", ylab = "Acceleration (0-100 km/h in s)",
            main = "Acceleration by Make",
            col = "green", las = 2, cex.names = 0.7)
  })
}

shinyApp(ui = ui, server = server)


shinyApp(ui = ui, server = server)
