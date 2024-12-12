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



#debug
#print(cleaned_data)



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

#shiny project
library(shiny)
library(dplyr)

# function to remove outliers based on IQR
remove_outliers <- function(df, x = 100) {
  df %>% 
    # apply to each numeric column
    mutate(across(where(is.numeric), function(x) {
      Q1 <- quantile(x, 0.25, na.rm = TRUE)
      Q3 <- quantile(x, 0.75, na.rm = TRUE)
      IQR_value <- IQR(x, na.rm = TRUE)
      
      lower_bound <- Q1 - x * IQR_value
      upper_bound <- Q3 + x * IQR_value
      
      # replace outliers with NA
      x <- ifelse(x < lower_bound | x > upper_bound, NA, x)
    })) %>%
    # remove rows with any NA values
    dplyr::filter(complete.cases(.))
}

print(summary(clean_data$number_of_seats))
# remove outliers
clean_data <- remove_outliers(clean_data)

# debug cleaned data
#print(cleaned_data)


# calc means for default cases
mean_year_from <- mean(clean_data$year_from, na.rm = TRUE)
mean_year_to <- mean(clean_data$year_to, na.rm = TRUE)
mean_city_fuel_per_100km_l <- mean(clean_data$city_fuel_per_100km_l,na.rm = TRUE)
mean_length_mm <- mean(clean_data$length_mm, na.rm = TRUE)
mean_width_mm <- mean(clean_data$width_mm, na.rm = TRUE)
mean_height_mm <- mean(clean_data$height_mm, na.rm = TRUE)
mean_wheelbase_mm <- mean(clean_data$wheelbase_mm, na.rm = TRUE)
mean_curb_weight_kg <- mean(clean_data$curb_weight_kg, na.rm = TRUE)
mean_engine_hp <- mean(clean_data$engine_hp, na.rm = TRUE)
print(summary(clean_data$number_of_seats))

# shiny UI
ui <- fluidPage(
  titlePanel("Car Specification Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("make", "Make:", choices = unique(clean_data$make), selected = NULL, multiple = TRUE),
      selectInput("modle", "Model:", choices = unique(clean_data$modle), selected = NULL, multiple = TRUE),
      sliderInput("year_from", "Year From:", min = min(clean_data$year_from, na.rm = TRUE), max = max(clean_data$year_from, na.rm = TRUE), value = mean_year_from),
      sliderInput("year_to", "Year To:", min = min(clean_data$year_to, na.rm = TRUE), max = max(clean_data$year_to, na.rm = TRUE), value = mean_year_to),
      sliderInput("length_mm", "Length (mm):", min = min(clean_data$length_mm, na.rm = TRUE), max = max(clean_data$length_mm, na.rm = TRUE), value = mean_length_mm),
      sliderInput("width_mm", "Width (mm):", min = min(clean_data$width_mm, na.rm = TRUE), max = max(clean_data$width_mm, na.rm = TRUE), value = mean_width_mm),
      sliderInput("height_mm", "Height (mm):", min = min(clean_data$height_mm, na.rm = TRUE), max = max(clean_data$height_mm, na.rm = TRUE), value = mean_height_mm),
      sliderInput("wheelbase_mm", "Wheelbase (mm):", min = min(clean_data$wheelbase_mm, na.rm = TRUE), max = max(clean_data$wheelbase_mm, na.rm = TRUE), value = mean_wheelbase_mm),
      sliderInput("curb_weight_kg", "Curb Weight (kg):", min = min(clean_data$curb_weight_kg, na.rm = TRUE), max = max(clean_data$curb_weight_kg, na.rm = TRUE), value = mean_curb_weight_kg),
      sliderInput("engine_hp", "Engine HP:", min = min(clean_data$engine_hp, na.rm = TRUE), max = max(clean_data$engine_hp, na.rm = TRUE), value = mean_engine_hp),
      sliderInput("city_fuel_per_100km_l", "City Fuel Consumption 100km:", min = min(clean_data$city_fuel_per_100km_l, na.rm = TRUE), max = max(clean_data$city_fuel_per_100km_l, na.rm = TRUE), value = mean_city_fuel_per_100km_l),
      actionButton("predict", "Predict")
    ),
    mainPanel(
      uiOutput("predictions"),
      plotOutput("max_speed_plot"), 
      plotOutput("accel_plot"),
      textOutput("mse_max_speed"), 
      textOutput("mse_accel"), 
      textOutput("rmse_max_speed"), 
      textOutput("rmse_accel")
    )
  )
)


server <- function(input, output) {
  observeEvent(input$predict, {
    #print("Predict button clicked")  # debug
    
    # normalize data
    input_make <- tolower(trimws(input$make))
    input_modle <- tolower(trimws(input$modle))
    
    # normalize data
    clean_data$make <- tolower(trimws(clean_data$make))
    clean_data$modle <- tolower(trimws(clean_data$modle))
    
    # ensure numeric
    numeric_data <- clean_data %>%
      mutate(across(c(city_fuel_per_100km_l, length_mm, width_mm, height_mm, wheelbase_mm, curb_weight_kg, engine_hp, year_from, year_to), as.numeric))
    
    # predictive models
    model_max_speed <- lm(max_speed_km_per_h ~ city_fuel_per_100km_l + length_mm + width_mm + height_mm + wheelbase_mm + curb_weight_kg + engine_hp + year_from + year_to + make + modle, data = numeric_data)
    model_accel <- lm(acceleration_0_100_km.h_s ~ city_fuel_per_100km_l + length_mm + width_mm + height_mm + wheelbase_mm + curb_weight_kg + engine_hp + year_from + year_to + make + modle, data = numeric_data)
    
    # MSE
    mse_max_speed <- mean(residuals(model_max_speed)^2)
    mse_accel <- mean(residuals(model_accel)^2)
    
    # RMSE
    rmse_max_speed <- sqrt(mse_max_speed)
    rmse_accel <- sqrt(mse_accel)
    
    # print MSE and RMSE
    print(paste("MSE for Max Speed: ", mse_max_speed))
    print(paste("RMSE for Max Speed: ", rmse_max_speed))
    print(paste("MSE for Acceleration: ", mse_accel))
    print(paste("RMSE for Acceleration: ", rmse_accel))
    
    # prepare outputs
    pred_max_speed_outputs <- tagList()
    pred_accel_outputs <- tagList()
    
    for (make in input_make) {
      for (modle in input_modle) {
        # Input/new data to be predicted
        new_data <- data.frame(
          city_fuel_per_100km_l = as.numeric(input$city_fuel_per_100km_l),
          length_mm = as.numeric(input$length_mm),
          width_mm = as.numeric(input$width_mm),
          height_mm = as.numeric(input$height_mm),
          wheelbase_mm = as.numeric(input$wheelbase_mm),
          curb_weight_kg = as.numeric(input$curb_weight_kg),
          engine_hp = as.numeric(input$engine_hp),
          year_from = as.numeric(input$year_from),
          year_to = as.numeric(input$year_to),
          make = as.factor(make),
          modle = as.factor(modle)
        )
        

        
        # ensure new_data contains only the predictors
        predictors <- names(model_max_speed$model)
        predictors <- predictors[!predictors %in% c("max_speed_km_per_h", "acceleration_0_100_km.h_s")]
        
        # missing columns debug
        missing_columns <- setdiff(predictors, names(new_data))
        if (length(missing_columns) > 0) {
          stop(paste("Missing columns in new_data:", paste(missing_columns, collapse = ", ")))
        }
        
        # predict
        pred_max_speed <- predict(model_max_speed, newdata = new_data)
        pred_accel <- predict(model_accel, newdata = new_data)
        
        # append predictions
        pred_max_speed_outputs <- tagAppendChild(pred_max_speed_outputs, 
                                                 div(paste("Make: ", make, " | Model: ", modle, " - Predicted Max Speed: ", round(pred_max_speed, 2), " km/h"))
        )
        pred_accel_outputs <- tagAppendChild(pred_accel_outputs, 
                                             div(paste("Make: ", make, " | Model: ", modle, " - Predicted Acceleration (0-100 km/h): ", round(pred_accel, 2), " s"))
        )
      }
    }
    
    # output the predictions
    output$predictions <- renderUI({
      tagList(
        h3("Predictions for Max Speed:"),
        pred_max_speed_outputs,
        h3("Predictions for Acceleration:"),
        pred_accel_outputs
      )
    })
    output$mse_max_speed <- renderText({ paste("MSE for Max Speed Model: ", round(mse_max_speed, 2)) })
    output$rmse_max_speed <- renderText({ paste("RMSE for Max Speed Model: ", round(rmse_max_speed, 2)) })
    output$mse_accel <- renderText({ paste("MSE for Acceleration Model: ", round(mse_accel, 2)) })
    output$rmse_accel <- renderText({ paste("RMSE for Acceleration Model: ", round(rmse_accel, 2)) })
  })
  
  # plot for max speed
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
  
  # plot for acceleration
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
