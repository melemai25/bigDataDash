library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(glmnet)
library(data.table)
library(tree)
library(ggcorrplot)

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

# Convert character columns to factors in cdata
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
#not sure what I was doing tbh
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


# Gets rid of useless columns and rows
attach(clean_data)
clean_data <- clean_data %>%
  select(where(~ mean(is.na(.)) <= 0.3))


clean_data<- clean_data[rowSums(is.na(clean_data)) <= 1, ]

incomplete_clean_data <- clean_data
clean_data<- clean_data[rowSums(is.na(clean_data)) <= 0, ]
plot(clean_data$max_speed_km_per_h, clean_data$engine_hp, main = "max speed vs engine hp", xlab = "engine hp", ylab = "max speed km/h")


#model1
#par(mar = c(0.1, 0.1, 0.1, 0.1))
set.seed(1)
train = sample(1:nrow(clean_data), nrow(clean_data)/2)
tree.car=tree(max_speed_km_per_h ~., clean_data,subset=train)

summary(tree.car)
plot(tree.car)
text(tree.car,pretty=0)

# see if pruning gives us a better tree
cv.car=cv.tree(tree.car)
plot(cv.car$size,cv.car$dev,type='b')
cv.car

# make predictions using the tree
yhat=predict(tree.car,newdata=clean_data[-train,])
car.test=clean_data[-train,"max_speed_km_per_h"]
plot(yhat,car.test)
abline(0,1)
mean((yhat-car.test)^2)

# make a linear regression model using lstat, rm, and dis
mod=lm(max_speed_km_per_h ~ acceleration_0_100_km.h_s+ minimum_trunk_capacity_l + height_mm + engine_hp + body_type
       , data=clean_data, subset=train)
yhat2=predict(mod,newdata=clean_data[-train,])
plot(yhat2,car.test)
abline(0,1)
mean((yhat2-car.test)^2)

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
# # Shiny UI
# ui <- fluidPage(
#   titlePanel("Max Speed Prediction"),
#   sidebarLayout(
#     sidebarPanel(
#       sliderInput(inputId = "year_range", 
#                   label = "Select Year Range:",
#                   min = 1990,
#                   max = 2024,
#                   value = c(2010, 2020),
#                   step = 1),
#       hr(),
#       verbatimTextOutput("selected_range"),
#       
#       sliderInput("tree_depth",
#                   "Select Tree Depth:",
#                   min = 2,
#                   max = 6,
#                   value = 6)
#       
#       
#       
#     ),
#     mainPanel(
#       plotOutput("plot")
# 
#     )
#   )
# )
# 
# 
# # Shiny server
# server <- function(input, output) {
#   
#   selected_data <- reactive({
#     # Get selected year range from the input
#     year_start <- input$year_range[1]
#     year_end <- input$year_range[2]
#     
#     # Filter the dataset based on the year range
#     selected_data <- clean_data %>%
#       filter(
#         (year_from >= year_start) & (year_from <= year_end)    # Ensure year_to is within or after the selected start year
#       )
#     
#     return(selected_data)
#   })
#   
#   tree.carr <- reactive({
#     # Use 'maxdepth' argument in the tree function directly
#     tree_model <- tree(
#       max_speed_km_per_h ~ ., main="Max Speed KM Decision Tree",
#       data = selected_data(), 
#       subset = train, 
#       control = tree.control(nobs = nrow(selected_data()))
#     )
#     
#     # Limit the depth of the tree using tree pruning
#     tree_model_pruned <- prune.tree(tree_model, best = input$tree_depth)
#     
#     return(tree_model_pruned)
#   })
#   
#   output$plot <- renderPlot({
#     plot(tree.carr())
#     labels <- tree.carr()
#     text(tree.carr(), use.n = TRUE, cex = 0.8)
#   })
#   
# }
#   
# 
# #Run the application
# shinyApp(ui = ui, server = server)
ui <- fluidPage(
  titlePanel("Car Specifications Correlation"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year_range", 
                  label = "Select Year Range:",
                  min = 1990,
                  max = 2020,
                  value = c(2000, 2010),
                  step = 1),
      selectInput("make_filter", 
                  "Select Car Make:", 
                  choices = c("All", unique(clean_data$make)),
                  selected = "All")
    ),
    
    mainPanel(
      plotOutput("correlation_plot")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive expression to filter data based on input sliders and selections
  selected_data <- reactive({
    # Start with full dataset
    data <- clean_data
    
    # Filter by year range
    data <- data %>%
      filter(year_from >= input$year_range[1] & year_to <= input$year_range[2])
    
    # Filter by car make (if selected)
    if (input$make_filter != "All") {
      data <- data %>%
        filter(make == input$make_filter)
    }
    
    return(data)
  })
  
  # Render the correlation plot
  output$correlation_plot <- renderPlot({
    corr_data <- selected_data() %>%
      select(engine_hp, max_speed_km_per_h, range_km, curb_weight_kg, cargo_volume_m3, city_fuel_per_100km_l, acceleration_0_100_km.h_s, wheelbase_mm, width_mm, height_mm, wheel_size_r14)
    
    # Calculate the correlation matrix
    corr_matrix <- cor(corr_data, use = "complete.obs")
    
    # Generate the correlation plot
    ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower", lab = TRUE)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)