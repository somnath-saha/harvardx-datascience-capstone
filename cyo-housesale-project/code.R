# House Sales in King County, USA
# This dataset contains house sale prices for King County, which includes Seattle. 
# It includes homes sold between May 2014 and May 2015.
# 
# Details of the columns
# id: house id
# date: date that the house was bought.
# price: house price
# bedrooms: number of bedroom
# bathrooms: number of bathroom
# sqft_living: Square foot Living
# sqft_lot: Square foot Lot
# floors: Number of floor
# waterfront: waterfront
# view: Number of view
# condition: condition
# grade: grade
# sqft_above: Square foot above
# sqft_basement: Square foot basement
# yr_built: year that house was built
# yr_renovated: year that house was renovated
# zipcode: zipcode
# lat: latitude
# long: longitude
# sqft_living15: Square foot Living in 2015
# sqft_lot15: Square foot Lot in 2015

# ========================================================================================
# Install and load required libraries
# ========================================================================================

if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(Deriv)) install.packages("Deriv", repos = "http://cran.us.r-project.org")
if(!require(neuralnet)) install.packages("neuralnet", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("neuralnet", repos = "http://cran.us.r-project.org")

library(caret)
library(dslabs)
library(Deriv)
library(neuralnet)
library(xgboost)
library(tidyverse)
library(gbm)

# Read data from CSV file
housedata <- read.csv("cyo-housesale-project/kc_house_data.csv", as.is = TRUE)

# ========================================================================================
# DATA INSIGHTS
# ========================================================================================

# Find number of unique values in every column
colNames <- colnames(housedata)
sapply(colNames, function(c) {
   n <- n_distinct(housedata[[c]])
}, simplify="array")

# Theme and caption to be used for plots
plot_theme <- theme(plot.caption = element_text(size = 7, face = "italic"), 
                    axis.title = element_text(size = 11))
caption_text <- "PH125.9x | Source: Kaggle KC House Data | Somnath Saha"

# Create scatterplot of all the features vs price
housedata %>%
   gather(-price, key = "var", value = "value") %>%
   ggplot(aes(x = value, y = price)) +
   geom_point(color = "blue", alpha=0.25) +
   stat_smooth() +
   facet_wrap(~ var, scales = "free") +
   theme_bw()

# Distribution of all the different price of houses (Upto 8M)
housedata %>% ggplot(aes(price)) +
   geom_histogram(binwidth = 100000, color = I("blue"), fill = "skyblue") +
   scale_y_continuous(breaks = seq(0, 4500, 500)) +
   scale_x_continuous(breaks = seq(0, 8000000, 500000), labels = sprintf("%sM", seq(0, 8, 0.5))) +
   labs(x = "Price Range ($)", y = "Number of Houses", caption = caption_text) + plot_theme

# Distribution of the house prices within 1.5M 
housedata %>% filter(price < 1000000) %>% ggplot(aes(price)) +
   geom_histogram(binwidth = 100000, color = I("blue"), fill = "skyblue") +
   scale_y_continuous(breaks = seq(0, 4500, 500)) +
   scale_x_continuous(breaks = seq(0, 1500000, 100000), labels = sprintf("%sK", seq(0, 1500, 100))) +
   labs(x = "Price Range ($)", y = "Number of Houses", caption = caption_text) + plot_theme

# Distribution of all sqft_living values of houses
housedata %>% filter(sqft_living < 10000) %>% ggplot(aes(sqft_living)) +
   geom_histogram(binwidth = 1000, color = I("blue"), fill = "skyblue") +
   scale_x_continuous(breaks = seq(0, 10000, 1000), labels = seq(0, 10000, 1000)) +
   labs(x = "Living Room Size", y = "Count", caption = caption_text) + plot_theme

# Distribution of all sqft_lot values of houses
housedata %>% filter(sqft_lot < 100000) %>% 
   ggplot(aes(sqft_lot)) +
   geom_histogram(binwidth = 10000, color = I("blue"), fill = "skyblue") +
   scale_y_continuous(breaks = seq(0, 20000, 1000)) +
   scale_x_continuous(breaks = seq(0, 100000, 5000), labels = sprintf("%s", seq(0, 10, 0.5))) +
   labs(x = "sqft_lot (In 1000 sqft)", y = "Count", caption = caption_text) + plot_theme

# Distribution of number of houses based on the following column values - 
# 'floors', 'waterfront', 'view', 'condition', 'grade'
grp_col_names <- c('floors', 'waterfront', 'view', 'condition', 'grade')
lapply(grp_col_names, function(c) {
   res <- housedata %>% group_by(.dots = c) %>%  
      summarise('Count' = n(), Percentage = n() * 100 / nrow(housedata)) %>%
      arrange(desc(Percentage))
   res %>% ggplot(aes_string(x=c, y='Count')) + 
      geom_bar(stat = "identity", color = I("white"), fill = "skyblue") +
      geom_text(aes_string(label='Count'), position=position_dodge(width=0.75), vjust=-0.3) +
      labs(x = c, y = "Count", caption = caption_text) + plot_theme
})

# Distribution of number of houses based on the following column values - 'yr_built', 'yr_renovated'
grp_col_names_yr <- c('yr_built', 'yr_renovated')
lapply(grp_col_names_yr, function(c) {
   res <- housedata %>% group_by(.dots = c) %>%  
      summarise('Count' = n(), Percentage = n() * 100 / nrow(housedata)) %>%
      arrange(desc(Percentage)) %>% top_n(10)
   res
})

# Influence of number of bedrooms on the price of house 
housedata %>% filter(bedrooms < 15) %>% ggplot(aes(x=bedrooms, y=price, group=bedrooms)) +
   geom_boxplot() +
   scale_x_continuous(breaks = seq(0, 15, 1)) +
   scale_y_continuous(breaks = seq(0, 8000000, 1000000), labels = sprintf("%sM", seq(0, 8, 1))) +
   labs(x = "No of Bedrooms", y = "Price of House ($)", caption = caption_text) + plot_theme

# Influence of number of bathrooms on the price of house 
housedata %>% ggplot(aes(x=bathrooms, y=price, group=bathrooms)) +
   geom_boxplot() +
   scale_x_continuous(breaks = seq(0, 15, 1)) +
   scale_y_continuous(breaks = seq(0, 8000000, 1000000), labels = sprintf("%sM", seq(0, 8, 1))) +
   labs(x = "No of Bathrooms", y = "Price of House ($)", caption = caption_text) + plot_theme

# Influence of number of sqft_living on the price of house 
housedata %>% filter(sqft_living < 7500) %>% ggplot(aes(x=sqft_living, y=price, group=sqft_living)) +
   geom_boxplot() +
   scale_y_continuous(breaks = seq(0, 8000000, 1000000), labels = sprintf("%sM", seq(0, 8, 1))) +
   labs(x = "sqft_living", y = "Price of House ($)", caption = caption_text) + plot_theme

# Influence of number of sqft_lot on the price of house 
housedata %>% ggplot(aes(x=sqft_lot, y=price, group=sqft_lot)) +
   geom_boxplot() +
   scale_x_continuous(breaks = seq(0, 15, 1)) +
   scale_y_continuous(breaks = seq(0, 8000000, 1000000), labels = sprintf("%sM", seq(0, 8, 1))) +
   labs(x = "sqft_lot", y = "Price of House ($)", caption = caption_text) + plot_theme

# Influence of number of floors on the price of house 
housedata %>% ggplot(aes(x=floors, y=price, group=floors)) +
   geom_boxplot() +
   scale_x_continuous(breaks = seq(0, 15, 1)) +
   scale_y_continuous(breaks = seq(0, 8000000, 1000000), labels = sprintf("%sM", seq(0, 8, 1))) +
   labs(x = "floors", y = "Price of House ($)", caption = caption_text) + plot_theme

# Influence of number of waterfront on the price of house 
housedata %>% ggplot(aes(x=waterfront, y=price, group=waterfront)) +
   geom_boxplot() +
   scale_x_continuous(breaks = seq(0, 15, 1)) +
   scale_y_continuous(breaks = seq(0, 8000000, 1000000), labels = sprintf("%sM", seq(0, 8, 1))) +
   labs(x = "waterfront", y = "Price of House ($)", caption = caption_text) + plot_theme

# Influence of view on the price of house 
housedata %>% ggplot(aes(x=view, y=price, group=view)) +
   geom_boxplot() +
   scale_x_continuous(breaks = seq(0, 15, 1)) +
   scale_y_continuous(breaks = seq(0, 8000000, 1000000), labels = sprintf("%sM", seq(0, 8, 1))) +
   labs(x = "view", y = "Price of House ($)", caption = caption_text) + plot_theme

# Influence of condition on the price of house 
housedata %>% ggplot(aes(x=condition, y=price, group=condition)) +
   geom_boxplot() +
   scale_x_continuous(breaks = seq(0, 15, 1)) +
   scale_y_continuous(breaks = seq(0, 8000000, 1000000), labels = sprintf("%sM", seq(0, 8, 1))) +
   labs(x = "condition", y = "Price of House ($)", caption = caption_text) + plot_theme

# Influence of grade on the price of house 
housedata %>% ggplot(aes(x=grade, y=price, group=grade)) +
   geom_boxplot() +
   scale_x_continuous(breaks = seq(0, 15, 1)) +
   scale_y_continuous(breaks = seq(0, 8000000, 1000000), labels = sprintf("%sM", seq(0, 8, 1))) +
   labs(x = "grade", y = "Price of House ($)", caption = caption_text) + plot_theme

# Study sqft_living vs. sqft_living15
cor(housedata$sqft_living, housedata$sqft_living15)
housedata %>%
   ggplot(aes(x = sqft_living, y = sqft_living15)) +
   geom_point(color = "blue", alpha=0.25) +
   stat_smooth() +
   theme_bw()

# Study sqft_lot vs. sqft_lot15
cor(housedata$sqft_lot, housedata$sqft_lot15)
housedata %>%
   ggplot(aes(x = sqft_lot15, y = sqft_lot)) +
   geom_point(color = "blue", alpha=0.5) +
   stat_smooth() +
   theme_bw()

# ========================================================================================
# Analyze and train different models to predict house price
# ========================================================================================

# Report the outliers as observed from data visualization
housedata %>% filter(sqft_living < 7500) %>% summarise(n = n(), percent = n / nrow(housedata))
housedata %>% filter(bedrooms < 15) %>% summarise(n = n(), percent = n / nrow(housedata)) 

# Drop the columns not required for training
housedata <- housedata %>% select(-id, -date, -sqft_living15, -sqft_lot15)

# Cut the continuous data values for the different areas in sqft
housedata <- housedata %>% filter(sqft_living < 7500 && bedrooms < 15) %>% 
   mutate(sqft_living = as.numeric(cut(sqft_living, 100)), sqft_lot = as.numeric(cut(sqft_lot, 1000)),
          sqft_above = as.numeric(cut(sqft_above, 100)), sqft_basement = as.numeric(cut(sqft_basement, 100)),
          lat = as.numeric(cut(lat, 1000)), long = as.numeric(cut(long, 100)))

# Convert necessary columns into factors
col_names_for_factor <- c('bedrooms' ,'bathrooms', "floors", "waterfront", "view", "condition", "grade", "zipcode")
housedata[,col_names_for_factor] <- lapply(housedata[,col_names_for_factor] , factor)

# Convert necessary columns into factors
str(housedata)

# Divide dataset into training and validation dataset
test_index <- createDataPartition(y = housedata$price, times = 1, p = 0.1, list = FALSE)
hdata_train <- housedata[-test_index,]
hdata_validation <- housedata[test_index,]

# Define the RMSE function to be used with the models
RMSE <- function (x, test) sqrt(mean((x-test)^2))

# Naive model of mean value of prices as prediction value
mu <- mean(hdata_train$price)
mean_model_rmse <- RMSE(mu, hdata_validation$price)

# Caret library to train better models
# 1.Linear Regression 
# 2.Generalized Linear Model 
# 3.Stochastic Gradient Boosting 
# 4.Generalized Additive Model using LOESS 
models <- c("lm", "glm", "gbm", "gamLoess")
fits <- lapply(models, function(model) { 
   print(model)
   train(price ~ ., method = model, data = hdata_train)
}) 

# Predict values on the validation set
pred <- sapply(fits, function(object) predict(object, newdata = hdata_validation))

# Define and find the RMSE values for the new models
res <- as.data.frame(lapply(as.data.frame(pred), FUN = RMSE, hdata_validation$price))
colnames(res) <- c("Linear Regression Model", "Generalized Linear Model", "Stochastic Gradient Boosting", "Gen Additive Model using LOESS")

# Comparison with a naive model of predicting value with just the mean of all prices
res["Mean Value Model"] = mean_model_rmse
improvement_percentage <- ((mean_model_rmse - min(res[1,])) * 100 )/mean_model_rmse
   
# #Advanced models to be executed on more powerful machines
# models2 <- c("knn", svmLinear", "rf", "xgbDART", "xgbLinear", "xgbTree", "elm", "neuralnet", "nnet", "pcaNNet")
# fits2 <- lapply(models2, function(model){ 
#    print(model)
#    train(price ~ ., method = model, data = hdata_train)
# }) 
# pred <- sapply(fits, function(object) predict(object, newdata = validation))
# RMSE <- function (x, test) sqrt(mean((x-test$price)^2))
# adv_res <- lapply(pred, FUN = RMSE, validation)

# References :
# https://en.wikipedia.org/wiki/Kaggle
# https://www.kaggle.com
# https://www.tutorialspoint.com/r/r_boxplots.htm