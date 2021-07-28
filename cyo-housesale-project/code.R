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

if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(Deriv)) install.packages("Deriv", repos = "http://cran.us.r-project.org")
if(!require(neuralnet)) install.packages("neuralnet", repos = "http://cran.us.r-project.org")

library(caret)
library(dslabs)
library(Deriv)
library(neuralnet)
library(xgboost)
library(tidyverse)

#Read data from CSV file
housedata <- read.csv("cyo-housesale-project/kc_house_data.csv", as.is = TRUE)

# Find number of unique values in every column
colNames <- colnames(housedata)
res <- sapply(colNames, function(c) {
   n <- n_distinct(housedata[[c]])
   #paste(c, '-', n)
}, simplify="array")
res

# Convert apt columns into factors
col_names_for_factor <- c('bedrooms' ,'bathrooms', "floors", "waterfront", "view", "condition", "grade", "zipcode")
housedata[,col_names_for_factor] <- lapply(housedata[,col_names_for_factor] , factor)
str(housedata)

# Create scatterplot of all the house price columns vs price
housedata %>%
   gather(-price, key = "var", value = "value") %>%
   ggplot(aes(x = value, y = price)) +
   geom_point() +
   stat_smooth() +
   facet_wrap(~ var, scales = "free") +
   theme_bw()

# Convert theme and caption to be used for plots
plot_theme <- theme(plot.caption = element_text(size = 7, face = "italic"), 
                    axis.title = element_text(size = 11))
caption_text <- "PH125.9x | Source: Kaggle KC House Data | Somnath Saha"

housedata %>% ggplot(aes(price)) +
   geom_histogram(binwidth = 1000000, color = I("white"), fill = "lightblue") +
   #scale_y_continuous(breaks = seq(0, 4500, 500)) +
   #scale_x_continuous(breaks = seq(0, 1500000, 100000), labels = sprintf("%sK", seq(0, 1500, 100))) +
   labs(x = "Price Range ($)", y = "count", caption = caption_text) 




housedataplot <- housedata %>% select(-id, -date, -zipcode)

#housedata %>% group_by(price > 1500000) %>% summarise(n = n())

housedataplot %>%
   gather(-price, key = "var", value = "value") %>%
   ggplot(aes(x = value, y = price)) +
   geom_point() +
   stat_smooth() +
   facet_wrap(~ var, scales = "free") +
   theme_bw()


cor(housedata$sqft_living, housedata$sqft_living15)
cor(housedata$sqft_lot, housedata$sqft_lot15)

housedata %>%
   ggplot(aes(x = sqft_living, y = sqft_living15)) +
   geom_point(color = "blue", alpha=0.5) +
   stat_smooth() +
   theme_bw()

housedata %>%
   ggplot(aes(x = sqft_lot15, y = sqft_lot)) +
   geom_point(color = "blue", alpha=0.5) +
   stat_smooth() +
   theme_bw()

# Distribution of all the different price of houses (Upto 8M)
housedata %>% ggplot(aes(price)) +
   geom_histogram(binwidth = 100000, color = I("blue"), fill = "skyblue") +
   scale_y_continuous(breaks = seq(0, 4500, 500)) +
   scale_x_continuous(breaks = seq(0, 8000000, 500000), labels = sprintf("%sM", seq(0, 8, 0.5))) +
   labs(x = "Price Range ($)", y = "Number of Houses", caption = caption_text) + plot_theme

# Distribution of the house prices within 1.5M 
housedata %>% filter(price < 1000000) %>% ggplot(aes(price)) +
   geom_histogram(binwidth = 100000, color = I("white"), fill = "lightblue") +
   scale_y_continuous(breaks = seq(0, 4500, 500)) +
   scale_x_continuous(breaks = seq(0, 1500000, 100000), labels = sprintf("%sK", seq(0, 1500, 100))) +
   labs(x = "Price Range ($)", y = "Number of Houses", caption = caption_text) + plot_theme

# Distribution of all sqft_living values of houses
housedata %>% filter(sqft_living < 10000) %>% ggplot(aes(sqft_living)) +
   geom_histogram(binwidth = 1000, color = I("blue"), fill = "skyblue") +
   #scale_y_continuous(breaks = seq(0, 4500, 500)) +
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

# Distribution of number of houses based on the following column values - 
# 'yr_built', 'yr_renovated'
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
housedata %>% filter(sqft_living < 10000) %>% ggplot(aes(x=sqft_living, y=price, group=sqft_living)) +
   geom_boxplot() +
   #scale_x_continuous(breaks = seq(0, 15, 1)) +
   scale_y_continuous(breaks = seq(0, 8000000, 1000000), labels = sprintf("%sM", seq(0, 8, 1))) +
   labs(x = "sqft_living", y = "Price of House ($)", caption = caption_text) + plot_theme

# Influence of number of sqft_lot on the price of house 
housedata %>% ggplot(aes(x=sqft_lot, y=price, group=sqft_lot)) +
   geom_boxplot() +
   scale_x_continuous(breaks = seq(0, 15, 1)) +
   scale_y_continuous(breaks = seq(0, 8000000, 1000000), labels = sprintf("%sM", seq(0, 8, 1))) +
   labs(x = "No of Bedrooms", y = "Price of House ($)", caption = caption_text) + plot_theme

#Predicting values of price based on other available parameters
#models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")
#lm, svmLinear


#Clean up data and remove outliers
housedata %>% filter(sqft_living < 10000) 
housedata %>% filter(bedrooms < 15)



hdata <- housedata %>% select(-id, -date, -sqft_living15, -sqft_lot15)
test_index <- createDataPartition(y = hdata$price, times = 1, p = 0.1, list = FALSE)
hdataTemp <- hdata[-test_index,]
validation <- hdata[test_index,]

test_index <- createDataPartition(y = hdataTemp$price, times = 1, p = 0.1, list = FALSE)
hdata_train <- hdataTemp[-test_index,]
hdata_test <- hdataTemp[test_index,]

rm(hdata, hdataTemp)

models <- c("lm", "glm", "gbm", "svmLinear", "knn", "gamLoess", "rf")

models2 <- c("xgbDART", "xgbLinear", "xgbTree", "elm", "neuralnet", "nnet", "pcaNNet")

hdata_train <- hdata_train %>% select(price, bedrooms, bathrooms, sqft_living, sqft_basement, sqft_above)
hdata_test <- hdata_test %>% select(price, bedrooms, bathrooms, sqft_living, sqft_basement, sqft_above)
validation <- validation %>% select(price, bedrooms, bathrooms, sqft_living, sqft_basement, sqft_above)

fits <- lapply(models, function(model){ 
   print(model)
   train(price ~ ., method = model, data = hdata_test)
}) 
pred <- sapply(fits, function(object) predict(object, newdata = validation))
acc <- colMeans(pred == validation$price)
RMSE(pred, validation$price)
acc

pred <- as.data.frame(pred)
RMSE <- function (x, test) sqrt(mean((x-test$price)^2))
#RMSE <- function (x, test) nrow(test)
x <- lapply(pred, FUN = RMSE, validation)

new_df <- as.data.frame(unlist(lapply(pred, FUN = RMSE, validation)), col.names=c("c","d"))

fit1 <- train(price ~ ., method = "lm", data = hdata_test)
fit1 <- train(price ~ ., method = "svmLinear", data = hdata_test)
fit1 <- train(price ~ ., method = "knn", data = hdata_test)
fit1 <- train(price ~ ., method = "gamLoess", data = hdata_test)
fit1 <- train(price ~ ., method = "rf", data = hdata_test)
fit1 <- train(price ~ ., method = "glm", data = hdata_test)

fit1 <- train(price ~ ., method = "xgbDART", data = hdata_test)
fit1 <- train(price ~ ., method = "xgbLinear", data = hdata_test)
fit1 <- train(price ~ ., method = "xgbTree", data = hdata_test)
fit1 <- train(price ~ ., method = "elm", data = hdata_test)
fit1 <- train(price ~ ., method = "neuralnet", data = hdata_test)
fit1 <- train(price ~ ., method = "nnet", data = hdata_test)
fit1 <- train(price ~ ., method = "pcaNNet", data = hdata_test)


pred1 <- predict(fit1, newdata = validation)
RMSE(pred1, validation$price)
acc1

df <- data.frame(pred1, validation$price)

names(fits) <- models
pred <- sapply(fits, function(object) predict(object, newdata = validation))

dim(pred)

acc <- colMeans(pred == mnist_27$test$y)
acc
mean(acc)

c <- 1

# price, bedrooms, bathrooms, sqft_living, sqft_basement, sqft_above
# floors, waterfront, view, condition, grade, bedrooms, bathrooms

## Model 6.0: Model with all features and regularization
#```{r, echo=TRUE, eval=TRUE}
# Regularise model, predict ratings and calculate RMSE for passed value of lambda
mu <- mean(hdata_train$price)
var1 <- "floors"
var2 <- "waterfront"
var3 <- "bedrooms"
var4 <- "bathrooms"
var5 <- "condition"
var6 <- "grade"

hdata_train <- hdata_train %>% select(price, floors, waterfront, bedrooms, bathrooms, condition)
hdata_test <- hdata_test %>% select(price, floors, waterfront, bedrooms, bathrooms, condition)
validation <- validation %>% select(price, floors, waterfront, bedrooms, bathrooms, condition)

train_predict_get_rmse <- function(l, trainSet, testSet)
{
   b_m   <- hdata_train %>%
      group_by(floors) %>%
      summarise(b_m = sum(price - mu)/(n()+l))
   b_u   <- hdata_train %>%
      left_join(b_m, by=var1) %>%
      group_by(waterfront) %>%
      summarise(b_u = sum(price - b_m - mu)/(n()+l))
   b_g   <-        hdata_train %>%
      left_join(b_m, by=var1) %>%
      left_join(b_u, by=var2) %>%
      group_by(bedrooms) %>%
      summarise(b_g = sum(price - b_m - b_u - mu)/(n()+l))
   b_yr_release <- hdata_train %>%
      left_join(b_m, by=var1) %>%
      left_join(b_u, by=var2) %>%
      left_join(b_g, by=var3) %>%
      group_by(bathrooms) %>%
      summarise(b_yr_release = sum(price - b_m - b_u - b_g - mu)/(n()+l))
   b_yr_review <-  hdata_train %>%
      left_join(b_m, by=var1) %>%
      left_join(b_u, by=var2) %>%
      left_join(b_g, by=var3) %>%
      left_join(b_yr_release, by=var4) %>%
      group_by(condition) %>%
      summarise(b_yr_review = sum(price - b_m - b_u - b_g - mu)/(n()+l))
   
   predicted_ratings <-  hdata_test %>%
      left_join(b_m, by=var1) %>%
      left_join(b_u, by=var2) %>%
      left_join(b_g, by=var3) %>%
      left_join(b_yr_release, by=var4) %>%
      left_join(b_yr_review, by=var5) %>%
      mutate(pred = mu + b_m + b_u + b_g + b_yr_release + b_yr_review) %>%
      pull(pred)
   
   return (RMSE(predicted_ratings, hdata_test$price))
}
# Generate a sequence of values for lambda ranging from 4 to 6 with 0.1 increments
inc <- 100
lambdas <- seq(1, 10000, inc)

x <- train_predict_get_rmse(10, hdata_train, hdata_test)

# Get RMSE values for all the lambdas
rmses <- sapply(lambdas, function(l) {  train_predict_get_rmse(l, hdata_train, hdata_test) })
qplot(lambdas, rmses)

# Assign optimal tuning parameter (lambda)
optimal_lambda <- lambdas[which.min(rmses)]

# Minimum RMSE achieved
regularised_rmse <- min(rmses) 

#Reference :
#   https://www.tutorialspoint.com/r/r_boxplots.htm










