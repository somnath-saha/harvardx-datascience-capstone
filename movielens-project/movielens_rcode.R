
# title: "PH125.9x Capstone Project 1: MovieLens"
# author: "Somnath Saha"
# date: "12/06/2021"


# Ready the data
   
## Fetch the data
   
# The following code is made available by the course to fetch the data from the grouplens website and extract a working dataset (edx) and a final validation dataset (validation) out of the dataset.

# Install any missing packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

# Library loading for the report 
library(lubridate)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>%
   semi_join(edx, by = "movieId") %>%
   semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


## Cleanup the data for more valuable features

#A re-usable function is defined to extract and add new columns for release and review years from already available data. The edX dataset is processed here. The validation dataset will also be mutated to add the release and review years after the final model is developed.
add_release_and_review_years <- function(dataset) {
   # Trim and split title (year) column into title and year columns
   dataset <- dataset %>% mutate(title = str_trim(title)) %>%
      extract(title, c("title_temp", "release_year"), 
              regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = F) %>%
      mutate(release_year = if_else((str_length((release_year)) > 4), 
                                    as.integer(str_split(release_year, "-", simplify = T)[1]), 
                                    as.integer(release_year))) %>%
      mutate(title = if_else(is.na(title_temp), title, title_temp)) %>%
      select(-title_temp)
   
   # Convert timestamp column into date format, removing time data & get review year from it
   dataset <- dataset %>% mutate(review_date = round_date(as_datetime(timestamp), unit = "week"))
   dataset <- dataset %>% mutate(review_year = year(review_date))
   dataset
}

# Add the release and review years to edx set as well
edx <- add_release_and_review_years(edx)

# ===============================================================================================================================================

# Data analysis and visualization

### Structure of edx dataset: Column Names & Types
str(edx)

### Data in edx dataset: First few rows
head(edx)

# Create plot theme to apply to ggplot2 element text throughout report
plot_theme <- theme(plot.caption = element_text(size = 7, face = "italic"), 
                    axis.title = element_text(size = 11))

caption_text <- "PH125.9x | Source: edx 10M MovieLens | Somnath Saha"

## Plot distribution of ratings in the edx dataset
edx %>% ggplot(aes(rating)) +
   geom_histogram(binwidth = 0.2, color = I("white"), fill = "skyblue") +
   scale_y_continuous(breaks = seq(0, 3000000, 500000), labels = sprintf("%sM", seq(0, 3, 0.5))) +
   scale_x_continuous(breaks = seq(0, 5, 0.5)) +
   labs(x = "Rating", y = "Count of Ratings", caption = caption_text) + plot_theme

## Plot average rating by movie in the edx dataset
edx %>% group_by(movieId) %>%
   summarise(ave_rating = sum(rating)/n()) %>%
   ggplot(aes(ave_rating)) +
   geom_histogram(bins=30, color = I("white"), fill = "pink") +
   labs(x = "Average rating", y = "Number of movies", caption = caption_text) + plot_theme

## Separate individual genres and ranking them by the total number of ratings in the edx dataset
edx %>% separate_rows(genres, sep = "\\|") %>%
   group_by(genres) %>%
   summarise("No. of Ratings" = n(), "Average Rating" = round(mean(rating), 3)) %>%
   arrange(desc("No. of Ratings")) %>% knitr::kable(caption = "Individual genres & their number of rankings")

## Plot average rating by genre for genre combinations with at least 50,000 ratings
edx %>% group_by(genres) %>%
   summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
   filter(n >= 50000) %>% 
   mutate(genres = reorder(genres, avg)) %>%
   ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
   geom_point() +
   geom_errorbar() + 
   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
   labs(x = "Genre combination", y = "Average Rating", caption = caption_text) + plot_theme

## Group and list top 10 movie titles based on number of ratings
edx %>% group_by(title) %>%
   summarise(n = n(), mu = mean(rating)) %>%
   slice_max(n, n=10) %>%
   select("Movie Name" = title, "No. of Ratings" = n, "Avg Rating" = mu) %>% 
   knitr::kable(caption = "Top 10 movie titles based on number of ratings")

## Plot average rating by year of release in the edx dataset
edx %>% group_by(release_year) %>%
   summarise(rating = mean(rating)) %>%
   ggplot(aes(release_year, rating)) +
   geom_point() +
   geom_smooth() +
   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
   labs(x = "Release Year", y = "Average Rating", caption = caption_text) + plot_theme

## Plot number of ratings by year of release in the edx dataset
edx %>% group_by(release_year) %>%
   summarise(count = n()) %>%
   ggplot(aes(release_year, count)) +
   geom_line() +
   scale_y_continuous(breaks = seq(0, 800000, 200000), labels = sprintf("%sK", seq(0, 800, 200))) +
   labs(x = "Release Year", y = "Number of Ratings (in 1000s)", caption = caption_text)

## Plot average rating by date of review in the edx dataset
edx %>% group_by(review_date) %>%
   summarize(rating = mean(rating)) %>%
   ggplot(aes(review_date, rating)) +
   geom_point() +
   geom_smooth() +
   labs(x = "Review Date", y = "Average Rating", caption = caption_text)

## Plot average rating by year of review in the edx dataset
edx %>% group_by(review_year) %>%
   summarize(rating = mean(rating)) %>%
   ggplot(aes(review_year, rating, label = round(rating, 2))) +
   scale_x_continuous("Year of Review", breaks = seq(1990, 2010, 1)) +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
   geom_point() +
   geom_text(size = 3, hjust = 1, vjust = 2) +
   geom_smooth() +
   labs(x = "Year of Review", y = "Average Rating", caption = caption_text)

# Creation of training and test datasets
   
set.seed(27, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index,]
edx_test <- edx[test_index,]

# Keep only those rows in edx_test and validation set which have 
# movieId and userId existing in edx_train dataset
edx_test <- edx_test %>% 
   semi_join(edx_train, by = "movieId") %>%
   semi_join(edx_train, by = "userId")

validation <- validation %>% 
   semi_join(edx_train, by = "movieId") %>%
   semi_join(edx_train, by = "userId")


str(edx_train)
str(edx_test)

RMSE <- function(actual_data, predicted_data){
   sqrt(mean((actual_data - predicted_data)^2))
}

# ===============================================================================================================================================
# Development of different models and their performance
# ===============================================================================================================================================

## Model 1.0: Simple mean of training set

# Get the mean rating as prediction value
mu <- mean(edx_train$rating)
# Get the RMSE for test set using mu as prediction value
rmse_model1 <- RMSE(edx_test$rating, mu)

rmse_table <- data_frame(method = "Model 1.0: Simple mean of training set", RMSE = rmse_model1)
rmse_table %>% knitr::kable(caption = "Simple Mean based Model")

# ---------------------------------------------------------------------------------------------------
## Model 2.x:  Models with single effects

# ---------------------------------------------------------------------------------------------------
### Model 2.1:  Mean with movie effect
#Train on the edx training set to find movie effect
movie_effects     <-  edx_train %>% 
   group_by(movieId) %>% 
   summarize(b_m = mean(rating - mu))

#Generate predictions on the edx test set
predicted_ratings <-  mu + (edx_test %>% 
                               left_join(movie_effects, by='movieId') %>%
                               .$b_m)

#Find RMSE for this model
rmse_model_b_m <- RMSE(edx_test$rating, predicted_ratings)

#Add the calculated RMSE to the RMSE Table
captionTableType1 <- "RMSE Values for 2.x Model series"
rmse_table <- bind_rows(rmse_table,
                        data_frame(method="Model 2.1: Mean with movie effect",
                                   RMSE = rmse_model_b_m ))
rmse_table %>% knitr::kable(caption = captionTableType1)
# ---------------------------------------------------------------------------------------------------


### Model 2.2:  Mean with user effect
#Train on the edx training set to find user effect
user_effects <-     edx_train %>% 
   group_by(userId) %>% 
   summarize(b_u = mean(rating - mu))

#Generate predictions on the edx test set
predicted_ratings <-  mu + (edx_test %>% 
                               left_join(user_effects, by='userId') %>%
                               .$b_u)

#Find RMSE for this model
rmse_model_b_u <- RMSE(edx_test$rating, predicted_ratings)

#Add the new RMSE to the RMSE Table
rmse_table <- bind_rows(rmse_table,
                        data_frame(method="Model 2.2: Mean with user effect",
                                   RMSE = rmse_model_b_u ))
rmse_table %>% knitr::kable(caption = captionTableType1)

# ---------------------------------------------------------------------------------------------------

### Model 2.3: Mean with genre effect
#Train on the edx training set to find genres effect
genre_effects   <-  edx_train %>%
   group_by(genres) %>%
   summarize(b_g = mean(rating - mu))

#Generate predictions on the edx test set
predicted_ratings <-  mu + (edx_test %>% 
                               left_join(genre_effects, by = 'genres') %>%
                               .$b_g)

#Find RMSE for this model
rmse_model_b_g <- RMSE(edx_test$rating, predicted_ratings)

#Add the RMSE to the RMSE Table
rmse_table <- bind_rows(rmse_table,
                        data_frame(method = "Model 2.3: Mean with genre effect",
                                   RMSE = rmse_model_b_g ))
rmse_table %>% knitr::kable(caption = captionTableType1)

# ---------------------------------------------------------------------------------------------------

### Model 2.4:  Mean with release year effect
#Train on the edx training set to find user effect
release_yr_effects <- edx_train %>% 
   group_by(release_year) %>% 
   summarize(b_yr_release = mean(rating - mu))

#Generate predictions on the edx test set
predicted_ratings <-  mu + (edx_test %>% 
                               left_join(release_yr_effects, by='release_year') %>%
                               .$b_yr_release)

#Find RMSE for this model
rmse_model_b_yr_release <- RMSE(edx_test$rating, predicted_ratings)

#Add the RMSE to the RMSE Table
rmse_table <- bind_rows(rmse_table,
                        data_frame(method="Model 2.4: Mean with release year effect",
                                   RMSE = rmse_model_b_yr_release ))
rmse_table %>% knitr::kable(caption = captionTableType1)

# ---------------------------------------------------------------------------------------------------

### Model 2.5:  Mean with review year effect
#Train on the edx training set to find user effect
review_yr_effects <-  edx_train %>% 
   group_by(review_year) %>% 
   summarize(b_yr_review = mean(rating - mu))

#Generate predictions on the edx test set
predicted_ratings <-  mu + (edx_test %>% 
                               left_join(review_yr_effects, by='review_year') %>%
                               .$b_yr_review)

#Find RMSE for this model
rmse_model_b_yr_review <- RMSE(edx_test$rating, predicted_ratings)

#Add the RMSE to the RMSE Table
rmse_table <- bind_rows(rmse_table,
                        data_frame(method="Model 2.5: Mean with review year effect",
                                   RMSE = rmse_model_b_yr_review ))
rmse_table %>% knitr::kable(caption = captionTableType1)

# ---------------------------------------------------------------------------------------------------

## Model 3.x:  Study of models involving two effects in different forms
# ---------------------------------------------------------------------------------------------------

### Model 3.1:  Mean with movie and user effect taken independently

# Combine Model 2.1 & Model 2.2
# Generate predictions on the edx test set
predicted_ratings <-  edx_test %>% 
   left_join(movie_effects, by='movieId') %>%
   left_join(user_effects, by='userId') %>%
   mutate(pred = mu + b_m + b_u) %>%
   .$pred

# Find RMSE for this model
rmse_model_3_1 <- RMSE(edx_test$rating, predicted_ratings)

# Add the new RMSE to the RMSE Table
captionTableType2 <- "RMSE Values for 3.x Model series"
rmse_table <- bind_rows(rmse_table,
                        data_frame(method="Model 3.1: Mean + Movie + User Effect",
                                   RMSE = rmse_model_3_1 ))
rmse_table %>% knitr::kable(caption = captionTableType2)

# ---------------------------------------------------------------------------------------------------

### Model 3.2:  Mean with movie and cumulative user effect
# Add user effect to Model 2.1 (Movie effects model)
user_effects_comb <-    edx_train %>% 
   left_join(movie_effects, by='movieId') %>%
   group_by(userId) %>% 
   summarize(b_u = mean(rating - mu - b_m))

# Generate predictions on the edx test set
predicted_ratings <-  edx_test %>% 
   left_join(movie_effects, by='movieId') %>%
   left_join(user_effects_comb, by='userId') %>%
   mutate(pred = mu + b_m + b_u) %>%
   .$pred

# Find RMSE for this model
rmse_model_3_2 <- RMSE(edx_test$rating, predicted_ratings)

#Add the new RMSE to the RMSE Table
rmse_table <- bind_rows(rmse_table,
                        data_frame(method="Model 3.2: Mean + Movie + Cum. User Effect",
                                   RMSE = rmse_model_3_2 ))
rmse_table %>% knitr::kable(caption = captionTableType2)

# ---------------------------------------------------------------------------------------------------

### Model 3.3:  Mean with user and cumulative movie effect
#Train on the edx training set to find movie effect including user effect
combined_user_movie_effects <-    edx_train %>% 
   left_join(user_effects, by='userId') %>%
   group_by(movieId) %>% 
   summarize(b_m = mean(rating - mu - b_u))

#Generate predictions on the edx test set
predicted_ratings <-  edx_test %>% 
   left_join(user_effects, by='userId') %>%
   left_join(combined_user_movie_effects, by='movieId') %>%
   mutate(pred = mu + b_m + b_u) %>%
   .$pred

#Find RMSE for this model
rmse_model_3_3 <- RMSE(edx_test$rating, predicted_ratings)

#Add the new RMSE to the RMSE Table
rmse_table <- bind_rows(rmse_table,
                        data_frame(method="Model 3.3: Mean + User + Cum. Movie Effect",
                                   RMSE = rmse_model_3_3 ))
rmse_table %>% knitr::kable(caption = captionTableType2)

# ---------------------------------------------------------------------------------------------------

## Model 4.x: Towards the model involving all the effects

### Model 4.1:  Model of movie, user, genre effects in respective order
# Add genre effect to Model 3.2 that already includes movie and user effects
genre_effects_comb <- edx_train %>%
   left_join(movie_effects, by = "movieId") %>%
   left_join(user_effects_comb, by = "userId") %>%
   group_by(genres) %>%
   summarise(b_g = mean(rating - mu - b_m - b_u))

# Predict ratings adjusting for movie, user and genre effects
predicted_ratings <-  edx_test %>%
   left_join(movie_effects, by = "movieId") %>%
   left_join(user_effects_comb, by = "userId") %>%
   left_join(genre_effects_comb, by = "genres") %>%
   mutate(pred = mu + b_m + b_u + b_g) %>%
   pull(pred)

# Calculate RMSE based on genre effects model
rmse_model_4_1 <- RMSE(predicted_ratings, edx_test$rating)

#Add the new RMSE to the RMSE Table
captionTableType4 <- "RMSE Values for 4.x Model series"
rmse_table <- bind_rows(rmse_table,
                        data_frame(method="Model 4.1: Mean + Movie + User + Genre Effect",
                                   RMSE = rmse_model_4_1 ))
rmse_table %>% knitr::kable(caption = captionTableType4)

### Model 4.2:  Model of movie, user, genre & release year effects in respective order 
# Estimate release year effect (b_yr_release)
release_year_effects_comb <-  edx_train %>%
   left_join(movie_effects, by = "movieId") %>%
   left_join(user_effects_comb, by = "userId") %>%
   left_join(genre_effects_comb, by = "genres") %>%
   group_by(release_year) %>%
   summarise(b_yr_release = mean(rating - mu - b_m - b_u - b_g))

# Predict ratings adjusting for movie, user, genre and year effects
predicted_ratings <-  edx_test %>%
   left_join(movie_effects, by = "movieId") %>%
   left_join(user_effects_comb, by = "userId") %>%
   left_join(genre_effects_comb, by = "genres") %>%
   left_join(release_year_effects_comb, by = "release_year") %>%
   mutate(pred = mu + b_m + b_u + b_g + b_yr_release) %>%
   pull(pred)

# Calculate RMSE based on year effects model
rmse_model_4_2 <- RMSE(predicted_ratings, edx_test$rating)

#Add the new RMSE to the RMSE Table
rmse_table <- bind_rows(rmse_table,
                        data_frame(method="Model 4.2: Mean + Genre + Movie + User + Release Yr Effect",
                                   RMSE = rmse_model_4_2 ))
rmse_table %>% knitr::kable(caption = captionTableType4)

### Model 4.3:  Model of movie, user, genre effects, release year & review year effects in respective order

# Estimate review date effect (b_yr_review)
review_yr_effects_comb <- edx_train %>%
   left_join(movie_effects, by = "movieId") %>%
   left_join(user_effects_comb, by = "userId") %>%
   left_join(genre_effects_comb, by = "genres") %>%
   left_join(release_year_effects_comb, by = "release_year") %>%
   group_by(review_year) %>%
   summarise(b_yr_review = mean(rating - mu - b_m - 
                                   b_u - b_g - b_yr_release))

# Predict ratings adjusting for movie, user, genre, year and review date effects
predicted_b_yr_review <- edx_test %>%
   left_join(movie_effects, by = "movieId") %>%
   left_join(user_effects_comb, by = "userId") %>%
   left_join(genre_effects_comb, by = "genres") %>%
   left_join(release_year_effects_comb, by = "release_year") %>%
   left_join(review_yr_effects_comb, by = "review_year") %>%
   mutate(pred = mu + b_m + b_u + b_g + b_yr_release + b_yr_review) %>%
   pull(pred)

# Calculate RMSE based on review date effects model
rmse_model_4_3 <- RMSE(predicted_b_yr_review, edx_test$rating)

#Add the new RMSE to the RMSE Table
rmse_table <- bind_rows(rmse_table,
                        data_frame(method="Model 4.3: Mean + Genre + Movie + User + Release Yr + Review Yr Effect",
                                   RMSE = rmse_model_4_3 ))
rmse_table %>% knitr::kable(caption = captionTableType4)

## Model 5.0: Effects of Regularization on the model

### Define function to find RMSE for given lambda on a predefined model

find_rmse_for_lambda <- function(l) {
   #Train on the edx training set to find user effect including movie effect
   combined_movie_user_effects <-  edx_train %>% 
      left_join(movie_effects, by='movieId') %>%
      group_by(userId) %>% 
      summarize(b_u = sum(rating - mu - b_m)/(n()+l))
   #Generate predictions on the edx test set
   predicted_ratings <-  edx_test %>% 
      left_join(movie_effects, by='movieId') %>%
      left_join(combined_movie_user_effects, by='userId') %>%
      mutate(pred = mu + b_m + b_u) %>%
      .$pred
   #Find RMSE for this model
   RMSE(edx_test$rating, predicted_ratings)
}  

### Finding the optimal value of lambda
# Towards optimal lambda - Step 1
lambdas <- seq(0, 100, 10)
rmses <- sapply(lambdas, function(l) {  find_rmse_for_lambda(l) })
qplot(x = lambdas, y = rmses, main = "Plot 1: Finding optimal lamda", 
      xlab = "Lamda", ylab = "RMSE", geom = c("point", "line"))

# Towards optimal lambda - Step 2
lambdas <- seq(0, 10, 1)
rmses <- sapply(lambdas, function(l) {  find_rmse_for_lambda(l) })
qplot(x = lambdas, y = rmses, main = "Plot 2: Finding optimal lamda", 
      xlab = "Lamda", ylab = "RMSE", geom = c("point", "line"))

# Towards optimal lambda - Step 3
lambdas <- seq(4, 6, 0.1)
rmses <- sapply(lambdas, function(l) {  find_rmse_for_lambda(l) })
qplot(x = lambdas, y = rmses, main = "Plot 3: Finding optimal lamda", 
      xlab = "Lamda", ylab = "RMSE", geom = c("point", "line"))


# Print optimal lamda and corresponding rmse
lambdas[which.min(rmses)]
min_rmse <- min(rmses)


#Add the new RMSE to the RMSE Table
rmse_table <- bind_rows(rmse_table,
                        data_frame(method="Model 5.0: Mean + Regularized Movie + Cum. User Effect",
                                   RMSE = min_rmse))
rmse_table %>% knitr::kable(caption = "RMSE Values for All Models")

## Model 6.0: Model with all features and regularization
# Regularise model, predict ratings and calculate RMSE for passed value of lambda
train_predict_get_rmse <- function(l, trainSet, testSet)
{
  b_m   <-        trainSet %>%
                  group_by(movieId) %>%
                  summarise(b_m = sum(rating - mu)/(n()+l))
  b_u   <-        trainSet %>%
                  left_join(b_m, by="movieId") %>%
                  group_by(userId) %>%
                  summarise(b_u = sum(rating - b_m - mu)/(n()+l))
  b_g   <-        trainSet %>%
                  left_join(b_m, by="movieId") %>%
                  left_join(b_u, by="userId") %>%
                  group_by(genres) %>%
                  summarise(b_g = sum(rating - b_m - b_u - mu)/(n()+l))
  b_yr_release <- trainSet %>%
                  left_join(b_m, by="movieId") %>%
                  left_join(b_u, by="userId") %>%
                  left_join(b_g, by="genres") %>%
                  group_by(release_year) %>%
                  summarise(b_yr_release = sum(rating - b_m - b_u - b_g - mu)/(n()+l))
  b_yr_review <-  trainSet %>%
                  left_join(b_m, by="movieId") %>%
                  left_join(b_u, by="userId") %>%
                  left_join(b_g, by="genres") %>%
                  left_join(b_yr_release, by="release_year") %>%
                  group_by(review_year) %>%
                  summarise(b_yr_review = sum(rating - b_m - b_u - b_g - mu)/(n()+l))
  
  predicted_ratings <-  testSet %>%
                        left_join(b_m, by="movieId") %>%
                        left_join(b_u, by="userId") %>%
                        left_join(b_g, by="genres") %>%
                        left_join(b_yr_release, by="release_year") %>%
                        left_join(b_yr_review, by="review_year") %>%
                        mutate(pred = mu + b_m + b_u + b_g + b_yr_release + b_yr_review) %>%
                        pull(pred)
  
  return (RMSE(predicted_ratings, testSet$rating))
}
# Generate a sequence of values for lambda ranging from 4 to 6 with 0.1 increments
inc <- 0.1
lambdas <- seq(4, 6, inc)

# Get RMSE values for all the lambdas
rmses <- sapply(lambdas, function(l) {  train_predict_get_rmse(l, edx_train, edx_test) })

# Assign optimal tuning parameter (lambda)
optimal_lambda <- lambdas[which.min(rmses)]

# Minimum RMSE achieved
regularised_rmse <- min(rmses) 

#Add the new RMSE to the RMSE Table
rmse_table <- bind_rows(rmse_table,
                        data_frame(method="Model 6: Regularized Model of Movie + User + Genre + Release Yr + Review Yr Effect",
                                   RMSE = regularised_rmse ))
rmse_table %>% knitr::kable(caption = "RMSE Values for All Models")

# RMSE on validation set with final chosen model

# Calculate and store the model parameters to be used in the MovieRecommender Model
# with the optimal value of lambda
  l     <-        optimal_lambda
  b_m   <-        edx_train %>%
                  group_by(movieId) %>%
                  summarise(b_m = sum(rating - mu)/(n()+l))
  b_u   <-        edx_train %>%
                  left_join(b_m, by="movieId") %>%
                  group_by(userId) %>%
                  summarise(b_u = sum(rating - b_m - mu)/(n()+l))
  b_g   <-        edx_train %>%
                  left_join(b_m, by="movieId") %>%
                  left_join(b_u, by="userId") %>%
                  group_by(genres) %>%
                  summarise(b_g = sum(rating - b_m - b_u - mu)/(n()+l))
  b_yr_release <- edx_train %>%
                  left_join(b_m, by="movieId") %>%
                  left_join(b_u, by="userId") %>%
                  left_join(b_g, by="genres") %>%
                  group_by(release_year) %>%
                  summarise(b_yr_release = sum(rating - b_m - b_u - b_g - mu)/(n()+l))
  b_yr_review <-  edx_train %>%
                  left_join(b_m, by="movieId") %>%
                  left_join(b_u, by="userId") %>%
                  left_join(b_g, by="genres") %>%
                  left_join(b_yr_release, by="release_year") %>%
                  group_by(review_year) %>%
                  summarise(b_yr_review = sum(rating - b_m - b_u - b_g - mu)/(n()+l))

MovieRecommenderModel <- function(predictInputSet)
{
      # Add release and review years that are used in our model
      predictInputSet <- add_release_and_review_years(predictInputSet)

      # Predict using the values of the various feature effects found
      predicted_ratings <-  predictInputSet %>%
                        left_join(b_m, by="movieId") %>%
                        left_join(b_u, by="userId") %>%
                        left_join(b_g, by="genres") %>%
                        left_join(b_yr_release, by="release_year") %>%
                        left_join(b_yr_review, by="review_year") %>%
                        mutate(pred = mu + b_m + b_u + b_g + b_yr_release + b_yr_review) %>%
                        pull(pred)
      
      #Return the predictions
      predicted_ratings
}

predicted_ratings <- MovieRecommenderModel(validation)
final_rmse_validation_set <- RMSE(validation$rating, predicted_ratings)

# Add the new RMSE to the RMSE Table
rmse_table <- bind_rows(rmse_table,
                        data_frame(method="RMSE on validation set using Model 5",
                                   RMSE = final_rmse_validation_set))
rmse_table %>% knitr::kable(caption = "RMSE Values for All Models")



