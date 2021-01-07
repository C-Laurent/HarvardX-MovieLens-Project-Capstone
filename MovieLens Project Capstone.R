#################################################
# MovieLens Project Capstone 
################################################

# 1. Introduction 

# For this project, which is one of the 2 projects of the HarvardX: PH125.9x Data Science: Capstone course, 
# we will create a movie recommendation system using the MovieLens dataset. 

# 2. Methodology and Analysis

## 2.1. Data import and preprocessing

# Check and install the required libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# Load the required libraries
library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(lubridate)
library(gridExtra)

# Set the number of digits for pdf print
options(pillar.sigfig = 7)

# Import date and create training and test sets

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


# if using R 4.0 or later:
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

# Display the dimensions of the training set (edx) and validation set (validation)
dim(edx)
dim(validation)

# Display the first rows of the training set
head(edx)

# Check if the training and validation sets have any missing values.
any(is.na.data.frame(edx))
any(is.na.data.frame(validation))

# Add the year of the movie to the training and validation sets
edx <- edx %>% mutate(movie_year = as.numeric(str_sub(title,-5,-2))) 
validation <- validation %>% mutate(movie_year = as.numeric(str_sub(title,-5,-2))) 
# Display the first rows of the trainig set to check
head(edx)

## 2.2. Data Exploration and Visualization

### 2.2.1. The rating

# Compute the percentage of movie ratings per rating for the training set
prop.table(table(edx$rating))

# Display side by side the distribution ratings for the training and validation sets
grid.arrange(qplot(edx$rating, xlab = "Rating for the training set", ylab = "Number of Ratings"),qplot(validation$rating, xlab = "Rating for the validation set"),ncol=2, top = "Ratings Distribution")

# Create a chart representing the evolution of the rating average over time
edx %>%
  mutate(date = as_datetime(timestamp))%>% # add a date column with a date format from the timestamp column
  group_by(month_date = round_date(date,unit="month")) %>% # group thee ratings by month of rating
  summarize(monthly_rating = mean(rating)) %>% # compute the average number of ratings per month
  ggplot(aes(y=monthly_rating,x=month_date)) + # Create a chart with the monthly averages and the corresponding smooth density line
  ylim(c(3, 4.5)) +
  geom_point() +
  geom_smooth()  +
  labs(x="Month", y="Monthly Average") +
  ggtitle("Evolution of the monthly average rating over time")

# Create a chart representing the evolution of the rating average depending on the year of release of the movie
edx %>%
  group_by(movie_year) %>% # Group the ratings my year of release
  summarize(average_rating = mean(rating)) %>% # Compute the corresponding yearly averages
  ggplot(aes(x=movie_year,y=average_rating)) + # Create a chart representing the yearly averages and the corresponding smooth density line
  ylim(c(3, 4.5)) +
  geom_point() +
  geom_smooth()  +
  labs(x="Year", y="Average Rating") +
  ggtitle("Average rating per release year")

# Create an histogram of the number of ratings per movie 
edx %>%
  count(movieId) %>%
  ggplot(aes(n))  +
  geom_histogram(bins=30,color='black') +
  scale_x_log10() +
  labs(x="Number of ratings per movie", y="Number of movies") +
  ggtitle("Ratings per movie Distribution")

# Counts the number of movies, which have less than 10 reviews
edx %>%
  count(movieId) %>%
  filter(n<10) %>%
  nrow()

# Create an histogram of the average ratings per movie
edx %>%
  group_by(movieId) %>%
  summarize(average_rating=mean(rating)) %>%
  ggplot(aes(average_rating))  +
  geom_histogram(bins=10,color='black') +
  labs(x="Average Rating", y="Number of movies") +
  ggtitle("Average Rating per Movie Distribution")

### 2.2.3. The users

# Create an histogram of the number of ratings per user
edx %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  labs(x="Number of ratings per user", y="Number of users") +
  ggtitle("Ratings per user distribution")

edx %>%
  group_by(userId) %>%
  summarize(average_rating=mean(rating)) %>%
  ggplot(aes(average_rating))  +
  geom_histogram(bins=10,color='black') +
  labs(x="Average Rating", y="Number of users") +
  ggtitle("Average Rating per User Distribution")

### 2.2.4. The genres

# Create a chart representing the average rating per combination of genres for the combinations having more than 100,000 ratings and sorted by highest average
edx %>%
  group_by(genres) %>% # Group the ratings by genre
  summarize(number = n(), average = mean(rating), se=sd(rating)/sqrt(number)) %>% # Compute the number, average and standard error of ratings per genre
  filter(number > 100000) %>% # Filter on the genres having more than 50,000 ratings
  arrange(desc(average)) %>% # Sort by descending order of average number of ratings per genre
  mutate(genres=reorder(genres,average)) %>% # Reorger the genres by ascending average for the chart
  ggplot(aes(x=genres, y=average, ymin=average-2*se,ymax=average+2*se)) + # Create the chart of average rating vs genre with the correspponding standard error.
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x="Genres Category", y="Average Rating") +
  ggtitle("Average Rating per Genres Category")

# Get the top 5 genres per number of ratings
top_5_genres <- edx %>% 
  group_by(genres) %>% # Group the ratings by genre
  summarize(number = n()) %>% # Compute the number of ratinges per genre
  arrange(desc(number)) %>% # Sort by descending number of ratings per genre
  head(5) # Get the top 5 genres with the corresponding number of rating

# Create line chart representing the evolution of the number of ratings per genre over time
edx %>%
  filter(genres %in% top_5_genres$genres) %>% # Filter the training set on the top 5 genres per number of ratings
  group_by(movie_year,genres) %>% # Group by year of release and genres
  summarize(rating_number = n()) %>% # Compute the corresponding number of ratings
  ggplot(aes(x = movie_year, y = rating_number)) + # Create the chart representing the evolution of number of ratings per genre over the year of release
  geom_line(aes(color=genres)) +
  labs(x="Release Year", y="Number of Ratings") +
  ggtitle("Evolution of number of ratings - Top 5 Genres Category")

# Create line chart representing the evolution of the average rating per genre over time
edx %>%
  filter(genres %in% top_5_genres$genres) %>% # Filter the training set on the top 5 genres per number of ratings
  group_by(movie_year,genres) %>% # Group by year of release and genres
  summarize(average_rating = mean(rating)) %>% # Compute the corresponding average rating
  ggplot(aes(x = movie_year, y = average_rating)) + # Create the chart representing the evolution of the average rating per genre over the year of release
  geom_line(aes(color=genres)) +
  labs(x="Release Year", y="Average Rating") +
  ggtitle("Evolution of Average Rating - Top 5 Genres Category")

## 2.3. Modeling Approach

# Define the RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Test set will be 10% of edx data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure that the userId, movieId and genres in test set are also present in the training set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId") %>%
  semi_join(train_set, by = "genres")

# Add rows removed from the test set back into training set
removed <- anti_join(temp, test_set)
train_set<- rbind(train_set, removed)
rm(test_index, temp, removed)

# 3. Model Selection

## 3.1. Basic Model

# Define the function to create, train the basic model and return the prediction
basic_model <- function(training_dataset){
  # Compute the average rating
  mu <- mean(training_dataset$rating)
  # Return the average rating
  return(mu)
}
# Compute the basic model with the training set
basic_model(train_set)

# Compute the predicted rating for the test set (= average)
basic_model_prediction <- basic_model(train_set)

# Compute the corresponding RSME
basic_model_rmse <- RMSE(test_set$rating, basic_model_prediction)

# Create the table and store the RMSE corresponding to our first basic model
rmse_results <- tibble(Method = "Average Rating Model", RMSE = basic_model_rmse)
head(rmse_results)

## 3.2. The movie Model

# Create a function to build, train the movie model based on a training dataset and return the predictions based on a test or validation test
movie_model <- function(training_dataset, test_dataset){
  # Compute the average rating
  mu <- mean(training_dataset$rating)
  # Compute the movie effect
  movie_effect <- training_dataset %>%
    group_by(movieId) %>% # Group the ratings by movie
    summarize(b_i = mean(rating - mu)) # Compute and store the movie effect the difference between the movie rating average and the overall average for all movies
  # Compute the predictions for the movies in the test set
  model_prediction <- mu + test_dataset %>% 
    left_join(movie_effect, by='movieId') %>%
    .$b_i 
  # Return the predictions
  return(model_prediction)
}

# Compute the predicted ratings based on the test set for this model
model_1_prediction <- movie_model(train_set,test_set)

# Compute the corresponding RMSE
model_1_rmse <- RMSE(test_set$rating, model_1_prediction)

# Update the RMSE results table with the RMSE of this model
rmse_results <- bind_rows(rmse_results,tibble(Method="Movie Effect Model", RMSE = model_1_rmse ))

# Create a function to build, train the movie + user model based on a training dataset and return the predictions based on a test or validation test
movie_user_model <- function(training_dataset, test_dataset){
  # Compute the average rating
  mu <- mean(training_dataset$rating)
  # Compute the movie effect
  movie_effect <- training_dataset %>%
    group_by(movieId) %>%
    summarize(b_i = mean(rating - mu))
  # Compute the user effect
  user_effect <- training_dataset %>%
    left_join(movie_effect, by='movieId') %>% # Add the movie effect to the training set
    group_by(userId) %>% # Group the rating by user id
    summarize(b_u = mean(rating - mu - b_i)) # Compute and store the user effect
  # Compute the predictions based on the test_dataset
  model_prediction <- test_dataset %>% 
    left_join(movie_effect, by='movieId') %>%
    left_join(user_effect, by='userId') %>%
    mutate(prediction = mu + b_i + b_u) %>%
    .$prediction
  # Return the predictions
  return(model_prediction)
}

# Compute the predicted ratings based on the test set for this model
model_2_prediction <- movie_user_model(train_set,test_set)

# Compute the corresponding RMSE
model_2_rmse <- RMSE(test_set$rating, model_2_prediction)

# Update the RMSE results table with the RMSE of this model
rmse_results <- bind_rows(rmse_results, tibble(Method="Movie + User Effects Model",  RMSE = model_2_rmse ))

## 3.4. The movie, user and genres Model

# Create a function to build, train the movie + user + genre model based on a training dataset and return the predictions based on a test or validation test
movie_user_model_genres <- function(training_dataset, test_dataset){
  # Compute the average rating
  mu <- mean(training_dataset$rating)
  # Compute the movie effect
  movie_effect <- training_dataset %>%
    group_by(movieId) %>%
    summarize(b_i = mean(rating - mu))
  # Compute the user effect
  user_effect <- training_dataset %>%
    left_join(movie_effect, by='movieId') %>% 
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu - b_i))
  # Compute the genres effect
  genres_effect <- training_dataset %>%
    left_join(movie_effect, by='movieId') %>% # Add the movie effect to the training set
    left_join(user_effect, by='userId') %>%  # Add the user effect to the training set
    group_by(genres) %>% # Group the ratings per genre
    summarize(b_g = mean(rating - mu - b_i - b_u)) # Compute and store the genre effect
  # Compute the predictions based on the test_dataset
  model_prediction <- test_dataset %>% 
    left_join(movie_effect, by ='movieId') %>%
    left_join(user_effect, by ='userId') %>%
    left_join(genres_effect, by ='genres') %>%
    mutate(prediction = mu + b_i + b_u + b_g) %>%
    .$prediction
  # Return the predictions
  return(model_prediction)
}

# Compute the predicted ratings based on the test set for this model
model_3_prediction <- movie_user_model_genres(train_set,test_set)

# Compute the corresponding RMSE
model_3_rmse <- RMSE(test_set$rating, model_3_prediction)

# Update the RMSE results table with the RMSE of this model
rmse_results <- bind_rows(rmse_results, data_frame(Method="Movie + User + Genres Effects Model",  RMSE = model_3_rmse))

## 3.5. The movie, user and genres Model with regularization

# Create a function to build, train the movie + user + genre model including regularization based on a training dataset and return the predictions based on a test or validation test
movie_user_model_genres_reg <- function(training_dataset, test_dataset,l){
  # Compute the average rating
  mu <- mean(training_dataset$rating)
  # Compute the movie effect
  movie_effect <- training_dataset %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  # Compute the user effect
  user_effect <- training_dataset %>%
    left_join(movie_effect, by='movieId') %>% 
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  # Compute the genres effect
  genres_effect <- training_dataset %>%
    left_join(movie_effect, by='movieId') %>%
    left_join(user_effect, by='userId') %>% 
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+l))
  # Compute the predictions based on the test_dataset
  model_prediction <- test_dataset %>% 
    left_join(movie_effect, by ='movieId') %>%
    left_join(user_effect, by ='userId') %>%
    left_join(genres_effect, by ='genres') %>%
    mutate(prediction = mu + b_i + b_u + b_g) %>%
    .$prediction
  # Return the predictions
  return(model_prediction)
}

# Set a list of different values of λ to test
lambdas <- seq(0, 10, 0.25)

# Compute the RMSE with the new functions with the different values of λ
rmses <- sapply(lambdas, function(l){
  model_4_prediction <- movie_user_model_genres_reg(train_set,test_set,l)
  return(RMSE(test_set$rating,model_4_prediction))
})

# Plot the RMSE versus the corresponding values of λ
qplot(lambdas, rmses)

# Store the best λ that minimize the RMSE
lambda <- lambdas[which.min(rmses)]

# Compute the predicted ratings based on the test set for this model
model_4_prediction <- movie_user_model_genres_reg(train_set,test_set, lambda)

# Compute the corresponding RMSE
model_4_rmse <- RMSE(test_set$rating, model_4_prediction)

# Update the RMSE results table with the RMSE of this model
rmse_results <- bind_rows(rmse_results, data_frame(Method="Movie + User + Genres Effects regularized Model",  RMSE = model_4_rmse))

# 4. Results

# Compute the predicted rating for the test set (= average)
basic_model_prediction_final <- basic_model(edx)

# Compute the corresponding RSME
basic_model_rmse_final <- RMSE(validation$rating, basic_model_prediction_final)

# Create the table and store the RMSE corresponding to our first basic model
rmse_results_final <- tibble(Method = "Average Rating Model", RMSE = basic_model_rmse_final)

# Compute the predicted ratings based on the test set for this model
model_1_prediction_final <- movie_model(edx,validation)

# Compute the corresponding RMSE
model_1_rmse_final <- RMSE(validation$rating, model_1_prediction_final)

# Update the RMSE results table with the RMSE of this model
rmse_results_final <- bind_rows(rmse_results_final,tibble(Method="Movie Effect Model", RMSE = model_1_rmse_final))

# Compute the predicted ratings based on the test set for this model
model_2_prediction_final <- movie_user_model(edx, validation)

# Compute the corresponding RMSE
model_2_rmse_final <- RMSE(validation$rating, model_2_prediction_final)

# Update the RMSE results table with the RMSE of this model
rmse_results_final <- bind_rows(rmse_results_final, tibble(Method="Movie + User Effects Model",  RMSE = model_2_rmse_final))

# Compute the predicted ratings based on the validation set for this model
model_3_prediction_final <- movie_user_model_genres(edx, validation)

# Compute the corresponding RMSE
model_3_rmse_final <- RMSE(validation$rating, model_3_prediction_final)

# Update the RMSE results table with the RMSE of this model
rmse_results_final <- bind_rows(rmse_results_final, tibble(Method="Movie + User + Genres Effects Model",  RMSE = model_3_rmse_final))

# Compute the predicted ratings based on the validation set for this model
model_4_prediction_final <- movie_user_model_genres_reg(edx,validation, lambda)

# Compute the corresponding RMSE
model_4_rmse_final <- RMSE(validation$rating, model_4_prediction_final)

# Update the RMSE results table with the RMSE of this model
rmse_results_final <- bind_rows(rmse_results_final, tibble(Method="Movie + User + Genres Effects regularized Model",  RMSE = model_4_rmse_final))

# Compute and display the gain of the last model vs the basic model in value
rmse_gain_value <- rmse_results_final[[1,"RMSE"]] - rmse_results_final[[5,"RMSE"]]
rmse_gain_perc <- rmse_gain_value/rmse_results_final[[1,"RMSE"]]*100
tibble("RSME gain in value"= rmse_gain_value,"RSME gain in percentage" = rmse_gain_perc)



