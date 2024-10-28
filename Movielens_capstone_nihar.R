##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################


if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

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
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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


#loading extra required libraries:

# extra libraries 

library(lubridate)
library(ggplot2)
library(dplyr)


#summary stats: 

str(edx)
head(edx)
summary(edx)

##Exploratory analysis: 
###########################################################

#distribution of ratings

rating_vector <- as.vector(edx$rating)
rating_vector <- rating_vector[rating_vector !=0]
rating_vector <- factor(rating_vector)
qplot(rating_vector, xlab = "ratings", ylab = "number of ratings")+ ggtitle("Distribution of ratings")


#distribution of ratings by movies
movie_sum <- edx %>% group_by(movieId) %>% summarize(n_rating_of_movie = n(), mu_movie = mean(rating), sd_movie = sd(rating))

qplot(movie_sum$n_rating_of_movie, log="x", xlab = "Movies", ylab = "number of ratings")+ ggtitle("Distribution of ratings by movies")


#distribution of ratings by users

User_sum <- edx %>% group_by(userId) %>% summarize(n_rating_of_movie = n(), mu_movie = mean(rating), sd_movie = sd(rating))

qplot(User_sum$n_rating_of_movie, log="x")

##Ratings distribution by Genres

genres_ind <- str_replace(edx$genres,"\\|.*","")
genres_ind <- genres_ind[!duplicated(genres_ind)]
genres_ind
## this helps us identify the unique genres present in the data set.

n_genres <- sapply(genres_ind, function(Genre_match){
  index <- str_which(edx$genres, Genre_match)
  length(edx$rating[index])
})
## this gets the count of ratings by genre.

genres_rating <- sapply(genres_ind, function(Genre_match){
  index <- str_which(edx$genres, Genre_match) 
  mean(edx$rating[index], na.rm = T)
})


##this gets the mean of the ratings against each genre

##combining both of these below: 

genres_sum <- data.frame(genres = genres_ind, 
                         n_genres = n_genres,
                         average_rating = genres_rating)
genres_sum <- genres_sum %>% arrange(desc(n_genres))

genres_sum


## ploting genre wise number of ratings

genres_sum %>% ggplot(aes(x=reorder(genres,n_genres), n_genres)) +
  geom_col() +
  
  coord_flip() +
  labs(title = "Distribution of Ratings by Genres",
       y = "number of rating",
       x = "genres")


##plotting the genre wise average rating

ggplot(genres_sum, aes(x = reorder(genres, average_rating), average_rating)) +
  geom_col() +
  coord_flip() +
  labs(title = "Mean rating by genres",
       y = "average rating", x = "genres",
  )


## Release year effect, 

##Modifying the data for the release year

edx <- edx %>% 
  mutate(rating_time = as.Date(as.POSIXct(timestamp, origin = "1970-01-01"))) %>% 
  mutate(rating_year = year(rating_time))

#Adding the release year of each movie.

edx <- edx %>%  mutate(release_year = as.integer(substr(title, str_length(title) - 4,
                                                        str_length(title) - 1)))



##release year grouping

release_year_sum <- edx %>% group_by(release_year) %>% summarize(n = n(), average_rating = mean(rating))

#r Release year based plot

release_year_sum %>% ggplot(aes(release_year, average_rating)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Mean Rating by release year",
  )


#Release year based plot 2

release_year_sum %>% ggplot(aes(release_year, n)) +
  geom_point() + 
  geom_line()+
  geom_smooth() +
  labs(title = "Mean Rating by release year",
  )


#################################################################################

#MOdeling and analysis. 

#naive approach - MOdel 1

mean_rating <- mean(edx$rating)

##the rating on average would be : 
Model_1 <- mean_rating

Model_1

RMSE_model_1 <- RMSE(Model_1, edx$rating)

RMSE_Table <- tibble(method="Model 1: Naive Method",  RMSE = RMSE_model_1)

RMSE_Table ##rmse table with result of model 1



##movie effect model 2

##Calculating the penalty term for the movie effect.

movie_effect <- edx %>% group_by(movieId) %>% summarize(b_m = mean(rating - mean_rating))

movie_effect %>% qplot(b_m, geom = "histogram", bins =20, data =.)

##The movie effect model is as below, 

edx <- edx %>% left_join(movie_effect, by = "movieId") %>%  mutate(Mean_m2 = mean_rating + b_m)

##RMSE calculation for model 2

RMSE_model_2 <- RMSE(edx$rating, edx$Mean_m2)

RMSE_Table <- bind_rows(RMSE_Table, data_frame(method="Model 2: Movie effect", RMSE =RMSE_model_2 ))

RMSE_Table




##User effect Model 3

##Calculating the penalty term for the movie effect.

user_effect <- edx %>% group_by(userId) %>% summarize(b_u = mean(rating - mean_rating))

user_effect %>% qplot(b_u, geom = "histogram", bins =20, data =.)

##The user effect model is as below, 

edx <- edx %>% left_join(user_effect, by = "userId") %>%  mutate(Mean_m3 = Mean_m2 + b_u)


##RMSE calculation for model 3

RMSE_model_3 <- RMSE(edx$rating, edx$Mean_m3)

RMSE_Table <- bind_rows(RMSE_Table, data_frame(method="Model 3: Movie + user effect", RMSE =RMSE_model_3 ))

RMSE_Table



##Model 4: Regularisation Approach (MOvie + users)

# p is the tuning parameter, we will cross validate it to chose the best value. 

p <- seq(0, 10, 0.25) ##p is set to vary from 0 to 10 in intervals of 0.25 this gives us 40 test conditions. 

# For each p, we will find b_m & b_u, and then run a prediction and test it against the data set.


rmses <- sapply(p, function(A){
  
  mean_r <- mean(edx$rating)
  
  b_movie <- edx %>% group_by(movieId) %>% summarize(b_movie = sum(rating - mean_r)/(n()+ A))
  b_user <- edx %>% left_join(b_movie, by="movieId") %>% group_by(userId) %>% summarize(b_user = sum(rating - b_movie - mean_r)/(n()+A))
  
  predicted_ratings <- edx %>% left_join(b_movie, by = "movieId") %>% left_join(b_user, by = "userId") %>% mutate(predicted = mean_r + b_movie + b_user) %>% .$predicted
  
  return(RMSE(edx$rating,predicted_ratings))
  
})

# Plotting the rmses vs p to select the optimal tuning factor

qplot(p, rmses)


p_min <- p[which.min(rmses)]

p_min ## the rmse is lowest for this value

##regularising the factors.

# Compute regularized estimates of b_movie using p_min
mean_r <- mean(edx$rating)

Movie_effect_r <- edx %>% group_by(movieId) %>% summarize (b_movie = sum(rating - mean_r)/(n()+p_min), n_movie = n())

# Compute regularized estimates of b_user using p_min
User_effect_r <- edx %>% left_join(Movie_effect_r, by='movieId') %>% group_by(userId) %>% summarize(b_user = sum(rating - mean_r - b_movie)/(n()+p_min), n_user = n())

# Predicting the ratings
predicted_ratings_r <- edx %>% left_join(Movie_effect_r, by='movieId') %>% left_join(User_effect_r, by='userId') %>%
  mutate(prediction = mean_r + b_movie + b_user) %>% .$prediction

# Test and save results
RMSE_model_4 <- RMSE(edx$rating, predicted_ratings_r)

RMSE_Table <- bind_rows(RMSE_Table, data_frame(method="Model 4: Regularized Movie and User Effect Model", RMSE = RMSE_model_4 ))

RMSE_Table %>% knitr::kable()

##regularising the factors.


# Compute regularized estimates of b_movie and B_user using p_min
mean_r <- mean(edx$rating)

Movie_effect_r <- edx %>% group_by(movieId) %>% summarize (b_movie = sum(rating - mean_r)/(n()+p_min), n_movie = n())

# Compute regularized estimates of b_user using p_min
User_effect_r <- edx %>% left_join(Movie_effect_r, by='movieId') %>% group_by(userId) %>% summarize(b_user = sum(rating - mean_r - b_movie)/(n()+p_min), n_user = n())

# Predicting the ratings
predicted_ratings_final <- validation %>% left_join(Movie_effect_r, by='movieId') %>% left_join(User_effect_r, by='userId') %>%
  mutate(prediction = mean_r + b_movie + b_user) %>% .$prediction

# Test and save results
RMSE_model_Final <- RMSE(validation$rating, predicted_ratings_final)

RMSE_Table <- bind_rows(RMSE_Table, data_frame(method="Validation Set", RMSE = RMSE_model_Final ))

RMSE_Table %>% knitr::kable()

###############################################################
###############################################################


