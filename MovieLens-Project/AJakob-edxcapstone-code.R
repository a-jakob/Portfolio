#First I have loaded the pre-whriten code given by the course. It installs the needed packages and mount the dataset.

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)


ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

dim(ratings)
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
dim(movies)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")
dim(movielens)
ls()
# Validation set will be 10% of MovieLens data

set.seed(1) # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")
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

#Beginning knowing the amount of rows and columns of the dataset.
dim(edx)
#Show the amount each rate.
count(edx,rating)
#Show the amount of differente movies and users.
names(edx)
edx %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))
#Rarely one movie is classified as a unique genre. So to know how many movies each genre has we must separate the rows, in the way that each movie has some rows, each of them having just one genre.
#Since the data frame is huge, I made a temporary data frame calles 'teste' with just the six first rows of the original data set, so I can test the code before running it in the whole data frame.
teste<-head(edx)
#Before separating the rows I changed the name 'Sci-Fi' to 'Sci.Fi', otherwise the separate_rows function would make two false genres 'Sci' and 'Fi'. Than I run separate_rows
teste$genres <- gsub('Sci-Fi', 'Sci.Fi', teste$genres)
teste
septeste<-separate_rows(teste,genres,convert=TRUE)
septeste
#Now that I know that my code works, I run it in tha major data frame.
edx$genres <- gsub('Sci-Fi', 'Sci.Fi', edx$genres)
head(edx)
sep<-separate_rows(edx,genres,convert=TRUE)
head(sep)
#And see how many movies any genre has.
sep %>% filter(rating>0&genres=="Drama")%>%tally()
sep %>% filter(rating>0&genres=="Comedy")%>%tally()
sep %>% filter(rating>0&genres=="Thriller")%>%tally()
sep %>% filter(rating>0&genres=="Romance")%>%tally()

names(edx)

#See whose are the most rated movies.
tableratings<-edx %>%summarize(n_users = n_distinct(userId),n_movies = n_distinct(movieId))
table(tableratings)
#And whit ratings are more commom.
count(edx,rating)
#In the graphic below we see that the users tend to give rounded ratings more than fractionated ones.
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()
#Defining previously the RMSE (obviously I will perfect this further)
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
#The first and most basical approach is predict a rating regardless of the user and movie.
mu_hat<-mean(edx$rating)
mu_hat
naive_rmse <- RMSE(validation$rating, mu_hat)
naive_rmse
predictions <- rep(3, nrow(validation))
RMSE(validation$rating, predictions)
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
rmse_results
#as expected,we have a RMSE for about to one, which is obviously not enough, since the possible ratings go from 0.5 to 5.
#Using just the average to predict the ratings, regardless of the criterias is almost good enough as guessing blindly. The first thing we will compute now is the bias for movies. 
#First we use the least squares method to estimate this bias. Since the whole dataset is enourmous, we therefore estimate this value from our train data set. 
mu <- mean(edx$rating) 
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

predicted_ratings <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

model_1_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",  
                                     RMSE = model_1_rmse))
rmse_results
#This time the prediction is a little better, not good enough though.
#Another bias is the user. Each user rates the movies according to their own criteria. Some users tend to give better ratings while others are more rigorous. Below I show a histogram for the average of stars each user gives. There is a substantial variability among them.
edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")
#For the same reason said above, we cannot run a prediction for the whole data set, so I estimate the bias from the training set instead.
user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
#And run the prediction to see if it have improved.
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)


model_2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse))
rmse_results
#It has, to 0.865.
#Even so, I will take a look at the greatest mistakes.
validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>% 
  select(title,  residual) %>% slice(1:10)
#To avoid obscure movies to mess the prediction
#Let's see the top and worse 10 movies according to the prediction.
movie_titles <- movielens %>% 
  select(movieId, title) %>%
  distinct()

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i) %>% 
  slice(1:10) 

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i) %>% 
  slice(1:10) 
#Let1s see how often the were rated.
edx %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) 
edx %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10)
#As expected, most of them have been rated just a few times. To avoid this, It is common to use a method of regularization of the data. In this case a proper way is to penalize some of the ratings. The penalty therm is called lambda. First, I need to find the better lambda value using cross validation.
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})
#Below a plot to see the lambdas and the minimum.
qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)]
lambda

#...Which is 5.25. Let's run the prediction using this lambda.
mu <- mean(edx$rating)
movie_reg_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())

data_frame(original = movie_avgs$b_i, 
           regularlized = movie_reg_avgs$b_i, 
           n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

predicted_ratings <- validation %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)

model_3_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = model_3_rmse))
rmse_results 
#...and compare with the other calculations.
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()
#As the table shows, after predicting the regularized Movie and user effect movel we got the minimum RMSE of 0.8648170.
ls()
#ps. trying to correct issues from knit
library(knitr)
library(tidyverse)
library(caret)
library(dplyr)
k1<-edx %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))
kable(k1)
