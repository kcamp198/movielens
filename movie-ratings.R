##### 1.1 Overview #####

## Set knitr options
knitr::opts_chunk$set(echo = TRUE)

## Create edx set, validation set (final hold-out test set), set more options, install packages
## Note: this process could take a couple of minutes

options(repos = list(CRAN="http://cran.rstudio.com/"))

knitr::opts_chunk$set(message = FALSE)

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(jtools)) install.packages("jtools", repos = "http://cran.us.r-project.org")
if(!require(huxtable)) install.packages("huxtable", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(kableExtra)
library(psych)
library(gridExtra)
library(jtools)
library(huxtable)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

download.file("https://www.dropbox.com/s/nspymeso8rmmak1/edx.rds?dl=1", "edx.rds", mode="wb")

download.file("https://www.dropbox.com/s/x0s477b0kzxpl6i/validation.rds?dl=1", "validation.rds", mode="wb")

edx = readRDS("edx.rds")

validation = readRDS("validation.rds")

## View first ten rows of edx data
edx %>% slice(1:10) %>% knitr::kable(caption = "Table 1.1. First ten rows of edx dataset",
                                     row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("scale_down","HOLD_position"))

## Create edx summary table
edx_summary <- data.frame(Rows = nrow(edx),
                          Columns = ncol(edx),
                          "Unique users" = n_distinct(edx$userId),
                          "Unique movies" = n_distinct(edx$movieId),
                          "Average rating" = round(mean(edx$rating),2),
                          "Number of genres" = n_distinct(edx$genres),
                          "Date of first rating" = 
                            as.Date(as.POSIXct(min(edx$timestamp), 
                                               origin = "1970-01-01")),
                          "Date of last rating" = 
                            as.Date(as.POSIXct(max(edx$timestamp),
                                               origin = "1970-01-01")),
                          check.names = FALSE)

## Display the first six columns of the edx summary table
edx_summary[,1:6] %>% 
  knitr::kable(caption = "Table 1.2. Summary of edx set") %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("HOLD_position"))

## Visualize sparsity using a random sample of our matrix of users, movies
install.packages("rafalib")
users <- sample(unique(edx$userId), 100)
rafalib::mypar()
edx %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

## Display the last two columns of the edx summary table
edx_summary[,7:8] %>% 
  knitr::kable(caption = "Table 1.3. Date range in edx set") %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("HOLD_position"))

## Display rows in edx with no genre information
edx %>%
  filter(genres == "(no genres listed)") %>% 
  knitr::kable(caption = "Table 1.4. Rows in edx lacking genre information",
               row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("HOLD_position"))

##### 2.0 Methods and analysis #####

## Create and display summary table of ratings
describe(edx$rating, fast = TRUE) %>%
  select(-vars) %>%
  knitr::kable(caption = "Table 2.1. Summary statistics for ratings",
               row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("HOLD_position"))

## Create a table with the sum of each user rating
rating_sum <- edx %>% group_by(rating) %>%
  summarize(count = n())

## Plot the count by rating using the rating sum table
rating_sum %>% mutate(rating = factor(rating)) %>%
  ggplot(aes(rating, count)) +
  geom_col(fill = "steel blue", color = "black") +
  theme_classic() +
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25)) + 
  labs(x = "Rating", y = "Count")

##### 2.1 Techniques and processes #####

## Update edx with timestamp converted to date format
edx <- edx %>% 
  mutate(rating_time = as.Date(as.POSIXct(timestamp, origin = "1970-01-01 00:00:00",tz = "GMT"))) %>% 
  mutate(rating_year = year(rating_time)) %>%
  select(-timestamp)

## View first five rows of updated edx dataset
edx %>% slice(1:5) %>% knitr::kable(caption = "Table 2.2. First five rows of updated edx dataset",
                                    row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("scale_down","HOLD_position"))

## Update edx to add release year
edx <- edx %>%
  mutate(release_year = as.integer(substr(title, str_length(title) - 4,
                                          str_length(title) - 1)))

## View first five rows of updated edx dataset
edx %>% slice(1:5) %>% knitr::kable(caption = "Table 2.3. First five rows of edx dataset with release year",
                                    row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("scale_down","HOLD_position"))

## Create and display summary table of ratings by movie
edx %>%
  dplyr::count(movieId) %>%
  describe(fast = TRUE) %>%
  select(-vars) %>%
  slice(2) %>%
  knitr::kable(caption = "Table 2.4. Summary statistics for ratings",
               row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("HOLD_position"))

## Visualize the distribution of ratings per movie
edx %>%
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(fill = "steel blue", color = "black") +
  theme_classic() +
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25)) + 
  labs(x = "Number of ratings", y = "Count of unique movies") + 
  scale_x_log10()

## Create quick summary table for scatterplot of average and number of ratings
movie_sum <- edx %>% group_by(movieId) %>%
  summarize(ratings = n(), 
            mu = mean(rating),
            sd = sd(rating))

## Display scatterplot of average and number of ratings
movie_sum %>% 
  ggplot(aes(ratings, mu)) +
  geom_point(color = "steel blue", alpha = 0.3) +
  geom_smooth() +
  geom_vline(aes(xintercept = mean(movie_sum$ratings)), color = "red") +
  annotate("text", x = 2000, y = 5,
           label = round(mean(movie_sum$ratings),0),
           color = "red", size = 3) +
  theme_classic() +
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25)) +
  labs(x = "Number of ratings per movie",
       y = "Average rating per movie")

## Plot average rating distribution alongside overall rating distribution
plot1 <- movie_sum %>% ggplot(aes(mu)) + 
  geom_histogram(fill = "steel blue", color = "black",
                 binwidth = 0.5) +
  labs(title = "Distribution of movie average ratings",
       x = "Rating",
       y = "Count") + 
  theme_classic() +
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25),
        plot.title = element_text(hjust = 0.5, size = 10))

plot2 <- rating_sum %>% mutate(rating = factor(rating)) %>%
  ggplot(aes(rating, count)) +
  geom_col(fill = "steel blue", color = "black") +
  theme_classic() +
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25),
        plot.title = element_text(hjust = 0.5, size = 10)) + 
  labs(title = "Distribution of movie ratings",
       x = "Rating",
       y = "Count")

grid.arrange(plot1, plot2, ncol=2)

## Create and display summary table of ratings by user
edx %>%
  dplyr::count(userId) %>%
  describe(fast = TRUE) %>%
  select(-vars) %>%
  slice(2) %>%
  knitr::kable(caption = "Table 2.5. Summary statistics for users",
               row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("HOLD_position"))

## Visualize the distribution of ratings per user
edx %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(fill = "steel blue", color = "black") +
  theme_classic() +
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25)) + 
  labs(x = "Number of ratings", y = "Count of unique users") + 
  scale_x_log10()

## Create quick user summary table for scatterplot of average and number of user-ratings
user_sum <- edx %>% group_by(userId) %>%
  summarize(user_ratings = n(), 
            user_mu = mean(rating),
            user_sd = sd(rating))

## Display scatterplot of average and number of user-ratings
user_sum %>% 
  ggplot(aes(user_ratings, user_mu)) +
  geom_point(color = "steel blue", alpha = 0.3) +
  geom_smooth() +
  geom_vline(aes(xintercept = mean(user_sum$user_ratings)), color = "red") +
  annotate("text", x = 400, y = 5,
           label = round(mean(user_sum$user_ratings),0),
           color = "red", size = 3) +
  theme_classic() +
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25)) +
  labs(x = "Number of ratings per user",
       y = "Average rating per user")

## Plot average rating distribution alongside overall rating distribution
plot3 <- user_sum %>% ggplot(aes(user_mu)) + 
  geom_histogram(fill = "steel blue", color = "black",
                 binwidth = 0.5) +
  labs(title = "Distribution of user-average ratings",
       x = "Rating",
       y = "Count") + 
  theme_classic() +
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25),
        plot.title = element_text(hjust = 0.5, size = 10))

plot4 <- rating_sum %>% mutate(rating = factor(rating)) %>%
  ggplot(aes(rating, count)) +
  geom_col(fill = "steel blue", color = "black") +
  theme_classic() +
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25),
        plot.title = element_text(hjust = 0.5, size = 10)) + 
  labs(title = "Distribution of movie ratings",
       x = "Rating",
       y = "Count")

grid.arrange(plot3, plot4, ncol=2)

## create quick summary table for plot of the number of ratings by release year
release_year_sum <- edx %>% group_by(release_year) %>%
  summarize(n = n(), average_rating = mean(rating))

## Display plot of number of ratings by release year
ggplot(release_year_sum, aes(release_year, n)) +
  geom_point(color = "steel blue", alpha = 0.6) +
  geom_line(color = "steel blue") +
  theme_classic() +
  labs(x = "Year",
       y = "Count") + 
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25))

## Plot the average rating by release year
ggplot(release_year_sum, aes(release_year, average_rating)) +
  geom_point(color = "steel blue", alpha = 0.6) +
  geom_smooth() +
  theme_classic() +
  labs(x = "Year",
       y = "Rating") + 
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25))

## Fit linear model of average rating on release year and release year squared
fit_lm1 <- lm(average_rating ~ I(release_year^2) +
                I(release_year), 
              data = release_year_sum)

## Fit linear model of average rating on release year, release year squared, and release year cubed
fit_lm2 <- lm(average_rating ~ I(release_year^3) + I(release_year^2) +
                I(release_year), 
              data = release_year_sum)

## Create table of results for both linear models
export_summs(fit_lm1, fit_lm2)

## Calculate the first rating time of each movie
first_sum <- edx %>% group_by(movieId) %>%
  summarize(first_ratings = n(),
            first_mu = mean(rating),
            first_rating_time = min(rating_time))

## Calculate the weeks elapsed since first rating
edx <- edx %>% left_join(first_sum, by = "movieId")
edx <- edx %>%
  mutate(weeks_elapsed = as.numeric(round((rating_time - first_rating_time)/7,0)))

## Create a summary table grouping by weeks elapsed
weeks_elapsed_sum <- edx %>% group_by(weeks_elapsed) %>%
  summarize(n_weeks_elapsed = n(),
            average_rating = mean(rating))

## Plot ratings count by weeks elapsed since first rating
ggplot(weeks_elapsed_sum, aes(weeks_elapsed, n_weeks_elapsed)) +
  geom_point(color = "steel blue", alpha = 0.6) +
  geom_line(color = "steel blue") +
  theme_classic() +
  labs(x = "Weeks since first rating",
       y = "Count") + 
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25))

## Plot average rating by weeks elapsed since first rating
ggplot(weeks_elapsed_sum, aes(weeks_elapsed, average_rating)) +
  geom_point(color = "steel blue", alpha = 0.6) +
  geom_line(color = "steel blue") +
  theme_classic() +
  labs(x = "Weeks since first rating",
       y = "Average rating") + 
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25))

## Create vector of genres in edx
genres <- str_replace(edx$genres, "\\|.*","")

## Drop duplicate genres from the vector
genres <- genres[!duplicated(genres)]

## Create a table listing the unique genres
genres %>%
  knitr::kable(caption = "Table 2.7. List of unique genres in edx",
               col.names = "Genre",
               row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("HOLD_position"))

## Calculate the number of movies per genre
n_genres <- sapply(genres, function(g){
  index <- str_which(edx$genres, g)
  length(edx$rating[index])
  
})

## Calculate the average rating by genre
genres_rating <- sapply(genres, function(g){
  index <- str_which(edx$genres, g) 
  mean(edx$rating[index], na.rm = T)
})

## Create summary table by genres
genres_sum <- data.frame(Genre = genres, 
                         Movies = n_genres,
                         "Average rating" = genres_rating,
                         check.names = FALSE)

## Display summary table by genres
genres_sum %>% arrange(desc(Movies)) %>% slice(1:5) %>%
  knitr::kable(caption = "Table 2.8. Most common genres in edx",
               digits = 2,
               row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("HOLD_position"))

## Plot average rating by genre
colnames(genres_sum)[3] <- "average_rating"
genres_sum %>%
  ggplot(
    aes(x = reorder(Genre, average_rating), average_rating)) +
  geom_col(fill = "steel blue", color = "black") +
  theme_classic() +
  coord_flip() +
  labs(
    y = "Average rating",
    x = "Genres") + 
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25))

## Create list of users close to the average
user_list <- user_sum %>% 
  filter(user_ratings >= round(mean(user_ratings),2)-1,
         user_ratings <= round(mean(user_ratings),2)+1) %>% 
  select(userId, user_mu)

## Sample 10 users at random
set.seed(1, sample.kind = "Rounding")
user_list <- sample(user_list$userId, 10)

## Create average rating figure by genre for the 10 random users
edx_random <- edx %>%
  filter(userId %in% user_list)

## Calculate the number of movies per genre
n_genres2 <- sapply(genres, function(g){
  index <- str_which(edx_random$genres, g)
  length(edx_random$rating[index])
})


## Calculate the average rating by genre
genres_rating2 <- sapply(genres, function(g){
  index <- str_which(edx_random$genres, g) 
  mean(edx_random$rating[index], na.rm = T)
})

## Create summary table by genres
genres_sum2 <- data.frame(Genre = genres, 
                          Movies = n_genres2,
                          average_rating = genres_rating2)
## Plot average rating by genre for random sample of 10 users
genres_sum2 %>%
  ggplot(
    aes(x = reorder(Genre, average_rating), average_rating)) +
  geom_col(fill = "steel blue", color = "black") +
  theme_classic() +
  coord_flip() +
  labs(
    y = "Average rating",
    x = "Genres") + 
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25))

##### 3.1 Model results #####

## Compute RMSE using the average of all ratings
rmses <- data_frame(method = "Only estimate is the average",
                    RMSE = RMSE(mean(edx$rating), edx$rating))
## Create the RMSE results table
rmses %>%
  knitr::kable(caption = "Table 3.1. RMSE by method I",
               row.names = FALSE,
               digits = 4) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

## Add movie--average to edx
movie_sum <- edx %>% group_by(movieId) %>%
  summarize(mu_movie = mean(rating))

edx <- left_join(edx, movie_sum, by = "movieId")
## Append the row to the RMSE results table
rmses <- bind_rows(rmses,
                   data_frame(method = "Add movie–average effect",
                              RMSE = RMSE(edx$mu_movie, edx$rating)))
## Display the RMSE results table
rmses %>%
  knitr::kable(caption = "Table 3.2. RMSE by method II",
               row.names = FALSE,
               digits = 4) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

## Add user--average to edx
b_u_sum <- edx %>% mutate(yhat = rating - mu_movie) %>%
  group_by(userId) %>%
  summarize(b_u = mean(yhat))

edx <- left_join(edx, b_u_sum, by = "userId") %>%
  mutate(mu_movie_user = mu_movie + b_u)
## Append the row to the RMSE results table
rmses <- bind_rows(rmses,
                   data_frame(method = "Add user–specific effect",
                              RMSE = RMSE(edx$mu_movie_user, edx$rating)))
## Display the RMSE results table
rmses %>%
  knitr::kable(caption = "Table 3.3. RMSE by method III",
               row.names = FALSE,
               digits = 4) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

## Add time effect to edx
b_t_sum <- edx %>% mutate(error = rating - mu_movie_user) %>%
  group_by(weeks_elapsed) %>%
  summarize(b_t = mean(error))

edx <- left_join(edx, b_t_sum, by = "weeks_elapsed") 

## Convert NAs to 0
edx$b_t[is.na(edx$b_t)] <- 0

edx <- edx %>%
  mutate(mu_movie_user_time = mu_movie_user + b_t)

## Append the row to the RMSE results table
rmses <- bind_rows(rmses,
                   data_frame(method = "Add time effect",
                              RMSE = RMSE(edx$mu_movie_user_time, edx$rating)))

## Display the RMSE results table
rmses %>%
  knitr::kable(caption = "Table 3.4. RMSE by method IV",
               row.names = FALSE,
               digits = 4) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

## Add genre effect to edx
b_g_sum <- edx %>% mutate(error = rating - mu_movie_user_time) %>%
  group_by(genres) %>%
  summarize(b_g = mean(error))

edx <- left_join(edx, b_g_sum, by = "genres") 

edx <- edx %>%
  mutate(mu_movie_user_time_genres = mu_movie_user_time + b_g)

## Append the row to the RMSE results table
rmses <- bind_rows(rmses,
                   data_frame(method = "Add genre effect",
                              RMSE = RMSE(edx$mu_movie_user_time_genres, edx$rating)))
## Display the RMSE results table
rmses %>%
  knitr::kable(caption = "Table 3.5. RMSE by method V",
               row.names = FALSE,
               digits = 4) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

##### 3.2 Model performance #####

## Update validation with timestamp converted to date format
validation <- validation %>% 
  mutate(rating_time = as.Date(as.POSIXct(timestamp, origin = "1970-01-01 00:00:00",tz = "GMT"))) %>% 
  mutate(rating_year = year(rating_time)) %>%
  select(-timestamp)

## Calculate the weeks elapsed
validation <- validation %>% left_join(first_sum, by = "movieId")
validation <- validation %>%
  mutate(weeks_elapsed = as.numeric(round((rating_time - first_rating_time)/7,0)))

## Append effects to validation
validation <- validation %>% left_join(b_u_sum, by = "userId") %>%
  left_join(b_t_sum, by = "weeks_elapsed") %>%
  left_join(b_g_sum, by = "genres")%>%
  left_join(movie_sum, by = "movieId")

## Check for NAs in validation set by displaying a table
knitr::kable(data.frame(NAs = colSums(is.na(validation[,12:14]))),
             caption = "Table 3.6. Check for NA values in validation set") %>% 
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

## Convert NAs to mean of time effect in validation
validation$b_t[is.na(validation$b_t)] <- mean(validation$b_t, na.rm = T)

## Combine effects for final predicted ratings
validation <- validation %>%
  mutate(predicted_rating = mu_movie + b_u + b_t + b_g)

## Create and display the final RMSE table
data_frame(method = "Movie, user, time, genre effect model",
           RMSE = RMSE(validation$rating, validation$predicted_rating)) %>%
  knitr::kable(caption = "Table 3.7. Final RMSE evaluation",
               row.names = FALSE,
               digits = 4) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

## Print final RMSE
RMSE(validation$rating, validation$predicted_rating)
