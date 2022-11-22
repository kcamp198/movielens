##### Create edx set, validation set (final hold-out test set) #####

# Note: this process could take a couple of minutes

options(repos = list(CRAN="http://cran.rstudio.com/"))

knitr::opts_chunk$set(message = FALSE)

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(kableExtra)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

download.file("https://www.dropbox.com/s/nspymeso8rmmak1/edx.rds?dl=1", "edx.rds", mode="wb")

download.file("https://www.dropbox.com/s/x0s477b0kzxpl6i/validation.rds?dl=1", "validation.rds", mode="wb")

edx = readRDS("edx.rds")

validation = readRDS("validation.rds")

##### Background and exploratory data analysis #####
# Take a look at sparsity using a random sample of our matrix of users, movies
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

# Visualize the distribution of ratings per movie
edx %>%
dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

edx %>%
  dplyr::count(movieId) %>%
  describe(fast = TRUE) %>%
  select(-vars) %>%
  slice(2) %>%
  knitr::kable(caption = "Table 2.4. Summary statistics for ratings",
                                         row.names = FALSE)

edx %>%
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(fill = "steel blue", color = "white") +
  theme_classic() +
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25)) + 
  labs(x = "Number of ratings", y = "Count of unique movies") + 
  scale_x_log10()

# Visualize that different users rate different numbers of movies
edx %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  ggtitle("Users")

##### Generate predicted movie ratings and calculate RMSE #####

edx_summary <- data.frame(Rows = nrow(edx),
                          Columns = ncol(edx),
                          Users = n_distinct(edx$userId),
                          Movies = n_distinct(edx$movieId),
                          Average_rating = round(mean(edx$rating),2),
                          Number_of_genres = n_distinct(edx$genres),
                          Date_of_first_rating = 
                            as.Date(as.POSIXct(min(edx$timestamp), 
                                               origin = "1970-01-01")),
                          Date_of_last_rating = 
                            as.Date(as.POSIXct(max(edx$timestamp),
                                               origin = "1970-01-01")))

install.packages("kableExtra")
library(kableExtra)

edx %>% slice(1:10) %>% knitr::kable() %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("scale_down","HOLD_position"))

edx_summary[,1:6] %>% 
  knitr::kable()

min(nchar(edx$genres))
which.min(nchar(edx$genres))
edx$genres[7799]

sum(str_count(edx$genres, "(no genres listed)"))
n_distinct(edx$genres)
unique(edx$genres)
match("(no genres listed)", edx$genres)
edx$genres[922544]
edx$title[922544]
edx[922544]
edx %>% filter(genres == "(no genres listed)")

#edx <- edx %>%
 # mutate(date = as.Date(as.POSIXct(timestamp,origin="1970-01-01 00:00:00",tz = "GMT"))
  #       ,year = as.numeric(substr(date,1,4))) %>%
  #select(-timestamp)

edx <- edx %>% 
  mutate(rating_time = as.Date(as.POSIXct(timestamp, origin = "1970-01-01 00:00:00",tz = "GMT"))) %>% 
  mutate(rating_year = year(rating_time)) %>%
  select(-timestamp)

edx <- edx %>%
  mutate(release_year = as.integer(substr(title, str_length(title) - 4,
                                          str_length(title) - 1)))

head(edx)

min(edx$rating)

#summary table of ratings
install.packages("psych")
library(psych)
describe(edx$rating, fast = TRUE) %>% knitr::kable()

describe(edx$rating, fast = TRUE) %>% select(-vars)

#create a summary table grouping by rating
rating_sum <- edx %>% group_by(rating) %>%
  summarize(count = n())
rating_sum

rating_sum <- edx %>% group_by(rating) %>%
  summarize(count = n())

rating_sum %>% mutate(rating = factor(rating)) %>%
  ggplot(aes(rating, count)) +
  geom_col(fill = "steel blue", color = "white") +
  theme_classic() + 
  labs(x = "Rating", y = "Count",
       title = "Number of rating",
       caption = "Figure 1 - Rating in edx dataset")


edx %>%
  dplyr::count(userId) %>%
  describe(fast = TRUE) %>%
  select(-vars) %>%
  slice(2) %>%
  knitr::kable(caption = "Table 2.5. Summary statistics for users",
               row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("HOLD_position"))

release_year_sum <- edx %>% group_by(release_year) %>%
  summarize(n = n(), average_rating = mean(rating))

fit_lm1 <- lm(average_rating ~ I(release_year^2) +
               I(release_year), 
             data = release_year_sum)

fit_lm2 <- lm(average_rating ~ I(release_year^3) + I(release_year^2) +
               I(release_year), 
             data = release_year_sum)


install.packages("jtools")
library(jtools)
install.packages("huxtable")
library(huxtable)
export_summs(fit_lm1, fit_lm2, scale = TRUE)

# Calculate the first rating time of each movie
first_sum <- edx %>% group_by(movieId) %>%
  summarize(first_ratings = n(),
            first_mu = mean(rating),
            first_rating_time = min(rating_time))

# Calculate the aging time
edx <- edx %>% left_join(first_sum, by = "movieId")
edx <- edx %>%
  mutate(aging_time = as.numeric(round((rating_time - first_rating_time)/7,0)))
head(edx)
max(edx$aging_time)

genres <- str_replace(edx$genres, "\\|.*","")
genres <- genres[!duplicated(genres)]
genres %>%
  knitr::kable(caption = "Table 2.7. List of unique genres in edx",
               col.names = "Genre",
               row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("HOLD_position"))



n_genres <- sapply(genres, function(g){
  index <- str_which(edx$genres, g)
  length(edx$rating[index])
  
})

# Calculate the average rating by genre
genres_rating <- sapply(genres, function(g){
  index <- str_which(edx$genres, g) 
  mean(edx$rating[index], na.rm = T)
})

# Create summary table by genres
genres_sum <- data.frame(Genre = genres, 
                         Movies = n_genres,
                         "Average rating" = genres_rating,
                         check.names = FALSE)

# Display summary table by genres
genres_sum %>% arrange(desc(Movies)) %>% slice(1:5) %>%
  knitr::kable(caption = "Table 2.8. Most common genres in edx",
               digits = 2,
               row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("HOLD_position"))

# Plot average rating by genre
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

genres_sum

head(edx)

movie_sum <- edx %>% group_by(movieId) %>%
  summarize(mu_movie = mean(rating))

movie_sum
edx <- left_join(edx, movie_sum, by = "movieId")
head(edx)

rmses <- data_frame(method = "Only estimate is the average",
                    RMSE = RMSE(mean(edx$rating), edx$rating))

rmses %>%
  knitr::kable(caption = "Table 3.1. RMSE by method",
               row.names = FALSE,
               digits = 4) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

RMSE(edx$mu_movie, edx$rating)


b_u_sum <- edx %>% mutate(yhat = rating - mu_movie) %>%
  group_by(userId) %>%
  summarize(b_u = mean(yhat))

edx <- left_join(edx, b_u_sum, by = "userId") %>%
  mutate(mu_movie_user = mu_movie + b_u)
# Append the row to the RMSE table
rmses <- bind_rows(rmses,
                   data_frame(method = "Add user–specific effect",
                              RMSE = RMSE(edx$mu_movie_user, edx$rating)))
# Display the RMSE table
rmses %>%
  knitr::kable(caption = "Table 3.2. RMSE by method II",
               row.names = FALSE,
               digits = 4) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

b_t_sum <- edx %>% mutate(error = rating - mu_movie_user) %>%
  group_by(aging_time) %>%
  summarize(b_t = mean(error))

edx <- left_join(edx, b_t_sum, by = "aging_time") 

# Convert NAs to 0
edx$b_t[is.na(edx$b_t)] <- 0

edx <- edx %>%
  mutate(mu_movie_user_time = mu_movie_user + b_t)

b_g_sum <- edx %>% mutate(error = rating - mu_movie_user_time) %>%
  group_by(genres) %>%
  summarize(b_g = mean(error))

edx <- left_join(edx, b_g_sum, by = "genres") 

edx <- edx %>%
  mutate(mu_movie_user_time_genres = mu_movie_user_time + b_g)



# Update validation with timestamp converted to date format
validation <- validation %>% 
  mutate(rating_time = as.Date(as.POSIXct(timestamp, origin = "1970-01-01 00:00:00",tz = "GMT"))) %>% 
  mutate(rating_year = year(rating_time)) %>%
  select(-timestamp)

# Calculate the aging time
validation <- validation %>% left_join(first_sum, by = "movieId")
validation <- validation %>%
  mutate(aging_time = as.numeric(round((rating_time - first_rating_time)/7,0)))

# Append effects to validation
validation <- validation %>% left_join(b_u_sum, by = "userId") %>%
  left_join(b_t_sum, by = "aging_time") %>%
  left_join(b_g_sum, by = "genres") %>%
  left_join(movie_sum, by = "movieId")

head(validation)

# Check validation set for NA values
knitr::kable(data.frame(n_NA = colSums(is.na(validation[,12:14]))),
      caption = "NA value check in validation set", 
      digits = 0) %>% 
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

# Combine effects for final predicted ratings
validation <- validation %>%
  mutate(predicted_rating = mu_movie + b_u + b_t + b_g)

# Create and display the final RMSE table
data_frame(method = "Movie, user, time, genre effect model",
           RMSE = RMSE(validation$rating, validation$predicted_rating)) %>%
  knitr::kable(caption = "Table 3.6. Final RMSE evaluation",
               row.names = FALSE,
               digits = 4) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

validation$b_t.x[is.na(validation$b_t.x)] <- mean(validation$b_t.x, na.rm = T)

validation <- validation %>%
  mutate(predicted_rating = mu_movie + b_u.x + b_t.x + b_g.x)

RMSE(validation$rating, validation$predicted_rating)


movie_sum <- edx %>% group_by(movieId) %>%
  summarize(mu_movie = mean(rating))
