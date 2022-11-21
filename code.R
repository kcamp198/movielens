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


head(edx)
