## Adrian Zinovei
## MovieLens Project Code for the HarvardX: PH125.9x - Capstone Project
## https://github.com/azinovei

# Summary code by parts:
# Part 1: INTRO: Initial Data loading from the Data Science EDX trainings        #
# Part 2: DATA ANALYSIS:  Checking, structuring and plotting basic data          #
### PART 3: MODEL SELECTION:  (1) Average movie rating model                     #
### PART 3: MODEL SELECTION:  (2) The movie effect model                         #
### PART 3: MODEL SELECTION: (3) Movie and user effect model                     #
### PART 3: MODEL SELECTION: (4) Regularized movie and user effect model         #
# Part 4: END: # Final computations, fine-tuning and results check               #

###########################################################################
# Part 1: INTRO: Initial Data loading from the Data Science EDX trainings #
###########################################################################
# Create edx data set, validation set, and submission file according to the provided script
# The code section below might take several minutes to run depending on your system characteristics and internet speed.
# To facilitate the reproduction of the results we will keep all the objects generated within the session of this script
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")

# 10% of the MovieLens data for validation. #
set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]
#Check  userId and movieId in validation set are in edx subset:#
validation <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set#
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
rm(dl, ratings, movies, test_index, temp, movielens, removed)
############################################################
# Part 2: DATA ANALYSIS:  Checking, structuring and plotting basic data  #
############################################################
# Head
head(edx) %>%
  print.data.frame()
# Unique movies and users #
summary(edx)
# Unique movies and users in the edx dataset #
edx %>%  summarize(nr_users = n_distinct(userId),     nr_movies = n_distinct(movieId))
# Ratings distribution across 
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.25, color = "blue") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000))) +
  ggtitle("Rating distribution")

# Nr of ratings per movie
edx %>%  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "blue") +
  scale_x_log10() +
  xlab(" # ratings") +
  ylab(" # movies") +
  ggtitle("Ratings/movie")

# Movie only rated once
edx %>%  group_by(movieId) %>%
  summarize(count = n()) %>%
  filter(count == 1) %>%
  left_join(edx, by = "movieId") %>%
  group_by(title) %>%
  summarize(rating = rating, n_rating = count) %>%
  slice(1:15) %>%
  knitr::kable()

# Nr of ratings given by users (plot)
edx %>%  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "blue") +
  scale_x_log10() +
  xlab("#  ratings") + 
  ylab("#  users") +
  ggtitle("# ratings given by users")

# Mean movie rating from users (plot) 
edx %>%  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "blue") +
  xlab("Mean ratings") +
  ylab("# users") +
  ggtitle("Mean movie rating from users ") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  theme_light()
######################################################
## PART 3: MODEL SELECTION: (1) Average movie rating model ##
######################################################
# Calculating the mean rating by computing through all edx dataset #
mean_of_ratings<- mean(edx$rating)
mean_of_ratings
# Test results based on simple prediction
test_naive <- RMSE(validation$rating, mean_of_ratings)
test_naive

# export prediction in data frame
rmse_results <- data_frame(method = "Average movie rating model", RMSE = test_naive)
rmse_results %>% knitr::kable()
######################################################
## PART 3: MODEL SELECTION: (2) The movie effect model         ##
######################################################

## The movie effect model ##
# Simple model taking into account the movie effect b_i
# Subtract the rating minus the mean for each rating the movie received
# Plot number of movies with the computed b_i
movie_averages <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mean_of_ratings))
movie_averages %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("blue"),
                         ylab = "# of movies", main = "# of movies computed with b_i")



# Test and save RMSE results 
forecasted_ratings <- mean_of_ratings+  validation %>%
  left_join(movie_averages, by='movieId') %>%
  pull(b_i)
1st_model_rmse <- RMSE(forecasted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie effect model",  
                                     RMSE = 1st_model_rmse ))
# Check results
rmse_results %>% knitr::kable()
######################################################
## PART 3: MODEL SELECTION: (3) Movie and user effect model ##
######################################################
# Plot penalty term user effect #
averages_per_user<- edx %>% 
  left_join(movie_averages, by='movieId') %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating - mean_of_ratings- b_i))
averages_per_user%>% qplot(b_u, geom ="histogram", bins = 30, data = ., color = I("black"))


averages_per_user <- edx %>%
  left_join(movie_averages, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mean_of_ratings- b_i))


# Test and save rmse results 
forecasted_ratings <- validation%>%
  left_join(movie_averages, by='movieId') %>%
  left_join(averages_per_user, by='userId') %>%
  mutate(pred = mean_of_ratings+ b_i + b_u) %>%
  pull(pred)

2nd_model_rmse  <- RMSE(forecasted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie and user effect model",  
                                     RMSE = 2nd_model_rmse ))

# Check result
rmse_results %>% knitr::kable()
################################################################
## PART 3: MODEL SELECTION: (4) Regularized movie and user effect model ##
################################################################

# lambda is a tuning parameter
# Use cross-validation to choose it.
lambdas <- seq(0, 10, 0.25)


# For each lambda,find b_i & b_u, followed by rating prediction & testing
# note:the below code could take some time  
all_rmses <- sapply(lambdas, function(l){
  
  mean_of_ratings<- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mean_of_ratings)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mean_of_ratings)/(n()+l))
  
  forecasted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mean_of_ratings+ b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(forecasted_ratings, validation$rating))
})
# Plot all_rmses vs lambdas to select the optimal lambda                                                             
qplot(lambdas, all_rmses)  

# The optimal lambda for all rmses                                                      
lambda <- lambdas[which.min(all_rmses)]
lambda

# Test and save results                                                             
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized movie and user effect model",  
                                     RMSE = min(all_rmses)))
###############################################################
# Part 4: END: # Final computations, fine-tuning and results check                    #
###############################################################
rmse_results %>% knitr::kable()
# RMSE final results overview                                                          
rmse_results %>% knitr::kable()
# Settings printing #
print("Operating System Settings Adrian Zinovei’s Notebook:")
version
