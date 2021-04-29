
install.packages("funModeling")
install.packages("data.table")
install.packages("kableExtra")
install.packages("radarchart")
install.packages("fmsb")
install.packages("DT")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(corrplot)
library(factoextra)
library(plyr)
library(funModeling)
library(DT)
library(data.table)
library(radarchart)
library(kableExtra)
library(fmsb)


# Get the Data

spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

# Or read in with tidytuesdayR package (https://github.com/thebioengineer/tidytuesdayR)
# PLEASE NOTE TO USE 2020 DATA YOU NEED TO UPDATE tidytuesdayR from GitHub

# Either ISO-8601 date or year/week works!

# Install via devtools::install_github("thebioengineer/tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2020-01-21') 
tuesdata <- tidytuesdayR::tt_load(2020, week = 4)

spotify_songs <- tuesdata$spotify_songs

# ---------
# Data cleaning

# To view rhe summary statics for all the columns of spotify_songs
summary(spotify_songs)

# to verify the NA.
any(is.na(spotify_songs))

# to see how many NA
sum(is.na(spotify_songs))

# To remove the NA
spotify_songs_clean <- spotify_songs %>% 
  filter(!is.na(track_name) & !is.na(track_artist) & !is.na(track_album_name))

#Removing unnecessary columns
spotify_songs_clean <- spotify_songs_clean%>%dplyr::select(-track_id,-track_album_id,-playlist_id)
summary(spotify_songs_clean)

# Revisit the cleaned dataset
dim(spotify_songs_clean)


# ---------

boxplot(spotify_songs_clean$danceability, 
        spotify_songs_clean$energy,
        spotify_songs_clean$key,
        spotify_songs_clean$loudness,
        spotify_songs_clean$mode,
        spotify_songs_clean$speechiness,
        spotify_songs_clean$acousticness,
        spotify_songs_clean$instrumentalness,
        spotify_songs_clean$liveness,
        spotify_songs_clean$valence,
        spotify_songs_clean$tempo,
        spotify_songs_clean$duration_ms,
        horizontal=TRUE)

boxplot(spotify_songs_clean$danceability, horizontal=TRUE)

# ---------

#Outlier with boxplot
speech_with_outiner <-spotify_songs_clean %>%
  ggplot(aes(y = speechiness, coef = 4)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = 'Speechiness, with outliers')

#removing outliner
speechiness_outliers <- boxplot(spotify_songs_clean$speechiness, 
                                plot = FALSE, range = 4)$out

playlist_songs_no_outliers <- spotify_songs_clean %>%
  filter(!speechiness %in% speechiness_outliers) 

# without outlier boxplot 
playlist_songs_no_outliers %>%
  ggplot(aes(y = speechiness)) +
  geom_boxplot(coef = 4) +
  coord_flip() +
  labs(title = 'Speechiness, outliers removed') 

length(speech_with_outiner) - length(playlist_songs_no_outliers)

# ---------

colnames(spotify_songs_clean)
spoti_new <- spotify_songs_clean [, c(1,2,3,4,5,6,7,8,11,13,16,9,10,12,14,15,17,18,19,20)]

colnames(spoti_new)

feature_names <- names(spoti_new) [12:20]

spotify_songs_clean %>%
  select(c('playlist_genre', feature_names)) %>%
  pivot_longer(cols = feature_names) %>%
  ggplot(aes(x = value)) +
  geom_density(aes(color = playlist_genre), alpha = 0.5) +
  facet_wrap(~name, ncol = 3, scales = 'free') +
  labs(title = 'Spotify Audio Feature Density - by Genre',
       x = '', y = 'density') +
  theme(axis.text.y = element_blank())


# ---------

# Radar chart

radar_chart <- function(arg){
  songs_clean_filtered <- songs_clean %>% filter(playlist_genre==arg)
  radar_data_v1 <- songs_clean_filtered %>%
    select(danceability,energy,loudness,speechiness,valence,acousticness)
  radar_data_v2 <- apply(radar_data_v1,2,function(x){(x-min(x)) / diff(range(x))})
  radar_data_v3 <- apply(radar_data_v2,2,mean)
  radar_data_v4 <- rbind(rep(1,6) , rep(0,6) , radar_data_v3)
  return(radarchart(as.data.frame(radar_data_v4),title=arg))
}

par(mfrow = c(2, 3))
Chart_edm<-radar_chart("edm")
Chart_pop<-radar_chart("pop")
Chart_rb<-radar_chart("r&b")
Chart_latin<-radar_chart("latin")
Chart_rap<-radar_chart("rap")
Chart_rock<-radar_chart("rock")

# ---------


ggplot(spotify_songs_clean, aes(x = energy, y = loudness, color = playlist_genre)) +
  geom_point(alpha = 0.5, position = "jitter")

coldplay = spotify_songs_clean %>%
  filter(track_artist == "Coldplay")

ggplot(coldplay, aes(x = energy, y = loudness, color = playlist_genre)) +
  geom_point(alpha = 0.5, position = "jitter")

enriq = spotify_songs_clean %>%
  filter(track_artist == "Enrique Iglesias")

ggplot(spotify_songs_clean, aes(x = energy, y = danceability, color = playlist_genre)) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(enriq, aes(x = energy, y = danceability, color = playlist_genre)) +
  geom_point(alpha = 0.5, position = "jitter")


maro = spotify_songs_clean %>%
  filter(track_artist == "Maroon 5")

ggplot(maro, aes(x = energy, y = danceability, color = playlist_genre)) +
  geom_point(alpha = 0.5, position = "jitter")


# ---------

#Proportion of playlist genres
songs_clean_plot_data<-spotify_songs %>% 
  group_by(playlist_genre) %>% 
  summarise(Total_number_of_tracks = length(playlist_genre))

#Drawing the diagram
ggplot(songs_clean_plot_data, aes(x=playlist_genre, y=Total_number_of_tracks)) + 
  geom_bar(width = 0.5, stat = "identity") +
  geom_text(aes(label = paste(round(Total_number_of_tracks / sum(Total_number_of_tracks) * 100, 1), "%")),
            position = position_stack(vjust = 1.05)) +
  theme(axis.text.x = element_text(angle = 90))

#Reorder 
ggplot(songs_clean_plot_data, aes(x= reorder(playlist_genre, -Total_number_of_tracks), y=Total_number_of_tracks)) + 
  geom_bar(width = 0.5, stat = "identity") +
  geom_text(aes(label = paste(round(Total_number_of_tracks / sum(Total_number_of_tracks) * 100, 1), "%")),
            position = position_stack(vjust = 1.05)) +
  theme(axis.text.x = element_text(angle = 90))

# pie chart
ggplot(songs_clean_plot_data, aes(x="", y=Total_number_of_tracks, fill=playlist_genre)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste(round(Total_number_of_tracks / sum(Total_number_of_tracks) * 100, 1), "%")),
            position = position_stack(vjust = 0.5))


# ---------

#Proportion of playlist subgenres
subsongs_clean_plot_data<-spotify_songs %>% 
  group_by(playlist_subgenre) %>% 
  summarise(Total_number_of_tracks = length(playlist_subgenre))

#Drawing the diagram
ggplot(subsongs_clean_plot_data, aes(x= reorder(playlist_subgenre, Total_number_of_tracks), y=Total_number_of_tracks)) + 
  geom_bar(width = 0.5, stat = "identity") +
  geom_text(aes(label = paste(round(Total_number_of_tracks / sum(Total_number_of_tracks) * 100, 1), "%")),
            position = position_stack(vjust = 1.05)) +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip()

#Drawing the diagram
ggplot(subsongs_clean_plot_data, aes(x=playlist_subgenre, y=Total_number_of_tracks)) + 
  geom_bar(width = 0.5, stat = "identity") +
  geom_text(aes(label = paste(round(Total_number_of_tracks / sum(Total_number_of_tracks) * 100, 1), "%")),
            position = position_stack(vjust = 1.05)) +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip()

# pie chart
ggplot(subsongs_clean_plot_data, aes(x="", y=Total_number_of_tracks, fill=playlist_subgenre)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste(round(Total_number_of_tracks / sum(Total_number_of_tracks) * 100, 1), "%")),
            position = position_stack(vjust = 0.5))

# ---------
# Scatter plot

ggplot(spotify_songs_clean, aes(x = energy, y = loudness, color = playlist_genre)) +
  geom_point(size = 0.5, alpha = 0.5)

ggplot(spotify_songs_clean, aes(x = energy, y = danceability, color = playlist_genre)) +
  geom_point(size = 0.5, alpha = 0.5)

ggplot(coldplay, aes(x= energy, y = loudness, color = playlist_genre)) +
  geom_point(size = 7, position = "jitter", alpha = 0.6)

ggplot(maro, aes(x= energy, y = danceability, color = playlist_genre)) +
  geom_point(size = 7, position = "jitter", alpha = 0.6)

ggplot(enriq, aes(x= energy, y = danceability, color = playlist_genre)) +
  geom_point(size = 7, position = "jitter", alpha = 0.6)

# ---------


spotify_histograms <- spotify_songs_clean [,-c(1,2,3,4,5,6,7,8,13)]
plot_num(spotify_histograms)

as.numeric(spotify_histograms$danceability)
is.numeric(spotify_histograms$danceability)
as.numeric(spotify_histograms$energy)
as.numeric(spotify_histograms$loudness)
as.numeric(spotify_histograms$speechiness)
as.numeric(spotify_histograms$acousticness)
as.numeric(spotify_histograms$instrumentalness)
as.numeric(spotify_histograms$liveness)

as.numeric(spotify_histograms)

spotify_songs_clean %>%
  select(c('playlist_genre', spotify_histograms)) %>%
  pivot_longer(cols = feature_names) %>%
  ggplot(aes(x = value)) +
  geom_density(aes(color = playlist_genre), alpha = 0.5) +
  facet_wrap(~name, ncol = 3, scales = 'free') +
  labs(title = 'Spotify Audio Feature Density - by Genre',
       x = '', y = 'density') +
  theme(axis.text.y = element_blank()) + 
  scale_color_kp(palette = 'mixed')


# ---------


# ---------

# Clustering  
install.packages("plotly")
library(tidyverse)
library(cluster)
library(factoextra)
library(tidymodels)
library(plotly)
library(janitor)
library(GGally)
library(ggplot2)

#exploratry data analysis

spotify_songs_clean%>%select(playlist_genre,
                             energy,liveness,tempo,speechiness,acousticness,
                             danceability,duration_ms,loudness,valence)%>%
  ggpairs(aes(color=playlist_genre))


# scaling
str(spotify_songs_clean)

spotify_scaled <- spotify_songs_clean%>%
  mutate(across(is.numeric,~as.numeric(scale(.))))

# Perform intitial k-means
kmeans_spoti <- kmeans(x=select(spotify_scaled, -c(1:8)), 
                       centers=12)

print(kmeans_spoti)

#Use the fviz_cluster option to visualize the clusters
#it takes two arguments one is the clustering result,
# Second is the original data-set
fviz_cluster(kmeans_spoti,
             spotify_songs_clean%>%select(-(1:8)))

# what is the optimal number of k

# method 1: within sum of squares - for the elbow plot
fviz_nbclust(spotify_scaled%>%select(-(1:8)), kmeans, method = "wss")


# method 2: silhouette method
fviz_nbclust(spotify_scaled%>%select(-(1:8)), kmeans, method = "silhouette")

#final K means with k 2
kmeans_spoti <- kmeans(x=select(spotify_scaled, -c(1:8)), 
                       centers=2)

print(kmeans_spoti)


# method 3:
# Lets do it the 'tidymodel' way and combine multiple clustering results
k_clust_investigate<-
  tibble(k= 1:10)%>%
  mutate(
    kmeans_spoti=map(k,~kmeans(spotify_scaled%>%select(-(1:8)),.x)),
    tidied = map(kmeans_spoti,tidy),
    glanced = map(kmeans_spoti,glance),
    augument = map(kmeans_spoti,augment,spotify_scaled))

k_clust_investigate%>%
  unnest(glanced)%>%
  ggplot(aes(k,tot.withinss))+
  geom_line()+
  geom_point()

final_clustering_model<-k_clust_investigate%>%
  filter(k==2)%>%
  select(augument)%>%
  unnest(augument)

#Printing the final cluster with k = 2 with energy vs danceability
inter <- ggplot(final_clustering_model,
                mapping = aes(x = energy,
                              y = danceability,
                              color = .cluster, name = track_artist)) +
  geom_point(size = 0.5, alpha = 0.5)

ggplotly(inter)

#Printing the final cluster with k = 2 with energy vs loudness
interactive_plot1<-ggplot(final_clustering_model,
                          mapping=aes(x=energy,
                                      y=loudness,
                                      color=.cluster, name = track_artist))+
  geom_point(size = 0.5, alpha = 0.5)+theme_minimal()


ggplotly(interactive_plot1)


# ---------

# Classification
install.packages("rpart")
install.packages("rpart.plot")

library(tidyverse)
library(rpart)
library(rpart.plot)


# step 1:

# step 2:
classi_spoti <- select(spotify_songs, 12:23, 10)

#turn all categorical variables into factors
str(classi_spoti)

classi_spoti <- mutate(classi_spoti, playlist_genre = factor(playlist_genre))

# Randomize the order of the rows.
shuffle_index <- sample(1:nrow(classi_spoti))
classi_spoti <- classi_spoti[shuffle_index, ]



### Split the data into a train and test set.

# Function to create a train/test split
#
#   data      Input tibble containing the full dataset
#   size      Size of the training set as proportion of total. For example, 
#             a value of 0.8 means 80% of the data goes into the train set
#   train     Are we extracting the training part of the dataset (TRUE) or the
#             test part (FALSE)?
create_train_test <- function(data, proportion = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = proportion * n_row
  train_sample <- 1:total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}


# Do the split.
proportion <- 0.8
data_train <- create_train_test(classi_spoti, proportion, train = TRUE)
data_test <- create_train_test(classi_spoti, proportion, train = FALSE)

# just checking up
prop.table(table(classi_spoti$playlist_genre))  # Share of pos/neg in complete dataset
# this can we check with the histogram we previsoly did 
# If i spilt my data, the train and test data should have a similar kind of data.
prop.table(table(data_train$playlist_genre)) # Share of pos/neg in train set
prop.table(table(data_test$playlist_genre)) # Share of pos/neg in test set
# we can see it has a similar spilting, which means the spilting is good to go!! 
# and we have divided our data proporly


### Train the decision tree algorithm.

# Set the parameters for the model.
param_settings <- rpart.control(maxdepth = 12, minbucket = 33, minsplit = 10, cp = 0)

# param_settings <- rpart.control(maxdepth = , minbucket = 10, minsplit = 10, cp = 0)

# Train the model.
fitted_model <- rpart(playlist_genre ~ ., 
                      data = data_train, 
                      method = 'class', 
                      control = param_settings)

# if we have run this command, then it has trained our dataset. 

#now lets visualize this

### Visualize the decision tree.

# The type and extra parameters control the way the learned decision tree
# is shown on screen (but not its structure, that remains the same)

# maj. label / class. rate / % of all cases
rpart.plot(fitted_model, type = 5, extra = 102, box.palette="RdGn") 

# maj. label / prob. per class / % of all cases
rpart.plot(fitted_model, extra = 104, box.palette="RdGn")

# maj. label / prob. of fitted class / % of all cases
rpart.plot(fitted_model, extra = 108, box.palette="RdGn")

# maj. label / prob. of fitted class
rpart.plot(fitted_model, extra = 08, box.palette="RdGn")



### Generate predictions on the unseen test data.

# Use the fitted model to generate predictions for the test set.
predictions <- predict(fitted_model, data_test, type = "class")

# Now lets compare tthe predictions to the real data. 
# Generate a confusion matrix.
confusion_matrix <- table(predictions, data_test$playlist_genre)
confusion_matrix

# Calculate accuracy, P, R, and F.
acc <- sum(diag(confusion_matrix)) / sum(confusion_matrix) # more efficient way
#calculating the precision 
# How often was I right. How often did I predict the positive match in total,  
P <- confusion_matrix[6,6] / sum(confusion_matrix[6, ])
# recall
R <- confusion_matrix[6,6] / sum(confusion_matrix[, 6])
F <- 2 * (P * R) / (P + R)

# the f score is .64
# this tells us how well it did

print(acc)
print(confusion_matrix)

#to see the confusion matrix in %
round((confusion_matrix/rowSums(confusion_matrix))*100,2)


### Parameter optimization.

# Make a tibble to store the results.
results <- tibble(maxdepth = numeric(), minbucket = numeric(), value = numeric())

# Loop through different values for maxdepth (1 through 20).
# (maxdepth controls how many levels the decision tree is max. allowed to have)
for (maxdepth_value in 1:20) {
  
  # Loop through different values for minbucket (2 through 50).
  # (minbucket controls how many instances each leaf node has to have at least)
  for (minbucket_value in 2:50) {
    
    # Define parameter settings with this value.
    param_settings <- rpart.control(maxdepth = maxdepth_value,   # take the value from the outer loop
                                    minbucket = minbucket_value, # take the value from the inner loop
                                    minsplit = 10, cp = 0)
    
    # Train the model using these parameter settings.
    fitted_model <- rpart(playlist_genre ~ ., 
                          data = data_train, 
                          method = 'class', 
                          control = param_settings)
    
    # Generate predictions on the test set.
    predictions <- predict(fitted_model, data_test, type = "class")
    
    # Generate the confusion matrix.
    confusion_matrix <- table(predictions, data_test$playlist_genre)
    
    # Calculate the relevant evaluation metric(s).
    acc <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    
    # Add the result to the tibble.
    results <- add_row(results, maxdepth = maxdepth_value, 
                       minbucket = minbucket_value, 
                       value = acc)
    
  }
}

print(results)


