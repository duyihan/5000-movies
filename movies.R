
library(ggplot2)
library(ggthemes)
library(reshape2)
library(taRifx)
library(igraph)



library(dplyr)
library(magrittr)
library(lubridate)
library(tidyr)
library(plotly)
library(shiny)
library(DT)
library(reshape2)
library(splitstackshape)
library(stringr)
library(corrplot)
library(corrr)




setwd("~/EISTI/proj/kaggle")

# Read .csv file
movies <- read.csv("movie_metadata.csv", header = T, stringsAsFactors = F)


### Data cleaning ####

# Remove instances which have at least one NA variable
movies <- movies[complete.cases(movies), ]
# Remove instances which are duplicated (duplicated based on title)
movies <- movies[!duplicated(movies$movie_title),]
movies <- movies[, c("movie_title", "gross", "imdb_score", "plot_keywords","title_year")]
# Function to remove ??, leading and trailing whitespace from movies$movie_title



plot(table(movies$title_year))
plot(table(movies$imdb_score))




### Keywords Analysis ####

### Plot Keywords ####

movies0 <- movies[movies$plot_keywords != "", ]
keywords <- c()
i <- 1
for (ins in movies0$plot_keywords){
  kw <- strsplit(ins, "[|]")
  if (length(kw) != 0){
    for (word in kw[[1]]){
      if (!(word %in% keywords)){
        keywords[i] <- word
        i = i + 1
      }
    }
  }
}
# Create a dataframe with logical values which 
# indiacte the keywords of each movie
movies0$plot_keywords <- strsplit(movies0$plot_keywords, "[|]")
keywords_idx <- movies0[, c("movie_title", "plot_keywords")]
i = 1
mat <- matrix(rep(0, (dim(movies0)[1] * length(keywords))), nrow = dim(movies0)[1])
for (word in keywords_idx$plot_keywords){
  idx <- which(keywords %in% word)
  mat[i, idx] <- 1
  i = i + 1
}
colnames(mat) <- keywords
movies_and_keywords <- data.frame(mat)

# Find how many movies belong in each keyword
sum <- rep(0, length(keywords))
for (i in 1:length(keywords)){
  sum[i] <- sum(movies_and_keywords[, i])
}
keywords_sum <- data.frame(keywords = factor(keywords), sum = sum)
keywords_sum <- keywords_sum[order(sum, decreasing = FALSE),]
keywords_sum$keywords <- factor(keywords_sum$keywords, levels = keywords_sum$keywords)
#keywords_sum <- keywords_sum[keywords_sum$sum > 39, ]
keywords_sum <- keywords_sum[(dim(keywords_sum)[1]-19):dim(keywords_sum)[1] ,]

# Number of most popular keywords
ggplot(keywords_sum, aes(x = keywords, y = sum, fill = keywords)) + 
  geom_bar(stat = "identity", colour = "black") + 
  coord_flip() +
  labs(title = "Most popular keywords", x = "", y = "") + 
  geom_text(aes(label = sum), hjust = -0.2, vjust = 0.4) + 
  theme_few() +
  theme(legend.position = "None") +
  theme(axis.text.x=element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank()) 


pie <- ggplot(keywords_sum$sum, labels = keywords_sum$keywords,aes(x ="", y = keywords_sum$sum, fill = keywords_sum$keywords))

df <- data.frame(group = keywords_sum$keywords,value = keywords_sum$sum)


ggplot(data.frame(group = keywords_sum$keywords,value = keywords_sum$sum), aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")+ 
  coord_polar("y", start=0)




## Create an appropriate dataframe with gross, imdb_score and keywords    for each movie
movies_and_keywords <- cbind(gross = movies0$gross, score = movies0$imdb_score, movies_and_keywords, stringsAsFactors = FALSE)
movies_and_keywords <- melt(movies_and_keywords, id = c("gross", "score"))
movies_and_keywords$variable <- gsub("[.]", " ", movies_and_keywords$variable)
movies_and_keywords <- movies_and_keywords[movies_and_keywords$value == 1, ] 
movies_and_keywords$value <- NULL
colnames(movies_and_keywords) <- c("gross", "score", "keywords")
movies_and_keywords$keywords <- factor(movies_and_keywords$keywords, levels = keywords_sum$keywords)
movies_and_keywords <- movies_and_keywords[complete.cases(movies_and_keywords), ]

# Boxplot of keywords and profit
ggplot(movies_and_keywords, aes(keywords, gross, fill = keywords)) + 
  geom_boxplot() + 
  coord_flip() +
  labs(title = "Gross revenue of movies", x = "", y = "") + 
  theme_bw() +
  theme(legend.position = "None") +
  theme(axis.ticks.y = element_blank(), panel.grid.major.y = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) + 
  scale_y_log10(breaks = c(1, 1e+02, 1e+04, 1e+06, 1e+08, 1e+10), 
                labels = c("1", "100", "10k", "1m", "100m", "10b"), limits = c(1, 1e+10)) 

# Scatter plots of keywords based on gross and imdb score
ggplot(movies_and_keywords, aes(x = score, y = gross, colour = keywords)) + 
  geom_jitter(alpha = 0.2) +
  scale_y_log10(breaks = c(1, 1e+02, 1e+04, 1e+06, 1e+08, 1e+10), 
                labels = c("1", "100", "10k", "1m", "100m", "10b"), limits = c(1, 1e+10)) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), limits = c(1, 10)) +
  labs(title = "Gross Revenue and IMDB score", y = "Gross Revenue", x = "IMDB Score") +
  facet_wrap(~ keywords, nrow = 4) +
  theme_bw() + 
  theme(legend.position = "None") +
  annotation_logticks(sides = "lr", colour = "gray") 

movies_and_keywords_stats <- as.data.frame(by(movies_and_keywords$score, movies_and_keywords$keywords, mean))
movies_and_keywords_stats$mean_gross <- as.data.frame(by(movies_and_keywords$gross, movies_and_keywords$keywords, mean))$value
colnames(movies_and_keywords_stats) <- c("keywords", "mean_score", "mean_gross")

# Scatter plot of mean score and mean gross revenue of most popular keywords

ggplot(movies_and_keywords_stats, aes(x = mean_score, y = mean_gross)) + 
  geom_point(alpha = 1, colour = "black", shape = 21, size = 2, fill = "darkgreen") +
  scale_y_continuous(breaks = c(3e+07, 4e+07, 5e+07, 6e+07, 7e+07), 
                     labels = c("30", "40", "50", "60", "70"), limits = c(2.9e+07, 7.1e+07)) +
  scale_x_continuous(breaks = c(6, 6.2, 6.4, 6.6, 6.8, 7), limits = c(5.9, 7.1)) +
  labs(title = "Mean gross Revenue and mean IMDB score", y = "Gross Revenue (in millions)", x = "IMDB Score") +
  geom_text(aes(label = keywords), hjust = 0, nudge_x = 0.015) +
  theme_bw() + 
  theme(legend.position = "None")


####


Score_year <- ggplot(movies, aes(x=title_year, y=imdb_score))+ geom_point(alpha = 1/5) + geom_smooth() 
ggplotly(Score_year)

Score_gross <- ggplot(movies, aes(x=gross, y=imdb_score))+ geom_point(alpha = 1/5) + geom_smooth() 
ggplotly(Score_gross)

####################################################################################################

for(i in 1:nrow(movies)){
  movies[i, "movies_Age"] <- (today() %>% year()) - movies[i, "title_year"]
}

movies$`Number of User Reviews` <- movies$num_user_for_reviews 
movies <- select(movies, -num_user_for_reviews)

movies$movies_title %<>% stringr::str_replace("??", replacement="") 

Score_Dur <- ggplot(movies, aes(x=duration, y=imdb_score,
                                text=paste("movies Title: ", movies$movies_title, "<br>Duration in Hours and Minutes: ",
                                           duration %>%
                                             minutes() %>%
                                             period_to_seconds() %>%
                                             seconds_to_period() %>%
                                             as.character() %>%
                                             stringr::str_replace(" 0S", replacement = ""),
                                           sep=""))) +
  geom_point(size=1, aes(colour=`Number of User Reviews`)) +
  scale_colour_gradientn(colours = rev(rainbow(6))) +
  theme(plot.title = element_text(face = "bold")) +
  labs(title = "Duration Vs. IMDB Score (and Number of User Reviews)", 
       x = "Duration", y = "IMDB Score")

ggplotly(Score_Dur, hoverinfo="text")



Score_Age <- ggplot(movies, aes(x=movies_Age, y=imdb_score))+ geom_point(alpha = 1/5) + geom_smooth() 
ggplotly(Score_Age)


movies_RM_NA <- movies
movies_RM_NA %<>% remove_missing()

####################################################################################################

#select numeric columns
nums <- sapply(movies_RM_NA, is.numeric)
#and here you have "the most important correlations" for variable cases excluding character variables
movies_Corr <- movies_RM_NA[,nums]  %>% correlate() %>% focus(imdb_score) %>% filter(imdb_score > 0.1 | imdb_score < -0.1)


MC <- movies_RM_NA[,nums]
movies_Corr_All <- MC[, c("imdb_score", unique(movies_Corr$rowname))] %>% cor()
corrplot(movies_Corr_All, method= "circle", main = "Correlation Matrix")


