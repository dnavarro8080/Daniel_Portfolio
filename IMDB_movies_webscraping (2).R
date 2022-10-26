#Have the ratings for the best Oscar Picture Winning movies become harsher or have they lessen throughout the years?
#What years have the best rated movies?
#Best Picture Winning (94 titles)

library(rvest)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(corrplot)

#Website being used
link = "https://www.imdb.com/search/title/?count=100&groups=oscar_best_picture_winners&sort=year%2Cdesc&ref_=nv_ch_osc"
page = read_html(link)

#-----------------------------------------------------------------------------------------------------------------------------------
#Data Variables from each movie
title = page %>% html_nodes('.lister-item-header a') %>% html_text()

movie_links = page %>% html_nodes('.lister-item-header a') %>% 
  html_attr('href') %>% paste('https://www.imdb.com', ., sep='')

year = page %>% html_nodes('.text-muted.unbold') %>% html_text()
year <- gsub('\\(|)|I', "", year) %>% as.numeric()

duration <- page %>% html_nodes(".runtime") %>% html_text()
duration <- gsub("min","", time) %>% as.numeric()

rating = page %>% html_nodes('.ratings-imdb-rating strong') %>% html_text() %>% as.numeric()

genres <- page %>% html_nodes(".genre") %>% html_text()
genres <- gsub("\n|,.*|' '", "", genres)

synopsis = page %>% html_nodes('.ratings-bar+ .text-muted') %>% html_text()

#DataFrame to fill with the data pulled from imdb.com
imdb_movies = data.frame(title, year, duration, rating, genres, synopsis)

#Visualizations--------------------------------------------------------------------------------------------------------------------------

#Barplot to determine the distribution of ratings
barplot(table(imdb_movies$rating),xlab = "Ratings", ylab = "Count", main = "Distribution of Ratings", col = 3, axis.lty = 1)


#Average of all the ratings.
ggplot(imdb_movies,aes(x=year,y=rating))+
  geom_boxplot(outlier.shape = NA)+
  labs(x="Year",y="Rating",title="Ratings distributed throughout the Years")

ggplot(imdb_movies, aes(y=rating, x=genres)) + geom_boxplot() 

ggplot(imdb_movies, aes(y=duration, x=genres)) + geom_boxplot() 

ggplot(imdb_movies, aes(genres)) + geom_bar() + theme_linedraw()

plot(imdb_movies$rating~imdb_movies$year,xlab='Year',ylab='Rating',main='Rating as a function of Year')
plot(imdb_movies$duration~imdb_movies$year,xlab='Year',ylab='Duration',main='Duration as a function of Year')
plot(imdb_movies$duration~imdb_movies$rating,xlab='Rating',ylab='Duration',main='Duration as a function of Rating')

#plot histograms to check for nomrality
hist(imdb_movies$rating, col='green',xlim=c(5,10),xlab='Movie Rating', main="Histogram for our Ratings ")

#Summary Statistics---------------------------------------------------------------------------------------------------------------------

summary(imdb_movies)

rating_mean_year = imdb_movies %>% group_by(year) %>%  summarise_at(vars("rating"), mean)
rating_std_year = imdb_movies %>% group_by(year) %>% summarise_at(vars("rating"), sd)

###########ANOVA#######################
one.way <- aov(rating ~ genres, data = imdb_movies)

summary(one.way)

#Ho: all genres have equal mean ratings
#H1: not all genres have equal mean ratings
#Decision: since our p-value is greater than our alpha = 0.01, we fail to reject our null hypothesis meaning there is not a significant difference between
#the means of the genres'.
tukey.one.way<-TukeyHSD(one.way)
tukey.one.way
tukey.plot.aov<-aov(rating ~ genres, data=imdb_movies)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)
#There are statistically significant differences (p < 0.05)








