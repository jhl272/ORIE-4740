# Final Project

library(tidyverse)

# Initial SetUp
setwd("~/ORIE 4740 Final Project")
songsdata <- read.csv("Master.csv")
colnames(songsdata)
songs <- select(songsdata, artist_name, title, acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, tempo, valence, popularity) 
View(songs)

sum(is.na(songs))
songs <- na.omit(songs)

# Linear Regression
lm.fit = lm(popularity~ .-artist_name -title, data=songs)
summary(lm.fit)
names(lm.fit)

# Most important predictors are: acousticness, instrumentalness, speechiness, valence
residual = array()

for (i in c(1:10)) {
  lm.fit2 = lm(popularity ~ poly(acousticness,i) + poly(instrumentalness, i) + poly(speechiness, i) + poly(valence, i), data=songs)
  residual[i] = lm.fit2$df.residual
}
plot(residual)