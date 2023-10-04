
library(ezids)
library(ggplot2)
games=read.csv("C:/Users/saira/OneDrive/Desktop/GWU Courses/Intro to DS/Project 1/Datasets/Games.csv")
head(games,5)



library(dplyr)

plat_na= games %>%
  group_by(Platform) %>%
  summarize(NAs_in_User_Score = sum(is.na(User_Score)))

print(plat_na)

