
library(ezids)
library(ggplot2)
games=read.csv("Video games sales.csv")
head(games,5)



games<-na.omit(games)
unique(games$Rating)
games$Rating <- ifelse( games$Rating== "", NA,games$Rating )
unique(games$Rating)
games<-subset(games,games$Rating!="NA")
games<-subset(games,games$Year_of_Release!="N/A")
nrow(games)

ggplot(data=games, mapping=aes(x=Global_Sales)) +
  geom_histogram(fill = "pink", bins = 20) +
  labs(
    title = "Distribution of Global Sales",
    x = "Global Sales",
    y = "Frequency"
  )

ggplot(data = games, mapping = aes(y = Global_Sales)) +
  geom_boxplot(fill = "pink") +
  labs(
    title = "Boxplot of Global Sales",
    y = "Global Sales"
  )



outlierKD2(games,Global_Sales,rm=FALSE,boxplt=TRUE,histogram = TRUE,qqplt=TRUE)



outlierKD2(games,Critic_Score,rm=FALSE,boxplt=TRUE,histogram = TRUE,qqplt=TRUE)

outlierKD2(games,User_Score,rm=FALSE,boxplt=TRUE,histogram = TRUE,qqplt=TRUE)


ggplot(data = games, aes(x = reorder(Genre, -table(Genre)[Genre]), fill = Genre)) +
  geom_bar() +
  labs(title = "Frequency Distribution of Genres",
       x = "Genre",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data = games, aes(x = reorder(Platform, -table(Platform)[Platform]), fill = Platform)) +
  geom_bar() +
  labs(title = "Frequency Distribution of Platforms",
       x = "Platform",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


