
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



dev_nintendo=subset(games,games$Developer=='Nintendo')
head(dev_nintendo,5)

dev_others=subset(games,games$Developer!='Nintendo')
head(dev_others,5)

#USer scores

t_test_nintendo <- t.test(dev_nintendo$User_Score,dev_others$User_Score,var.equal = FALSE,conf.level=0.99) 

t_test_nintendo

#Critic scores

t_test_nintendo <- t.test(dev_nintendo$Critic_Score,dev_others$Critic_Score,var.equal = FALSE,conf.level=0.99) 

t_test_nintendo



rating_above =subset(games,games$User_Score>=7.5)
rating_below =subset(games,games$User_Score<7.5)

t_test_rating <- t.test(rating_above$Global_Sales,rating_below$Global_Sales,var.equal = FALSE,conf.level=0.99) 

t_test_rating


ggplot(games, aes(x = User_Score, y = Global_Sales, color = User_Score >= 7.5)) +
  geom_point() +
  scale_color_manual(values = c("red", "green")) +
  labs(x = "User Score", y = "Global Sales", title = "Scatter Plot of User Score vs. Global Sales") +
  guides(color = guide_legend(title = "User_Score")) +
  theme_minimal()
