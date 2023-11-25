
library(ezids)
library(ggplot2)
library(dplyr)
library(tidyr)

games=read.csv("Video games sales.csv")
head(games,5)



games<-na.omit(games)
unique(games$Rating)
games$Rating <- ifelse( games$Rating== "", NA,games$Rating )
unique(games$Rating)
games<-subset(games,games$Rating!="NA")
games<-subset(games,games$Year_of_Release!="N/A")
nrow(games)


games_final= outlierKD2(games,Global_Sales,rm=TRUE,hist=FALSE)
games_final<-na.omit(games_final)


library(reshape2)



agg_data <- games_final %>%
  group_by(Year_of_Release) %>%
  summarise(NA_Sales = sum(NA_Sales), JP_Sales = sum(JP_Sales), EU_Sales = sum(EU_Sales))

# Melt the aggregated dataframe to long format
agg_data_long <- tidyr::gather(agg_data, key = 'Region', value = 'Sales', -Year_of_Release)

# Plot the aggregated sales data
ggplot(agg_data_long, aes(x = Year_of_Release, y = Sales, color = Region, group = Region)) +
  geom_line() +
  labs(title = 'Total Sales by Region Over Time',
       x = 'Year of Release',
       y = 'Total Sales',
       color = 'Region') +
  theme_minimal()


