
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

# Q2 Is there an impact when a game is released across multiple platforms vs when it’s targeted towards a specific platform?

grouped_by_platform <- split(games_final, games_final$Platform)
platform_summary <- lapply(grouped_by_platform, function(x) summary(x$Global_Sales))
boxplot(Global_Sales ~ Platform, data = games_final, main="Global Sales by Platform", xlab="Platform", ylab="Global Sales")


games_final <- games_final %>%
  group_by(Name) %>%
  mutate(Platform_Count = n())

single_platform_games <- filter(games_final, Platform_Count == 1)
multiple_platform_games <- filter(games_final, Platform_Count > 1)


average_sales_single_platform <- mean(single_platform_games$Global_Sales)
average_sales_multiple_platforms <- mean(multiple_platform_games$Global_Sales)

average_rating_single_platform <- mean(single_platform_games$User_Score, na.rm = TRUE)
average_rating_multiple_platforms <- mean(multiple_platform_games$User_Score, na.rm = TRUE)

cat("Average Sales - Single Platform Games:", average_sales_single_platform, "\n")
cat("Average Sales - Multiple Platform Games:", average_sales_multiple_platforms, "\n\n")

cat("Average Rating - Single Platform Games:", average_rating_single_platform, "\n")
cat("Average Rating - Multiple Platform Games:", average_rating_multiple_platforms, "\n")



# For single platform games

platform_occurrence <- single_platform_games %>%
  group_by(Platform) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

ggplot(platform_occurrence, aes(x = reorder(Platform, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Occurrence of Platforms for Single-Platform Games",
       x = "Platform",
       y = "Count") +
  theme_minimal()


# For multi platform games

platform_occurrence <- multiple_platform_games %>%
  group_by(Platform) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

ggplot(platform_occurrence, aes(x = reorder(Platform, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Occurrence of Platforms for Multiple-Platform Games",
       x = "Platform",
       y = "Count") +
  theme_minimal()


