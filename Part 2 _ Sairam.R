
library(ezids)
library(ggplot2)
library(dplyr)
library(tidyr)

setwd("C:/Users/saira/OneDrive/Documents/GitHub/Intro-to-DS-Group-6")

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

platform_occurrence_multi <- multiple_platform_games %>%
  group_by(Platform) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

ggplot(platform_occurrence_multi, aes(x = reorder(Platform, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "lightcoral", color = "black") +
  labs(title = "Occurrence of Platforms for Multi-Platform Games",
       x = "Platform",
       y = "Count") +
  theme_minimal()

platform_difference <- merge(platform_occurrence, platform_occurrence_multi, by = "Platform", suffixes = c("_Single", "_Multi"))
platform_difference$Difference <- platform_difference$Count_Multi - platform_difference$Count_Single

ggplot(platform_difference, aes(x = reorder(Platform, -Difference), y = Difference)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  labs(title = "Difference in Platform Occurrence between Single and Multi-Platform Games",
       x = "Platform",
       y = "Difference") +
  theme_minimal()


#----------------------------------------------------------------------------------------------------------------------
  
# Decision tree



library(rpart)
library(caret)

# Selecting a subset of columns
selected_columns <- c("Platform","Year_of_Release","Publisher", "Developer" ,"Rating","Genre")

# Creating a subset of the data with selected columns
subset_data <- games_final[, selected_columns]

subset_data$Platform <- as.factor(subset_data$Platform)
subset_data$Publisher <- as.factor(subset_data$Publisher)
subset_data$Developer <- as.factor(subset_data$Developer)
subset_data$Rating <- as.factor(subset_data$Rating)
subset_data$Genre <- as.factor(subset_data$Genre)

# Create a decision tree model
#model <- rpart(Genre ~ ., data = subset_data, method = "class")

# Make predictions on the training set
predictions <- predict(model, subset_data, type = "class")

# Create a confusion matrix
conf_matrix <- confusionMatrix(predictions, subset_data$Genre)

# Print the confusion matrix
print(conf_matrix)

# Plot the confusion matrix
conf_matrix_plot <- plot(conf_matrix$table, col = conf_matrix$byClass, 
                         main = "Confusion Matrix", 
                         color = c("lightblue", "lightcoral"))




#-----------------------------------------------------------------------------------------------------------------

# Decision Tree 


library(caret)
library(rpart)
library(rpart.plot)

data=games_final
hit_threshold <- quantile(data$Global_Sales, 0.75)
data$Hit <- ifelse(data$Global_Sales > hit_threshold, 1, 0)

# Convert factors if necessary
data$Platform <- as.factor(data$Platform)
data$Genre <- as.factor(data$Genre)
data$Year_of_Release <- as.factor(data$Year_of_Release) # Treat as categorical

# Split data into training and testing sets
set.seed(123) # for reproducibility
index <- createDataPartition(data$Hit, p = 0.8, list = FALSE)
train_data <- data[index, ]
test_data <- data[-index, ]

# Fit a decision tree model
tree_model <- rpart(Hit ~ Critic_Score + User_Score + Platform + Genre + Year_of_Release, 
                    data = train_data, method = "class")

# Plot the decision tree
rpart.plot(tree_model)

#-----------------------------------------------------------------------------------------------------------------

# Logistic Regression


