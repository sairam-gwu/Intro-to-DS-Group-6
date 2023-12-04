
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

average_rating_single_platform <- mean(single_platform_games$Critic_Score, na.rm = TRUE)
average_rating_multiple_platforms <- mean(multiple_platform_games$Critic_Score, na.rm = TRUE)

cat("Average Sales - Single Platform Games:", average_sales_single_platform, "\n")
cat("Average Sales - Multiple Platform Games:", average_sales_multiple_platforms, "\n\n")

cat("Average Rating - Single Platform Games:", average_rating_single_platform, "\n")
cat("Average Rating - Multiple Platform Games:", average_rating_multiple_platforms, "\n")


combined_data <- rbind(data.frame(Sales = single_platform_games$Global_Sales, Platform = "Single Platform"),
                       data.frame(Sales = multiple_platform_games$Global_Sales, Platform = "Multiple Platforms"))

# Global Sales

ggplot(combined_data, aes(x = Platform, y = Sales, fill = Platform)) +
  geom_violin(trim = FALSE, scale = "width", width = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", color = "black", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Comparison of Global Sales for single platform vs multi platform games",
       x = "Platform Count",
       y = "Global Sales (Millions of Copies)") +
  theme(legend.position = "none")



# Critic Score
combined_data <- rbind(data.frame(Score = single_platform_games$Critic_Score, Platform = "Single Platform"),
                       data.frame(Score = multiple_platform_games$Critic_Score, Platform = "Multiple Platforms"))


ggplot(combined_data, aes(x = Platform, y = Score, fill = Platform)) +
  geom_violin(trim = FALSE, scale = "width", width = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", color = "black", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Comparison of Critic Scores",
       x = "Platform Count",
       y = "Critic Score") +
  theme(legend.position = "none")







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
  labs(title = "Difference in Platform Occurrence between Multi and Single-Platform Games",
       x = "Platform",
       y = "Difference in Games Sold") +
  theme_minimal()


#----------------------------------------------------------------------------------------------------------------------
  
# Decision tree



library(rpart)
library(caret)

# Selecting a subset of columns
selected_columns <- c("Critic_Score","Developer","Genre")

# Creating a subset of the data with selected columns
subset_data <- games_final[, selected_columns]

#subset_data$Platform <- as.factor(subset_data$Platform)
#subset_data$Publisher <- as.factor(subset_data$Publisher)

subset_data$Developer <- as.factor(subset_data$Developer)
#subset_data$Rating <- as.factor(subset_data$Rating)
subset_data$Genre <- as.factor(subset_data$Genre)

# Create a decision tree model
model <- rpart(Genre ~ ., data = subset_data, method = "class")

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


model_data <- data.frame(
  Platform = games_final$Platform,
  Year_of_Release = games_final$Year_of_Release,
  Rating = games_final$Rating,
  Genre = games_final$Genre,
  Developer=games_final$Developer,
  stringsAsFactors = FALSE  # Make sure strings are treated as characters, not factors
)

# Function to bin categories
bin_categories <- function(column, top_n) {
  counts <- table(column)
  top_categories <- names(sort(counts, decreasing = TRUE)[1:top_n])
  column_binned <- ifelse(column %in% top_categories, column, "Others")
  return(column_binned)
}

model_data$Developer <- bin_categories(games_final$Developer, top_n = 5)

model_data$Publisher <- bin_categories(games_final$Publisher, top_n = 5)
model_data$Platform <- bin_categories(games_final$Platform, top_n = 5)


# Check the structure of 'model_data'
str(model_data)




#----------------------------------------------------------------------------------------------------------------------

# Decision tree - Genre Prediction



library(rpart)
library(caret)
library(rpart.plot)


# Create a decision tree model
model <- rpart(Genre ~ ., data = model_data, method = "class")

# Make predictions on the training set
predictions <- predict(model, model_data, type = "class")

# Create a confusion matrix
conf_matrix <- confusionMatrix(predictions, model_data$Genre)

# Print the confusion matrix
print(conf_matrix)

# Plot the confusion matrix
conf_matrix_plot <- plot(conf_matrix$table, col = conf_matrix$byClass, 
                         main = "Confusion Matrix", 
                         color = c("lightblue", "lightcoral"))



# Plot the decision tree
rpart.plot(model)





#---------------------------------------------------------------------------------------------------------------

# Decision Tree - Iteration 1 looking at platforms as well as years


library(caret)
library(rpart)
library(rpart.plot)

data=games_final
hit_threshold <- quantile(data$Global_Sales, 0.5)
data$Hit <- ifelse(data$Global_Sales > hit_threshold, 1, 0)

data$Platform <- as.factor(data$Platform)
data$Genre <- as.factor(data$Genre)
data$Developer <- as.factor(data$Genre)

data$Year_of_Release <- as.numeric(data$Year_of_Release)

set.seed(123) 
index <- createDataPartition(data$Hit, p = 0.8, list = FALSE)
train_data <- data[index, ]
test_data <- data[-index, ]

tree_model <- rpart(Hit ~ Critic_Score + User_Score + Platform + Genre + Year_of_Release, 
                    data = train_data, method = "class")


rpart.plot(tree_model, extra = 101, under = TRUE, cex = 0.8, tweak = 1.2)

#-----------------------------------------------------------------------------------------------------------------
# Decision Tree - Iteration 2 Dropping platform and years


bin_categories <- function(column, top_n) {
  counts <- table(column)
  top_categories <- names(sort(counts, decreasing = TRUE)[1:top_n])
  column_binned <- ifelse(column %in% top_categories, column, "Others")
  return(column_binned)
}

data$Developer <- bin_categories(data$Developer, top_n = 10)

data$Publisher <- bin_categories(data$Publisher, top_n = 10)



set.seed(123) 
index <- createDataPartition(data$Hit, p = 0.8, list = FALSE)
train_data <- data[index, ]
test_data <- data[-index, ]


tree_model <- rpart(Hit ~ Critic_Score + User_Score + Developer + Genre, 
                    data = train_data, method = "class")


rpart.plot(tree_model, extra = 101, under = TRUE, cex = 0.8, tweak = 1.2)



#-----------------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------------

# Logistic Regression

data$Developer <- as.factor(data$Developer) 
data$Publisher <- as.factor(data$Publisher) 

log_model <- glm(Hit ~ Critic_Score + Genre, data=data,family='binomial')

print(summary(log_model))





#-----------------------------------------------------------------------------------------------------------------



ggplot(data, aes(x = Critic_Score, y = Global_Sales, color = Genre)) +
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Global Sales by Genre",
       x = "Global Sales",
       y = "Global Sales") +
  theme(legend.position = "right")

#-----------------------------------------------------------------------------------------------------------------

top_developers <- data %>%
  group_by(Developer) %>%
  summarise(Total_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
  top_n(20, Total_Sales) %>%
  pull(Developer)

filtered_data <- data %>%
  filter(Developer %in% top_developers)

ggplot(filtered_data, aes(x = Developer, y = Global_Sales, color = Genre)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), alpha = 0.7) +
  labs(title = "Global Sales for Top 20 Developers",
       x = "Developer",
       y = "Global Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


