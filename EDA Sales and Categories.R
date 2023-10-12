
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

  

  
# Confidence Interval shooter games
  
  
  
library(dplyr)
library(stats)
  
  
  
subset_data <- games %>% filter(Genre == 'Shooter' & User_Score > 8.0)
  
sample_proportion <- nrow(subset_data) / nrow(games)
  
confidence_level <- 0.95
  
standard_error <- sqrt((sample_proportion * (1 - sample_proportion)) / nrow(games))
  
df <- nrow(games) - 1  # Degrees of freedom
t_critical <- qt(1 - (1 - confidence_level) / 2, df)  # t-critical value
margin_of_error <- t_critical * standard_error

# Calculate the confidence interval
lower_bound <- sample_proportion - margin_of_error
upper_bound <- sample_proportion + margin_of_error

# Print the results
cat("Sample Proportion:", sample_proportion, "\n")
cat("95% Confidence Interval :", lower_bound, "to", upper_bound, "\n")

# Rating vs Sales

ggplot(games, aes(x = User_Score, y = Global_Sales, color = User_Score >= 7.5)) +
  geom_point() +
  scale_color_manual(values = c("red", "green")) +
  labs(x = "User Score", y = "Global Sales", title = "Scatter Plot of User Score vs. Global Sales") +
  guides(color = guide_legend(title = "User Score greater than 7.5")) +
  theme_minimal()


#




# Iterating to find threshold


best_rating_split <- 2.0
best_diff_means <- 0
best_p_value <- 1  

# Define the minimum threshold difference in means and maximum p-value
min_threshold_difference <- 0.3  # Adjust as needed
max_p_value <- 0.05  # Adjust as needed

# Iterate over user rating thresholds from 4.0 to 8.0 in 0.1 increments
for (rating_threshold in seq(4.0, 8.0, by = 0.1)) {
  # Subset data for games with user scores above and below the threshold
  rating_above <- subset(games, games$User_Score >= rating_threshold)
  rating_below <- subset(games, games$User_Score < rating_threshold)
  
  # Calculate the sample means
  mean_sales_above <- mean(rating_above$Global_Sales)
  mean_sales_below <- mean(rating_below$Global_Sales)
  
  # Calculate the difference in means
  diff_means <- mean_sales_above - mean_sales_below
  
  # Perform a t-test for the two groups with unequal variances
  t_test_result <- t.test(rating_above$Global_Sales, rating_below$Global_Sales, var.equal = FALSE)
  
  # Check if the p-value is below the maximum
  if (t_test_result$p.value < max_p_value) {
    # Check if the difference in means is greater than the best result
    if (diff_means > best_diff_means) {
      best_rating_split <- rating_threshold
      best_diff_means <- diff_means
      best_p_value <- t_test_result$p.value
    }
  }
}

# Print the best result
cat("Best rating split with the highest difference in means and p-value below", max_p_value, "is:", best_rating_split, "\n")
cat("Mean Difference in Sales:", best_diff_means, "\n")
cat("P-Value for the T-Test:", best_p_value, "\n")





