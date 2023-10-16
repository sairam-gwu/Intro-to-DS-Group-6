
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


qqnorm(games$Global_Sales)
qqline(games$Global_Sales)



# Without Outliers

games_3=games


games_3=outlierKD2(games_3,Global_Sales,rm=TRUE)



ggplot(data = games_3, mapping = aes(x = Global_Sales)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 15) +  
  labs(
    title = "Distribution of Global Sales",
    x = "Global Sales",
    y = "Frequency"
  ) +
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5))  



ggplot(data = games_3, mapping = aes(y = Global_Sales)) +
  geom_boxplot(fill = "orange", color = "black") +  
  labs(
    title = "Boxplot of Global Sales",
    y = "Global Sales"
  ) +
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5)) 

qqnorm(games_3$Global_Sales)
qqline(games_3$Global_Sales)










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

#--- Visually showing results


ggplot(games, aes(x = ifelse(grepl("Nintendo", games$Developer), "Nintendo", "Other"), y = User_Score, color = ifelse(grepl("Nintendo", games$Developer), "Nintendo", "Other"))) +
  geom_point() +
  labs(
    title = "User Score for Nintendo Games vs. Others",
    x = "User Score",
    y = "User Score"
  ) +
  scale_color_manual(name = "Developer", values = c("Nintendo" = "skyblue", "Other" = "orange")) +
  scale_x_discrete(labels = c("Nintendo" = "Nintendo", "Other" = "Other")) +
  theme_minimal() +
  theme(legend.position = "top")
#----


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



#------------------------





#-------------------------

# Rating vs Sales

ggplot(games, aes(x = User_Score, y = Global_Sales, color = User_Score >= 7.5)) +
  geom_point() +
  scale_color_manual(values = c("red", "green")) +
  labs(x = "User Score", y = "Global Sales", title = "Scatter Plot of User Score vs. Global Sales") +
  guides(color = guide_legend(title = "User Score greater than 7.5")) +
  theme_minimal()


#




# Iterating to find threshold

# Create empty vectors to store results
thresholds <- numeric()
diff_means_values <- numeric()
p_values <- numeric()

# Initialize variables to track the best result
best_rating_threshold <- NA
best_diff_means <- -Inf
best_p_value <- 1  # Initialize with a value above 0.05

# Iterate over user rating thresholds from 4.0 to 8.0 in 0.1 increments
for (rating_threshold in seq(4.0, 8.5, by = 0.1)) {
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
  
  # Store the results in vectors
  thresholds <- c(thresholds, rating_threshold)
  diff_means_values <- c(diff_means_values, diff_means)
  p_values <- c(p_values, t_test_result$p.value)
  
  # Check if the result is better than the current best result
  if (diff_means > best_diff_means && t_test_result$p.value < 0.05) {
    best_diff_means <- diff_means
    best_p_value <- t_test_result$p.value
    best_rating_threshold <- rating_threshold
  }
}

# Print the best result
cat("Best rating split with the highest mean difference and p-value below 0.05 is:", best_rating_threshold, "\n")
cat("Mean Difference in Sales:", best_diff_means, "\n")
cat("P-Value for the T-Test:", best_p_value, "\n")








#-- Visually showing the result

plot(
  thresholds, 
  diff_means_values, 
  type = "l", 
  col = "blue", 
  xlab = "Rating Threshold", 
  ylab = "Mean Difference in Sales",
  main = "Threshold vs. Mean Difference in Sales",
  xlim = c(4, 8),
  ylim = c(max(diff_means_values)*0.5, max(diff_means_values)*1.1)
)

# Label points with maximum mean difference
max_diff_idx <- which.max(diff_means_values)
text(
  thresholds[max_diff_idx], 
  diff_means_values[max_diff_idx], 
  labels = paste("Threshold:", thresholds[max_diff_idx], "\n Difference:", round(diff_means_values[max_diff_idx], 2)),
  pos = 3,
  col = "blue"
)



# Create a new plot for p-value vs. rating
par(mar = c(5, 5, 4, 5))  # Adjust margin for labels
plot(
  thresholds, 
  -log10(p_values), 
  type = "l", 
  col = "red", 
  xlab = "Rating Threshold", 
  ylab = "-log10(P-Value)",
  main = "User Rating vs. P-Value",
  xlim = c(4, 8),
  ylim = c(0, max(-log10(p_values)) + 2)
)

#-------











# Plot the threshold vs. rating
plot(thresholds, diff_means_values, type = "l", col = "blue", xlab = "Rating Threshold", ylab = "Mean Difference in Sales")

# Create a new plot for p-value vs. rating
par(mar = c(5, 5, 4, 5))  # Adjust margin for labels
plot(thresholds, -log10(p_values), type = "l", col = "red", xlab = "Rating Threshold", ylab = "-log10(P-Value)")
abline(h = -log10(0.05), col = "red")










#NA Sales vs Global Sales

# Calculate Rest of the World Sales
games$ROW_Sales <- games$Global_Sales - games$NA_Sales

colors <- c("Action" = "red", "Adventure" = "blue", "Sports" = "green", "Shooter" = "orange", "Role-Playing" = "purple", "Misc" = "cyan", "Platform" = "magenta", "Fighting" = "brown", "Simulation" = "pink", "Strategy" = "gray")

plot(games$NA_Sales, games$JP_Sales, xlab = "NA Sales", ylab = "Rest of the World Sales", main = "Scatter Plot: NA Sales vs Ex-US Sales", col = colors[games$Genre])
legend("topright", legend = unique(games$Genre), fill = colors[unique(games$Genre)])











#----------------------------------------------------------------------
# Repeating the Hypothesis tests with outliers removed for sales 


games_final= outlierKD2(games,Global_Sales,rm=TRUE,hist=FALSE)


games_final<-na.omit(games_final)
nrow(games_final)



ggplot(data = games_final, mapping = aes(x = Global_Sales)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 12) +  
  labs(
    title = "Distribution of Global Sales",
    x = "Global Sales",
    y = "Frequency"
  ) +
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5))  




#------------------






# SMART Q - Threshold Rating




# Create empty vectors to store results
thresholds <- numeric()
diff_means_values <- numeric()
p_values <- numeric()

# Initialize variables to track the best result
best_rating_threshold <- NA
best_diff_means <- -Inf
best_p_value <- 1  # Initialize with a value above 0.05

# Iterate over user rating thresholds from 4.0 to 8.0 in 0.1 increments
for (rating_threshold in seq(4.0, 8.5, by = 0.1)) {
  # Subset data for games with user scores above and below the threshold
  rating_above <- subset(games_final, games_final$User_Score >= rating_threshold)
  rating_below <- subset(games_final, games_final$User_Score < rating_threshold)
  
  # Calculate the sample means
  mean_sales_above <- mean(rating_above$Global_Sales)
  mean_sales_below <- mean(rating_below$Global_Sales)
  
  # Calculate the difference in means
  diff_means <- mean_sales_above - mean_sales_below
  
  # Perform a t-test for the two groups with unequal variances
  t_test_result <- t.test(rating_above$Global_Sales, rating_below$Global_Sales, var.equal = FALSE)
  
  # Store the results in vectors
  thresholds <- c(thresholds, rating_threshold)
  diff_means_values <- c(diff_means_values, diff_means)
  p_values <- c(p_values, t_test_result$p.value)
  
  # Check if the result is better than the current best result
  if (diff_means > best_diff_means && t_test_result$p.value < 0.05) {
    best_diff_means <- diff_means
    best_p_value <- t_test_result$p.value
    best_rating_threshold <- rating_threshold
  }
}

# Print the best result
cat("Best rating split with the highest mean difference and p-value below 0.05 is:", best_rating_threshold, "\n")
cat("Mean Difference in Sales:", best_diff_means, "\n")
cat("P-Value for the T-Test:", best_p_value, "\n")




#-- Visually showing the result

plot(
  thresholds, 
  diff_means_values, 
  type = "l", 
  col = "blue", 
  xlab = "Rating Threshold", 
  ylab = "Mean Difference in Sales",
  main = "Threshold vs. Mean Difference in Sales",
  xlim = c(4, 8),
  ylim = c(max(diff_means_values)*0.5, max(diff_means_values)*1.1)
)

# Label points with maximum mean difference
max_diff_idx <- which.max(diff_means_values)
text(
  thresholds[max_diff_idx], 
  diff_means_values[max_diff_idx], 
  labels = paste("Threshold:", thresholds[max_diff_idx], "\n Difference:", round(diff_means_values[max_diff_idx], 2)),
  pos = 3,
  col = "blue"
)



# Create a new plot for p-value vs. rating
par(mar = c(5, 5, 4, 5))  # Adjust margin for labels
plot(
  thresholds, 
  -log10(p_values), 
  type = "l", 
  col = "red", 
  xlab = "Rating Threshold", 
  ylab = "-log10(P-Value)",
  main = "User Rating vs. P-Value",
  xlim = c(4, 8),
  ylim = c(0, max(-log10(p_values)) + 2)
)

#-------


ggplot(games_final, aes(x = Critic_Score, y = Global_Sales, color = Critic_Score >= 75)) +
  geom_point() +
  scale_color_manual(values = c("red", "green")) +
  labs(x = "User Score", y = "Global Sales", title = "Scatter Plot of User Score vs. Global Sales") +
  guides(color = guide_legend(title = "User Score greater than 7.5")) +
  theme_minimal()














