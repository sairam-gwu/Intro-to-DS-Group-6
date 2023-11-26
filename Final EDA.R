---
  title: "Project"
author: "Group6"
date: "`r Sys.Date()`"
output: html_document
---
  
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r}

library(ezids)
library(ggplot2)

games=read.csv("~/Documents/GitHub/Intro-to-DS-Group-6/Video games sales.csv")
head(games,5)
```


```{r}
nrow(games)
str(games)
```
```{r}
unique(games$Platform) #need to convert platform into factor 27 different platforms
unique(games$Genre)
unique(games$Rating)

length(unique(games$Name))#.  #11563 Names
length(unique(games$Platform)) #31 platforms
length(unique(games$Genre))  #13 genres
length(unique(games$Publisher)) #583 publishers
length(unique(games$Developer)) #1697 developers
length(unique(games$Rating)) #9 categories of rating
```
```{r}
sum(is.na(games$Critic_Count)) #8582 null values
sum(is.na(games$Critic_Score))
sum(is.na(games$User_Count)) #9129 null values
x<- sum(is.na(games$User_Score))
x
```


```{r}

```

#Null values cleaning 

```{r}
games<-na.omit(games)
unique(games$Rating)
games$Rating <- ifelse(games$Rating== "", NA,games$Rating )
unique(games$Rating)
games<-subset(games,games$Rating!="NA")
games<-subset(games,games$Year_of_Release!="N/A")
nrow(games)

```
#Critic_Count	User_Score



```{r}
library(ezids)
library(ggplot2)
library(dplyr)
library(tidyr)

games=read.csv("Video games sales.csv")
head(games,5)
```

````{r}
games<-na.omit(games)
unique(games$Rating)
games$Rating <- ifelse( games$Rating== "", NA,games$Rating )
unique(games$Rating)
games<-subset(games,games$Rating!="NA")
games<-subset(games,games$Year_of_Release!="N/A")
nrow(games)

```


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
# Repeating the Hypothesis tests and EDA with outliers removed for sales 

outlierKD2(games,Global_Sales,rm=FALSE,hist=TRUE,boxplt=TRUE,qqplt=TRUE)


games_final= outlierKD2(games,Global_Sales,rm=TRUE,hist=FALSE)
games_final<-na.omit(games_final)






ggplot(data = games_final, aes(x = reorder(Genre, -table(Genre)[Genre]), fill = Genre)) +
  geom_bar() +
  labs(title = "Frequency Distribution of Genres",
       x = "Genre",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data = games_final, aes(x = reorder(Platform, -table(Platform)[Platform]), fill = Platform)) +
  geom_bar() +
  labs(title = "Frequency Distribution of Platforms",
       x = "Platform",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#-----------------------------------------------------------------------------------------------------------------
# Smart Question 3 - Trend of Global sales by year of release

sales_by_year <- aggregate(Global_Sales ~ Year_of_Release, data = games_final, FUN = mean)

# Create a bar plot
ggplot(sales_by_year, aes(x = Year_of_Release, y = Global_Sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Average Global Sales by Year of Release",
    x = "Year of Release",
    y = "Average Global Sales"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



sales_by_year <- aggregate(Global_Sales ~ Year_of_Release, data = games_final, FUN = sum)

# Create a bar plot
ggplot(sales_by_year, aes(x = Year_of_Release, y = Global_Sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Total Global Sales by Year of Release",
    x = "Year of Release",
    y = "Total Global Sales"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




max_platform <- games_final %>%
  group_by(Year_of_Release, Platform) %>%
  summarise(Total_Sales = sum(Global_Sales)) %>%
  top_n(1, Total_Sales)


ggplot(data = max_platform, aes(x = Year_of_Release, y = Total_Sales, fill = Platform)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Total Global Sales by Year for the best performing platform",
    x = "Year of Release",
    y = "Total Global Sales"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#--------------------------------------------------------------------------------------------------------------------
#USer Score


ggplot(data = games_final, mapping = aes(x = User_Score)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 12) +  
  labs(
    title = "Distribution of User Score",
    x = "User Score",
    y = "Frequency"
  ) +
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5))  






ggplot(data = games_final, mapping = aes(y = User_Score)) +
  geom_boxplot(fill = "orange", color = "black") +  
  labs(
    title = "Boxplot of User Scores",
    y = "User Score"
  ) +
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5)) 



#--------------------------------------------------------------------------------------------------------------------























#----------------


ggplot(data = games_final, mapping = aes(x = Global_Sales)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 12) +  
  labs(
    title = "Distribution of Global Sales",
    x = "Global Sales",
    y = "Frequency"
  ) +
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5))  





ggplot(data = games_final, mapping = aes(y = Global_Sales)) +
  geom_boxplot(fill = "orange", color = "black") +  
  labs(
    title = "Boxplot of Global Sales",
    y = "Global Sales"
  ) +
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5)) 

qqplot_data <- qqnorm(games_final$Global_Sales, plot = FALSE)
qqplot_data <- data.frame(Theoretical = qqplot_data$x, Observed = qqplot_data$y)


ggplot(qqplot_data, aes(x = Theoretical, y = Observed)) +
  geom_point(shape = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(
    title = "QQ Plot of Global Sales",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

#------------------




#----------------------------------------------------------------------------------------------------------------
# SMART Q - Confidence interval for Shooter game ratings

subset_data <- subset(games_final,games_final$Genre == 'Shooter' & games_final$User_Score >= 8.0)
shooter_data <- subset(games_final,games_final$Genre == 'Shooter')


sample_proportion <- nrow(subset_data) / nrow(shooter_data)

confidence_level <- 0.95

standard_error <- sqrt((sample_proportion * (1 - sample_proportion)) / nrow(shooter_data))

df <- nrow(shooter_data) - 1  # Degrees of freedom
t_critical <- qt(1 - (1 - confidence_level) / 2, df)  # t-critical value
margin_of_error <- t_critical * standard_error

# Calculate the confidence interval
lower_bound <- sample_proportion - margin_of_error
upper_bound <- sample_proportion + margin_of_error

# Print the results
cat("Sample Proportion:", sample_proportion, "\n")
cat("95% Confidence Interval :", lower_bound, "to", upper_bound, "\n")

x <- seq(sample_proportion - 4 * standard_error, sample_proportion + 4 * standard_error, length = 1000)


y <- dt((x - sample_proportion) / standard_error, df) 

# Create a data frame for plotting
plot_data <- data.frame(x = x, y = y)



ggplot(plot_data, aes(x = x, y = y)) +
  geom_line() +
  geom_vline(xintercept = c(lower_bound, upper_bound), color = "blue", linetype = "dashed", size = 1) +
  geom_ribbon(data = plot_data[plot_data$x >= lower_bound & plot_data$x <= upper_bound, ], aes(x = x, ymin = 0, ymax = y), fill = "pink", alpha = 0.5) +
  labs(
    title = "Sample Distribution for the propotion of game in the Shooter Genre with a rating >=8.0",
    x = "Proportion of Shooter Games with User Score > 8.0",
    y = "Probability Density"
  )+
  theme_minimal()




#------------------------------------------------------------------------------------------------------------------


high_rated_data <- subset(games_final,games_final$User_Score >= 8.0)


overall_proportion <- nrow(high_rated_data) / nrow(games_final)

cat("Sample proportion is ",overall_proportion)





#-----------------------------------------------------------------------------------------------------------------
# SMART Q - Threshold Rating


rating_above <- subset(games_final, games_final$User_Score >= 7.5)
rating_below <- subset(games_final, games_final$User_Score < 7.5)

# Calculate the sample means
mean_sales_above <- mean(rating_above$Global_Sales)
mean_sales_below <- mean(rating_below$Global_Sales)

# Calculate the difference in means
diff_means <- mean_sales_above - mean_sales_below

# Perform a t-test for the two groups with unequal variances
t_test_result <- t.test(rating_above$Global_Sales, rating_below$Global_Sales, var.equal = FALSE)

cat("Mean difference is:", diff_means, "\n")
cat("P-Value for the T-Test:", t_test_result$p.value, "\n")






#-----------------------------------------------------------------------------------------------------------------

#Iterating to find threshold

thresholds <- numeric()
diff_means_values <- numeric()
p_values <- numeric()

best_rating_threshold <- NA
best_diff_means <- -Inf
best_p_value <- 1  # Initialize with a value above 0.05


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
  xlim = c(3.8, 8),
  ylim = c(max(diff_means_values) * 0.5, max(diff_means_values) * 1.1)
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




#-----------------------------------------------------------------------------------------------------------------


ggplot(games_final, aes(x = User_Score, y = Global_Sales, color = User_Score >= 4.1)) +
  geom_point() +
  scale_color_manual(values = c("red", "green"), 
                     breaks = c(FALSE, TRUE), 
                     labels = c("< 4.1", ">= 4.1")) +
  labs(x = "User Score", y = "Global Sales (Millions)", title = "User Score vs. Global Sales") +
  guides(color = guide_legend(title = "User Score")) +
  theme_minimal()


correlation <- cor(games_final$User_Score, games_final$Global_Sales)

cat(" The correlation is : ",correlation)

#-------------------------------------------------------------------------------------------------------------------
# Misc Scatter Plots


ggplot(games_final, aes(x = Year_of_Release, y = Genre, color = Genre)) +
  geom_point(position = position_jitter(w = 0.2, h = 0.1), size = 3) +
  labs(title = "Scatter Plot of Developer vs. Platform (Colored by Genre)") +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_color_brewer(palette = "Set1")


genre_counts <- games_final %>%
  group_by(Year_of_Release, Genre) %>%
  summarize(Count = n()) %>%
  ungroup()

# Create the bubble chart
bubble_chart <- ggplot(genre_counts, aes(x = Year_of_Release, y = Genre, size = Count, color = Genre)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(3, 20)) +
  labs(title = "Popular Genres by Year",
       x = "Year of Release",
       y = "Genre",
       size = "Count by Year") +
  theme_minimal()

# Print the bubble chart
print(bubble_chart)





genre_counts <- games_final %>%
  group_by(Year_of_Release, Genre) %>%
  summarize(Count = n()) %>%
  ungroup()

# Create the stacked column chart
ggplot(genre_counts, aes(x = Year_of_Release, y = Count, fill = Genre)) +
  geom_col(position = "stack") +
  labs(title = "Popular Genres by Year",
       x = "Year of Release",
       y = "Count by Year")  +  # You can choose a different color palette
  theme_minimal()

# Correlation plots 

numerical_cols <- games_final[, c('Critic_Score', 'Global_Sales', 'User_Score')]

# Compute the correlation matrix
correlation_matrix <- cor(numerical_cols)

# Reshape the correlation matrix
correlation_data <- as.data.frame(correlation_matrix)
correlation_data$variable <- rownames(correlation_data)
correlation_data_long <- pivot_longer(correlation_data, cols = -variable, names_to = "Variable", values_to = "Correlation")

# Create a heatmap of the correlation matrix
ggplot(correlation_data_long, aes(variable, Variable, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue") +
  theme_minimal() +
  labs(title = "Correlation Matrix")


# Boxplots by Genre for Sales
ggplot(games_final, aes(x = Genre, y = Global_Sales)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(title = "Boxplot of Global Sales by Genre",
       x = "Genre",
       y = "Global Sales (in millions)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Boxplots by Platform for Sales
ggplot(games_final, aes(x = Platform, y = Global_Sales)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(title = "Boxplot of Global Sales by Platform",
       x = "Platform",
       y = "Global Sales (in millions)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Scatter for scores colored by genre
ggplot(games_final, aes(x = Critic_Score, y = User_Score, color = Genre)) +
  geom_point() +
  labs(title = "Scatter Plot of User Score vs. Critic Score (Colored by Genre)",
       x = "Critic Score",
       y = "User Score") +  
  theme_minimal()


ggplot(games_final, aes(x = Critic_Score, y = User_Score, color = Global_Sales)) +
  geom_point() +
  labs(title = "Scatter Plot of User Score vs. Critic Score (Colored by Global Sales)",
       x = "Critic Score",
       y = "User Score") +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal()


#

# Part 2 project

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