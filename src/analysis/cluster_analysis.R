# Load required libraries
library(tidyverse)      # For data manipulation and visualization
library(cluster)        # For clustering algorithms
library(factoextra)     # For cluster visualization
library(NbClust)        # For determining optimal number of clusters
library(corrplot)       # For correlation visualization
library(scales)         # For formatting scales in plots
library(gridExtra)      # For arranging multiple plots
library(dendextend)     # For dendrogram visualization
library(ggrepel)        # For improved text labeling in plots
library(reshape2)       # For data reshaping

# Set seed for reproducibility
set.seed(123)

# Import the dataset
smart_fresh <- read.csv("F:\\UoB Study\\Marketing Analysis & Behaviour Science\\Assignment 1\\SmartFresh_Retail_Clean.csv")


# Create age variable from birth year (using current year as reference)
current_year <- 2025  # Based on current date
smart_fresh$Age <- current_year - smart_fresh$Year_Birth

# Create a total spending variable
smart_fresh$Total_Spending <- smart_fresh$Spend_Wine + 
  smart_fresh$Spend_OrganicFood + 
  smart_fresh$Spend_Meat + 
  smart_fresh$Spend_WellnessProducts + 
  smart_fresh$Spend_Treats + 
  smart_fresh$Spend_LuxuryGoods

# Create a variable for total purchases
smart_fresh$Total_Purchases <- smart_fresh$Purchases_Online + 
  smart_fresh$Purchases_Catalog + 
  smart_fresh$Purchases_Store

# Create a variable for average spending per purchase
smart_fresh$Avg_Spend_Per_Purchase <- smart_fresh$Total_Spending / 
  smart_fresh$Total_Purchases

# Create a variable for online shopping preference (proportion of online purchases)
smart_fresh$Online_Preference <- smart_fresh$Purchases_Online / 
  smart_fresh$Total_Purchases

# Create a variable for promotion responsiveness
smart_fresh$Promo_Response_Rate <- smart_fresh$Promo_Purchases / 
  smart_fresh$Total_Purchases

# Select variables for clustering
# We'll focus on spending patterns, shopping behavior, and demographic information
cluster_vars <- c("Age", "Annual_Income", "Kidhome", "Teenhome", 
                  "Spend_Wine", "Spend_OrganicFood", "Spend_Meat", 
                  "Spend_WellnessProducts", "Spend_Treats", "Spend_LuxuryGoods",
                  "Purchases_Online", "Purchases_Catalog", "Purchases_Store", 
                  "Visits_OnlineLastMonth", "Promo_Purchases", 
                  "Online_Preference", "Promo_Response_Rate")

# Create a subset with only the variables for clustering
cluster_data <- smart_fresh[, cluster_vars]

# Check for missing values
missing_values <- colSums(is.na(cluster_data))
print(missing_values)

# Handle missing values (if any)
# For this analysis, we'll use mean imputation for simplicity
for(i in 1:ncol(cluster_data)) {
  cluster_data[is.na(cluster_data[,i]), i] <- mean(cluster_data[,i], na.rm = TRUE)
}

# Check for and handle infinite values column by column
for(i in 1:ncol(cluster_data)) {
  # Replace infinite values with NA in this column
  cluster_data[is.infinite(cluster_data[,i]), i] <- NA
  # Then replace NA values with column mean
  cluster_data[is.na(cluster_data[,i]), i] <- mean(cluster_data[,i], na.rm = TRUE)
}
# Scale the data (standardize to mean 0, sd 1)
# This is crucial for clustering as it ensures all variables contribute equally
cluster_data_scaled <- scale(cluster_data)

# Examine correlations between variables
correlation_matrix <- cor(cluster_data)
corrplot(correlation_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         title = "Correlation Matrix of Customer Variables")

# Elbow Method
# The "elbow" in the plot indicates the optimal number of clusters
wss <- function(k) {
  kmeans(cluster_data_scaled, k, nstart = 25)$tot.withinss
}

k_values <- 1:10
wss_values <- sapply(k_values, wss)

elbow_plot <- ggplot(data.frame(k = k_values, wss = wss_values), aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  labs(title = "Elbow Method for Optimal k",
       x = "Number of Clusters (k)",
       y = "Total Within-Cluster Sum of Squares") +
  theme_minimal()

print(elbow_plot)

# Silhouette Method
# Higher average silhouette width indicates better clustering
silhouette_scores <- function(k) {
  km <- kmeans(cluster_data_scaled, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(cluster_data_scaled))
  mean(ss[, 3])
}

sil_values <- sapply(2:10, silhouette_scores)

silhouette_plot <- ggplot(data.frame(k = 2:10, sil = sil_values), aes(x = k, y = sil)) +
  geom_line() +
  geom_point() +
  labs(title = "Silhouette Method for Optimal k",
       x = "Number of Clusters (k)",
       y = "Average Silhouette Width") +
  theme_minimal()

print(silhouette_plot)

# Gap Statistic Method
# The optimal number of clusters is where the gap statistic is maximized
gap_stat <- clusGap(cluster_data_scaled, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
print(gap_stat)
fviz_gap_stat(gap_stat)

# Based on the above methods, determine the optimal number of clusters
# For this example, let's say the optimal number is 4
# (You should adjust this based on the actual results)
optimal_k <- 5

# Perform k-means clustering with the optimal number of clusters
kmeans_result <- kmeans(cluster_data_scaled, centers = optimal_k, nstart = 25)

# Add cluster assignments to the original dataset
smart_fresh$cluster_kmeans <- as.factor(kmeans_result$cluster)

# Visualize the clusters using PCA for dimensionality reduction
pca_result <- prcomp(cluster_data_scaled)
pca_data <- as.data.frame(pca_result$x[, 1:2])
pca_data$cluster <- kmeans_result$cluster

# Plot the first two principal components
kmeans_pca_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = as.factor(cluster))) +
  geom_point(alpha = 0.7) +
  labs(title = "K-means Clusters Visualized with PCA",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Cluster") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

print(kmeans_pca_plot)

# Calculate the percentage of variance explained by each principal component
variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2) * 100
pc1_var <- round(variance_explained[1], 1)
pc2_var <- round(variance_explained[2], 1)

# Create a more informative PCA plot with variance explained
kmeans_pca_plot_enhanced <- ggplot(pca_data, aes(x = PC1, y = PC2, color = as.factor(cluster))) +
  geom_point(alpha = 0.7) +
  labs(title = "Customer Segments in Principal Component Space",
       subtitle = paste0("PC1 explains ", pc1_var, "% of variance, PC2 explains ", pc2_var, "% of variance"),
       x = paste0("Principal Component 1 (", pc1_var, "%)"),
       y = paste0("Principal Component 2 (", pc2_var, "%)"),
       color = "Segment") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  stat_ellipse(level = 0.95)

print(kmeans_pca_plot_enhanced)

# Visualize the cluster centers
centers <- as.data.frame(kmeans_result$centers)
centers_scaled <- as.data.frame(scale(centers, center = FALSE, 
                                      scale = apply(cluster_data_scaled, 2, sd)))

# Use reshape2::melt with proper id.vars parameter
centers_long <- reshape2::melt(centers_scaled, id.vars = NULL, variable.name = "Variable", value.name = "Value")

# Add Cluster column based on row numbers
centers_long$Cluster <- rep(1:nrow(centers_scaled), each = ncol(centers_scaled))
centers_long$Cluster <- as.factor(centers_long$Cluster)

# Plot the cluster centers
centers_plot <- ggplot(centers_long, aes(x = Variable, y = Value, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cluster Centers Across Variables",
       x = "",
       y = "Scaled Mean Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(centers_plot)


# Compute distance matrix
dist_matrix <- dist(cluster_data_scaled)

# Perform hierarchical clustering
hc_complete <- hclust(dist_matrix, method = "complete")
hc_average <- hclust(dist_matrix, method = "average")
hc_ward <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrograms
par(mfrow = c(1, 3))
plot(hc_complete, main = "Complete Linkage", xlab = "", sub = "", cex = 0.7)
rect.hclust(hc_complete, k = optimal_k, border = 2:5)

plot(hc_average, main = "Average Linkage", xlab = "", sub = "", cex = 0.7)
rect.hclust(hc_average, k = optimal_k, border = 2:5)

plot(hc_ward, main = "Ward's Method", xlab = "", sub = "", cex = 0.7)
rect.hclust(hc_ward, k = optimal_k, border = 2:5)
par(mfrow = c(1, 1))

# Create a more visually appealing dendrogram using dendextend
dend_ward <- as.dendrogram(hc_ward)
dend_colored <- color_branches(dend_ward, k = optimal_k)
plot(dend_colored, 
     main = "Hierarchical Clustering Dendrogram (Ward's Method)",
     ylab = "Height",
     leaflab = "none")

# Cut the dendrogram to get cluster assignments
hc_clusters <- cutree(hc_ward, k = optimal_k)

# Add hierarchical clustering assignments to the dataset
smart_fresh$cluster_hc <- as.factor(hc_clusters)

# Compare k-means and hierarchical clustering results
comparison_table <- table(smart_fresh$cluster_kmeans, smart_fresh$cluster_hc)
print(comparison_table)

# Calculate agreement percentage
agreement <- sum(diag(comparison_table)) / sum(comparison_table) * 100
cat("Agreement between k-means and hierarchical clustering:", round(agreement, 2), "%\n")

# For further analysis, we'll use the k-means clustering results
# as they are more commonly used for large datasets
smart_fresh$cluster <- smart_fresh$cluster_kmeans

# Create a function to calculate cluster profiles
calculate_profile <- function(data, cluster_var, profile_vars) {
  profile <- data %>%
    group_by(!!sym(cluster_var)) %>%
    summarize(across(all_of(profile_vars), 
                     list(mean = ~mean(., na.rm = TRUE),
                          sd = ~sd(., na.rm = TRUE),
                          median = ~median(., na.rm = TRUE)),
                     .names = "{.col}_{.fn}"),
              count = n(),
              percentage = n() / nrow(data) * 100)
  
  return(profile)
}

# Define variables for profiling
profile_vars <- c("Age", "Annual_Income", "Kidhome", "Teenhome", 
                  "Total_Spending", "Avg_Spend_Per_Purchase", 
                  "Spend_Wine", "Spend_OrganicFood", "Spend_Meat", 
                  "Spend_WellnessProducts", "Spend_Treats", "Spend_LuxuryGoods",
                  "Purchases_Online", "Purchases_Catalog", "Purchases_Store", 
                  "Visits_OnlineLastMonth", "Promo_Purchases", 
                  "Online_Preference", "Promo_Response_Rate")

# Calculate cluster profiles
cluster_profile <- calculate_profile(smart_fresh, "cluster", profile_vars)
print(cluster_profile)

# Create a more readable profile table for presentation
presentation_profile <- cluster_profile %>%
  select(cluster, count, percentage,
         Age_mean, Annual_Income_mean, 
         Total_Spending_mean, Avg_Spend_Per_Purchase_mean,
         Spend_Wine_mean, Spend_OrganicFood_mean, Spend_Meat_mean, 
         Spend_WellnessProducts_mean, Spend_Treats_mean, Spend_LuxuryGoods_mean,
         Purchases_Online_mean, Purchases_Catalog_mean, Purchases_Store_mean,
         Online_Preference_mean, Promo_Response_Rate_mean)

# Format the presentation profile for better readability
formatted_profile <- presentation_profile %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  rename(Cluster = cluster,
         Count = count,
         Percentage = percentage,
         Age = Age_mean,
         Income = Annual_Income_mean,
         Total_Spend = Total_Spending_mean,
         Avg_Spend = Avg_Spend_Per_Purchase_mean)

print(formatted_profile)

# Statistical tests to identify significant differences between clusters
# ANOVA for continuous variables with proper error handling
anova_results <- list()
for (var in profile_vars) {
  # Create a temporary data frame without NA/Inf values for this variable
  temp_data <- smart_fresh %>%
    filter(!is.na(get(var)) & is.finite(get(var)))
  
  # Only run ANOVA if we have sufficient data
  if(nrow(temp_data) > 0) {
    formula <- as.formula(paste(var, "~ cluster"))
    tryCatch({
      anova_results[[var]] <- summary(aov(formula, data = temp_data))
      cat("\nANOVA for", var, ":\n")
      print(anova_results[[var]])
    }, error = function(e) {
      cat("\nError in ANOVA for", var, ":", e$message, "\n")
    })
  } else {
    cat("\nSkipping ANOVA for", var, "- insufficient valid data\n")
  }
}


# Print ANOVA results
for (var in names(anova_results)) {
  cat("\nANOVA for", var, ":\n")
  print(anova_results[[var]])
}

# Create visualizations comparing clusters
# 1. Radar chart of key variables by cluster
# First, prepare data for radar chart
radar_vars <- c("Spend_Wine", "Spend_OrganicFood", "Spend_Meat", 
                "Spend_WellnessProducts", "Spend_Treats", "Spend_LuxuryGoods",
                "Purchases_Online", "Purchases_Store", 
                "Online_Preference", "Promo_Response_Rate")

# Scale values for radar chart (0-1 scale)
radar_data <- smart_fresh %>%
  group_by(cluster) %>%
  summarize(across(all_of(radar_vars), ~mean(., na.rm = TRUE))) %>%
  ungroup()

# Scale each variable to 0-1 range
radar_data_scaled <- radar_data
for (var in radar_vars) {
  min_val <- min(radar_data[[var]])
  max_val <- max(radar_data[[var]])
  radar_data_scaled[[var]] <- (radar_data[[var]] - min_val) / (max_val - min_val)
}

# Convert to long format for plotting
radar_long <- radar_data_scaled %>%
  pivot_longer(cols = all_of(radar_vars), 
               names_to = "variable", 
               values_to = "value")

# Create radar chart using ggplot2
# First, calculate the angles for each variable
radar_long$angle <- (as.numeric(factor(radar_long$variable)) - 1) * 
  (2 * pi / length(unique(radar_long$variable)))
radar_long$hjust <- ifelse(sin(radar_long$angle) > 0, 0, 1)
radar_long$vjust <- ifelse(cos(radar_long$angle) < 0, 1, 0)

# Create the radar chart
ggplot(radar_long, aes(x = variable, y = value, group = cluster, color = as.factor(cluster))) +
  geom_polygon(aes(fill = as.factor(cluster)), alpha = 0.1) +
  geom_line() +
  geom_point() +
  coord_polar() +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Cluster Profiles Across Key Variables",
       x = "",
       y = "",
       color = "Cluster",
       fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0))

# 2. Bar charts for key spending variables
spend_vars <- c("Spend_Wine", "Spend_OrganicFood", "Spend_Meat", 
                "Spend_WellnessProducts", "Spend_Treats", "Spend_LuxuryGoods")

spend_data <- smart_fresh %>%
  group_by(cluster) %>%
  summarize(across(all_of(spend_vars), ~mean(., na.rm = TRUE))) %>%
  pivot_longer(cols = all_of(spend_vars), 
               names_to = "category", 
               values_to = "spending")

# Create bar chart
ggplot(spend_data, aes(x = category, y = spending, fill = as.factor(cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Spending by Category and Cluster",
       x = "Product Category",
       y = "Average Spending ($)",
       fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = dollar_format()) +
  scale_fill_brewer(palette = "Set1")

# 3. Boxplots for income and total spending by cluster
income_plot <- ggplot(smart_fresh, aes(x = cluster, y = Annual_Income, fill = cluster)) +
  geom_boxplot() +
  labs(title = "Annual Income Distribution by Cluster",
       x = "Cluster",
       y = "Annual Income ($)") +
  theme_minimal() +
  scale_y_continuous(labels = dollar_format()) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "none")

spending_plot <- ggplot(smart_fresh, aes(x = cluster, y = Total_Spending, fill = cluster)) +
  geom_boxplot() +
  labs(title = "Total Spending Distribution by Cluster",
       x = "Cluster",
       y = "Total Spending ($)") +
  theme_minimal() +
  scale_y_continuous(labels = dollar_format()) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "none")

# Arrange the plots side by side
grid.arrange(income_plot, spending_plot, ncol = 2)
