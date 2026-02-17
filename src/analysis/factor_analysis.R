# Load required libraries
library(tidyverse)      # For data manipulation and visualization
library(psych)          # For factor analysis functions
library(corrplot)       # For correlation visualization
library(GPArotation)    # For factor rotation methods
library(factoextra)     # For factor visualization
library(ggplot2)        # For advanced plotting
library(gridExtra)      # For arranging multiple plots
library(mice)           # For imputation of missing values
library(MVN)            # For multivariate normality testing

# Set seed for reproducibility
set.seed(123)

# -------------------------------------------------------------------------
# 1. DATA PREPARATION
# -------------------------------------------------------------------------

# Import the dataset
smartfresh_data <- read.csv("F:\\UoB Study\\Marketing Analysis & Behaviour Science\\Assignment 1\\SmartFresh_Retail_Clean.csv")

# Calculate age from birth year (using current date as reference)
current_year <- 2025  # Based on the current date
smartfresh_data$Age <- current_year - smartfresh_data$Year_Birth

# Create a new variable for total spending
smartfresh_data$Total_Spending <- smartfresh_data$Spend_Wine + 
  smartfresh_data$Spend_OrganicFood + 
  smartfresh_data$Spend_Meat + 
  smartfresh_data$Spend_WellnessProducts + 
  smartfresh_data$Spend_Treats + 
  smartfresh_data$Spend_LuxuryGoods

# Create a variable for average spending per purchase
smartfresh_data$Avg_Spend_Per_Purchase <- smartfresh_data$Total_Spending / 
  (smartfresh_data$Purchases_Online + 
     smartfresh_data$Purchases_Catalog + 
     smartfresh_data$Purchases_Store)

# Create a variable for online shopping preference
smartfresh_data$Online_Preference <- smartfresh_data$Purchases_Online / 
  (smartfresh_data$Purchases_Online + 
     smartfresh_data$Purchases_Catalog + 
     smartfresh_data$Purchases_Store)

# Create a variable for promotion responsiveness
smartfresh_data$Promo_Response_Rate <- smartfresh_data$Promo_Purchases / 
  (smartfresh_data$Purchases_Online + 
     smartfresh_data$Purchases_Catalog + 
     smartfresh_data$Purchases_Store)

# Select variables for factor analysis
# We'll focus on spending patterns, shopping behavior, and promotional response
factor_vars <- c("Spend_Wine", "Spend_OrganicFood", "Spend_Meat", 
                 "Spend_WellnessProducts", "Spend_Treats", "Spend_LuxuryGoods",
                 "Purchases_Online", "Purchases_Catalog", "Purchases_Store", 
                 "Visits_OnlineLastMonth", "Promo_Purchases", 
                 "Online_Preference", "Promo_Response_Rate")

# Create a subset with only the variables for factor analysis
factor_data <- smartfresh_data[, factor_vars]

# Check for missing values
missing_values <- colSums(is.na(factor_data))
print(missing_values)

# If there are missing values, impute them
if(sum(missing_values) > 0) {
  # Impute missing values using predictive mean matching
  imputation_model <- mice(factor_data, method = "pmm", m = 5, maxit = 50, seed = 123)
  factor_data <- complete(imputation_model)
}

# Check for outliers using boxplots
par(mfrow = c(3, 5))
for(i in 1:length(factor_vars)) {
  boxplot(factor_data[, i], main = factor_vars[i], col = "lightblue")
}
par(mfrow = c(1, 1))

# Handle outliers using winsorization (capping at 95th percentile)
for(i in 1:length(factor_vars)) {
  cap_value <- quantile(factor_data[, i], 0.95, na.rm = TRUE)
  factor_data[, i] <- pmin(factor_data[, i], cap_value)
}

# Scale the data (standardize to mean 0, sd 1)
factor_data_scaled <- scale(factor_data)

summary(factor_data_scaled)
# -------------------------------------------------------------------------
# 2. PRELIMINARY ANALYSIS FOR FACTOR ANALYSIS SUITABILITY
# -------------------------------------------------------------------------

# Compute correlation matrix
corr_matrix <- cor(factor_data_scaled)

# Visualize correlation matrix
corrplot(corr_matrix, method = "color", 
         tl.col = "black", tl.srt = 45, 
         title = "Correlation Matrix of Shopping Variables")

# Bartlett's test of sphericity
# Tests the null hypothesis that the correlation matrix is an identity matrix
bartlett_test <- cortest.bartlett(corr_matrix, n = nrow(factor_data_scaled))
print(bartlett_test)

# Kaiser-Meyer-Olkin (KMO) measure of sampling adequacy
# Values close to 1 indicate that factor analysis may be useful
kmo_result <- KMO(corr_matrix)
print(kmo_result)

# -------------------------------------------------------------------------
# 3. DETERMINE OPTIMAL NUMBER OF FACTORS
# -------------------------------------------------------------------------

# Scree plot
scree_plot <- scree(corr_matrix, factors = FALSE)

# Parallel analysis
parallel_result <- fa.parallel(factor_data_scaled, fm = "ml", fa = "fa")

# Based on Kaiser criterion (eigenvalues > 1)
eigen_values <- eigen(corr_matrix)$values
num_factors_kaiser <- sum(eigen_values > 1)
cat("Number of factors suggested by Kaiser criterion:", num_factors_kaiser, "\n")

# Based on parallel analysis
num_factors_parallel <- parallel_result$nfact
cat("Number of factors suggested by parallel analysis:", num_factors_parallel, "\n")

# Let's determine the optimal number of factors based on these methods
# For this analysis, we'll use the number suggested by parallel analysis
num_factors <- num_factors_parallel

# -------------------------------------------------------------------------
# 4. FACTOR EXTRACTION
# -------------------------------------------------------------------------

# Principal Component Analysis (PCA)
pca_result <- principal(factor_data_scaled, nfactors = num_factors, rotate = "none")
print(pca_result)

# Maximum Likelihood Factor Analysis
ml_result <- fa(factor_data_scaled, nfactors = num_factors, rotate = "none", fm = "ml")
print(ml_result)

# -------------------------------------------------------------------------
# 5. FACTOR ROTATION
# -------------------------------------------------------------------------

# Orthogonal rotation (Varimax) - assumes factors are uncorrelated
varimax_result <- fa(factor_data_scaled, nfactors = num_factors, rotate = "varimax", fm = "ml")
print(varimax_result)

# Oblique rotation (Promax) - allows factors to be correlated
promax_result <- fa(factor_data_scaled, nfactors = num_factors, rotate = "promax", fm = "ml")
print(promax_result)

# Compare factor correlation matrix from oblique rotation
# If correlations are substantial, oblique rotation is preferred
print(promax_result$Phi)

# Determine which rotation to use based on factor correlations
# If factor correlations are > 0.3, use oblique rotation (Promax)
# Otherwise, use orthogonal rotation (Varimax)
use_oblique <- any(abs(promax_result$Phi[upper.tri(promax_result$Phi)]) > 0.3)

if(use_oblique) {
  final_rotation <- "promax"
  final_result <- promax_result
  cat("Using oblique rotation (Promax) due to correlated factors\n")
} else {
  final_rotation <- "varimax"
  final_result <- varimax_result
  cat("Using orthogonal rotation (Varimax) due to uncorrelated factors\n")
}

# -------------------------------------------------------------------------
# 6. VISUALIZE AND INTERPRET FACTOR LOADINGS
# -------------------------------------------------------------------------

# Create a heatmap of factor loadings
loadings_matrix <- as.data.frame(unclass(final_result$loadings))
colnames(loadings_matrix) <- paste0("Factor_", 1:num_factors)
loadings_matrix$Variable <- rownames(loadings_matrix)
loadings_long <- pivot_longer(loadings_matrix, 
                              cols = starts_with("Factor_"),
                              names_to = "Factor",
                              values_to = "Loading")

# Plot the heatmap
ggplot(loadings_long, aes(x = Factor, y = Variable, fill = Loading)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Factor Loadings Heatmap", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.text.y = element_text(hjust = 1))

# Create a table with significant loadings (> 0.3)
loadings_table <- loadings_matrix
for (i in 1:num_factors) {
  col_name <- paste0("Factor_", i)
  loadings_table[, col_name] <- ifelse(abs(loadings_table[, col_name]) < 0.3, "", 
                                       round(loadings_table[, col_name], 2))
}
print(loadings_table)

# -------------------------------------------------------------------------
# 7. COMPUTE FACTOR SCORES
# -------------------------------------------------------------------------

# Compute factor scores for each customer
factor_scores <- factor.scores(factor_data_scaled, final_result)$scores
colnames(factor_scores) <- paste0("Factor_", 1:num_factors)

# Add factor scores to the original dataset
smartfresh_data_with_scores <- cbind(smartfresh_data, factor_scores)

# -------------------------------------------------------------------------
# 8. INTERPRET FACTORS AND NAME THEM
# -------------------------------------------------------------------------

# Create a function to get the top loading variables for each factor
get_top_loadings <- function(loadings_matrix, factor_num, top_n = 5) {
  factor_col <- paste0("Factor_", factor_num)
  sorted_loadings <- loadings_matrix[order(abs(loadings_matrix[, factor_col]), decreasing = TRUE), ]
  return(sorted_loadings[1:top_n, c("Variable", factor_col)])
}

# Print top loadings for each factor
for (i in 1:num_factors) {
  cat("\nTop loadings for Factor", i, ":\n")
  print(get_top_loadings(loadings_matrix, i))
}

# Based on the top loadings, suggest names for each factor
# This is a placeholder - actual names will depend on the results
factor_names <- c(
  "Health and Wellness Orientation",
  "In-Store Shopping Behavior",
  "Catalog Shopping Preference",
  "Promotion-Sensitive Customers",
  "Digital Engagement"
)

# If the number of factors is different, adjust accordingly
if(length(factor_names) != num_factors) {
  factor_names <- paste0("Factor ", 1:num_factors)
}

# Rename the factor score columns with meaningful names
colnames(factor_scores) <- factor_names
colnames(smartfresh_data_with_scores)[(ncol(smartfresh_data_with_scores) - num_factors + 1):ncol(smartfresh_data_with_scores)] <- factor_names

# -------------------------------------------------------------------------
# 9. ASSESS RELIABILITY OF FACTOR STRUCTURE
# -------------------------------------------------------------------------

# Calculate Cronbach's alpha for each factor
for (i in 1:num_factors) {
  # Get variables with high loadings on this factor (absolute value > 0.3)
  factor_col <- paste0("Factor_", i)
  high_loading_vars <- rownames(loadings_matrix)[abs(loadings_matrix[, factor_col]) > 0.3]
  
  if (length(high_loading_vars) > 1) {
    # Calculate Cronbach's alpha
    alpha_result <- psych::alpha(factor_data[, high_loading_vars])
    cat("\nCronbach's alpha for", factor_names[i], ":", round(alpha_result$total$raw_alpha, 3), "\n")
  } else {
    cat("\nFactor", factor_names[i], "has fewer than 2 high-loading variables, cannot calculate alpha\n")
  }
}

# -------------------------------------------------------------------------
# 10. VISUALIZE CUSTOMERS IN FACTOR SPACE
# -------------------------------------------------------------------------

# Create scatter plots of customers in the factor space
if (num_factors >= 2) {
  # Plot first two factors
  ggplot(as.data.frame(factor_scores), aes(x = factor_scores[,1], y = factor_scores[,2])) +
    geom_point(alpha = 0.5) +
    labs(title = "Customers in Factor Space",
         x = factor_names[1],
         y = factor_names[2]) +
    theme_minimal()
  
  # If there are 3 or more factors, create a pairs plot
  if (num_factors >= 3) {
    pairs.panels(factor_scores[, 1:min(num_factors, 4)], 
                 method = "pearson", 
                 hist.col = "#00AFBB",
                 density = TRUE,
                 ellipses = TRUE)
  }
}

# -------------------------------------------------------------------------
# 11. ENHANCED FACTOR VISUALIZATION
# -------------------------------------------------------------------------

# Create a biplot for the first two factors
if (num_factors >= 2) {
  # Extract loadings for the first two factors
  loadings_for_biplot <- loadings_matrix[, c("Factor_1", "Factor_2", "Variable")]
  
  # Create a biplot
  ggplot() +
    # Plot points (factor scores)
    geom_point(data = as.data.frame(factor_scores), 
               aes(x = factor_scores[,1], y = factor_scores[,2]), 
               alpha = 0.3) +
    # Plot arrows (factor loadings)
    geom_segment(data = loadings_for_biplot,
                 aes(x = 0, y = 0, 
                     xend = Factor_1 * 3, # Scale for visibility
                     yend = Factor_2 * 3),
                 arrow = arrow(length = unit(0.2, "cm")),
                 color = "red") +
    # Add variable labels
    geom_text(data = loadings_for_biplot,
              aes(x = Factor_1 * 3.2, # Position labels slightly beyond arrow tips
                  y = Factor_2 * 3.2,
                  label = Variable),
              size = 3) +
    # Add theme and labels
    theme_minimal() +
    labs(title = "Biplot: Variables and Observations in Factor Space",
         x = paste(factor_names[1]),
         y = paste(factor_names[2])) +
    # Add a unit circle for reference
    geom_path(data = data.frame(x = cos(seq(0, 2*pi, length.out = 100)),
                                y = sin(seq(0, 2*pi, length.out = 100))),
              aes(x = x, y = y),
              linetype = "dashed", color = "gray") +
    coord_fixed() # Equal scaling for x and y axes
}



