# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(car)
library(effsize)
library(pwr)

# Import the dataset
smart_fresh <- read.csv("F:\\UoB Study\\Marketing Analysis & Behaviour Science\\Assignment 1\\SmartFresh_Retail_Clean.csv")


# Create a spending per store visit variable
smart_fresh$spending_per_visit <- ifelse(smart_fresh$Purchases_Store > 0, 
                                         (smart_fresh$Spend_Wine + 
                                            smart_fresh$Spend_OrganicFood + 
                                            smart_fresh$Spend_Meat + 
                                            smart_fresh$Spend_WellnessProducts + 
                                            smart_fresh$Spend_Treats + 
                                            smart_fresh$Spend_LuxuryGoods) / 
                                           smart_fresh$Purchases_Store, 
                                         NA)

# Since we don't have explicit urban/suburban information, we'll create a proxy
# Assumption: Higher income and more online activity might indicate urban customers
# This is a simplification for demonstration purposes
smart_fresh$location_type <- ifelse(smart_fresh$Annual_Income > median(smart_fresh$Annual_Income) & 
                                      smart_fresh$Visits_OnlineLastMonth > median(smart_fresh$Visits_OnlineLastMonth),
                                    "Urban", "Suburban")

# Convert to factor
smart_fresh$location_type <- as.factor(smart_fresh$location_type)

#Exploratory Data Analysis
# Remove rows with NA in spending_per_visit (customers with no store purchases)
smart_fresh_filtered <- smart_fresh %>%
  filter(!is.na(spending_per_visit))

# Summary statistics by location type
summary_stats <- smart_fresh_filtered %>%
  group_by(location_type) %>%
  summarize(
    count = n(),
    mean_spending = mean(spending_per_visit, na.rm = TRUE),
    median_spending = median(spending_per_visit, na.rm = TRUE),
    sd_spending = sd(spending_per_visit, na.rm = TRUE),
    min_spending = min(spending_per_visit, na.rm = TRUE),
    max_spending = max(spending_per_visit, na.rm = TRUE),
    IQR_spending = IQR(spending_per_visit, na.rm = TRUE)
  )

print(summary_stats)

# Distribution of spending per visit by location type
ggplot(smart_fresh_filtered, aes(x = spending_per_visit, fill = location_type)) +
  geom_histogram(alpha = 0.7, position = "dodge", bins = 30) +
  labs(title = "Distribution of Spending per Store Visit",
       x = "Spending per Visit ($)",
       y = "Frequency",
       fill = "Location Type") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~ location_type, scales = "free_y")

# Boxplot for comparison
ggplot(smart_fresh_filtered, aes(x = location_type, y = spending_per_visit, fill = location_type)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Spending per Store Visit by Location Type",
       x = "Location Type",
       y = "Spending per Visit ($)",
       fill = "Location Type") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  coord_flip() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black")

# Density plot
ggplot(smart_fresh_filtered, aes(x = spending_per_visit, fill = location_type)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Spending per Store Visit",
       x = "Spending per Visit ($)",
       y = "Density",
       fill = "Location Type") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")


# 1. Check for normality within each group
# Shapiro-Wilk test for each group
urban_data <- smart_fresh_filtered %>% 
  filter(location_type == "Urban") %>% 
  pull(spending_per_visit)

suburban_data <- smart_fresh_filtered %>% 
  filter(location_type == "Suburban") %>% 
  pull(spending_per_visit)

shapiro_urban <- shapiro.test(urban_data)
shapiro_suburban <- shapiro.test(suburban_data)

print(shapiro_urban)
print(shapiro_suburban)

# QQ plots for each group
par(mfrow = c(1, 2))
qqPlot(urban_data, main = "QQ Plot - Urban Customers")
qqPlot(suburban_data, main = "QQ Plot - Suburban Customers")
par(mfrow = c(1, 1))

# 2. Check for homogeneity of variances
levene_test <- leveneTest(spending_per_visit ~ location_type, data = smart_fresh_filtered)
print(levene_test)

# 3. Check for outliers using boxplots (already created above)

# If assumptions are violated, we might need to:
# 1. Transform the data (log transformation is common for spending data)
# 2. Use non-parametric tests like Wilcoxon rank-sum test
# Let's prepare for both scenarios

# Log transformation (adding small constant to handle zeros)
smart_fresh_filtered$log_spending_per_visit <- log(smart_fresh_filtered$spending_per_visit + 1)

# Check normality of log-transformed data
shapiro_urban_log <- shapiro.test(log(urban_data + 1))
shapiro_suburban_log <- shapiro.test(log(suburban_data + 1))

print(shapiro_urban_log)
print(shapiro_suburban_log)

#T-Tests
# Based on the assumption checks, choose the appropriate test
# If assumptions are met, use t-test
t_test_result <- t.test(spending_per_visit ~ location_type, data = smart_fresh_filtered, 
                        var.equal = (levene_test$p.value > 0.05))
print(t_test_result)

# If normality assumptions are violated, use Wilcoxon rank-sum test
wilcox_test_result <- wilcox.test(spending_per_visit ~ location_type, data = smart_fresh_filtered)
print(wilcox_test_result)

# If log transformation improved normality, use t-test on log-transformed data
t_test_log_result <- t.test(log_spending_per_visit ~ location_type, data = smart_fresh_filtered,
                            var.equal = (leveneTest(log_spending_per_visit ~ location_type, 
                                                    data = smart_fresh_filtered)$p.value > 0.05))
print(t_test_log_result)

#Effect Size and Power Analysis
# Calculate Cohen's d effect size
effect_size <- cohen.d(smart_fresh_filtered$spending_per_visit, 
                       smart_fresh_filtered$location_type)
print(effect_size)

# Calculate effect size for log-transformed data
effect_size_log <- cohen.d(smart_fresh_filtered$log_spending_per_visit, 
                           smart_fresh_filtered$location_type)
print(effect_size_log)

# Power analysis
# Get sample sizes
n_urban <- sum(smart_fresh_filtered$location_type == "Urban")
n_suburban <- sum(smart_fresh_filtered$location_type == "Suburban")

# Power analysis based on observed effect size
power_analysis <- pwr.t2n.test(n1 = n_urban, 
                               n2 = n_suburban, 
                               d = abs(effect_size$estimate), 
                               sig.level = 0.05,
                               power = NULL)
print(power_analysis)

# Calculate required sample size for 80% power
sample_size_analysis <- pwr.t2n.test(n1 = NULL, 
                                     n2 = NULL, 
                                     d = abs(effect_size$estimate), 
                                     sig.level = 0.05,
                                     power = 0.8)
print(sample_size_analysis)

# Create a more informative comparison plot
ggplot(smart_fresh_filtered, aes(x = location_type, y = spending_per_visit, fill = location_type)) +
  geom_violin(alpha = 0.6) +
  geom_boxplot(width = 0.2, alpha = 0.8) +
  labs(title = "Spending per Store Visit by Location Type",
       subtitle = paste("p-value =", round(t_test_result$p.value, 4), 
                        "| Cohen's d =", round(effect_size$estimate, 2)),
       x = "Location Type",
       y = "Spending per Visit ($)",
       fill = "Location Type") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  geom_text(data = summary_stats, 
            aes(x = location_type, y = max_spending * 0.9, 
                label = paste("Mean: $", round(mean_spending, 2), "\nSD: $", round(sd_spending, 2))),
            size = 3.5)

# Create a bar chart with error bars
ggplot(summary_stats, aes(x = location_type, y = mean_spending, fill = location_type)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_spending - sd_spending/sqrt(count), 
                    ymax = mean_spending + sd_spending/sqrt(count)),
                width = 0.2) +
  labs(title = "Average Spending per Store Visit by Location Type",
       subtitle = paste("Error bars represent standard error of the mean"),
       x = "Location Type",
       y = "Mean Spending per Visit ($)",
       fill = "Location Type") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  geom_text(aes(label = paste("$", round(mean_spending, 2))), vjust = -1, size = 4)

# Create a scatter plot of income vs. spending per visit, colored by location type
ggplot(smart_fresh_filtered, aes(x = Annual_Income, y = spending_per_visit, color = location_type)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  labs(title = "Relationship Between Income and Spending per Store Visit",
       x = "Annual Income ($)",
       y = "Spending per Visit ($)",
       color = "Location Type") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~ location_type, scales = "free")


# Create a summary of results
cat("\n=== URBAN VS SUBURBAN SPENDING ANALYSIS SUMMARY ===\n")
cat("Urban Customers (n =", n_urban, "):\n")
cat("  Mean Spending per Visit: $", round(summary_stats$mean_spending[summary_stats$location_type == "Urban"], 2), "\n")
cat("  Median Spending per Visit: $", round(summary_stats$median_spending[summary_stats$location_type == "Urban"], 2), "\n")
cat("  SD: $", round(summary_stats$sd_spending[summary_stats$location_type == "Urban"], 2), "\n\n")

cat("Suburban Customers (n =", n_suburban, "):\n")
cat("  Mean Spending per Visit: $", round(summary_stats$mean_spending[summary_stats$location_type == "Suburban"], 2), "\n")
cat("  Median Spending per Visit: $", round(summary_stats$median_spending[summary_stats$location_type == "Suburban"], 2), "\n")
cat("  SD: $", round(summary_stats$sd_spending[summary_stats$location_type == "Suburban"], 2), "\n\n")

cat("Statistical Tests:\n")
cat("  T-test p-value:", round(t_test_result$p.value, 4), "\n")
cat("  Wilcoxon test p-value:", round(wilcox_test_result$p.value, 4), "\n")
cat("  Effect size (Cohen's d):", round(effect_size$estimate, 2), "\n")
cat("  Interpretation:", ifelse(abs(effect_size$estimate) < 0.2, "Small effect", 
                                ifelse(abs(effect_size$estimate) < 0.5, "Medium effect", "Large effect")), "\n\n")

cat("Power Analysis:\n")
cat("  Statistical Power:", round(power_analysis$power, 2), "\n")
cat("  Required sample size for 80% power:", ceiling(sample_size_analysis$n1), "per group\n\n")

cat("Conclusion: ", ifelse(t_test_result$p.value < 0.05, 
                           "Urban customers spend significantly more per store visit than suburban customers.", 
                           "There is no significant difference in spending per store visit between urban and suburban customers."), "\n")

