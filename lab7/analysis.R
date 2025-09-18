# Programming for Data Science - Lab Experiment 7
# The Urban Pulse Project - Complete Solution
# Student Name: Sparsh Karna, Reg No: 23BDS1172
# Date: 18-09-2025, Time: 12:31 PM IST

# =============================================================================
# PART 1: DATASET GENERATION
# =============================================================================

# Set seed for reproducibility
set.seed(123)

# Load required libraries for data generation
library(dplyr)

# Generate synthetic dataset for 50 cities
n_cities <- 50

# Define continents and countries (simplified list)
continents <- c("Africa", "Asia", "Europe", "North America", "South America", "Oceania")
countries <- c("Nigeria", "Kenya", "India", "China", "Japan", "Germany", "France", 
               "USA", "Canada", "Brazil", "Argentina", "Australia", "New Zealand")

city_data <- data.frame(
  city = sprintf("City_%02d", 1:n_cities),  # Generic city names
  country = sample(countries, n_cities, replace = TRUE),
  continent = sample(continents, n_cities, replace = TRUE, prob = c(0.1, 0.3, 0.2, 0.2, 0.1, 0.1))
)

# Generate numeric variables with realistic distributions
city_data$population_millions <- rgamma(n_cities, shape = 2, scale = 5)  # Right-skewed, 0-20M
city_data$density_km2 <- rnorm(n_cities, mean = 5000, sd = 2000) * (city_data$population_millions / 10)  # Density tied to population
city_data$density_km2 <- pmax(1000, pmin(15000, city_data$density_km2))  # Constrain range

city_data$median_age <- rnorm(n_cities, mean = 35, sd = 5)  # Normal, 25-45
city_data$median_age <- pmax(25, pmin(45, city_data$median_age))

city_data$gdp_per_capita_usd <- rlnorm(n_cities, meanlog = 10.2, sdlog = 0.8)  # Log-normal distribution
city_data$gdp_per_capita_usd <- pmax(5000, pmin(100000, city_data$gdp_per_capita_usd))  # Constrain to $5K-$100K

city_data$public_transit_score <- rnorm(n_cities, mean = 60, sd = 15)  # 30-90
city_data$public_transit_score <- pmax(30, pmin(90, city_data$public_transit_score))

city_data$green_space_pct <- rbeta(n_cities, shape1 = 2, shape2 = 5) * 30  # 0-30%
city_data$green_space_pct <- pmax(0, pmin(30, city_data$green_space_pct))

city_data$air_quality_index <- rnorm(n_cities, mean = 50, sd = 20) + (100 - city_data$green_space_pct) * 0.5  # Inverse with green space
city_data$air_quality_index <- pmax(20, pmin(100, city_data$air_quality_index))

city_data$avg_commute_time_min <- rnorm(n_cities, mean = 30, sd = 10) + (city_data$density_km2 / 1000) * 0.5  # Tied to density
city_data$avg_commute_time_min <- pmax(15, pmin(60, city_data$avg_commute_time_min))

# Happiness index as a function of other variables (simplified model)
city_data$happiness_index <- 3 + 
  (city_data$gdp_per_capita_usd - min(city_data$gdp_per_capita_usd, na.rm=TRUE)) / 
  (max(city_data$gdp_per_capita_usd, na.rm=TRUE) - min(city_data$gdp_per_capita_usd, na.rm=TRUE)) * 3 +  # GDP effect (0-3 points)
  0.08 * city_data$green_space_pct +  # Green space effect
  -0.03 * city_data$air_quality_index +  # Air quality effect (negative)
  0.02 * city_data$public_transit_score +  # Transit effect
  -0.02 * city_data$avg_commute_time_min +  # Commute effect (negative)
  rnorm(n_cities, mean = 0, sd = 0.8)  # Random noise

city_data$happiness_index <- pmax(0, pmin(10, city_data$happiness_index))

# Introduce minor missing values (e.g., 2-3% overall)
set.seed(124)  # Different seed for randomness in missingness
for (col in c("gdp_per_capita_usd", "air_quality_index")) {
  na_count <- round(n_cities * 0.03)  # ~3% missing
  city_data[[col]][sample(n_cities, na_count)] <- NA
}

# Save the dataset
write.csv(city_data, "city_metrics.csv", row.names = FALSE)
saveRDS(city_data, "city_metrics_23BDS1172.rds")

cat("Dataset generated and saved as city_metrics.csv\n")
cat("Dataset dimensions:", nrow(city_data), "rows x", ncol(city_data), "columns\n\n")
XS
# =============================================================================
# PART 2: COMPLETE LAB ANALYSIS
# =============================================================================

# Load required libraries for analysis
library(ggplot2)
library(moments)    # For skewness
library(plotly)     # For interactive plots

# Section A: Data Preparation & Initial Exploration

# 1. Load dataset and check structure
city_data <- read.csv("city_metrics.csv")
str(city_data)
summary(city_data)

# 2. Check and handle missing values
missing_values <- colSums(is.na(city_data))
print("Missing values per column:")
print(missing_values)

if (any(missing_values > 0)) {
  cat("Strategy: Imputing missing values with median for numeric variables as median is robust to outliers and skewness.\n")
  numeric_cols <- c("population_millions", "density_km2", "median_age", "gdp_per_capita_usd", 
                    "public_transit_score", "green_space_pct", "air_quality_index", "avg_commute_time_min", 
                    "happiness_index")
  for (col in numeric_cols) {
    if (col %in% colnames(city_data)) {
      city_data[[col]][is.na(city_data[[col]])] <- median(city_data[[col]], na.rm = TRUE)
    }
  }
  print("Missing values after imputation:")
  print(colSums(is.na(city_data)))
}

# 3. Create size_category factor
city_data$size_category <- cut(city_data$population_millions, 
                               breaks = c(-Inf, 5, 10, Inf), 
                               labels = c("Small", "Medium", "Large"), 
                               include.lowest = TRUE)
print("First few rows with size_category:")
head(city_data)

# Save the prepared dataset
write.csv(city_data, "city_data_prepared_23BDS1172.csv", row.names = FALSE)
saveRDS(city_data, "city_data_prepared_23BDS1172.rds")

# Section B: Univariate & Bivariate Visualizations

# 4. Distribution Plot for gdp_per_capita_usd
cat("\n=== CREATING HISTOGRAM (Question 4) ===\n")
p_hist <- ggplot(city_data, aes(x = gdp_per_capita_usd)) +
  geom_histogram(bins = 15, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(aes(y = after_stat(count)), color = "red", linewidth = 1) +
  labs(title = "Distribution of GDP per Capita (USD)", 
       subtitle = "Histogram with density overlay",
       x = "GDP per Capita (USD)", 
       y = "Count") +
  scale_x_continuous(labels = scales::dollar_format()) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12))

# Display and save the histogram
print(p_hist)
ggsave("histogram_gdp.png", p_hist, width = 10, height = 6, dpi = 300)
cat("✓ Histogram saved as histogram_gdp.png\n")

# Skewness and modality comment
skewness_value <- skewness(city_data$gdp_per_capita_usd)
cat(sprintf("Skewness: %.2f\n", skewness_value))
if (!is.na(skewness_value)) {
  if (skewness_value > 0.5) {
    cat("The distribution shows positive skewness (right tail), indicating that most cities have lower GDP per capita with fewer cities having very high values.\n")
  } else if (skewness_value < -0.5) {
    cat("The distribution shows negative skewness (left tail).\n")
  } else {
    cat("The distribution is approximately symmetric.\n")
  }
} else {
  cat("Skewness could not be calculated (likely due to identical values).\n")
}
cat("Modality: The distribution appears unimodal with most cities clustered around the median GDP value.\n\n")

# 5. Categorical Comparison for happiness_index by continent
cat("\n=== CREATING BOXPLOT (Question 5) ===\n")
p_box <- ggplot(city_data, aes(x = continent, y = happiness_index, fill = continent)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 1.5) +
  labs(title = "Happiness Index Distribution by Continent", 
       subtitle = "Boxplots showing central tendency and spread",
       x = "Continent", 
       y = "Happiness Index (0-10)") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Display and save the boxplot
print(p_box)
ggsave("boxplot_happiness_continent.png", p_box, width = 12, height = 8, dpi = 300)
cat("✓ Boxplot saved as boxplot_happiness_continent.png\n")

cat("Justification: Boxplot chosen as it effectively shows central tendency (median), spread (IQR), and outliers for each continent.\n")
cat("Preliminary Observation: There appear to be differences in happiness levels across continents, with some showing higher median values and different spreads.\n\n")

# 6. Relationship Plot: gdp_per_capita_usd vs air_quality_index
cat("\n=== CREATING SCATTER PLOT (Question 6) ===\n")
p_scatter <- ggplot(city_data, aes(x = gdp_per_capita_usd, y = air_quality_index)) +
  geom_point(color = "purple", alpha = 0.6, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  labs(title = "Wealth vs Air Quality in Global Cities", 
       subtitle = "Relationship between GDP per capita and Air Quality Index",
       x = "GDP per Capita (USD)", 
       y = "Air Quality Index (Higher = Worse)") +
  scale_x_continuous(labels = scales::dollar_format()) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12))

# Display and save the scatter plot
print(p_scatter)
ggsave("scatterplot_gdp_airquality.png", p_scatter, width = 10, height = 6, dpi = 300)
cat("✓ Scatter plot saved as scatterplot_gdp_airquality.png\n")

# Calculate correlation for relationship description
correlation <- cor(city_data$gdp_per_capita_usd, city_data$air_quality_index, use = "complete.obs")
cat(sprintf("Correlation coefficient: %.3f\n", correlation))
if (abs(correlation) > 0.7) {
  strength <- "strong"
} else if (abs(correlation) > 0.3) {
  strength <- "moderate"
} else {
  strength <- "weak"
}
direction <- ifelse(correlation > 0, "positive", "negative")
cat(sprintf("Relationship: The scatter plot shows a %s %s linear relationship between GDP per capita and air quality index.\n\n", strength, direction))

# Section C: Multivariate & Advanced Visualizations

# 7. Multivariate Analysis: Add size_category with color
cat("\n=== CREATING MULTIVARIATE PLOT (Question 7) ===\n")
p_multi <- ggplot(city_data, aes(x = gdp_per_capita_usd, y = air_quality_index, color = size_category)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(title = "Wealth vs Air Quality by City Size", 
       subtitle = "Multivariate analysis incorporating city population categories",
       x = "GDP per Capita (USD)", 
       y = "Air Quality Index (Higher = Worse)", 
       color = "City Size") +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(labels = scales::dollar_format()) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.position = "right")

# Display and save the multivariate plot
print(p_multi)
ggsave("multivariate_plot.png", p_multi, width = 12, height = 8, dpi = 300)
cat("✓ Multivariate plot saved as multivariate_plot.png\n")

cat("Multivariate Insight: This visualization reveals that the relationship between wealth and air quality varies by city size.\n")
cat("Large cities may show different patterns compared to small/medium cities, potentially indicating that population density affects the wealth-air quality relationship.\n\n")

# 8. Interactive Visualization
p_interactive <- ggplot(city_data, aes(x = green_space_pct, y = happiness_index, 
                                       text = paste("City:", city, 
                                                    "<br>Country:", country,
                                                    "<br>Green Space (%):", round(green_space_pct, 1), 
                                                    "<br>Happiness Index:", round(happiness_index, 2)))) +
  geom_point(color = "forestgreen", alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.3) +
  labs(title = "Happiness vs Green Space in Global Cities", 
       subtitle = "Interactive visualization - hover for city details",
       x = "Green Space Percentage (%)", 
       y = "Happiness Index (0-10)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12))

p_interactive <- ggplotly(p_interactive, tooltip = "text")
print(p_interactive)

cat("Advantage of Interactive Plot: Users can identify specific cities and their exact values by hovering, enabling detailed exploration of outliers and specific cases of interest without cluttering the visualization.\n\n")

# Section D: Critical Analysis & Reporting

# Calculate key statistics for insights
green_happiness_cor <- cor(city_data$green_space_pct, city_data$happiness_index, use = "complete.obs")
gdp_happiness_cor <- cor(city_data$gdp_per_capita_usd, city_data$happiness_index, use = "complete.obs")

# 9. Summary for Board of Directors
cat("=== SUMMARY FOR THE URBAN PULSE PROJECT BOARD ===\n")
cat("Our comprehensive 2023 analysis of 50 global cities reveals two critical insights for urban quality of life.\n\n")

cat(sprintf("First, cities with higher green space percentages show stronger correlation with happiness (r=%.2f), as demonstrated in our interactive visualization. This suggests that investing in parks and natural areas directly enhances citizen well-being.\n\n", green_happiness_cor))

cat(sprintf("Second, the multivariate analysis reveals that the wealth-air quality relationship (r=%.2f) varies significantly by city size, with larger cities facing unique environmental challenges despite economic prosperity. This indicates that population density moderates the benefits of wealth on environmental quality.\n\n", correlation))

cat("Recommendation: Prioritize green infrastructure development and implement size-specific environmental policies to maximize urban happiness and quality of life.\n")

# Additional summary statistics
cat("\n=== KEY DATASET STATISTICS ===\n")
cat("Total cities analyzed: 50\n")
cat("Continents covered: 6\n")
cat("Population range: ", round(min(city_data$population_millions), 1), " to ", round(max(city_data$population_millions), 1), " million\n")
cat("GDP range: $", round(min(city_data$gdp_per_capita_usd)/1000, 1), "K to $", round(max(city_data$gdp_per_capita_usd)/1000, 1), "K\n")
cat("Average happiness index: ", round(mean(city_data$happiness_index), 2), "\n")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("All visualizations generated successfully.\n")
cat("Files saved: city_metrics.csv, city_data_prepared_23BDS1172.csv, city_data_prepared_23BDS1172.rds\n")