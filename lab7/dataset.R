# Programming for Data Science - Lab Experiment 7
# Dataset Generation for The Urban Pulse Project
# Student Name: Sparsh Karna, Reg No: 23BDS1172
# Date: 18-09-2025, Time: 12:31 PM IST

# Set seed for reproducibility
set.seed(123)

# Load required libraries
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

city_data$gdp_per_capita_usd <- rlnorm(n_cities, meanlog = 10, sdlog = 0.5) * 1000  # Log-normal, $5K-$100K
city_data$gdp_per_capita_usd <- pmax(5000, pmin(100000, city_data$gdp_per_capita_usd))

city_data$public_transit_score <- rnorm(n_cities, mean = 60, sd = 15)  # 30-90
city_data$public_transit_score <- pmax(30, pmin(90, city_data$public_transit_score))

city_data$green_space_pct <- rbeta(n_cities, shape1 = 2, shape2 = 5) * 30  # 0-30%
city_data$green_space_pct <- pmax(0, pmin(30, city_data$green_space_pct))

city_data$air_quality_index <- rnorm(n_cities, mean = 50, sd = 20) + (100 - city_data$green_space_pct) * 0.5  # Inverse with green space
city_data$air_quality_index <- pmax(20, pmin(100, city_data$air_quality_index))

city_data$avg_commute_time_min <- rnorm(n_cities, mean = 30, sd = 10) + (city_data$density_km2 / 1000) * 0.5  # Tied to density
city_data$avg_commute_time_min <- pmax(15, pmin(60, city_data$avg_commute_time_min))

# Happiness index as a function of other variables (simplified model)
city_data$happiness_index <- 5 + 
  0.01 * city_data$gdp_per_capita_usd / 1000 +  # Positive wealth effect
  0.1 * city_data$green_space_pct -  # Positive green space effect
  0.05 * city_data$air_quality_index +  # Negative air quality effect
  0.05 * city_data$public_transit_score -  # Positive transit effect
  0.02 * city_data$avg_commute_time_min +  # Negative commute effect
  rnorm(n_cities, mean = 0, sd = 0.5)  # Random noise
city_data$happiness_index <- pmax(0, pmin(10, city_data$happiness_index))

# Introduce minor missing values (e.g., 2-3% overall)
set.seed(124)  # Different seed for randomness in missingness
for (col in c("gdp_per_capita_usd", "air_quality_index")) {
  na_count <- round(n_cities * 0.03)  # ~3% missing
  city_data[[col]][sample(n_cities, na_count)] <- NA
}

# Verify structure and summary
str(city_data)
summary(city_data)

# Save the dataset
write.csv(city_data, "city_metrics.csv", row.names = FALSE)
saveRDS(city_data, "city_metrics_23BDS1172.rds")

# End of Script