# ===================================================
# Optimizing Urban Flood Control using R Programming
# Lab Assignment – 4
# Student Name: Sparsh Karna
# Reg. No.: 23BDS1172
# ===================================================

# ============== Dataset Creation Section ==============

# 20 Urban Zones
zone_name_1172 <- paste("Zone", 1:20)

# Simulated Data
set.seed(1172)  # For reproducibility
rainfall_mm_1172 <- sample(50:500, 20, replace = TRUE)
drainage_capacity_1172 <- sample(50:500, 20, replace = TRUE)
population_1172 <- sample(1000:10000, 20, replace = TRUE)
water_logging_cm_1172 <- sample(50:200, 20, replace = TRUE)
green_cover_percent_1172 <- sample(10:50, 20, replace = TRUE)

# Creating the list
city_flood_data_1172 <- list(
  zone_name = zone_name_1172,
  rainfall_mm = rainfall_mm_1172,
  drainage_capacity = drainage_capacity_1172,
  population = population_1172,
  water_logging_cm = water_logging_cm_1172,
  green_cover_percent = green_cover_percent_1172
)

# ============== User-Defined Function: Display Structure ==============

#' @title Display Data Structure
#' @description Prints the dataset and its structure
display_data_structure_1172 <- function() {
  cat("----- City Flood Dataset -----\n\n")
  print(city_flood_data_1172)
  cat("\n----- Structure of the Dataset -----\n\n")
  str(city_flood_data_1172)
}

# ============== Built-in Functions Section ==============

# Zone with highest rainfall
max_rainfall_index_1172 <- which.max(city_flood_data_1172$rainfall_mm)
cat("Zone with Highest Rainfall:", city_flood_data_1172$zone_name[max_rainfall_index_1172], "\n\n")

# Average water logging
avg_water_logging_1172 <- mean(city_flood_data_1172$water_logging_cm)
cat("Average Water Logging (cm):", round(avg_water_logging_1172, 2), "\n\n")

# Below-average green cover zones
below_avg_gc_1172 <- city_flood_data_1172$green_cover_percent < mean(city_flood_data_1172$green_cover_percent)
cat("Zones with Below-Average Green Cover:\n")
print(city_flood_data_1172$zone_name[below_avg_gc_1172])
cat("\n")

# Zones sorted by population
sorted_population_indices_1172 <- order(city_flood_data_1172$population, decreasing = TRUE)
cat("Zones Sorted by Population (Descending):\n")
print(city_flood_data_1172$zone_name[sorted_population_indices_1172])
cat("\n")

# ============== User-defined Functions Section ==============

# Flood risk assessment
assess_flood_risk_1172 <- function(rainfall, drainage) {
  diff <- rainfall - drainage
  if (diff > 200) {
    return("High")
  } else if (diff > 50) {
    return("Moderate")
  } else {
    return("Low")
  }
}

# Suggest mitigation strategy
suggest_mitigation_1172 <- function(green_cover, population) {
  if (green_cover < 20 && population > 5000) {
    return("Increase Tree Plantation")
  } else if (green_cover < 30) {
    return("Install Rain Gardens")
  } else {
    return("Create Green Roofs")
  }
}

# Apply functions to all zones
risk_level_1172 <- mapply(assess_flood_risk_1172, city_flood_data_1172$rainfall_mm, city_flood_data_1172$drainage_capacity)
mitigation_suggestion_1172 <- mapply(suggest_mitigation_1172, city_flood_data_1172$green_cover_percent, city_flood_data_1172$population)

# Add to list
city_flood_data_1172$risk_level <- risk_level_1172
city_flood_data_1172$mitigation_suggestion <- mitigation_suggestion_1172

# ============== Vector Operations Section ==============

# Adjusted water logging
adjusted_water_logging_cm_1172 <- city_flood_data_1172$water_logging_cm
adjusted_water_logging_cm_1172[city_flood_data_1172$green_cover_percent > 30] <-
  adjusted_water_logging_cm_1172[city_flood_data_1172$green_cover_percent > 30] * 0.8

# Severity index calculation
severity_index_1172 <- (city_flood_data_1172$rainfall_mm -
                          city_flood_data_1172$drainage_capacity +
                          adjusted_water_logging_cm_1172) /
  city_flood_data_1172$population

# Add to dataset
city_flood_data_1172$adjusted_water_logging_cm <- round(adjusted_water_logging_cm_1172, 2)
city_flood_data_1172$severity_index <- round(severity_index_1172, 4)

# ============== Filtering and Summary Section ==============

cat("--- High Risk Zone Summary (Severity Index > 0.05) ---\n\n")
high_risk_indices_1172 <- which(city_flood_data_1172$severity_index > 0.05)

# Print summary
for (i in high_risk_indices_1172) {
  cat("Zone:", city_flood_data_1172$zone_name[i], "\t")
  cat("Severity Index:", city_flood_data_1172$severity_index[i], "\t")
  cat("Mitigation:", city_flood_data_1172$mitigation_suggestion[i], "\n")
}

# ============== Display Full Dataset Structure ==============
cat("\n\n--- Dataset Display ---\n\n")
display_data_structure_1172()

cat("\n=== Analysis Complete ===\n")
