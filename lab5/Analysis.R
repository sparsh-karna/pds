# ================================
# Solar System Data Representation and Analysis
# Registration Number: 23BDS1172
# ================================

cat("=== Solar System Lab EXP-5 ===\n\n")

# -------------------------------
# Part A: Array (10 Marks)
# -------------------------------
planet_names_1172 <- c("Mercury","Venus","Earth","Mars",
                       "Jupiter","Saturn","Uranus","Neptune")
orbital_period_days_1172 <- c(88,225,365,687,4333,10759,30687,60190)

# Create array of planets and periods
planet_array_1172 <- array(c(planet_names_1172, orbital_period_days_1172),
                           dim=c(8,2),
                           dimnames=list(NULL,c("Planet","Orbital_Period_Days")))

# Display orbital period of Earth
cat("Orbital period of Earth (days):", orbital_period_days_1172[3], "\n\n")

# -------------------------------
# Part B: Matrix (15 Marks)
# -------------------------------
# Diameter (km) and Distance from Sun (million km)
diameter_km_1172 <- c(4879,12104,12742,6779,139820,116460,50724,49244)
distance_million_km_1172 <- c(57.9,108.2,149.6,227.9,778.5,1434,2871,4495)

planet_matrix_1172 <- matrix(c(diameter_km_1172, distance_million_km_1172),
                             nrow=8, ncol=2,
                             dimnames=list(planet_names_1172,
                                           c("Diameter_km","Distance_million_km")))

cat("Outer planets (Jupiter to Neptune):\n")
print(planet_matrix_1172[5:8, ])
cat("\n")

# -------------------------------
# Part C: List (15 Marks)
# -------------------------------
earth_list_1172 <- list(
  Name="Earth",
  Diameter_km=12742,
  Distance_million_km=149.6,
  Moons=1
)

cat("Earth List Details:\n")
cat("Name:", earth_list_1172$Name, "\n")
cat("Diameter (km):", earth_list_1172$Diameter_km, "\n")
cat("Distance from Sun (million km):", earth_list_1172$Distance_million_km, "\n")
cat("Moons:", earth_list_1172$Moons, "\n\n")

# -------------------------------
# Part D: Data Frame (30 Marks)
# -------------------------------
moons_1172 <- c(0,0,1,2,79,83,27,14)

planet_df_1172 <- data.frame(
  Planet=planet_names_1172,
  Diameter_km=diameter_km_1172,
  Distance_MillionKm=distance_million_km_1172,
  Orbital_Period_days=orbital_period_days_1172,
  No_of_Moons=moons_1172,
  stringsAsFactors=FALSE
)

cat("Terrestrial planets:\n")
print(subset(planet_df_1172, Planet %in% c("Mercury","Venus","Earth","Mars")))
cat("\n")

cat("Planet with maximum moons:\n")
print(planet_df_1172[which.max(planet_df_1172$No_of_Moons), ])
cat("\n")

cat("Planets sorted by distance:\n")
print(planet_df_1172[order(planet_df_1172$Distance_MillionKm), ])
cat("\n")

# -------------------------------
# Part E: User-Defined Functions (30 Marks)
# -------------------------------

#' @title Convert Orbital Period
#' @description Converts orbital period from days to Earth years (365 days = 1 year)
#' @param days Orbital period in days
#' @return Orbital period in years (rounded to 2 decimals)
convert_to_years_1172 <- function(days) {
  return(round(days/365, 2))
}
planet_df_1172$Orbital_Period_years <- convert_to_years_1172(planet_df_1172$Orbital_Period_days)

#' @title Planet Summary
#' @description Returns summary information for a given planet
#' @param name Name of the planet (string)
#' @return Character string summarizing diameter, distance, and moons
planet_summary_1172 <- function(name) {
  row <- subset(planet_df_1172, Planet==name)
  if (nrow(row)==0) {
    return(paste("Planet", name, "not found."))
  }
  return(paste("Planet:", row$Planet,
               "| Diameter:", row$Diameter_km, "km",
               "| Distance:", row$Distance_MillionKm, "million km",
               "| Moons:", row$No_of_Moons))
}

cat("Summary for Jupiter:\n")
cat(planet_summary_1172("Jupiter"), "\n\n")

#' @title Kepler k Calculator
#' @description Computes Kepler's constant k = P² / a³ for planets
#' @param period_days Orbital period in days
#' @param distance_AU Distance from Sun in Astronomical Units (AU)
#' @return Numeric value of Kepler's k (rounded to 3 decimals)
kepler_k_1172 <- function(period_days, distance_AU) {
  P_years <- period_days / 365
  return(round((P_years^2) / (distance_AU^3), 3))
}

planet_df_1172$Distance_AU <- distance_million_km_1172 / 149.6
planet_df_1172$Kepler_k <- kepler_k_1172(planet_df_1172$Orbital_Period_days,
                                         planet_df_1172$Distance_AU)

cat("Final Data Frame with Orbital Period in Years and Kepler k:\n")
print(planet_df_1172)
cat("\n=== Analysis Complete ===\n")
