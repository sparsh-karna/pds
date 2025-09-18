# --------------------------------------------------
# Name: Sparsh Karna
# Registration No: 23BDS1172
# Experiment No: 02
# Title: Crash Site-Based Vector Analysis
# Date: 24/Jul/2025
# --------------------------------------------------

# Vector Initialization Section
age <- sample(18:70, 30, replace=TRUE)  # Generate 30 ages between 18 and 70
weight <- round(runif(30, 50, 100), 1)  # Generate 30 weights between 50 and 100 kg
survival <- sample(c(0, 1), 30, replace=TRUE)  # Generate binary survival status (0=deceased, 1=survived)
gender <- sample(c("M", "F"), 30, replace=TRUE)  # Generate gender as M or F
class <- sample(c("E", "B", "F"), 30, replace=TRUE)  # Generate class as Economy (E), Business (B), or First (F)
health_score <- sample(0:100, 30, replace=TRUE)  # Generate health scores between 0 and 100
crash_site <- sample(c("Forest", "Sea", "Mountain"), 30, replace=TRUE)  # Generate crash site locations

# User-Defined Functions Section

#' @title: Calculate Total Survival Rate
#' @description: Computes and prints the total number of survivors and survival percentage
#' @param survival: Vector of survival status (0=deceased, 1=survived)
#' @returns None: Prints total survivors and survival rate
#' @examples
#' # Calculate survival statistics
#' calculate_survival_rate(survival)
calculate_survival_rate <- function(survival) {
  total_survivors <- sum(survival)  # Sum the survival vector to count survivors
  percent_survived <- (total_survivors / length(survival)) * 100  # Calculate percentage
  cat("Total Survivors:", total_survivors, "\n")  # Print number of survivors
  cat("Survival Rate:", round(percent_survived, 2), "%\n")  # Print percentage rounded to 2 decimals
}

#' @title: Calculate Average Age of Survivors
#' @description: Computes and prints the average age of passengers who survived
#' @param age: Vector of passenger ages
#' @param survival: Vector of survival status
#' @returns None: Prints average age of survivors
#' @examples
#' # Calculate average age of survivors
#' average_age_survivors(age, survival)
average_age_survivors <- function(age, survival) {
  survivor_ages <- age[survival == 1]  # Filter ages where survival is 1
  avg_age <- mean(survivor_ages)  # Calculate mean age of survivors
  cat("Average Age of Survivors:", round(avg_age, 2), "\n")  # Print rounded average age
}

#' @title: Calculate Average Health Score of Deceased
#' @description: Computes and prints the average health score of deceased passengers
#' @param health_score: Vector of health scores
#' @param survival: Vector of survival status
#' @returns None: Prints average health score of deceased
#' @examples
#' # Calculate average health score of deceased
#' average_health_score_deceased(health_score, survival)
average_health_score_deceased <- function(health_score, survival) {
  deceased_scores <- health_score[survival == 0]  # Filter health scores where survival is 0
  avg_score <- mean(deceased_scores)  # Calculate mean health score
  cat("Average Health Score of Deceased:", round(avg_score, 2), "\n")  # Print rounded average
}

#' @title: Find Heaviest Survivor
#' @description: Identifies and prints the weight of the heaviest surviving passenger
#' @param weight: Vector of passenger weights
#' @param survival: Vector of survival status
#' @returns None: Prints weight of heaviest survivor
#' @examples
#' # Find heaviest survivor
#' heaviest_survivor(weight, survival)
heaviest_survivor <- function(weight, survival) {
  survivor_weights <- weight[survival == 1]  # Filter weights where survival is 1
  max_weight <- max(survivor_weights)  # Find maximum weight
  cat("Heaviest Survivor Weight:", max_weight, "kg\n")  # Print heaviest weight
}

#' @title: Find Youngest First-Class Survivor
#' @description: Identifies and prints the age of the youngest first-class survivor
#' @param age: Vector of passenger ages
#' @param survival: Vector of survival status
#' @param class: Vector of passenger classes
#' @returns None: Prints age of youngest first-class survivor
#' @examples
#' # Find youngest first-class survivor
#' youngest_first_class_survivor(age, survival, class)
youngest_first_class_survivor <- function(age, survival, class) {
  first_class_survivors <- age[survival == 1 & class == "F"]  # Filter ages where survival is 1 and class is F
  min_age <- min(first_class_survivors)  # Find minimum age
  cat("Youngest First Class Survivor Age:", min_age, "\n")  # Print youngest age
}

#' @title: Calculate Gender-Wise Survival Ratio
#' @description: Computes and prints survival percentages for male and female passengers
#' @param survival: Vector of survival status
#' @param gender: Vector of passenger genders
#' @returns None: Prints male and female survival rates
#' @examples
#' # Calculate gender-wise survival ratios
#' gender_wise_survival_ratio(survival, gender)
gender_wise_survival_ratio <- function(survival, gender) {
  male_survivors <- sum(survival == 1 & gender == "M")  # Count male survivors
  female_survivors <- sum(survival == 1 & gender == "F")  # Count female survivors
  total_males <- sum(gender == "M")  # Count total males
  total_females <- sum(gender == "F")  # Count total females
  male_rate <- (male_survivors / total_males) * 100  # Calculate male survival percentage
  female_rate <- (female_survivors / total_females) * 100  # Calculate female survival percentage
  cat("Male Survival Rate:", round(male_rate, 2), "%\n")  # Print male rate
  cat("Female Survival Rate:", round(female_rate, 2), "%\n")  # Print female rate
}

#' @title: Categorize Health Scores
#' @description: Categorizes health scores into Low, Medium, High and prints counts
#' @param health_score: Vector of health scores
#' @returns categories: Vector of health categories
#' @examples
#' # Categorize health scores
#' health_cats <- health_category(health_score)
health_category <- function(health_score) {
  categories <- ifelse(health_score < 35, "Low",  # Assign Low if score < 35
                       ifelse(health_score < 70, "Medium", "High"))  # Assign Medium if < 70, else High
  print(table(categories))  # Print frequency table of categories
  return(categories)  # Return category vector
}

#' @title: Survival by Health Category
#' @description: Counts and prints survivors in each health category
#' @param health_score: Vector of health scores
#' @param survival: Vector of survival status
#' @returns None: Prints survivor counts per health category
#' @examples
#' # Calculate survivors by health category
#' survival_by_health_category(health_score, survival)
survival_by_health_category <- function(health_score, survival) {
  categories <- health_category(health_score)  # Get health categories
  survivor_categories <- categories[survival == 1]  # Filter categories for survivors
  cat("Survivors by Health Category:\n")  # Print header
  print(table(survivor_categories))  # Print frequency table of survivor categories
}

#' @title: Most Common Class Among Survivors
#' @description: Identifies and prints the most common passenger class among survivors
#' @param class: Vector of passenger classes
#' @param survival: Vector of survival status
#' @returns None: Prints most common class
#' @examples
#' # Find most common class among survivors
#' most_common_class_survived(class, survival)
most_common_class_survived <- function(class, survival) {
  survivor_classes <- class[survival == 1]  # Filter classes for survivors
  class_count <- table(survivor_classes)  # Count frequency of each class
  common_class <- names(class_count[class_count == max(class_count)])  # Find class with max count
  cat("Most Common Class Among Survivors:", common_class, "\n")  # Print most common class
}

#' @title: Survivors by Crash Site
#' @description: Counts and prints number of survivors at each crash site
#' @param crash_site: Vector of crash site locations
#' @param survival: Vector of survival status
#' @returns None: Prints survivor counts per crash site
#' @examples
#' # Calculate survivors by crash site
#' survivors_by_crash_site(crash_site, survival)
survivors_by_crash_site <- function(crash_site, survival) {
  survivor_sites <- crash_site[survival == 1]  # Filter crash sites for survivors
  site_survivors <- table(survivor_sites)  # Count survivors per site
  cat("Survivors by Crash Site:\n")  # Print header
  print(site_survivors)  # Print frequency table
}

#' @title: Average Health Score by Crash Site
#' @description: Computes and prints average health score for each crash site
#' @param health_score: Vector of health scores
#' @param crash_site: Vector of crash site locations
#' @returns None: Prints average health score per site
#' @examples
#' # Calculate average health by crash site
#' average_health_by_site(health_score, crash_site)
average_health_by_site <- function(health_score, crash_site) {
  unique_sites <- unique(crash_site)  # Get unique crash sites
  for (site in unique_sites) {  # Loop through each site
    site_scores <- health_score[crash_site == site]  # Filter health scores for current site
    avg <- mean(site_scores)  # Calculate average health score
    cat("Average Health at", site, ":", round(avg, 2), "\n")  # Print rounded average
  }
}

#' @title: Site with Highest Survival
#' @description: Identifies and prints the crash site with the most survivors
#' @param crash_site: Vector of crash site locations
#' @param survival: Vector of survival status
#' @returns None: Prints site with highest survivors
#' @examples
#' # Find site with highest survival
#' site_with_highest_survival(crash_site, survival)
site_with_highest_survival <- function(crash_site, survival) {
  survivor_sites <- crash_site[survival == 1]  # Filter crash sites for survivors
  site_counts <- table(survivor_sites)  # Count survivors per site
  max_site <- names(site_counts[site_counts == max(site_counts)])  # Find site with max survivors
  cat("Site with Highest Survivors:", max_site, "\n")  # Print site
}

#' @title: Print Full Summary Report
#' @description: Prints a comprehensive summary of crash analysis
#' @param age: Vector of passenger ages
#' @param survival: Vector of survival status
#' @param gender: Vector of passenger genders
#' @param class: Vector of passenger classes
#' @param health_score: Vector of health scores
#' @param crash_site: Vector of crash site locations
#' @returns None: Prints detailed summary
#' @examples
#' # Print full summary
#' print_full_summary(age, survival, gender, class, health_score, crash_site)
print_full_summary <- function(age, survival, gender, class, health_score, crash_site) {
  cat("=== CRASH SITE SURVIVAL ANALYSIS SUMMARY ===\n")  # Print header
  cat("Total Passengers:", length(age), "\n")  # Print total passengers
  total_survivors <- sum(survival)  # Count survivors
  cat("Total Survivors:", total_survivors, "\n")  # Print survivors
  cat("Total Deceased:", length(age) - total_survivors, "\n")  # Print deceased
  cat("\nGender-Wise Survival:\n")  # Print gender header
  gender_wise_survival_ratio(survival, gender)  # Call gender survival function
  cat("\nClass-Wise Survival:\n")  # Print class header
  most_common_class_survived(class, survival)  # Call class survival function
  survivor_health <- health_score[survival == 1]  # Filter health scores for survivors
  cat("Average Health Score of Survivors:", round(mean(survivor_health), 2), "\n")  # Print average health
  cat("\nCrash Site Analysis:\n")  # Print crash site header
  site_with_highest_survival(crash_site, survival)  # Call site survival function
  cat("===========================================\n")  # Print footer
}

# Function Calls Section
cat("=== Crash Site-Based Vector Analysis ===\n\n")

# 1. Calculate total survival rate
cat("1. Survival Rate Analysis:\n")
calculate_survival_rate(survival)
cat("\n")

# 2. Calculate average age of survivors
cat("2. Average Age of Survivors:\n")
average_age_survivors(age, survival)
cat("\n")

# 3. Calculate average health score of deceased
cat("3. Average Health Score of Deceased:\n")
average_health_score_deceased(health_score, survival)
cat("\n")

# 4. Find heaviest survivor
cat("4. Heaviest Survivor:\n")
heaviest_survivor(weight, survival)
cat("\n")

# 5. Find youngest first-class survivor
cat("5. Youngest First-Class Survivor:\n")
youngest_first_class_survivor(age, survival, class)
cat("\n")

# 6. Calculate gender-wise survival ratio
cat("6. Gender-Wise Survival Ratio:\n")
gender_wise_survival_ratio(survival, gender)
cat("\n")

# 7. Categorize health scores
cat("7. Health Score Categories:\n")
health_cats <- health_category(health_score)
cat("\n")

# 8. Calculate survivors by health category
cat("8. Survivors by Health Category:\n")
survival_by_health_category(health_score, survival)
cat("\n")

# 9. Find most common class among survivors
cat("9. Most Common Class Among Survivors:\n")
most_common_class_survived(class, survival)
cat("\n")

# 10. Calculate survivors by crash site
cat("10. Survivors by Crash Site:\n")
survivors_by_crash_site(crash_site, survival)
cat("\n")

# 11. Calculate average health by crash site
cat("11. Average Health by Crash Site:\n")
average_health_by_site(health_score, crash_site)
cat("\n")

# 12. Find site with highest survival
cat("12. Site with Highest Survival:\n")
site_with_highest_survival(crash_site, survival)
cat("\n")

# 13. Print full summary
cat("13. Full Summary Report:\n")
print_full_summary(age, survival, gender, class, health_score, crash_site)
cat("\n")

cat("=== Analysis Complete ===\n")