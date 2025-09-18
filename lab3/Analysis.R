# Vector Initialization Section

# Random capacities for 5 bike parking slots (150–500)
bike_capacity <- sample(150:500, 5, replace=TRUE)
bike_used <- rep(0, 5)  # Initialize usage to 0

# Random capacities for 4 car parking slots (150–500)
car_capacity <- sample(150:500, 4, replace=TRUE)
car_used <- rep(0, 4)  # Initialize usage to 0

# VIP Parking has a fixed capacity of 10
vip_capacity <- 10
vip_used <- 0

# Simulate number of arriving vehicles
bikes_arriving <- sample(500:800, 1)
cars_arriving <- sample(400:600, 1)
vips_arriving <- sample(5:15, 1)

#  User-Defined Functions Section

#' @title: Allocate Vehicles
#' @description: Allocates incoming vehicles to available parking slots
#' @param arriving: number of incoming vehicles
#' @param capacity: vector of slot capacities
#' @param used: vector of used slots (modifiable)
#' @returns updated 'used' vector
allocate_parking <- function(arriving, capacity, used) {
  for (i in seq_along(capacity)) {
    available <- capacity[i] - used[i]
    if (arriving > 0 && available > 0) {
      to_allocate <- min(arriving, available)
      used[i] <- used[i] + to_allocate
      arriving <- arriving - to_allocate
    }
  }
  if (arriving > 0) {
    cat("No more available space in this category.\n")
  }
  return(used)
}

#' @title: Allocate VIP Vehicles
#' @description: Allocates VIP vehicles to the VIP parking only
#' @param arriving: number of VIPs
#' @param capacity: fixed capacity (10)
#' @param used: current VIP usage (modifiable)
#' @returns updated 'used' value
allocate_vip <- function(arriving, capacity, used) {
  available <- capacity - used
  to_allocate <- min(arriving, available)
  used <- used + to_allocate
  cat("Only", to_allocate, "VIPs accommodated out of", arriving, "\n")
  if (to_allocate < arriving) {
    cat("No more available space in this category.\n")
  }
  return(used)
}

#' @title: Display Parking Status
#' @description: Prints used vs total capacity of each parking slot
#' @param label: string label (BikeParking/CarParking)
#' @param capacity: vector of capacities
#' @param used: vector of used slots
display_parking_status <- function(label, capacity, used) {
  cat(label, "Status:\n")
  full_slots <- c()
  for (i in seq_along(capacity)) {
    status <- ifelse(used[i] == capacity[i], "[FULL]", "")
    cat("Slot", i, "-> Used:", used[i], "/", capacity[i], status, "\n")
    if (used[i] == capacity[i]) full_slots <- c(full_slots, i)
  }
  if (length(full_slots) > 0) {
    cat("\n", label, "FULL in slot(s):", paste(full_slots, collapse=", "), "\n\n")
  }
}

#' @title: Display Utilization Report
#' @description: Prints total, used, remaining and percentage utilization
#' @param total_capacity: numeric vector of all capacities
#' @param total_used: numeric vector of all usages
display_utilization_report <- function(total_capacity, total_used) {
  total_cap <- sum(total_capacity)
  used_cap <- sum(total_used)
  remaining_cap <- total_cap - used_cap
  utilization <- round((used_cap / total_cap) * 100, 2)
  cat("=========== Parking Utilization Report ===========\n")
  cat("Total Capacity:", total_cap, "\n")
  cat("Used Capacity:", used_cap, "\n")
  cat("Remaining Capacity:", remaining_cap, "\n")
  cat("Utilization:", utilization, "%\n")
}

# Allocation Execution Section

cat("=== Parking Allocation Simulation ===\n\n")

# Allocate VIPs
vip_used <- allocate_vip(vips_arriving, vip_capacity, vip_used)
cat("\n")

# Allocate Bikes
bike_used <- allocate_parking(bikes_arriving, bike_capacity, bike_used)
cat("\n")

# Allocate Cars
car_used <- allocate_parking(cars_arriving, car_capacity, car_used)
cat("\n")

# Final Parking Status Summary

cat("=========== Final Parking Status ===========\n\n")
display_parking_status("BikeParking", bike_capacity, bike_used)
display_parking_status("CarParking", car_capacity, car_used)
cat("VIP Parking Used:", vip_used, "/", vip_capacity)
if (vip_used == vip_capacity) cat(" [FULL]\n") else cat("\n")
cat("\n")

# Utilization Report
all_capacities <- c(bike_capacity, car_capacity, vip_capacity)
all_usages <- c(bike_used, car_used, vip_used)
display_utilization_report(all_capacities, all_usages)

cat("\n=== Analysis Complete ===\n")
