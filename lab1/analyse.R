# Billing System and Customer Behavior Analysis using Functions in R

customer_names <- c("Sparsh Karna", "Arkita Barua", "Diraq Girach", "Aryan Risi", "Lavanaya Malhotra", "Stuti Handa", "Divyansh Aggarwal", "Mishti Mattu", "Akshat Majila", "Rose Priya"); # This defines the 10 customer names into the vector
customer_type <- c("Regular", "New", "New", "New", "Regular", "Regular", "Regular", "New", "New", "New"); # This defines the type of customer mapped to each customer name
item_name <- c("Rice", "Wheat", "Sugar", "Oil", "Rice", "Tea", "Coffee", "Sugar", "Oil", "Wheat", "Rice", "Tea"); # This defines some of the items present in the store
price_per_unit <- c(50, 40, 45, 120, 50, 200, 150, 45, 120, 40, 50, 200); # This defines the price per unit of each given item
quantity_purchased <- c(5, 10, 2, 3, 8, 1, 2, 4, 1, 5, 6, 3); # This defines the the quantity of each item purchased by the customer

#' @title: Function to calculate the total bill of the user
#' @description: function calculate_total_bill() takes two vectors (price and quantity) and returns a vector of total bill values for each customer.
#' @param price: The vector defining the price per unit of each item
#' @param quantity: The vector defining the quantity purchased by the customer of each item
#' @returns total_bill: Contains the total bill of each customer
#' @example
#' # Calculate the total bill of each customer
#' total_bill <- calculate_total_bill(price, quantity)
#' print(total_bill)

calculate_total_bill <- function(price, quantity){
  total_bill <- price * quantity; # Calculate the total price by multiply quantity of each by its respective price
  return(total_bill); # Return the total bill amount
}

#' @title: Function to calculate the total revenue
#' @description: functions to compute and print the total revenue of the shop
#' @param total_bill: The vector defining the bill of each customer
#' @returns revenue: Contains the total revenue of the shop
#' @example
#' # Calculate the total bill of each customer
#' total_bill <- calculate_total_bill(price, quantity)
#' # Calculate the total revenue of the shop
#' revenue <- calculate_total_revenue(total_bill)

calculate_total_revenue <- function(total_bill){
  revenue <- sum(total_bill); # Calculate the sum of bill of each customer
  return(revenue); # Return the revenue
}

#' @title: Function to calculate Average bill amount
#' @description: functions to compute and print the Average bill amount of the shop
#' @param total_bill: The vector defining the bill of each customer
#' @returns average_bill: Contains the average bill amount
#' @example
#' # Calculate the total bill of each customer
#' total_bill <- calculate_total_bill(price, quantity)
#' # Calculate the average bill amount
#' average_bill <- calculate_average_bill(total_bill)

calculate_average_bill <- function(total_bill){
  total_revenue <- calculate_total_revenue(total_bill); # Calculates the total revenue of shop
  total_customer <- length(total_bill) # Calculates the total number of customers
  average_bill <- total_revenue / total_customer; # Calculates the average bill amount of the shop
  return(average_bill); # Returns the average bill amount
}

#' @title: Function to calculate Maximum and minimum bill amount
#' @description: functions to compute and print the Maximum and minimum bill amount of the shop
#' @param total_bill: The vector defining the bill of each customer
#' @returns max_bill: Contains the maximum bill amount
#' @returns min_bill: Contains the minimum bill amount 
#' @example
#' # Calculate the total bill of each customer
#' total_bill <- calculate_total_bill(price, quantity)
#' # Calculate the minimum bill amount
#' min_bill <- calculate_min_max_bill_amount(total_bill)[2]
#' # Calculate the maximum bill amount
#' max_bill <- calculate_min_max_bill_amount(total_bill)[1]

calculate_min_max_bill_amount <- function(total_bill){
  max_bill <- max(total_bill) # Calculates the maximum bill amount
  min_bill <- min(total_bill) # Calculates the minimum bill amount
  return(c(max_bill, min_bill)); # Return the maximum and minimum bill maount
}

#' @title: Function to calculate Total number of "Regular" vs "New" customers
#' @description: functions to compute and print the Total number of "Regular" vs "New" customers of the shop
#' @param customer_type: The vector defining the type of each customer
#' @returns reg: Contains the total number of regular customer
#' @returns new: Contains the total number of new customer
#' @example
#' # Calculate the total number of regular customer
#' reg <- calculate_reg_new_customers(customer_type)[1]
#' # Calculate the total number of new customer
#' new <-calculate_reg_new_customers(customer_type)[2]

calculate_reg_new_customers <- function(customer_type){
  count_reg = 0; # Keeps the count of the regular customers
  count_new = 0; # Keeps the count of the new customers
  # Iterate over the customer_type vector
  for(customer in customer_type){
    if(customer == "Regular"){ # Check if the customer is regular
      count_reg = count_reg + 1; # If regular then increase the count of the regular customer
    }
    else{ # If the customer is not regular then must be new
      count_new = count_new + 1; # If new then increase the count of the new customer 
    }
  }
  return(c(count_reg, count_new)) # Return the count of new customer and regular customer
}

#' @title: Function to figure out highest spender
#' @description: functions hat takes the total bill vector and customer names and returns names of customers who spent more than Rs. 500.
#' @param customer_name: Names of all the customers
#' @param total_bill: Total bill of each customer
#' @returns high_spender_names: The vector containing the customer name who spend more than 500
#' @example
#' # Names of customer who spend more than 500
#' names <- get_high_spenders(customer_name, total_bill)

get_high_spenders <- function(customer_name, total_bill){
  high_spender_names <- c();
  # Iterate over the customers vector using indexes
  for(i in 1:length(customer_name)){
    if(total_bill[i] > 500){ # Check if the total bill of that customer is more than 500
      high_spender_names <- c(high_spender_names, customer_name[i]); # If it is then append the name of that customer to the vector
    }
  }
  return(high_spender_names); # Return the names of the high spender vector
}

#' @title: Function to calculate item-wise total quantity sold
#' @description: Calculate total quantity sold for each unique item
#' @param item_name: Names of all the items
#' @param quantity_purchased: Quantity of each item purchased
#' @returns total_quantities: named numeric vector with total quantity for each item
#' @example
#' # Get unique counts of each item
#' unique_quantity <- item_wise_quantity(item_name, quantity_purchased)

item_wise_quantity <- function(items_vector, quantities_vector) {
  # Get unique items
  unique_items <- unique(items_vector)
  
  # Initialize vector to store total quantities
  total_quantities <- numeric(length(unique_items))
  names(total_quantities) <- unique_items
  
  # Calculate total quantity for each item
  for(item in unique_items) {
    # Find all indices where this item appears
    item_indices <- which(items_vector == item)
    # Sum quantities for this item
    total_quantities[item] <- sum(quantities_vector[item_indices])
  }
  
  return(total_quantities)
}

# Main execution - Demonstrating all functions with the given data
cat("=== Billing System and Customer Behavior Analysis ===\n\n")

# Calculate total bill for each customer
total_bill <- calculate_total_bill(price_per_unit, quantity_purchased)
cat("1. Total bill for each customer:\n")
for(i in 1:length(customer_names)){
  cat("   ", customer_names[i], ": Rs.", total_bill[i], "\n")
}
cat("\n")

# Calculate total revenue
total_revenue <- calculate_total_revenue(total_bill)
cat("2. Total revenue of the shop: Rs.", total_revenue, "\n\n")

# Calculate average bill amount
average_bill <- calculate_average_bill(total_bill)
cat("3. Average bill amount: Rs.", round(average_bill, 2), "\n\n")

# Calculate maximum and minimum bill amounts
min_max_bill <- calculate_min_max_bill_amount(total_bill)
max_bill <- min_max_bill[1]
min_bill <- min_max_bill[2]
cat("4. Maximum bill amount: Rs.", max_bill, "\n")
cat("   Minimum bill amount: Rs.", min_bill, "\n\n")

# Calculate regular vs new customers
reg_new_count <- calculate_reg_new_customers(customer_type)
regular_count <- reg_new_count[1]
new_count <- reg_new_count[2]
cat("5. Customer analysis:\n")
cat("   Regular customers:", regular_count, "\n")
cat("   New customers:", new_count, "\n\n")

# Get high spenders (customers who spent more than Rs. 500)
high_spenders <- get_high_spenders(customer_names, total_bill)
cat("6. High spenders (customers who spent more than Rs. 500):\n")
if(length(high_spenders) > 0) {
  for(spender in high_spenders) {
    cat("   -", spender, "\n")
  }
} else {
  cat("   No customers spent more than Rs. 500\n")
}
cat("\n")

# Calculate item-wise total quantity sold
item_quantities <- item_wise_quantity(item_name, quantity_purchased)
cat("7. Item-wise total quantity sold:\n")
for(i in 1:length(item_quantities)) {
  cat("   ", names(item_quantities)[i], ":", item_quantities[i], "units\n")
}
cat("\n")

cat("=== Analysis Complete ===\n")















