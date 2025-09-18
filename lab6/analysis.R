# ---------------------------------------------------------
# Lab Experiment 6 - Retail Sales Data Analysis
# Author : Sparsh Karna
# Date   : 04-09-2025
# ---------------------------------------------------------

# Load required libraries
# (install them first if not already installed: install.packages("dplyr"), etc.)
library(dplyr)
library(ggplot2)
library(lubridate)
library(mice)       # For predictive imputation
library(VIM)        # For visualization of missing values

# ----------------------------
# 1. Data Generation
# ----------------------------

set.seed(123) # for reproducibility

n <- 1000   # number of records

# Generate synthetic attributes
CustomerID <- 1:n
Age <- sample(18:65, n, replace = TRUE)
Gender <- sample(c("Male", "Female", "Other"), n, replace = TRUE, prob = c(0.45, 0.45, 0.1))
ProductCategory <- sample(c("Electronics", "Clothing", "Grocery", "Furniture"), n, replace = TRUE)

# Price ranges based on category
Price <- ifelse(ProductCategory == "Electronics", sample(500:2000, n, replace = TRUE),
                ifelse(ProductCategory == "Clothing", sample(200:1000, n, replace = TRUE),
                       ifelse(ProductCategory == "Grocery", sample(50:500, n, replace = TRUE),
                              sample(1000:5000, n, replace = TRUE))))

Quantity <- sample(1:10, n, replace = TRUE)

# Random purchase dates within last 2 years
PurchaseDate <- sample(seq(as.Date("2023-01-01"), as.Date("2025-09-01"), by = "day"), n, replace = TRUE)

PaymentMode <- sample(c("Cash", "Credit Card", "UPI", "NetBanking"), n, replace = TRUE)

# Create data frame
retail_sales <- data.frame(CustomerID, Age, Gender, ProductCategory,
                           Quantity, Price, PurchaseDate, PaymentMode)

# Introduce 5% missing values randomly
introduce_missing <- function(x) {
  n_missing <- round(0.05 * length(x))
  idx <- sample(1:length(x), n_missing)
  x[idx] <- NA
  return(x)
}

retail_sales_missing <- as.data.frame(lapply(retail_sales, introduce_missing))

# Save dataset to CSV
write.csv(retail_sales_missing, "retail_sales.csv", row.names = FALSE)

# ----------------------------
# 2. Reading & Exploring Data
# ----------------------------

# Read the dataset
data <- read.csv("retail_sales.csv")

# Display first few rows
head(data)

# Summary of dataset
summary(data)

# Count missing values in each column
colSums(is.na(data))

# ----------------------------
# 3. Handling Missing Data
# ----------------------------

# Method 1: Removal of missing values (Complete Case Analysis)
data_removed <- na.omit(data)

# Method 2: Mean/Median/Mode Imputation
data_imputed <- data
data_imputed$Age[is.na(data_imputed$Age)] <- mean(data$Age, na.rm = TRUE)
data_imputed$Quantity[is.na(data_imputed$Quantity)] <- median(data$Quantity, na.rm = TRUE)
mode_gender <- names(sort(table(data$Gender), decreasing = TRUE))[1]
data_imputed$Gender[is.na(data_imputed$Gender)] <- mode_gender

# Method 3: Predictive Imputation (using mice)
mice_data <- mice(data, m = 1, maxit = 5, method = 'pmm', seed = 123)
data_mice <- complete(mice_data)

# ----------------------------
# 4. Data Analysis
# ----------------------------

# a) Total sales per product category
data_mice %>%
  group_by(ProductCategory) %>%
  summarise(TotalSales = sum(Price * Quantity, na.rm = TRUE))

# b) Average spending per customer
data_mice %>%
  group_by(CustomerID) %>%
  summarise(AvgSpending = mean(Price * Quantity, na.rm = TRUE)) %>%
  head(10) # show first 10 customers

# c) Trend of sales over time (monthly)
data_mice %>%
  mutate(Month = floor_date(as.Date(PurchaseDate), "month")) %>%
  group_by(Month) %>%
  summarise(MonthlySales = sum(Price * Quantity, na.rm = TRUE))

# d) Payment mode preference
table(data_mice$PaymentMode)

# ----------------------------
# 5. Data Visualization
# ----------------------------

# a) Bar chart of product category sales
ggplot(data_mice, aes(x = ProductCategory, y = Price * Quantity, fill = ProductCategory)) +
  geom_bar(stat = "summary", fun = "sum") +
  labs(title = "Total Sales per Product Category", y = "Total Sales", x = "Product Category")

# b) Line chart of sales trend over time
sales_trend <- data_mice %>%
  mutate(Month = floor_date(as.Date(PurchaseDate), "month")) %>%
  group_by(Month) %>%
  summarise(MonthlySales = sum(Price * Quantity, na.rm = TRUE))

ggplot(sales_trend, aes(x = Month, y = MonthlySales)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(title = "Monthly Sales Trend", x = "Month", y = "Sales")

# c) Pie chart for payment mode distribution
payment_data <- data_mice %>%
  group_by(PaymentMode) %>%
  summarise(Count = n())

ggplot(payment_data, aes(x = "", y = Count, fill = PaymentMode)) +
  geom_col(width = 1) +
  coord_polar("y") +
  labs(title = "Payment Mode Distribution")

# ---------------------------------------------------------
# END OF SCRIPT
# ---------------------------------------------------------
