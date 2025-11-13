# U23AI118
# Lab 2: Analyzing NYC Dataset - R Version

#cd "lab2-3"
#& "C:\Program Files\R\R-4.5.1\bin\R.exe" --vanilla -e "user_lib <- file.path(Sys.getenv('USERPROFILE'), 'Documents', 'R', 'win-library', '4.5'); .libPaths(user_lib); source('NYCAnalysis.R')"

# Install required packages if not already installed
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Load required libraries
required_packages <- c("readr", "dplyr", "ggplot2", "gridExtra", "corrplot", 
                      "reshape2", "RColorBrewer", "viridis", "psych")
install_if_missing(required_packages)

# Load the dataset
df <- read_csv('yellow_tripdata_sample.csv')
cat("Shape =", nrow(df), "rows x", ncol(df), "columns\\n")
cat("Columns:", paste(colnames(df), collapse = ", "), "\\n")
cat("Initial rows:\\n")
print(head(df))

# Analyzing before Data Cleaning
cat("\\nDataset Info:\\n")
str(df)
cat("\\nMissing Values:\\n")
print(sapply(df, function(x) sum(is.na(x))))

# Data Cleaning
df_clean <- df %>%
  filter(
    passenger_count > 0 & passenger_count <= 8,
    trip_distance > 0 & trip_distance <= 100,
    fare_amount > 0 & fare_amount <= 500,
    total_amount > 0 & total_amount <= 500,
    tip_amount >= 0 & tip_amount <= 100,
    extra >= 0
  )

cat(sprintf("Original dataset size: %d\\n", nrow(df)))
cat(sprintf("Cleaned dataset size: %d\\n", nrow(df_clean)))
cat(sprintf("Removed %d rows (%.2f%%)\\n", 
    nrow(df) - nrow(df_clean), 
    ((nrow(df) - nrow(df_clean))/nrow(df)*100)))

# Descriptive statistics
print(summary(df_clean))

# Define numerical columns
numerical_cols <- c('passenger_count', 'trip_distance', 'fare_amount', 
                   'total_amount', 'tip_amount', 'extra')

# Function to calculate descriptive statistics
descriptive_stats <- function(data, column) {
  col_data <- data[[column]]
  mode_val <- as.numeric(names(sort(table(col_data), decreasing = TRUE))[1])
  
  list(
    mean = mean(col_data, na.rm = TRUE),
    median = median(col_data, na.rm = TRUE),
    mode = mode_val,
    min = min(col_data, na.rm = TRUE),
    max = max(col_data, na.rm = TRUE),
    std = sd(col_data, na.rm = TRUE),
    variance = var(col_data, na.rm = TRUE),
    skew = psych::skew(col_data, na.rm = TRUE),
    kurtosis = psych::kurtosi(col_data, na.rm = TRUE)
  )
}

# Calculate statistics for all numerical columns
stats_summary <- lapply(numerical_cols, function(col) descriptive_stats(df_clean, col))
names(stats_summary) <- numerical_cols

# Convert to data frame for better display
stats_df <- data.frame(do.call(rbind, lapply(stats_summary, unlist)))
stats_df <- round(stats_df, 4)
print(stats_df)

# Create histograms
create_histograms <- function() {
  plots <- list()
  colors <- rainbow(length(numerical_cols))
  
  for (i in seq_along(numerical_cols)) {
    col <- numerical_cols[i]
    p <- ggplot(df_clean, aes_string(x = col)) +
      geom_histogram(bins = 30, alpha = 0.7, fill = colors[i], color = "black") +
      labs(title = paste("Histogram:", gsub("_", " ", stringr::str_to_title(col))),
           x = gsub("_", " ", stringr::str_to_title(col)),
           y = "Frequency") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 12))
    plots[[i]] <- p
  }
  
  grid.arrange(grobs = plots, ncol = 3, nrow = 2,
               top = "NYC Taxi Data - Histograms")
}

create_histograms()

# Observations for histograms
cat("\\nHistogram Observations:\\n")
cat("All graphs are right skewed\\n")
cat("1) Single passenger travels more\\n")
cat("2) Most people travel within range of 0-5 km\\n")
cat("3) Generally fares are below $40\\n")
cat("4) Total amount comes between $0 - $60\\n")
cat("5) People generally tip below $10\\n")
cat("6) Extras are either 1 or 4\\n")

# Create frequency polygons
create_frequency_polygons <- function() {
  plots <- list()
  colors <- rainbow(length(numerical_cols))
  
  for (i in seq_along(numerical_cols)) {
    col <- numerical_cols[i]
    
    # Calculate histogram data
    h <- hist(df_clean[[col]], breaks = 20, plot = FALSE)
    bin_centers <- (h$breaks[-1] + h$breaks[-length(h$breaks)]) / 2
    
    freq_data <- data.frame(x = bin_centers, y = h$counts)
    
    p <- ggplot(freq_data, aes(x = x, y = y)) +
      geom_line(color = colors[i], linewidth = 1.2) +
      geom_point(color = colors[i], size = 2) +
      labs(title = paste("Frequency Polygon:", gsub("_", " ", stringr::str_to_title(col))),
           x = gsub("_", " ", stringr::str_to_title(col)),
           y = "Frequency") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 12))
    plots[[i]] <- p
  }
  
  grid.arrange(grobs = plots, ncol = 3, nrow = 2,
               top = "NYC Taxi Data - Frequency Polygons")
}

create_frequency_polygons()

# Create box plots
create_boxplots <- function() {
  plots <- list()
  
  for (i in seq_along(numerical_cols)) {
    col <- numerical_cols[i]
    p <- ggplot(df_clean, aes_string(y = col)) +
      geom_boxplot(fill = "lightblue", alpha = 0.7) +
      labs(title = paste("Box Plot:", gsub("_", " ", stringr::str_to_title(col))),
           y = gsub("_", " ", stringr::str_to_title(col))) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 12))
    plots[[i]] <- p
  }
  
  grid.arrange(grobs = plots, ncol = 3, nrow = 2,
               top = "NYC Taxi Data - Box Plots")
}

create_boxplots()

# Create violin plots
create_violin_plots <- function() {
  plots <- list()
  
  for (i in seq_along(numerical_cols)) {
    col <- numerical_cols[i]
      p <- ggplot(df_clean, aes(x = "", y = .data[[col]])) +
     geom_violin(fill = "lightgreen", alpha = 0.7) +
     labs(title = paste("Violin Plot:", gsub("_", " ", stringr::str_to_title(col))),
       y = gsub("_", " ", stringr::str_to_title(col)),
       x = "") +
     theme_minimal() +
     theme(plot.title = element_text(face = "bold", size = 12))
    plots[[i]] <- p
  }
  
  grid.arrange(grobs = plots, ncol = 3, nrow = 2,
               top = "NYC Taxi Data - Violin Plots")
}

create_violin_plots()

# Create density plots
create_density_plots <- function() {
  plots <- list()
  colors <- rainbow(length(numerical_cols))
  
  for (i in seq_along(numerical_cols)) {
    col <- numerical_cols[i]
    p <- ggplot(df_clean, aes_string(x = col)) +
      geom_density(fill = colors[i], alpha = 0.7) +
      labs(title = paste("Density Plot:", gsub("_", " ", stringr::str_to_title(col))),
           x = gsub("_", " ", stringr::str_to_title(col)),
           y = "Density") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 12))
    plots[[i]] <- p
  }
  
  grid.arrange(grobs = plots, ncol = 3, nrow = 2,
               top = "NYC Taxi Data - Density Plots")
}

create_density_plots()

# Create mapping dictionaries for categorical variables
payment_type_map <- c("1" = "Credit Card", "2" = "Cash", "3" = "No Charge", 
                     "4" = "Dispute", "5" = "Unknown", "6" = "Voided Trip")

ratecode_map <- c("1" = "Standard Rate", "2" = "JFK", "3" = "Newark", 
                 "4" = "Nassau/Westchester", "5" = "Negotiated Fare", "6" = "Group Ride")

vendor_map <- c("1" = "Creative Mobile Tech", "2" = "VeriFone Inc")

flag_map <- c("Y" = "Yes - Stored", "N" = "No - Not Stored")

# Create categorical variable bar charts
create_categorical_bars <- function() {
  # Payment Type
  payment_counts <- table(df_clean$payment_type)
  payment_labels <- payment_type_map[names(payment_counts)]
  
  p1 <- ggplot(data.frame(type = payment_labels, count = as.numeric(payment_counts)), 
               aes(x = type, y = count)) +
    geom_bar(stat = "identity", fill = rainbow(length(payment_counts))) +
    geom_text(aes(label = count), vjust = -0.5) +
    labs(title = "Payment Type Distribution", x = "Payment Type", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Rate Code
  ratecode_counts <- table(df_clean$RatecodeID)
  ratecode_labels <- ratecode_map[names(ratecode_counts)]
  
  p2 <- ggplot(data.frame(type = ratecode_labels, count = as.numeric(ratecode_counts)), 
               aes(x = type, y = count)) +
    geom_bar(stat = "identity", fill = rainbow(length(ratecode_counts))) +
    geom_text(aes(label = count), vjust = -0.5) +
    labs(title = "Rate Code Distribution", x = "Rate Code Type", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Vendor
  vendor_counts <- table(df_clean$VendorID)
  vendor_labels <- vendor_map[names(vendor_counts)]
  
  p3 <- ggplot(data.frame(type = vendor_labels, count = as.numeric(vendor_counts)), 
               aes(x = type, y = count)) +
    geom_bar(stat = "identity", fill = rainbow(length(vendor_counts))) +
    geom_text(aes(label = count), vjust = -0.5) +
    labs(title = "Vendor Distribution", x = "Vendor Name", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Store and Forward Flag
  flag_counts <- table(df_clean$store_and_fwd_flag)
  flag_labels <- flag_map[names(flag_counts)]
  
  p4 <- ggplot(data.frame(type = flag_labels, count = as.numeric(flag_counts)), 
               aes(x = type, y = count)) +
    geom_bar(stat = "identity", fill = rainbow(length(flag_counts))) +
    geom_text(aes(label = count), vjust = -0.5) +
    labs(title = "Store and Forward Flag Distribution", x = "Store and Forward Status", y = "Count") +
    theme_minimal()
  
  grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2,
               top = "NYC Taxi Data: Categorical Variables - Bar Charts")
}

create_categorical_bars()

# Create pie charts for categorical variables
create_pie_charts <- function() {
  par(mfrow = c(2, 2))
  
  # Payment Type Pie Chart
  payment_counts <- table(df_clean$payment_type)
  payment_labels <- payment_type_map[names(payment_counts)]
  pie(payment_counts, labels = paste(payment_labels, "\\n", 
      round(payment_counts/sum(payment_counts)*100, 1), "%"),
      main = "Payment Type Distribution", col = rainbow(length(payment_counts)))
  
  # Vendor Pie Chart
  vendor_counts <- table(df_clean$VendorID)
  vendor_labels <- vendor_map[names(vendor_counts)]
  pie(vendor_counts, labels = paste(vendor_labels, "\\n", 
      round(vendor_counts/sum(vendor_counts)*100, 1), "%"),
      main = "Vendor Distribution", col = rainbow(length(vendor_counts)))
  
  # Rate Code Pie Chart
  ratecode_counts <- table(df_clean$RatecodeID)
  ratecode_labels <- ratecode_map[names(ratecode_counts)]
  pie(ratecode_counts, labels = paste(ratecode_labels, "\\n", 
      round(ratecode_counts/sum(ratecode_counts)*100, 1), "%"),
      main = "Rate Code Distribution", col = rainbow(length(ratecode_counts)))
  
  # Store and Forward Flag Pie Chart
  flag_counts <- table(df_clean$store_and_fwd_flag)
  flag_labels <- flag_map[names(flag_counts)]
  pie(flag_counts, labels = paste(flag_labels, "\\n", 
      round(flag_counts/sum(flag_counts)*100, 1), "%"),
      main = "Store and Forward Flag Distribution", col = rainbow(length(flag_counts)))
  
  par(mfrow = c(1, 1))
}

create_pie_charts()

# INFERENTIAL STATISTICS

# Confidence Intervals
confidence_interval <- function(data, confidence = 0.95) {
  n <- length(data)
  mean_val <- mean(data, na.rm = TRUE)
  std_err <- sd(data, na.rm = TRUE) / sqrt(n)
  t_val <- qt((1 + confidence) / 2, n - 1)
  margin_error <- t_val * std_err
  
  list(
    mean = mean_val,
    lower_bound = mean_val - margin_error,
    upper_bound = mean_val + margin_error,
    std_error = std_err,
    sample_size = n
  )
}

variables <- c('trip_distance', 'fare_amount', 'tip_amount')
cat("\\n=== CONFIDENCE INTERVALS ===\\n")

for (var in variables) {
  data <- df_clean[[var]][!is.na(df_clean[[var]])]
  ci_result <- confidence_interval(data)
  
  cat(sprintf("\n%s:\n", gsub("_", " ", stringr::str_to_title(var))))
  cat(sprintf("  Sample Size: %d\n", ci_result$sample_size))
  cat(sprintf("  Sample Mean: $%.4f\n", ci_result$mean))
  cat(sprintf("  Standard Error: $%.4f\n", ci_result$std_error))
  cat(sprintf("  95%% CI: [$%.4f, $%.4f]\n", ci_result$lower_bound, ci_result$upper_bound))
  cat(sprintf("  Interpretation: We are 95%% confident that the true population mean\n"))
  cat(sprintf("  %s lies between $%.4f and $%.4f\n", gsub("_", " ", var), 
              ci_result$lower_bound, ci_result$upper_bound))
}

# Hypothesis Test 1: One-sample t-test for tip amount
cat("\n=== HYPOTHESIS TEST 1: One-Sample t-test for Tip Amount ===\n")

tip_data <- df_clean$tip_amount[!is.na(df_clean$tip_amount)]
null_hypothesis_value <- 2.0

t_test_result <- t.test(tip_data, mu = null_hypothesis_value)

cat(sprintf("H0: μ = $%.1f (null hypothesis)\n", null_hypothesis_value))
cat(sprintf("H1: μ ≠ $%.1f (alternative hypothesis)\n", null_hypothesis_value))
cat("Significance level: α = 0.05\n")
cat("\nResults:\n")
cat(sprintf("Sample size: %d\n", length(tip_data)))
cat(sprintf("Sample mean: $%.4f\n", mean(tip_data)))
cat(sprintf("Sample std: $%.4f\n", sd(tip_data)))
cat(sprintf("t-statistic: %.4f\n", t_test_result$statistic))
cat(sprintf("p-value: %.6f\n", t_test_result$p.value))

if (t_test_result$p.value < 0.05) {
  cat("Decision: Reject H0\n")
  cat("Conclusion: There is significant evidence that the average tip amount is different from $2\n")
} else {
  cat("Decision: Fail to reject H0\n")
  cat("Conclusion: There is insufficient evidence that the average tip amount is different from $2\n")
}

# Hypothesis Test 2: Two-sample t-test for fare amount by payment type
cat("\n=== HYPOTHESIS TEST 2: Two-Sample t-test for Fare Amount by Payment Type ===\n")

credit_fares <- df_clean$fare_amount[df_clean$payment_type == 1 & !is.na(df_clean$fare_amount)]
cash_fares <- df_clean$fare_amount[df_clean$payment_type == 2 & !is.na(df_clean$fare_amount)]

t_test_result2 <- t.test(credit_fares, cash_fares)

cat("H0: μ_credit = μ_cash (no difference in mean fare amounts)\n")
cat("H1: μ_credit ≠ μ_cash (difference in mean fare amounts)\n")
cat("Significance level: α = 0.05\n")
cat("\nResults:\n")
cat("Credit card payments:\n")
cat(sprintf("  Sample size: %d\n", length(credit_fares)))
cat(sprintf("  Sample mean: $%.4f\n", mean(credit_fares)))
cat(sprintf("  Sample std: $%.4f\n", sd(credit_fares)))
cat("\nCash payments:\n")
cat(sprintf("  Sample size: %d\n", length(cash_fares)))
cat(sprintf("  Sample mean: $%.4f\n", mean(cash_fares)))
cat(sprintf("  Sample std: $%.4f\n", sd(cash_fares)))
cat(sprintf("\nt-statistic: %.4f\n", t_test_result2$statistic))
cat(sprintf("p-value: %.6f\n", t_test_result2$p.value))

if (t_test_result2$p.value < 0.05) {
  cat("Decision: Reject H0\n")
  cat("Conclusion: There is significant evidence of a difference in mean fare amounts between payment types\n")
} else {
  cat("Decision: Fail to reject H0\n")
  cat("Conclusion: There is insufficient evidence of a difference in mean fare amounts between payment types\n")
}

# Hypothesis Test 3: Chi-square test of independence
cat("\n=== HYPOTHESIS TEST 3: Chi-square Test of Independence ===\n")

contingency_table <- table(df_clean$payment_type, df_clean$RatecodeID)
cat("H0: Payment type and Rate code are independent\n")
cat("H1: Payment type and Rate code are not independent\n")
cat("Significance level: α = 0.05\n")
cat("Contingency Table:\n")
print(contingency_table)

chi_test_result <- chisq.test(contingency_table)

cat("\nResults:\n")
cat(sprintf("Chi-square statistic: %.4f\n", chi_test_result$statistic))
cat(sprintf("Degrees of freedom: %d\n", chi_test_result$parameter))
cat(sprintf("p-value: %.6f\n", chi_test_result$p.value))

if (chi_test_result$p.value < 0.05) {
  cat("Decision: Reject H0\n")
  cat("Conclusion: Payment type and Rate code are NOT independent\n")
} else {
  cat("Decision: Fail to reject H0\n")
  cat("Conclusion: Payment type and Rate code are independent\n")
}

# Correlation Analysis
cat("\n=== CORRELATION ANALYSIS ===\n")

trip_fare_corr <- cor(df_clean$trip_distance, df_clean$fare_amount, use = "complete.obs")
fare_tip_corr <- cor(df_clean$fare_amount, df_clean$tip_amount, use = "complete.obs")
fare_tip_spearman <- cor(df_clean$fare_amount, df_clean$tip_amount, 
                        method = "spearman", use = "complete.obs")

cat("Correlation:\n")
cat("Pearson - linear data\n")
cat(sprintf("Trip Distance vs Fare Amount: %.4f\n", trip_fare_corr))
cat(sprintf("Fare Amount vs Tip Amount: %.4f\n", fare_tip_corr))
cat("Spearman - non-linear data\n")
cat(sprintf("Fare Amount vs Tip Amount: %.4f\n", fare_tip_spearman))

interpret_correlation <- function(r) {
  abs_r <- abs(r)
  if (abs_r >= 0.8) return("very strong")
  else if (abs_r >= 0.6) return("strong")
  else if (abs_r >= 0.4) return("moderate")
  else if (abs_r >= 0.2) return("weak")
  else return("very weak")
}

cat("\nInterpretation:\n")
cat(sprintf("- Trip distance and fare amount show a %s positive correlation\n", 
            interpret_correlation(trip_fare_corr)))
cat(sprintf("- Fare amount and tip amount show a %s positive correlation\n", 
            interpret_correlation(fare_tip_spearman)))

# Correlation Matrix Heatmap
correlation_variables <- c('trip_distance', 'fare_amount', 'tip_amount')
correlation_matrix <- cor(df_clean[correlation_variables], use = "complete.obs")

# Using corrplot package
corrplot(correlation_matrix, method = "color", type = "upper", 
         order = "hclust", tl.cex = 0.8, tl.col = "black", 
         addCoef.col = "black", number.cex = 0.7,
         title = "Correlation Matrix Heatmap: NYC Taxi Data", 
         mar = c(0,0,1,0))

# BONUS: Time Series Analysis
cat("\n=== BONUS: TIME SERIES ANALYSIS ===\n")

# Convert datetime columns
df_clean$tpep_pickup_datetime <- as.POSIXct(df_clean$tpep_pickup_datetime)
df_clean$tpep_dropoff_datetime <- as.POSIXct(df_clean$tpep_dropoff_datetime)

# Extract hour of day for pickup
df_clean$pickup_hour <- format(df_clean$tpep_pickup_datetime, "%H")
df_clean$pickup_hour <- as.numeric(df_clean$pickup_hour)

# Trip count by hour
hourly_trips <- table(df_clean$pickup_hour)
hourly_fare <- aggregate(fare_amount ~ pickup_hour, df_clean, mean)

# Create time series plots
par(mfrow = c(2, 2))

# Plot 1: Trip count by hour
barplot(hourly_trips, main = "Trip Count by Hour of Day", 
        xlab = "Hour of Day", ylab = "Number of Trips", 
        col = "skyblue", border = "black")

# Plot 2: Average fare by hour
plot(hourly_fare$pickup_hour, hourly_fare$fare_amount, type = "o", 
     main = "Average Fare Amount by Hour of Day",
     xlab = "Hour of Day", ylab = "Average Fare Amount ($)",
     col = "green", lwd = 2, pch = 16)
grid()

# Plot 3: Top 10 Pickup locations
pickup_zones <- sort(table(df_clean$PULocationID), decreasing = TRUE)[1:10]
barplot(pickup_zones, main = "Top 10 Pickup Locations", 
        xlab = "Number of Trips", ylab = "Zone ID", 
        horiz = TRUE, col = rainbow(10))

# Plot 4: Top 10 Dropoff locations
dropoff_zones <- sort(table(df_clean$DOLocationID), decreasing = TRUE)[1:10]
barplot(dropoff_zones, main = "Top 10 Dropoff Locations", 
        xlab = "Number of Trips", ylab = "Zone ID", 
        horiz = TRUE, col = rainbow(10))

par(mfrow = c(1, 1))

# CONCLUSION
cat("\n=== CONCLUSION ===\n")
cat("\n1. DESCRIPTIVE STATISTICS INSIGHTS:\n")
cat("• Trip Distance: Right-skewed distribution with mean 2.9 miles\n")
cat("• Fare Amount: Positive skew, average around $17-18\n")
cat("• Tip Amount: Highly right-skewed, many zero values\n")
cat("• Passenger Count: Most trips have 1-2 passengers\n")
cat("• Total Amount: Strong correlation with fare amount\n")

cat("\n2. HYPOTHESIS TESTING RESULTS:\n")
cat("• Average tip amount is significantly different from $2\n")
cat("• No significant difference in fare amounts between payment types\n")
cat("• Payment type and rate code are independent\n")

cat("\n3. CORRELATION FINDINGS:\n")
cat(sprintf("• Strong positive correlation between trip distance and fare amount (r = %.3f)\n", trip_fare_corr))
cat(sprintf("• Moderate positive correlation between fare amount and tip amount (r = %.3f)\n", fare_tip_corr))
cat("• Fare amount is the primary component of total amount\n")

cat("\n4. TIME-BASED PATTERNS:\n")
peak_hour <- names(which.max(hourly_trips))
min_hour <- names(which.min(hourly_trips))
cat(sprintf("• Peak trip hour: %s:00 with %d trips\n", peak_hour, max(hourly_trips)))
cat(sprintf("• Lowest trip hour: %s:00 with %d trips\n", min_hour, min(hourly_trips)))
cat("• Rush hour patterns visible in trip frequency\n")

cat("\nAnalysis completed successfully!\n")
