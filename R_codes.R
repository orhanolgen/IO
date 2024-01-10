#Install packages
install.packages("tidyverse", "dplyr", "tidyr", "readxl", "MASS")

# Load necessary libraries
library(dplyr)
library(tidyr)
library(readxl)
library(tidyverse)
library(MASS)
library(knitr)
library(ggplot2)

# Set Working Directory
setwd("C:/Users/Orhan Olgen/OneDrive - boun.edu.tr/Masaüstü/BOUN/2022_2023/2022_2023 - Spring/EC604/EC604 Term Project/EC604_FINAL/")

# Read the CSV files
data <- read.csv("data/country.csv")
prices <- read.csv("data/price_all.csv", encoding = "UTF-8")

#To check if there's any problem with the names
print(colnames(prices))
colnames(prices)[1] <- "LOCATION"

# Filter DOM (domestic) and IMP (import) variables
dom_vars <- c("DOM_01T02", "DOM_03", "DOM_05T06", "DOM_07T08", "DOM_09", "DOM_10T12", "DOM_13T15", 
              "DOM_16", "DOM_17T18", "DOM_19", "DOM_20", "DOM_21", "DOM_22", "DOM_23", "DOM_24", 
              "DOM_25", "DOM_26", "DOM_27", "DOM_28", "DOM_29", "DOM_30", "DOM_31T33", "DOM_35", 
              "DOM_36T39", "DOM_41T43", "DOM_45T47", "DOM_49", "DOM_50", "DOM_51", "DOM_52", "DOM_53", 
              "DOM_55T56", "DOM_58T60", "DOM_61", "DOM_62T63", "DOM_64T66", "DOM_68", "DOM_69T75", 
              "DOM_77T82", "DOM_84", "DOM_85", "DOM_86T88", "DOM_90T93", "DOM_94T96", "DOM_97T98")
imp_vars <- gsub("DOM", "IMP", dom_vars)

# Filter COL variables
col_vars <- c("D01T02", "D03", "D05T06", "D07T08", "D09", "D10T12", "D13T15", "D16", "D17T18", "D19", 
              "D20", "D21", "D22", "D23", "D24", "D25", "D26", "D27", "D28", "D29", "D30", "D31T33", 
              "D35", "D36T39", "D41T43", "D45T47", "D49", "D50", "D51", "D52", "D53", "D55T56", "D58T60", 
              "D61", "D62T63", "D64T66", "D68", "D69T75", "D77T82", "D84", "D85", "D86T88", "D90T93", "D94T96", "D97T98")

mapping <- c("D01T02" = "PA", "D03" = "PA", "D05T06" = "PB", "D07T08" = "PB", "D09" = "PB",
             "D10T12" = "P10_12", "D13T15" = "P13_15", "D16" = "P16", "D17T18" = "P17", "D19" = "P19",
             "D20" = "P20", "D21" = "P21", "D22" = "P22", "D23" = "P23", "D24" = "P24", "D25" = "P25",
             "D26" = "P26", "D27" = "P27", "D28" = "P28", "D29" = "P29", "D30" = "P30")


# Calculate A and M coefficients

calculate_coefficients <- function(df, country, year) {
  a_coeffs <- matrix(nrow = length(dom_vars), ncol = length(col_vars))
  m_coeffs <- matrix(nrow = length(dom_vars), ncol = length(col_vars))
  
  filtered_data <- df %>%
    filter(COU == country, TIME == year)
  
  for (i in seq_along(dom_vars)) {
    dom_var <- dom_vars[i]
    imp_var <- imp_vars[i]
    
    for (j in seq_along(col_vars)) {
      col_var <- col_vars[j]
      
      dom_value <- filtered_data$Value[filtered_data$ROW == dom_var & filtered_data$COL == col_var]
      imp_value <- filtered_data$Value[filtered_data$ROW == imp_var & filtered_data$COL == col_var]
      total_output <- filtered_data$Value[filtered_data$ROW == "OUTPUT" & filtered_data$COL == col_var]
      
      if (length(dom_value) == 0) dom_value <- 0
      if (length(imp_value) == 0) imp_value <- 0
      if (length(total_output) == 0) total_output <- 0
      
      a_coeffs[i, j] <- dom_value / total_output
      m_coeffs[i, j] <- imp_value / total_output
    }
  }
  
  return(list(a_coeffs = a_coeffs, m_coeffs = m_coeffs))
}


# Calculate exchange rate effect on CPI ( ???p/???e= (I-A^T)^-1 * (M^T x p*))

calculate_exchange_rate_effect <- function(a_coeffs, m_coeffs, price_matrix, year) {
  A <- a_coeffs
  M <- m_coeffs
  I <- diag(length(dom_vars))
  
  # Calculate (I - A^T)^-1
  inv_I_minus_A_T <- solve(I - t(A))
  
  # Select the corresponding column of the price matrix for the specified year
  price_vector <- price_matrix[, year - min(unique_years) + 1]
  
  # Calculate M^T * price_vector
  M_T_times_price_vector <- t(M) %*% price_vector
  
  # Calculate (I - A^T)^-1 (M^T * price_vector)
  result <- inv_I_minus_A_T %*% M_T_times_price_vector
  
  #Take the average of the result
  #result <- mean(result, na.rm = TRUE)

  
  return(result)
}

# Create price matrix for each country and year

create_price_matrix <- function(country, years) {
  price_matrix <- matrix(100, nrow = length(col_vars), ncol = length(years))
  
  for (row in 1:length(col_vars)) {
    sector_code <- col_vars[row]
    cpa_code <- mapping[sector_code]
    
    if (!is.na(cpa_code)) {
      for (col in 1:length(years)) {
        value <- prices %>%
          filter(LOCATION == country,
                 CPA_VER_2_1 == cpa_code,
                 Flow == "Imports",
                 TIME == years[col]) %>%
          pull(Value)
        
        if (length(value) > 0) {
          price_matrix[row, col] <- value
        }
      }
    }
  }
  
  return(price_matrix / 100)
}

# Initialize result list
result <- list()

# Get unique countries and years from the data
unique_countries <- unique(data$COU)
unique_years <- unique(data$TIME)

#####################
price_matrices <- list()

for (country in unique_countries) {
  price_matrices[[country]] <- create_price_matrix(country, unique_years)
}

for (country in unique_countries) {
  cat("Price matrix for", country, ":\n")
  print(price_matrices[[country]])
  cat("\n")
}
####################

############################################################################################################################################
# SIMPLIFIED CORRECTED

# Define the column names for HFCE, NPISH, and GGFC
col_names <- c("HFCE", "NPISH", "GGFC")
row_names <- c("TTL_INT_FNL")

# Initialize a data frame to store the results
weights_df <- data.frame(Country = character(), Year = integer(), Sector = character(), Sectoral_Expenditure = numeric(), Aggregate_Expenditure = numeric(), Share = numeric())

# Loop through each country and year
for (country in unique_countries) {
  for (year in unique_years) {
    # Calculate aggregate consumption expenditure
    aggregate_expenditure <- 0  # Initialize total aggregate expenditure
    
    for (i in 1:length(dom_vars)) {
      dom_var <- dom_vars[i]
      imp_var <- imp_vars[i]
      
      # Filter the data for sectoral expenditure for the specified country, year, and sectors
      filtered_data_sectoral <- data %>%
        filter(COU == country, TIME == year, ROW %in% c(dom_var, imp_var), COL %in% col_names)
      
      # Calculate the sectoral expenditure by summing the values
      sectoral_expenditure <- sum(filtered_data_sectoral$Value)
      
      # Filter the data for aggregate expenditure for the specified country, year, and sectors
      filtered_data_aggregate <- data %>%
        filter(COU == country, TIME == year, ROW %in% row_names, COL %in% col_names)
      
      # Calculate the aggregate expenditure by summing the values
      aggregate_expenditure <- sum(filtered_data_aggregate$Value)
      
      # Calculate the sector share
      sector_share <- sectoral_expenditure / aggregate_expenditure
      
      # Append the results to weights_df
      weights_data <- data.frame(
        Country = rep(country, 1),
        Year = rep(year, 1),
        Sector = col_vars[i],
        Sectoral_Expenditure = sectoral_expenditure,
        Aggregate_Expenditure = aggregate_expenditure,
        Share = sector_share
      )
      weights_df <- rbind(weights_df, weights_data)
    }
  }
}

# Print the results
print(weights_df)

####################################

# Initialize an empty data frame to store the results
results_df <- data.frame(Country = character(), Year = integer(), AvgExchangeRateEffect = numeric())

# Loop through each country and year
for (country in unique(data$COU)) {
  for (year in unique(data$TIME)) {
    # Retrieve the pre-calculated coefficients and exchange rate effects
    coeffs <- calculate_coefficients(data, country, year)
    price_matrix <- create_price_matrix(country, unique(data$TIME))
    exchange_rate_effect <- calculate_exchange_rate_effect(coeffs$a_coeffs, coeffs$m_coeffs, price_matrix, year)
    
    # Retrieve the sector shares for the current country and year from weights_df
    shares <- weights_df$Share[weights_df$Country == country & weights_df$Year == year]
    
    # Ensure that you are using the correct elements of exchange_rate_effect and shares
    #weighted_avg_exchange_rate_effect <- sum(shares * exchange_rate_effect)
    
    # Add the result to the results_df data frame
    results_df <- rbind(results_df, data.frame(Country = country, Year = year, WeightedAvgExchangeRateEffect = weighted_avg_exchange_rate_effect))
    
  }
}

# Display the results_df data frame
print(results_df)


#Save the file
#write.csv(results_df, "results/results_sector_ZAF_VNM_weighted.csv", row.names = FALSE)



############################################################################################################################################


