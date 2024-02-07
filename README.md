You can find the pdf file of the research "Exchange Rate Pass-Through A Comparative Analysis" also R-codes to calculate exchange rate pass-through.

Here is a summary of the code:

1. **Package Installation and Library Loading:**
   - Installs and loads several R packages, including tidyverse, dplyr, tidyr, readxl, MASS, knitr, and ggplot2.

2. **Working Directory and Data Loading:**
   - Sets the working directory.
   - Reads two CSV files ("country_all.csv" and "price_all.csv") into data frames (`data` and `prices`).

3. **Data Cleaning and Variable Filtering:**
   - Renames a column in the `prices` data frame.
   - Defines and filters variables related to domestic (`DOM`) and import (`IMP`) categories.
   - Creates variables for consumption expenditure (`col_vars`) and maps them to specific codes (`mapping`).

4. **Coefficient Calculation:**
   - Defines a function (`calculate_coefficients`) to calculate coefficients (`a_coeffs` and `m_coeffs`) based on specific conditions and filters.

5. **Exchange Rate Effect Calculation:**
   - Defines a function (`calculate_exchange_rate_effect`) to calculate the exchange rate effect on the Consumer Price Index (CPI) using matrices and formulas.

6. **Price Matrix Creation:**
   - Defines a function (`create_price_matrix`) to create a price matrix for each country and year based on specific conditions.

7. **Data Processing and Output:**
   - Processes unique countries and years from the data.
   - Creates and prints price matrices for each country.

8. **Simplified Corrected Calculation:**
   - Defines variables and initializes a data frame to store sectoral and aggregate expenditures results.

9. **Loop for Expenditure Calculation:**
   - Loops through each country and year, calculates sectoral and aggregate expenditures, and stores the results in a data frame (`weights_df`).

10. **Prints the Weighted Results:**
    - Prints the resulting data frame (`weights_df`).

11. **Results DataFrame Initialization:**
    - Initializes an empty data frame (`results_df`) to store results related to the weighted average exchange rate effect.

12. **Loop for Weighted Exchange Rate Effect Calculation:**
    - Loops through each country and year, retrieves pre-calculated coefficients and exchange rate effects, calculates weighted averages, and stores the results in a data frame (`results_df`).

13. **Prints the Final Results:**
    - Prints the resulting data frame (`results_df`).

14. **File Saving:**
    - The final results data frame is saved to a CSV file (currently commented out).
