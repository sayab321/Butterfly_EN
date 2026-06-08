##############
###evenness
##################
# libraries
library(vegan); library(readxl)

# load data
data <- read_excel("~/Desktop/Analysis/1_Land_wise_data.xlsx")
#data <- read_excel("~/Desktop/Analysis/2_1_Forest_month_data.xlsx")
#data <- read_excel("~/Desktop/Analysis/2_2_Cropland_month_data.xlsx") #cropland
#data <- read_excel("~/Desktop/Analysis/2_3_Settlement_month_data.xlsx") #settlement
#data <- read_excel("~/Desktop/Analysis/2_Combined_month_data.xlsx")

# Remove species column for analysis
abundance_only <- data[, -1]
rownames(abundance_only) <- data$Species

# Function to calculate evenness
calculate_evenness <- function(abundances) {
  H <- diversity(abundances, index = "shannon")
  S <- sum(abundances > 0)
  if (S > 1) {
    J <- H / log(S)
  } else {
    J <- NA  # Evenness undefined if only one species
  } 
  return(J)}

# Apply evenness calculation to each forest
evenness_results <- apply(abundance_only, 2, calculate_evenness)

# Print results
evenness_results
