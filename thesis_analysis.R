# Setting working directory
setwd("C:/Users/Eier/Documents/MAE4000/data/")

# Loading required libraries
library(mirt)
library(mirtCAT)
library(readxl)
library(tidyr)
library(dplyr)
library(lavaan)
library(parallel)

# Import real response data of May 2023 session
may23 <- read_excel("may2023.xlsx",col_types = "numeric")

# These items were used in the real test
response <- may23[c(1:60, 68:80, 88:100)]

# Recoding polytomous items
response[, c(60, 73)] <- lapply(response[, c(60, 73)], function(col) {
  ifelse(col %in% c(2, 3, 4), 1, col)
})
# Replace 9s and 99s
response <- apply(response, 2, function(col) {
  col[col == 99] <- NA  # items that are not seen and not completed
  col[col == 9] <- 0    # items that are seen but not completed
  return(col)
})
# Rescoring TRUE/False to 1 and 0
response <- ifelse(response == TRUE, 1, ifelse(response == FALSE, 0, response))

# Defining the structure of the multi-stage test with items for each module
modules <- list(
  Pretest1 = 1:7,    # items 1 to 7 are for Pretest1
  `2A` = 8:14,       # items 8 to 14 are for module 2A
  `2B` = 15:22,      # items 15 to 22 are for module 2B
  A1A2 = 23:38,      # items 23 to 38 are for module A1A2
  A2B1 = 39:60,      # items 39 to 60 are for module A2B1
  B1B2 = 61:86       # items 61 to 86 are for module B1B2
)

# Defining routing rules based on the number of correct answers
routing_rules <- list(
  Pretest1 = function(score) {
    if (!is.na(score) && score <= 4) {
      return("2A")
    } else if (!is.na(score)) {
      return("2B")
    } else {
      return(NULL)
    }
  },
  `2A` = function(score) {
    if (!is.na(score) && score <= 5) {
      return("A1A2")
    } else {
      return("A2B1")
    }
  },
  `2B` = function(score) {
    if (!is.na(score) && score <= 4) {
      return("A2B1")
    } else {
      return("B1B2")
    }
  }
)
# Estimating through 2PL model
READM1 <- mirt(response[, 1:86], 1, itemtype = "2PL", SE = TRUE,
               technical = list(NCYCLES = 4000), module = modules, routing = routing_rules)

coef(READM1, IRTpars = TRUE, simplify = TRUE)
READPars1 <- as.data.frame(coef(READM1, IRTpars = TRUE, simplify = TRUE))
READPars1 <- READPars1[, 1:2]

######## Descriptive #########

# Create a function to count the number of participants per module
count_participants_per_module <- function(response, modules) {
  module_counts <- sapply(modules, function(module_range) {
    sum(rowSums(!is.na(response[, module_range])) > 0)  # Count respondents with at least one non-NA response in each module
  })
  return(module_counts)
}

# Call the function to get the module-wise participant counts
module_participant_counts <- count_participants_per_module(response, modules)

# Print the module-wise participant counts
print(module_participant_counts)

#TO COUNT HOW MANY MISSING RESPONSE BY DESIGN
total_items <- prod(dim(response))
total_na <- sum(is.na(response))
cat("Total number of NAs in the entire dataset:", total_na, "\n")
cat("Percentage of total NAs in the entire dataset:", (total_na / total_items) * 100, "%\n\n")

# Count NAs for each module in MST design
na_by_module <- sapply(modules, function(items) {
  sum(is.na(response[, items]))
})

# Calculate the number of possible items in each module
possible_items_by_module <- sapply(modules, function(items) {
  nrow(response) * length(items)
})

# Calculate percentage of NAs by module
percentage_na_by_module <- (na_by_module / possible_items_by_module) * 100
cat("Percentage of NAs by module:\n")
print(percentage_na_by_module)

# Calculate the number of NAs due to participants not reaching a module 
not_reached_na <- sapply(modules, function(items) {
  sum(rowSums(is.na(response[, items])) == length(items)) * length(items)
})

# Calculate percentage of NAs due to not reaching (by design)
percentage_not_reached_na <- (not_reached_na / possible_items_by_module) * 100
cat("\nPercentage of NAs due to not reaching (by design):\n")
print(percentage_not_reached_na)

# Calculate the number of NAs that are not due to not reaching 
actual_na <- na_by_module - not_reached_na
percentage_actual_na <- (actual_na / possible_items_by_module) * 100
cat("\nPercentage of NAs not due to not reaching (actual missing data):\n")
print(percentage_actual_na)


###################################################
################ TEST DESIGN##############
set.seed(123)

#####Creating a hybrid data with response in every item#####
thetas_mst <- fscores(READM1)
simulated_responses_mst <- matrix(NA, nrow = length(thetas_mst), ncol = 86)
for (i in 1:length(thetas_mst)) {
  pat_mst <- generate_pattern(READM1, Theta = thetas_mst[i])
  simulated_responses_mst[i, ] <- pat_mst
}
### Convert hybrid simulated responses to a data frame
simulated_responses_mst <- as.data.frame(simulated_responses_mst)
colnames(simulated_responses_mst) <- paste0("Item_", 1:86)
model_simulated_mst <- mirt(simulated_responses_mst[, c(1:86)], itemtype = "2PL")

####Model Fit####

M2(model_simulated_mst)

########DIFFICULY AND DISCRIMINATION PARAMETER
# Extract the full item parameters including difficulty and discrimination
item_params <- coef(model_simulated_mst, IRTpars = TRUE, simplify = TRUE)  # Extracting IRT parameters
# Extract the item parameter matrix
item_matrix <- item_params$items
# Initialize lists to store the ranges
difficulty_range <- list()
discrimination_range <- list()
# Iterate over each module to calculate the ranges
for (module_name in names(modules)) {
  # Get the item indices for the current module
  item_indices <- modules[[module_name]]
  # Extract the difficulty and discrimination parameters for the items in this module
  difficulties <- item_matrix[item_indices, "b"]
  discriminations <- item_matrix[item_indices, "a"]
  # Calculate the range for difficulty and discrimination
  difficulty_range[[module_name]] <- range(difficulties, na.rm = TRUE)
  discrimination_range[[module_name]] <- range(discriminations, na.rm = TRUE)
}

# Function to print the range with two decimal places
print_range <- function(range_list, label) {
  cat(label, ":\n")
  for (module in names(range_list)) {
    range_values <- range_list[[module]]
    formatted_values <- sprintf("%.2f", range_values)  # Format to two decimal points
    cat(module, ": [", formatted_values[1], ", ", formatted_values[2], "]\n")
  }
}

# Print the difficulty ranges
print_range(difficulty_range, "Item Difficulty Ranges by Module")

# Print the discrimination ranges
print_range(discrimination_range, "Item Discrimination Ranges by Module")


#########TRUE THETA#############
thetas_full <- as.matrix(fscores(model_simulated_mst, method = 'MAP', response.pattern = simulated_responses_mst, append_response.pattern = FALSE)[, "F1"])

#####Outcome Measures#####
I_full <- ncol(simulated_responses_mst)
pat_full <- as.matrix(simulated_responses_mst)
#Define function to extract relevant information from different test outputs
EXTRACT <- function(OUT, thetatrue) {
  out <- as.data.frame(t(sapply(OUT, function(x) {
    thetatrue <- x$true_thetas 
    J <- length(x$items_answered)
    thetahat <- x$thetas 
    SE <- x$SE_thetas 
    INFOPI <- (1 / (SE^2)) / J
    return(c(J = J, thetahat = thetahat, SE = SE, INFOPI = INFOPI))
  })))
  out$thetatrue <- as.vector(thetatrue)
  out$BIAS <- out$thetahat - out$thetatrue
  out$BIASPI <- out$BIAS / out$J
  out <- out[, c("thetatrue", "J", "thetahat", "BIAS", "SE", "INFOPI")]
  return(out)
}


##### Fit Full-Length Test ######

model_full <- mirt(simulated_responses_mst[, c(1:86)], itemtype = "2PL")

# Run Full-Length Test
I <- ncol(simulated_responses_mst)
cl <- makeCluster(detectCores() - 1)
design_FULL <- list(min_items = I, max_items = I)
out_FULL <- mirtCAT(mo = model_simulated_mst, criteria = "seq", method = "MAP", local_pattern = pat_full, start_item = 1, design = design_FULL, cl = cl)
stopCluster(cl)

##### FIT MST #####
simulated_responses_mst <- as.matrix(simulated_responses_mst)
model_mst_sim <- mirt(simulated_responses_mst, 1, itemtype = '2PL', module = modules, routing = routing_rules)

cl_mst <- makeCluster(detectCores() - 1)
out_MST <- mirtCAT(mo = model_mst_sim, criteria = "seq", method = "MAP", local_pattern = simulated_responses_mst, start_item = 1, design = list(min_items = 30, max_items = 41), cl = cl_mst)
stopCluster(cl_mst)


#####FIT EQUAL LENGTH CAT#####
#Extract the number of items answered in MST for each individual
num_items_mst <- sapply(out_MST, function(x) length(x$items_answered))
print(num_items_mst)

out_CAT_fix = out_MST
for(i in 1:length(out_CAT_fix)){
  design_CAT_fix = list(min_items = num_items_mst[i],max_items = num_items_mst[i])
  out_CAT_fix[[i]] =  mirtCAT(mo = model_simulated_mst, criteria = "MI", method = "MAP", local_pattern = pat_full[i,], start_item = "random", design = design_CAT_fix)
}

##### FIT EQUAL PRECISION CAT #####

# Extract results for CAT EQUAL PRECISION
mst_SE <- sapply(out_MST, function(x) x$SE_thetas)

# Function to run CAT with a stopping criterion based on the MST SE
run_custom_CAT <- function(model, pattern, target_SE) {
  # Define the design for this specific person's CAT
  design <- list(
    min_SEM = target_SE,  # Set the stopping rule based on the MST SE
    max_items = I         # Ensure the CAT can administer as many items as needed
  )
  
  # Run the CAT for this individual
  out <- mirtCAT(
    mo = model, 
    criteria = "MI", 
    method = "MAP", 
    local_pattern = pattern, 
    start_item = "random", 
    design = design
  )
  
  return(out)
}

# Apply the custom CAT function to each individual
out_CAT_var <- lapply(1:nrow(pat_full), function(i) {
  run_custom_CAT(model_simulated_mst, pat_full[i, ], mst_SE[i])
})


#####FIT FIXED PRECISION CAT #####

cl <- makeCluster(detectCores()-1)
design_CAT <- list(min_SEM = 0.387, max_items = I)

#### Run CAT
out_CAT <- mirtCAT(mo = model_simulated_mst, criteria = "MI", method = "MAP", local_pattern = pat_full, start_item = "random", design = design_CAT,cl=cl)
stopCluster(cl)


##### FIT LINEAR TEST #####
# Function to count item usage in CAT
count_item_usage <- function(out_CAT_fix) {
  item_usage <- sapply(out_CAT_fix, function(x) x$items_answered)
  item_usage <- unlist(item_usage)
  item_freq <- table(item_usage)
  item_freq <- as.data.frame(item_freq)
  colnames(item_freq) <- c("Item", "Frequency")
  item_freq <- item_freq[order(-item_freq$Frequency), ]
  return(item_freq)
}

# Count item usage in CAT
item_usage_freq <- count_item_usage(out_CAT_fix)

# Select the top 22 most frequently used items
top_items <- as.numeric(item_usage_freq$Item[1:22])

# Subset the simulated_responses_mst to include only the top 22 items
Lin_response <- as.data.frame(simulated_responses_mst[, top_items])
# Fit the model to the linear test responses
model_linear <- mirt(Lin_response, 1, itemtype = '2PL')

# Define the design for the linear test where everyone will take all items
design_linear_test <- list(min_items = 22, max_items = 22)

# Ensure start item is valid and the local pattern is correctly specified
start_item <- 1
local_pattern <- as.matrix(Lin_response)

# Run the linear test
out_LIN <- mirtCAT(mo = model_linear, criteria = "seq", method = "MAP", local_pattern = local_pattern, start_item = start_item, design = design_linear_test)

#########Extract Outcome###########
# Extract results for Full-Length Test
dataFULL <- EXTRACT(out_FULL, thetas_full)

# Extract results for MST
thetatrue_mst <- fscores(model_mst_sim)
dataMST <- EXTRACT(out_MST, thetas_full)

# Extract results for Equal length CAT
dataCATfix <- EXTRACT(out_CAT_fix, thetas_full)

# Extract results equal precision CAT 
dataCATvar <- EXTRACT(out_CAT_var, thetas_full)

# Extract results for Linear Test
dataLIN <- EXTRACT(out_LIN, thetas_full)

# Extract results for fixed precision CAT 
dataCAT <- EXTRACT(out_CAT, thetas_full)

#####Compare efficiency#####
compare_efficiency <- function(dataFULL, dataMST,dataCATfix, dataCATvar, dataLIN, dataCAT) {
  mean_items_FULL <- if (!is.null(dataFULL)) mean(dataFULL$J) else NA
  mean_SE_FULL <- if (!is.null(dataFULL)) mean(dataFULL$SE) else NA
  mean_bias_FULL <- if (!is.null(dataFULL)) mean(dataFULL$BIAS) else NA
  mean_info_FULL <- if (!is.null(dataFULL)) mean(dataFULL$INFOPI) else NA
  
  mean_items_MST <- if (!is.null(dataMST)) mean(dataMST$J) else NA
  mean_SE_MST <- if (!is.null(dataMST)) mean(dataMST$SE) else NA
  mean_bias_MST <- if (!is.null(dataMST)) mean(dataMST$BIAS) else NA
  mean_info_MST <- if (!is.null(dataMST)) mean(dataMST$INFOPI) else NA
  
  
  mean_items_CATfix <- if (!is.null(dataCATfix)) mean(dataCATfix$J) else NA
  mean_SE_CATfix <- if (!is.null(dataCATfix)) mean(dataCATfix$SE) else NA
  mean_bias_CATfix <- if (!is.null(dataCATfix)) mean(dataCATfix$BIAS) else NA
  mean_info_CATfix <- if (!is.null(dataCATfix)) mean(dataCATfix$INFOPI) else NA
  
  mean_items_CATvar <- if (!is.null(dataCATvar)) mean(dataCATvar$J) else NA
  mean_SE_CATvar <- if (!is.null(dataCATvar)) mean(dataCATvar$SE) else NA
  mean_bias_CATvar <- if (!is.null(dataCATvar)) mean(dataCATvar$BIAS) else NA
  mean_info_CATvar <- if (!is.null(dataCATvar)) mean(dataCATvar$INFOPI) else NA
  
  mean_items_LIN <- if (!is.null(dataLIN)) mean(dataLIN$J) else NA
  mean_SE_LIN <- if (!is.null(dataLIN)) mean(dataLIN$SE) else NA
  mean_bias_LIN <- if (!is.null(dataLIN)) mean(dataLIN$BIAS) else NA
  mean_info_LIN <- if (!is.null(dataLIN)) mean(dataLIN$INFOPI) else NA
  
  mean_items_CAT <- if (!is.null(dataCAT)) mean(dataCAT$J) else NA
  mean_SE_CAT <- if (!is.null(dataCAT)) mean(dataCAT$SE) else NA
  mean_bias_CAT <- if (!is.null(dataCAT)) mean(dataCAT$BIAS) else NA
  mean_info_CAT <- if (!is.null(dataCAT)) mean(dataCAT$INFOPI) else NA
  
  efficiency_metrics <- data.frame(
    Test = c("FULL", "MST", "CAT-EL", "CAT-EP", "LIN","CAT-P85"),
    Mean_Items = c(mean_items_FULL, mean_items_MST, mean_items_CATfix, mean_items_CATvar, mean_items_LIN, mean_items_CAT),
    Mean_SE = c(mean_SE_FULL, mean_SE_MST,mean_SE_CATfix, mean_SE_CATvar, mean_SE_LIN,mean_SE_CAT),
    mean_bias = c(mean_bias_FULL, mean_bias_MST,mean_bias_CATfix, mean_bias_CATvar, mean_bias_LIN,mean_bias_CAT),
    Mean_Info = c(mean_info_FULL, mean_info_MST,mean_info_CATfix, mean_info_CATvar, mean_info_LIN,mean_info_CAT)
  )
  return(efficiency_metrics)
}

# Calculate Efficiency Metrics
efficiency_comparison <- compare_efficiency(dataFULL, dataMST, dataCATfix, dataCATvar, dataLIN,dataCAT)
print(efficiency_comparison)

# Round the dataframe to two decimal places
efficiency_comparison_rounded <- efficiency_comparison
efficiency_comparison_rounded[, -1] <- round(efficiency_comparison[, -1], 3)

# Print the rounded results
print(efficiency_comparison_rounded)


# Add a 'Test' column to each dataframe
dataFULL$Test <- "FULL"
dataMST$Test <- "MST"
dataCATfix$Test <- "CAT-EL"
dataCATvar$Test <- "CAT-EP"
dataLIN$Test <- "LIN"
dataCAT$Test <- "CAT-P85"

# Combine all dataframes into a single dataframe
combined_data <- bind_rows(dataFULL, dataMST, dataCATfix, dataCATvar, dataLIN, dataCAT)
# Save combined_data as an RDS file
saveRDS(combined_data, file = "combined_data.rds")

####################

##### ITEM USAGE UNDER CAT-P85#####

# Filter the data for CAT tests
cat_data <- combined_data[combined_data$Test == "CAT-P85", ]

# Check if there is data for CAT tests
if (nrow(cat_data) > 0) {
  # Define the intervals for J
  intervals <- list(
    "Upto 5" = c(0, 5),
    "Upto 10" = c(6, 10),
    "Upto 15" = c(11, 15),
    "Upto 30" = c(16, 30),
    "30+" = c(31, Inf)
  )
  
  # Calculate the count of J values within each interval
  interval_counts <- sapply(intervals, function(range) {
    sum(cat_data$J >= range[1] & cat_data$J <= range[2])
  })
  
  # Calculate percentages
  total_count <- nrow(cat_data)
  interval_percentages <- (interval_counts / total_count) * 100
  
  # Print percentages
  print(interval_percentages)
} else {
  cat("No data available for CAT tests in the dataset.\n")
}
# Filter the data for CAT test and participants who used exactly 86 items
participants_86_items_CAT <- combined_data[combined_data$Test == "CAT-P85" & combined_data$J == 86, ]

# Count the number of participants who used 86 items in CAT
count_86_items_CAT <- nrow(participants_86_items_CAT)

# Print the count
cat("Number of participants who used 86 items in CAT:", count_86_items_CAT, "\n")


##### ITEM USAGE UNDER CAT-EP#####

catvar_data <- combined_data[combined_data$Test == "CAT-EP", ]

# Check if there is data for CATvar tests
if (nrow(catvar_data) > 0) {
  # Define the intervals for J
  intervals_catvar <- list(
    "Upto 15" = c(0, 15),
    "Upto 20" = c(16, 20),
    "Upto 25" = c(21, 25),
    "25+" = c(26, Inf)
  )
  
  # Calculate the count of J values within each interval
  interval_counts_catvar <- sapply(intervals_catvar, function(range) {
    sum(catvar_data$J >= range[1] & catvar_data$J <= range[2])
  })
  
  # Calculate percentages
  total_count_catvar <- nrow(catvar_data)
  interval_percentages_catvar <- (interval_counts_catvar / total_count_catvar) * 100
  
  # Print percentages
  print(interval_percentages_catvar)
} else {
  cat("No data available for CATvar tests in the dataset.\n")
}


# Filter out CAT-EP and MST data
cat_ep_data <- combined_data %>% filter(Test == "CAT-EP") %>% select(J, thetatrue)
mst_data <- combined_data %>% filter(Test == "MST") %>% select(J, thetatrue)

# Merge on the 'thetatrue' column, assuming this matches participants across test formats
comparison_data <- merge(cat_ep_data, mst_data, by = "thetatrue", suffixes = c("_CAT_EP", "_MST"))

# Calculate the count of participants with shorter test length in CAT-EP than MST
shorter_test_length_count <- sum(comparison_data$J_CAT_EP < comparison_data$J_MST)

# Calculate percentage
total_count <- nrow(comparison_data)
shorter_test_length_percentage <- (shorter_test_length_count / total_count) * 100

# Print the percentage
cat("Percentage of participants with a shorter test length in CAT-EP compared to MST:", shorter_test_length_percentage, "%\n")


# Filter out CAT-EL and MST data
cat_el_data <- combined_data %>% filter(Test == "CAT-EL") %>% select(SE, thetatrue)
mst_data <- combined_data %>% filter(Test == "MST") %>% select(SE, thetatrue)

# Merge on the 'thetatrue' column, assuming this matches participants across test formats
comparison_data <- merge(cat_el_data, mst_data, by = "thetatrue", suffixes = c("_CAT_EL", "_MST"))

# Calculate the count of participants with lower SE in CAT-EL than MST
lower_se_count <- sum(comparison_data$SE_CAT_EL < comparison_data$SE_MST)

# Calculate percentage
total_count <- nrow(comparison_data)
lower_se_percentage <- (lower_se_count / total_count) * 100

# Print the percentage
cat("Percentage of participants with a lower SE in CAT-EL compared to MST:", lower_se_percentage, "%\n")

