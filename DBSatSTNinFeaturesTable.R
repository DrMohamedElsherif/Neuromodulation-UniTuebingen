data <- read.csv("MOCA_DBS_combined_PPMI_w_coding.csv")

# Filter rows where ID starts with "I"
filtered_patnos <- data[grep("^I", data$ID), ]

# Select the PATNO column
patno_column <- filtered_patnos$PATNO

# Print the selected PATNO column
print(patno_column)

# Assuming patno_column is already defined as the vector of PATNO values
patno_column <- c(3124, 3362, 3365, 3366, 3367, 3371, 3372, 3374, 3377, 3380, 3383)

# Read the LEDD_Concomitant_Medication_Log_Calculated_STN_DBSonly.csv file
concomitant_data <- read.csv("LEDD_Concomitant_Medication_Log_Calculated_STN_DBSonly.csv")

# Filter rows based on PATNO column
filtered_data <- concomitant_data[concomitant_data$PATNO %in% patno_column, ]

# Save filtered data to a new CSV file
write.csv(filtered_data, "LEDD_Concomitant_Medication_Log_Calculated_STN_DBSonly_whohasFeatures.csv", row.names = FALSE)

# Optionally, you can print a message indicating success
cat("Filtered data saved to LEDD_Concomitant_Medication_Log_Calculated_STN_DBSonly_whohasFeatures.csv\n")