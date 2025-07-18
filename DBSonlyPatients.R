# Load necessary libraries
library(readr)
library(dplyr)

# Load the two tables
LEDD_Concomitant_Medication_Log_Calculated <- read_csv("LEDD_Concomitant_Medication_Log_Calculated.csv")
Surgery_for_PD_Log <- read_csv("Surgery_for_PD_Log_20Jun2024.csv")

# Filter the Surgery_for_PD_Log table to contain only patients with value 2 in the PDSRGLOC column
Surgery_for_PD_Log <- Surgery_for_PD_Log %>%
  filter(PDSRGLOC == 2)

# Find the patient numbers in LEDD_Concomitant_Medication_Log_Calculated that are also present in Surgery_for_PD_Log
DBS_patients <- LEDD_Concomitant_Medication_Log_Calculated %>%
  filter(PATNO %in% Surgery_for_PD_Log$PATNO)

# Save the extracted data to a new CSV file
write.csv(DBS_patients, file = "LEDD_Concomitant_Medication_Log_Calculated_STN_DBSonly.csv", row.names = FALSE)

# Count the number of unique patient numbers in the new table
unique_patient_count <- n_distinct(DBS_patients$PATNO)

# Print the number of unique patient numbers
print(paste("Number of unique patient numbers:", unique_patient_count))