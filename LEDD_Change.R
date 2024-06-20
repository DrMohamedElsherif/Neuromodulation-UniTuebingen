library(readr)
library(dplyr)
library(stringr)
# Load table into data frames
LEDD_Concomitant_Medication_Log <- read_csv ("LEDD_Concomitant_Medication_Log_20Jun2024.csv")
# Convert the start and stop dates from MM/YYYYY to 01/MM/YYYY 
LEDD_Concomitant_Medication_Log <- mutate(LEDD_Concomitant_Medication_Log, STARTDT = 
                                            as.Date(paste("01/", as.character(STARTDT)), "%d/%m/%Y"))
# Add 1 month to the STOPDT because we assume that the dosage is taken until the end 
# of the month, hence the change takes effect from the start of the next month
LEDD_Concomitant_Medication_Log <- mutate(LEDD_Concomitant_Medication_Log, STOPDT = 
                                            as.Date(paste("01/", (as.integer(str_sub(STOPDT, 1, 2))%%12+1), "/", 
                                                          as.integer(str_sub(STOPDT, 4, 7))+(as.integer(str_sub(STOPDT, 1, 2))+1)%/%13, sep=""), 
                                                    "%d/%m/%Y"))
# Get the set of unique dates when LEDD changes for each PATNO
LEDD <- unique(rbind(LEDD_Concomitant_Medication_Log %>% select(PATNO, STARTDT),
                     LEDD_Concomitant_Medication_Log %>% select(PATNO, STARTDT = 
                                                                  STOPDT) %>% filter(!is.na(STARTDT)))) %>% arrange (PATNO, STARTDT)
# Get the base L-Dopa value at each point in time
LDOPA_VALUES <- LEDD_Concomitant_Medication_Log[str_detect(LEDD_Concomitant_Medication_Log$LEDTRT,regex("LEV|DOP|RYTA|SINEMET|STANEK|STALEVO|INBRIJA|ISICOM|NACOM|PROLOPA",ignore_case=TRUE)
  ),] %>%
  select(PATNO,STARTDT,STOPDT,LEDD,LEDDSTRMG,LEDDOSE,LEDDOSFRQ) %>%
  # Also calculate the LEDD value from base medication details, if available, in case 
  # not populated in LEDD column
mutate(LEDD3 = case_when (is.numeric(LEDDSTRMG) ~ (LEDDSTRMG * LEDDOSE * LEDDOSFRQ) , 
                          TRUE ~ 0.0)) %>%
  mutate(LEDD2 = case_when (!is.na(as.double(as.character(LEDD))) ~ as.double 
                            (as.character(LEDD)) , TRUE ~ LEDD3))

LEDD <- left_join (LEDD, 
                   left_join(LEDD, LDOPA_VALUES, by="PATNO") %>% 
                     filter(STARTDT.y <= STARTDT.x & (is.na(STOPDT) | (STOPDT > STARTDT.x & STOPDT > STARTDT.y))) %>%
                     mutate (LEDD2 = case_when (is.na(LEDD2) ~ as.double (0.0), TRUE ~ LEDD2)) %>%
                     group_by(PATNO,STARTDT.x) %>%
                     summarize (LDOPA = sum(LEDD2), .groups = "drop") %>% 
                     rename (STARTDT = STARTDT.x), by = c("PATNO","STARTDT"))

# Get the base LEDD value at each point in time
# If the drug is levodopa and the LEDD value is not available, attempt to derive it 
# from base medication details
LEDD_VALUES <- mutate(LEDD_Concomitant_Medication_Log, LEDD3 = case_when(str_detect(LEDD_Concomitant_Medication_Log$LEDTRT,regex("LEV|DOP|RYTA|SINEMET|STANEK|STALEVO|INBRIJA|ISICOM|NACOM|PROLOPA",ignore_case=TRUE)) ~ (LEDDSTRMG * LEDDOSE * LEDDOSFRQ) , TRUE ~ 0.0)) %>%
  mutate(LEDD2 = case_when (!is.na(as.double(as.character(LEDD))) ~ as.double(as.character(LEDD)) , TRUE ~ LEDD3)) 

LEDD <- left_join (LEDD, left_join(LEDD, LEDD_VALUES, by="PATNO") %>% 
                     filter(STARTDT.y <= STARTDT.x & (is.na(STOPDT) | (STOPDT > STARTDT.x & STOPDT > STARTDT.y)) & !is.na(LEDD2)) %>%
                     group_by(PATNO,STARTDT.x) %>%
                     summarize (LEDD = sum(LEDD2), .groups = "drop") %>% 
                     rename (STARTDT = STARTDT.x), by = c("PATNO","STARTDT"))

# Update base LEDD value for drugs with LEDD profile of the form LD * A
VALUES_TO_UPDATE <- LEDD_Concomitant_Medication_Log %>% 
  filter (substr(LEDD,1,4) =="LD x") %>% 
  select (PATNO, STARTDT,STOPDT, LEDD) %>%
  mutate (FACTOR = as.double(str_match(LEDD, "\\d+\\.\\d+$")))

LEDD <- left_join (LEDD, left_join(LEDD, VALUES_TO_UPDATE, by="PATNO") %>% 
                     filter(STARTDT.y <= STARTDT.x & (is.na(STOPDT) | (STOPDT > STARTDT.x & STOPDT > STARTDT.y))) %>%
                     group_by(PATNO,STARTDT.x) %>%
                     summarize (FACTOR1 = sum(FACTOR), .groups = "drop") %>% 
                     rename (STARTDT = STARTDT.x), by = c("PATNO","STARTDT"))

# Update base LEDD value for drugs with LEDD profile of the form (B + LD) x A
VALUES_TO_UPDATE <- LEDD_Concomitant_Medication_Log %>% 
  filter (str_detect(LEDD,"\\+\\ LD")) %>% 
  select (PATNO, STARTDT,STOPDT, LEDD) %>%
  mutate (FACTOR2 = as.double(str_match(LEDD, "\\d+\\.\\d+")),
          FACTOR3 = as.double(str_match(LEDD, "\\d+\\.\\d+$")),
          FACTOR4 = as.double(str_match(LEDD, "\\d+\\.\\d+$")))

LEDD <- left_join (LEDD, left_join(LEDD, VALUES_TO_UPDATE, by="PATNO") %>% 
                     filter(STARTDT.y <= STARTDT.x & (is.na(STOPDT) | (STOPDT > STARTDT.x & STOPDT > STARTDT.y))) %>%
                     group_by(PATNO,STARTDT.x) %>%
                     summarize (FACTOR2 = sum(FACTOR2), FACTOR3 = mean(FACTOR3),FACTOR4 = sum(FACTOR4), .groups = "drop") %>% rename (STARTDT = STARTDT.x), by = c("PATNO","STARTDT"))

# Tidy up null values and make final calculations
LEDD <- mutate (LEDD, LDOPA = case_when(is.na(LDOPA) ~ 0.0, TRUE ~ LDOPA), LEDD = case_when(is.na(LEDD) ~ 0.0, TRUE ~ LEDD))

LEDD <- mutate (LEDD, LEDD = LEDD 
                + case_when(!is.na(FACTOR1) ~ FACTOR1 * LDOPA, TRUE ~ 0.0) 
                + case_when(!is.na(FACTOR2) ~ (FACTOR2 * FACTOR3) + (LDOPA * FACTOR4), TRUE ~ 0.0)) %>%
  select (-FACTOR1, -FACTOR2, -FACTOR3, -FACTOR4)

# Display results
LEDD

# Save the LEDD DataFrame as a CSV file
write.csv(LEDD, file = "LEDD_Concomitant_Medication_Log_Calculated.csv", row.names = FALSE)
