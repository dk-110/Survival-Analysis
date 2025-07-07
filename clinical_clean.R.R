library(readr)
library(dplyr)
library(stringr)
library(forcats)

# Load raw data
clinical <- read_tsv("clinical.tsv")

# Clean the data
clinical_clean <- clinical %>%
  mutate(across(where(is.factor), as.character)) %>%
  mutate(across(where(is.character), ~ na_if(str_replace_all(.x, "[`'\"]", "") %>% trimws(), "--"))) %>%
  mutate(treatments.treatment_or_therapy = case_when(
    treatments.treatment_or_therapy %in% c("yes", "Yes", "TRUE", TRUE) ~ TRUE,
    treatments.treatment_or_therapy %in% c("no", "No", "FALSE", FALSE) ~ FALSE,
    TRUE ~ NA
  )) %>%
  select(-matches("\\.\\.\\.")) %>%
  select(where(~ n_distinct(., na.rm = TRUE) > 1)) %>%
  distinct() %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(
    survival_time = ifelse(
      as.character(demographic.vital_status) == "Dead",
      as.numeric(as.character(demographic.days_to_death)),
      as.numeric(diagnoses.days_to_last_follow_up)
    ),
    event = ifelse(as.character(demographic.vital_status) == "Dead", 1, 0),
    survival_years = survival_time / 365,
    treatments.treatment_outcome = fct_explicit_na(treatments.treatment_outcome, na_level = "Not Reported")
  )

# Save the cleaned data
saveRDS(clinical_clean, file = "clinical_clean.rds")