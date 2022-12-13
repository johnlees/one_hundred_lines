library(dplyr)
library(readxl)

# Read raw input linelist
input_linelist <- read_excel("Linelist_grp4.xlsx")

# Convert to dplyr object
input_df <- dplyr::as_data_frame(input_linelist)

# ID & name: check no duplicates
# Other code would be needed to fix this on failure
stopifnot(length(input_df$id) == length(unique(input_df$id)))
stopifnot(length(input_df$name) == length(unique(input_df$name)))

# Case type: check possible values, correct factors
case_type_counts <- 
  input_df %>%
    count(case_type)
stopifnot(case_type_counts$case_type == c("confirmed","probable","suspected"))
          
# Gender: Check all 0 or 1
gender_name <- Vectorize(function(x) {
  if (x == 0) {
    "male"
  } else if (x == 1) {
    "female"
  } else {
    stop("Unknown gender encountered")
  }
})

# Replace with "male" or "female"
input_df < input_df %>%
  mutate(gender = gender_name(gender))

# Age: Check all numeric, in reasonable range, plot
input_df %>%
  filter(age > 80)

# TODO steps below here:
# Hospitalisation: convert into yes, no
# All dates converted to days into 2023 (i.e. 1st Jan is day 1)
# Outcome: either alive, dead, missing
# Death date: latest date, outcome != alive (then put both as unknown)
# Date first contact < date last contact
# Origin: rural or urban
# Exposure: animal or human
# Remove: case name, Ct_value
