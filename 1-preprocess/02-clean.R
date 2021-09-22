
# Preamble ----------------------------------------------------------------

# Code author  : Ihsan Fadilah
# Email        : ifadilah@eocru.org
# Last updated : Late September 2021
# Project      : Indonesia Brain Infection Study

# This script provides reproducible code for cleaning the data. It does not have
# the dependency inherent in the script file `01-import-merge.R`. However, this
# code depends on the assumption that the format of data collection remains 
# the same throughout the project. For instance, `feversym` at the Jakarta site
# uses '9' to represent missingness and no new categories of variables are
# introduced to this dataset afterwards. If so, these new categories would be
# mistakenly read as missing data.

# Setup -------------------------------------------------------------------

library(tidyverse)  # Tidy code
library(labelled)   # Label variables
library(here)       # Navigate files
library(lubridate)  # Date-time variable
library(haven)      # Write datasets in SPSS format

# Load --------------------------------------------------------------------

ibis_raw <- here('0-data', 'ibis_merged.rds') |> read_rds()
glimpse(ibis_raw) # Take a look at the raw data (after merging)

# Clean -------------------------------------------------------------------

ibis <- ibis_raw |>

  # Redefine categories or values
  mutate(
    sex = case_when(site == 'Jakarta' & sex == 'F' ~ 'Female',
                    site == 'Jakarta' & sex == 'M' ~ 'Male',
                    site == 'Bandung' & sex == '0' ~ 'Female',
                    site == 'Bandung' & sex == '1' ~ 'Male',
                    TRUE ~ NA_character_),
    symdays = if_else(is.na(symdays) | symdays == 99999, NA_real_, symdays),
    feversym = case_when(feversym == '1' ~ TRUE,
                         feversym == '0' ~ FALSE,
                         site == 'Jakarta' & feversym == '9' ~ NA,
                         site == 'Bandung' & feversym == '8' ~ NA,
                         TRUE ~ NA)
  ) |>
  
  # Change the class
  mutate(
    sex = as_factor(sex)
  )
  
  # Order variable appearance in the dataset
  # ...
  
# Label variables
var_label(ibis) <- list(
  site = 'Study site',
  siteid = 'Study site ID',
  subjid = 'Unique patient ID',
  age = 'Age (years)',
  sex = 'Biological sex',
  symdays = 'Duration of symptoms (days)',
  feversym = 'Fever'
)

# Take a look at the cleaned dataset
glimpse(ibis)

# Save --------------------------------------------------------------------

# R
# SPSS

# Appendix ----------------------------------------------------------------

sessioninfo::platform_info()
