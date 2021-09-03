
# Preamble ----------------------------------------------------------------

# Code author  : Ihsan Fadilah
# Email        : ifadilah@eocru.org
# Last updated : Early September 2021
# Project      : Indonesia Brain Infection Study

# This script provides reproducible code for merging, standardising, and
# cleaning the data independently collected from two sites of the study, namely
# Jakarta and Bandung. The raw datasets are expected to be left untouched after
# downloading from the clinical databases to make sure code below and what
# follows remain reproducible throughout the project, i.e., when new
# participants are added to each of the databases.

# Setup -------------------------------------------------------------------

library(tidyverse)  # Tidy code
library(readxl)     # Read Excel files
library(labelled)   # Label variables
library(here)       # Navigate files
library(janitor)    # Change variable names
library(lubridate)  # Date-time variable

# Import ------------------------------------------------------------------

# File names
## Jakarta
file_jkt <- here('0-data', 'DATABASE IBIS JAKARTA') |>
  list.files(pattern = '*.xlsx') # List file names

## Bandung
file_bdg <- here('0-data', 'Dataset IBIS Bandung sd 31 Januari 2021') |>
  list.files(pattern = '*.xlsx')

# Read the data in
## Jakarta
df_jkt <- NULL # Initialise
matching_col_full <- c('SUBJID', 'EVENT', 'STUDYID', 'SITEID', 'INITIAL')
matching_col_thorax <- c('SUBJID', 'EVENT', 'STUDYID', 'INITIAL')
matching_col_brain <- c('SUBJID', 'EVENT', 'SITEID')

for (i in (1:(length(file_jkt) - 1))) {
  
  if (i == 1) {
    
    df_temp <- here('0-data',
                    'DATABASE IBIS JAKARTA',
                    file_jkt[i]) |>
      read_xlsx()
    
    df_jkt <- bind_cols(df_jkt, df_temp)
    remove(df_temp)
    
  } else if (file_jkt[i] == '8.2 Brain Imaging.xlsx') {
  
    df_temp <- here('0-data',
                    'DATABASE IBIS JAKARTA',
                    file_jkt[i]) |>
      read_xlsx()
    
    df_temp <- df_temp |>
      mutate(SUBJID = str_sub(USUBJID, 5, -1), # Extract 'SUBJID'
             SITEID = str_sub(USUBJID, 1, 3)) |> # Extract 'SITEID'
      select(-(USUBJID))
    
    df_jkt <- full_join(df_jkt, df_temp, by = matching_col_brain)
    remove(df_temp)
  
  } else if (file_jkt[i] == '8.1 Thorax Imaging.xlsx') {
    
    df_temp <- here('0-data',
                    'DATABASE IBIS JAKARTA',
                    file_jkt[i]) |>
      read_xlsx()
    
    df_jkt <- full_join(df_jkt, df_temp, by = matching_col_thorax)
    remove(df_temp)
    
  } 
  
  else {
    
    df_temp <- here('0-data',
                    'DATABASE IBIS JAKARTA',
                    file_jkt[i]) |>
      read_xlsx()
    
    df_jkt <- full_join(df_jkt, df_temp, by = matching_col_full)
    remove(df_temp)
    
  }

}

## Bandung
df_bdg <- NULL # Initialise
matching_col <- c('stnum')

for (i in (1:(length(file_bdg) - 1))) {
  
  if (i == 1) {
    
    df_temp <- here('0-data',
                    'Dataset IBIS Bandung sd 31 Januari 2021',
                    file_bdg[i]) |>
      read_xlsx()
    
    df_bdg <- bind_cols(df_bdg, df_temp)
    remove(df_temp)
    
  } else {
    
    df_temp <- here('0-data',
                    'Dataset IBIS Bandung sd 31 Januari 2021',
                    file_bdg[i]) |>
      read_xlsx()
    
    df_bdg <- full_join(df_bdg, df_temp, by = matching_col)
    remove(df_temp)
    
  }
  
}

# Clean the data ----------------------------------------------------------

# Rename variables of interest
df_jkt <- clean_names(df_jkt) # All to lower-case

# Manipulate data
df_jkt <- df_jkt |>
  
  # Change categories to be more self-explanatory
  mutate(siteid = if_else(siteid == '054', 'Jakarta', as.character(siteid)),
         siteid = if_else(is.na(siteid), 'Jakarta', as.character(siteid)),
         site_num = rep('054', nrow(df_jkt)),
         subjid = str_c(site_num, subjid)) |>
  select(-c(site_num)) |>

  # Create date of birth
  mutate(dob_new = str_c(yob, mob, dob, sep = '/'),
         dob_new = ymd(dob_new)) |>
  
  # Clarify different GCS
  rename(gcs = gcs_x,
         gcs2 = gcs_y) |>
  
  # Add non-existent variables to matcth the other site
  mutate(motordef = rep(NA, nrow(df_jkt)))

# Add `siteid` to `df_bdg`
df_bdg <- df_bdg |>
  mutate(siteid = rep('Bandung', nrow(df_bdg)))

# Select variables of interest
# When adding a variable of interest in the merged dataset, update:
# 1. `vars_of_interest`
# 2. `df_bdg_selected`, where necessary. Usually due to different classes.
#    If so, an error will pop up when merging the data
# 3. Check if reasonable to remove the duplicated rows
# 4. Remove the duplicated rows, considering the new variable of interest

vars_of_interest <- c(
  'siteid', 'subjid',
  'age', 'sex',
  'symdays', 'feversym', 'feverday',
  'headsym', 'headday',
  'vomitsym', 'vomitday',
  'alconday',
  'bechsym', 'bechday',
  'seizusym', 'seizuonset',
  'cough',
  'htemp',
  'gcs', 'palsy', 'papille', 'neckstiff',
    'motordef', 'hemipare', 'parapare', 'tetrapare'
)

# Subset the datasets
df_jkt_selected <- df_jkt |>
  select(all_of(vars_of_interest))

df_bdg_selected <- df_bdg |>
  select(all_of(vars_of_interest)) |>
  mutate(subjid = as.character(subjid),
         sex = as.character(sex),
         feversym = as.character(feversym),
         headsym = as.character(headsym),
         vomitsym = as.character(vomitsym),
         bechsym = as.character(bechsym),
         seizusym = as.character(seizusym),
         cough = as.character(cough),
         palsy = as.character(palsy),
         papille = as.character(papille),
         neckstiff = as.character(neckstiff),
         hemipare = as.character(hemipare),
         parapare = as.character(parapare),
         tetrapare = as.character(tetrapare))

# Merge across sites
ibis <- bind_rows(df_jkt_selected, df_bdg_selected) |>
  unique() # Deduplicate rows with respect to the selected variables

remaining_duplicated_rows <- table(ibis$subjid) |>
  data.frame() |>
  arrange(desc(Freq)) |>
  filter(Freq >= 2) |>
  pull(Var1)

# Look at the remaining duplicated data
ibis |>
  arrange(subjid) |>
  filter(subjid %in% remaining_duplicated_rows) |>
  View()

# Note that all these came from Jakarta, possibly due to one of the
# sheets having duplicated rows.

ibis <- ibis |>
  arrange(siteid, subjid,
          is.na(age), is.na(sex),
          is.na(symdays), is.na(feversym), is.na(feverday),
          is.na(headsym), is.na(headday),
          is.na(vomitsym), is.na(vomitday),
          is.na(alconday),
          is.na(bechsym), is.na(bechday),
          is.na(seizusym), is.na(seizuonset),
          is.na(cough),
          is.na(htemp),
          is.na(gcs), is.na(palsy), is.na(papille), is.na(neckstiff),
            is.na(motordef),
            is.na(hemipare), is.na(parapare), is.na(tetrapare)) |>
  distinct(subjid, .keep_all = TRUE) # Keep all except the duplicated rows that
                                     # have been sorted so that more complete
                                     # missingness is found later, hence,
                                     # unselected by `distinct`

# remove(list = ls())

# Save the dataset
write_rds(x = ibis, file = here('0-data', 'ibis_merged_20210903.rds'))

# Appendix ----------------------------------------------------------------


