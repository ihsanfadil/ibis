
# Preamble ----------------------------------------------------------------

# Code author  : Ihsan Fadilah
# Email        : ifadilah@eocru.org
# Last updated : Late September 2021
# Project      : Indonesia Brain Infection Study

# This script provides reproducible code for merging, standardising, and
# cleaning the data independently collected from two sites of the study, namely
# Jakarta and Bandung. The raw datasets are expected to be left untouched after
# downloading from the clinical databases to make sure code below and what
# follows remain reproducible throughout the project, i.e., when new
# participants are added to each of the databases.

# Adding new variables from the database will not break the code.
# Changing the names of existing variables will.

# Setup -------------------------------------------------------------------

library(tidyverse)  # Tidy code
library(readxl)     # Read Excel files
library(labelled)   # Label variables
library(here)       # Navigate files
library(janitor)    # Change variable names
library(lubridate)  # Date-time variable
library(haven)      # Save datasets in SPSS format

# Import ------------------------------------------------------------------

# File names
## Jakarta
file_jkt <- here('0-data', 'Dataset Jakarta Siap Merging') |>
  list.files(pattern = '*.xlsx') # List file names

## Bandung
file_bdg <- here('0-data', 'Dataset IBIS Bandung sd 31 Januari 2021') |>
  list.files(pattern = '*.xlsx')

file_bdg <- file_bdg[1:(length(file_bdg) - 1)] # Exclude the last file in
                                               # the folder

# Read the data in
## Jakarta
df_jkt <- NULL # Initialise

# Matching variables for Jakarta
matching_col_jkt <- c('SITEID', 'SUBJID', 'INITIAL')

for (i in (1:length(file_jkt))) {
  
  if (i == 1) {
    
    df_temp <- here('0-data',
                    'Dataset Jakarta Siap Merging',
                    file_jkt[i]) |>
      read_xlsx()
    
    df_jkt <- bind_cols(df_jkt, df_temp)
    remove(df_temp)
    
  } else {
    
    df_temp <- here('0-data',
                    'Dataset Jakarta Siap Merging',
                    file_jkt[i]) |>
      read_xlsx()
    
    df_jkt <- full_join(df_jkt, df_temp, by = matching_col_jkt)
    remove(df_temp)
    
  }

}

## Bandung
df_bdg <- NULL # Initialise
matching_col_bdg <- c('stnum')

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
    
    df_bdg <- full_join(df_bdg, df_temp, by = matching_col_bdg)
    remove(df_temp)
    
  }
  
}

# Clean the data ----------------------------------------------------------

# Rename variables of interest
df_jkt <- clean_names(df_jkt) # All to lower-case

# Manipulate data
jkt_raw <- df_jkt |>
  
  # Make variables more self-explanatory
  mutate(
    site = if_else(siteid == '054',
                   'Jakarta',
                   as.character(siteid)) |> factor(),
    siteid = if_else(siteid == '054',
                     '54',
                     as.character(siteid)) |> factor(),
    subjid = str_c(siteid, subjid)
  ) |>
  
  # Clarify different GCS
  rename(gcs = gcs_x,  # Baseline
         gcs2 = gcs_y, # Some other point
         antiigg_res = antiiggvalue, # Qualitative results
         hivstt = hivstt_y) |> # Some other point
  
  # Add non-existent variables to matcth the other site
  mutate(motordef = rep(NA, nrow(df_jkt)))

,
         antiigg_res = rep(NA, nrow(jkt_raw)),
         ratio_glucose = rep(NA, nrow(jkt_raw))) |>
  
  # Make variables uniform across sites
  rename(xray_ores = xrayoth)

bdg_raw <- df_bdg |>
  
  # Make variables more self-explanatory
  mutate(
    site = rep(x = 'Bandung', nrow(df_bdg)) |> factor(),
    siteid = rep(x = '55', nrow(df_bdg)) |> factor(),
    subjid = str_sub(subjid, -4, -1),
    subjid = str_c(siteid, subjid)
  ) |>
  
  # Add non-existent variables to match the other site
  mutate(monoparesis = rep(NA, nrow(df_bdg)),
         hivstt = rep(NA, nrow(df_bdg)),
         whcellc1 = whcellc,
         whcellc2 = rep(NA, nrow(df_bdg)),
         polycellc1 = polycellc,
         polycellc2 = rep(NA, nrow(df_bdg)),
         monocellc1 = monocellc,
         monocellc2 = rep(NA, nrow(df_bdg)),
         protein1 = protein,
         protein2 = rep(NA, nrow(df_bdg)),
         pblglucose1 = pblglucose,
         pblglucose2 = rep(NA, nrow(df_bdg)))
  
  # Make variables uniform across sites
  rename(xraynm = xray_res___0,
         xrayinf = xray_res___2,
         xraymili = xray_res___3,
         xraycav = xray_res___1,
         bihernia = rest_rad1___3,
         bienceph = rest_rad1___7)

# Select variables of interest
# When adding a variable of interest in the merged dataset, update:
# 1. `vars_of_interest`
# 2. `df_bdg_selected`, where necessary. Usually due to different classes.
#    If so, an error will pop up when merging the data

vars_of_interest <- c(
  'site', 'siteid', 'subjid',
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
    'motordef', 'hemipare', 'parapare', 'tetrapare', 'monoparesis',
  'hemovalue', 'wcellcvalue', 'platevalue',
  'hivvalue', 'cd4value', 'antiigg_res', 'hivstt',
  'whcellc1', 'whcellc2', 'polycellc1', 'polycellc2', 'monocellc1',
    'monocellc2', 'protein1', 'protein2', 'pblglucose1', 'pblglucose2'
)
  
'polycellc', 'monocellc'
  'protein', 'pblglucose', 'ratio_glucose',
  'crag', 'xpert', 'xpertrif',
  'csfcmv', 'csfhsv', 'csfebv', 'csfvzv',
    'csfvdrl', 'csftpha',
  
  'xraynm', 'xrayinf', 'xraymili', 'xraycav', 'xray_ores',
  'bihernia', 'bienceph',
  
  'paticond'
)

# Subset the datasets
df_jkt_selected <- jkt_raw |>
  select(all_of(vars_of_interest))

df_bdg_selected <- bdg_raw |>
  select(all_of(vars_of_interest)) |>
  mutate(
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
    tetrapare = as.character(tetrapare),
    hivvalue = as.character(hivvalue),
    antiigg_res = as.character(antiigg_res)
  )


         


         ,
         crag = as.character(crag),
         xpert = as.character(xpert),
         xpertrif = as.character(xpertrif),
         csfcmv = as.character(csfcmv),
         csfhsv = as.character(csfhsv),
         csfebv = as.character(csfebv),
         csfvzv = as.character(csfvzv),
         csfvdrl = as.character(csfvdrl),
         csftpha = as.character(csftpha),
         paticond = as.character(paticond))

# Merge across sites
ibis <- bind_rows(df_jkt_selected, df_bdg_selected)

# Check if there is any duplication
remaining_duplicated_rows <- table(ibis$subjid) |>
  data.frame() |>
  arrange(desc(Freq)) |>
  filter(Freq >= 2) |>
  pull(Var1)

# Look at the remaining duplicated data, if any
duplicated <- ibis |>
  arrange(subjid) |>
  filter(subjid %in% remaining_duplicated_rows)
duplicated %>% view(title = 'duplicated')

# ibis <- ibis |>
#   arrange(siteid, subjid,
#           is.na(age), is.na(sex),
#           is.na(symdays), is.na(feversym), is.na(feverday),
#           is.na(headsym), is.na(headday),
#           is.na(vomitsym), is.na(vomitday),
#           is.na(alconday),
#           is.na(bechsym), is.na(bechday),
#           is.na(seizusym), is.na(seizuonset),
#           is.na(cough),
#           is.na(htemp),
#           is.na(gcs), is.na(palsy), is.na(papille), is.na(neckstiff),
#             is.na(motordef),
#             is.na(hemipare), is.na(parapare), is.na(tetrapare),
#           is.na(hemovalue), is.na(wcellcvalue), is.na(platevalue),
#           is.na(hivvalue), is.na(cd4value),
#             is.na(antiigg_res), is.na(antiiggvalue),
#           is.na(whcellc), is.na(polycellc), is.na(monocellc),
#           is.na(protein), is.na(pblglucose), is.na(ratio_glucose),
#           is.na(crag), is.na(xpert), is.na(xpertrif),
#           is.na(csfcmv), is.na(csfhsv), is.na(csfebv), is.na(csfvzv),
#             is.na(csfvdrl), is.na(csftpha),
#           is.na(xraynm), is.na(xrayinf), is.na(xraymili), is.na(xraycav),
#             is.na(xray_ores),
#           is.na(bihernia), is.na(bienceph),
#           
#           is.na(paticond)) |>
#   distinct(subjid, .keep_all = TRUE) # Keep all except the duplicated rows that
#                                      # have been sorted so that more complete
#                                      # missingness is found later, hence,
#                                      # unselected by `distinct`

# Take a glimpse of the data
glimpse(ibis)

# Save the dataset
write_rds(x = ibis, file = here('0-data', 'ibis_merged.rds')) # R format
write_sav(x = ibis, file = here('0-data', 'ibis_merged.sav')) # SPSS format

# Appendix ----------------------------------------------------------------

sessioninfo::platform_info()

remove(list = ls())

# set.seed(13)
# sample(x = c('b', 'm'), size = 30, replace = TRUE, prob = c(0.5, 0.5))
