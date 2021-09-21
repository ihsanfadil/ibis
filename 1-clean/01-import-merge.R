
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

for (i in (1:length(file_bdg))) {
  
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


# Clean -------------------------------------------------------------------

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
  
  # Clarify different names
  rename(gcs = gcs_x,  # Baseline
         gcs2 = gcs_y, # Some other point
         antiigg_res = antiiggvalue, # Qualitative results
         hivstt = hivstt_y, # Some other point
         ratio_glucose1 = csfgluratio1,
         ratio_glucose2 = csfgluratio2,
         etiothba = etiothba_x,     # All below belong to Discharge-Death
         etiothmuco = etiothmuco_x,
         etiothbm = etiothbm_x,
         etiothneu = etiothneu_x,
         etiohtlym = etiothlym_x,
         etiohnmdar = etiothnmdar_x,
         etiothspec = etiothspec_x) |> 
  
  # Add non-existent variables to matcth the other site
  mutate(motordef = rep(NA, nrow(df_jkt))) |>
  
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
         pblglucose2 = rep(NA, nrow(df_bdg)),
         ratio_glucose1 = ratio_glucose,
         ratio_glucose2 = rep(NA, nrow(df_bdg)),
         crag1 = crag,
         crag2 = rep(NA, nrow(df_bdg)),
         xpert1 = xpert,
         xpert2 = rep(NA, nrow(df_bdg)),
         xpertrif1 = xpertrif,
         xpertrif2 = rep(NA, nrow(df_bdg)),
         csfcmv1 = csfcmv,
         csfcmv2 = rep(NA, nrow(df_bdg)),
         csfhsv1 = csfhsv,
         csfhsv2 = rep(NA, nrow(df_bdg)),
         csfebv1 = csfebv,
         csfebv2 = rep(NA, nrow(df_bdg)),
         csfvzv1 = csfvzv,
         csfvzv2 = rep(NA, nrow(df_bdg)),
         csfvdrl1 = csfvdrl,
         csfvdrl2 = rep(NA, nrow(df_bdg)),
         csftpha1 = csftpha,
         csftpha2 = rep(NA, nrow(df_bdg)),
         bihernia2 = rep(NA, nrow(df_bdg)),
         bihernia3 = rep(NA, nrow(df_bdg)),
         bienceph2 = rep(NA, nrow(df_bdg)),
         bienceph3 = rep(NA, nrow(df_bdg))) |>
  
  # Make variables uniform across sites
  rename(xraynm = xray_res___0,
         xrayinf = xray_res___2,
         xraymili = xray_res___3,
         xraycav = xray_res___1,
         bihernia1 = rest_rad1___3,
         bienceph1 = rest_rad1___7,
         staydur = disc_day,
         etiothba = etioth_spec___1,
         etiothmuco = etioth_spec___2,
         etiothbm = etioth_spec___3,
         etiothneu = etioth_spec___4,
         etiohtlym = etioth_spec___5,
         etiohnmdar = etioth_spec___6) 


# Merge -------------------------------------------------------------------

# Select variables of interest
# When adding a variable of interest in the merged dataset, update:
# 1. `vars_of_interest`
# 2. `df_bdg_selected`, where necessary. Usually due to different classes.
#    If so, an error will pop up when merging the data

vars_of_interest <- c(
  
  # Baseline
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
  
  # Blood
  'hemovalue', 'wcellcvalue', 'platevalue',
  'hivvalue', 'cd4value', 'antiigg_res',
  
  # HIV
  'hivstt',
  
  # CSF
  'whcellc1', 'whcellc2', 'polycellc1', 'polycellc2', 'monocellc1',
    'monocellc2', 'protein1', 'protein2', 'pblglucose1', 'pblglucose2',
    'ratio_glucose1', 'ratio_glucose2',
  'crag1', 'crag2', 'xpert1', 'xpert2', 'xpertrif1', 'xpertrif2',
  'csfcmv1', 'csfcmv2', 'csfhsv1', 'csfhsv2', 'csfebv1', 'csfebv2', 'csfvzv1',
    'csfvzv2', 'csfvdrl1', 'csfvdrl2', 'csftpha1', 'csftpha2',
  
  # Radiologic
  'xraynm', 'xrayinf', 'xraymili', 'xraycav', 'xray_ores',
  'bihernia1','bihernia2','bihernia3', 'bienceph1', 'bienceph2', 'bienceph3',
  
  # Discharge-Death
  'staydur', 'outcome', 'nonneuspec', 'etiomtuber', 'etmtustt', 'etitoxoenc',
    'etitoxostt', 'eticryp', 'eticrypstt', 'etibac', 'etibacstt', 'etivence',
    'etivencestt', 'etiothba', 'etiothmuco', 'etiothbm', 'etiothneu',
    'etiohtlym', 'etiohnmdar',
  
  # Study Completion
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
    antiigg_res = as.character(antiigg_res),
    crag1 = as.character(crag1),
    xpert1 = as.character(xpert1),
    xpertrif1 = as.character(xpertrif1),
    csfcmv1 = as.character(csfcmv1),
    csfhsv1 = as.character(csfhsv1),
    csfebv1 = as.character(csfebv1),
    csfvzv1 = as.character(csfvzv1),
    csfvdrl1 = as.character(csfvdrl1),
    csftpha1 = as.character(csftpha1),
    outcome = as.character(outcome),
    etmtustt = as.character(etmtustt),
    etitoxostt = as.character(etitoxostt),
    eticrypstt = as.character(eticrypstt),
    etibacstt = as.character(etibacstt),
    etivencestt = as.character(etivencestt),
    paticond = as.character(paticond)
  )

# Merge across sites
ibis <- bind_rows(df_jkt_selected, df_bdg_selected)
# names(ibis)

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

if (dim(duplicated)[1] == 0) {
  print('You have no duplicated rows.')
} else {
  print('Duplicated rows found.')
}

# Take a glimpse of the merged data
glimpse(ibis)

# Save the dataset
ibis |> write_rds(here('0-data', 'ibis_merged.rds')) # R format
ibis |> write_sav(here('0-data', 'ibis_merged.sav')) # SPSS format

# Appendix ----------------------------------------------------------------

sessioninfo::platform_info()
