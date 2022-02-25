
# Preamble ----------------------------------------------------------------

# Code author  : Ihsan Fadilah
# Email        : ifadilah@eocru.org
# Last updated : Early January 2022
# Project      : Indonesia Brain Infection Study

# This script provides reproducible code for merging and harmonising the data
# independently collected from two sites of the study, namely Jakarta and
# Bandung. The raw datasets are expected to be left untouched after
# downloading from the clinical databases to make sure code below and what
# follows remain reproducible throughout the project, i.e., when new
# participants are added to each of the databases.

# Adding new variables from the database will not break the code.
# Changing the names of existing variables will.

# Assume that the file 'Variable List IBIS bdg-jkt.xlsx', created on
# 12 September 2021 at 9:31 am is valid. The ordering of the variables was
# deemed invalid and I considered matching the identically named variables
# as valid.

# Setup -------------------------------------------------------------------

library(tidyverse)  # Tidy code
library(readxl)     # Read Excel files
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
         etiothba = etiothba_x, # All below belong to Discharge-Death
         etiothmuco = etiothmuco_x,
         etiothbm = etiothbm_x,
         etiothneu = etiothneu_x,
         etiohtlym = etiothlym_x,
         etiohnmdar = etiothnmdar_x,
         etioothspec = etiothspec_x,
         eyesc = eyesc_x,
         motoric = motoric_x,
         verbal = verbal_x,
         veruni = veruni_x,
         verunirs = verunirs_x,
         acyclodtc = acyclodtc_x,
         art = art_x,
         artspec = artspec_x,
         artdtc = artdtc_x,
         hivdtc = hivdtc_x,
         cd4dtc = cd4dtc_x,
         artdtc2 = artdtc_y,
         cd4dtc2 = cd4dtc_y,
         hivdtc2 = hivdtc_y,
         csfsample1 = csf_sample1,
         csfsample2 = csf_sample2,
         other_specify1 = bioth1,
         other_specify2 = bioth2,
         other_specify3 = bioth3,
         tbmgrade = tbmgrade_x,
         etioth = etioth_x,
         eyesc2 = eyesc_y,
         motoric2 = motoric_y,
         verbal2 = verbal_y,
         veruni2 = veruni_y,
         verunirs2 = verunirs_y,
         pyrimedtc2 = pyrimedtc_y,
         pyrimestt2 = pyrimestt_y,
         flucodtc2 = flucodtc_y,
         flucostt2 = flucostt_y,
         amphodtc2 = amphodtc_y,
         amphostt2 = amphostt_y,
         ceftriadtc2 = ceftriadtc_y,
         ceftriastt2 = ceftriastt_y,
         acyclodtc2 = acyclodtc_y,
         acyclostt2 = acyclostt_y,
         valgandtc2 = valgandtc_y,
         valganstt2 = valganstt_y,
         methyldtc2 = methyldtc_y,
         methylstt2 = methylstt_y,
         furosedtc2 = furosedtc_y,
         furosestt2 = furosestt_y,
         xray_s = xray_y,
         art_sc = art_y,
         artspec_sc = artspec_y,
         neurocase_sc = neurocase,
         tbmedi_sc = tbmedi,
         mtuber_sc = mtuber,
         tbmgrade_sc = tbmgrade_y,
         pararea_sc = pararea,
         etiothba_sc = etiothba_y,
         etiothmuco_sc = etiothmuco_y,
         etiothbm_sc = etiothbm_y,
         etiothneu_sc = etiothneu_y,
         etiothlym_sc = etiothlym_y,
         etiothnmdar_sc = etiothnmdar_y,
         etioth_sc = etioth_y,
         etiothspec_sc = etiothspec_y,
         deathdtc_sc = deathdtc_y,
         tbdrug_sc = tbdrug_y
         ) |> 
  
  # Add non-existent variables to match the other site
  mutate(motordef = rep(NA, nrow(df_jkt)),
         oth3text = rep(NA, nrow(df_jkt)),
         oth3 = rep(NA, nrow(df_jkt)),
         oth3dtc = rep(NA, nrow(df_jkt)),
         oth3stt = rep(NA, nrow(df_jkt)),
  
         oth4text = rep(NA, nrow(df_jkt)),
         oth4 = rep(NA, nrow(df_jkt)),
         oth4dtc = rep(NA, nrow(df_jkt)),
         oth4stt = rep(NA, nrow(df_jkt)),
  
         oth5text = rep(NA, nrow(df_jkt)),
         oth5 = rep(NA, nrow(df_jkt)),
         oth5dtc = rep(NA, nrow(df_jkt)),
         oth5stt = rep(NA, nrow(df_jkt)),
         
         oth6text = rep(NA, nrow(df_jkt)),
         oth6 = rep(NA, nrow(df_jkt)),
         oth6dtc = rep(NA, nrow(df_jkt)),
         oth6stt = rep(NA, nrow(df_jkt)),
         
         oth7text = rep(NA, nrow(df_jkt)),
         oth7 = rep(NA, nrow(df_jkt)),
         oth7dtc = rep(NA, nrow(df_jkt)),
         oth7stt = rep(NA, nrow(df_jkt)),
         
         oth8text = rep(NA, nrow(df_jkt)),
         oth8 = rep(NA, nrow(df_jkt)),
         oth8dtc = rep(NA, nrow(df_jkt)),
         oth8stt = rep(NA, nrow(df_jkt)),
         
         oth9text = rep(NA, nrow(df_jkt)),
         oth9 = rep(NA, nrow(df_jkt)),
         oth9dtc = rep(NA, nrow(df_jkt)),
         oth9stt = rep(NA, nrow(df_jkt)),
         
         oth10text = rep(NA, nrow(df_jkt)),
         oth10 = rep(NA, nrow(df_jkt)),
         oth10dtc = rep(NA, nrow(df_jkt)),
         oth10stt = rep(NA, nrow(df_jkt)),
         
         oth11text = rep(NA, nrow(df_jkt)),
         oth11 = rep(NA, nrow(df_jkt)),
         oth11dtc = rep(NA, nrow(df_jkt)),
         oth11stt = rep(NA, nrow(df_jkt)),
         
         oth12text = rep(NA, nrow(df_jkt)),
         oth12 = rep(NA, nrow(df_jkt)),
         oth12dtc = rep(NA, nrow(df_jkt)),
         oth12stt = rep(NA, nrow(df_jkt)),
         
         oth13text = rep(NA, nrow(df_jkt)),
         oth13 = rep(NA, nrow(df_jkt)),
         oth13dtc = rep(NA, nrow(df_jkt)),
         oth13stt = rep(NA, nrow(df_jkt)),
         
         oth14text = rep(NA, nrow(df_jkt)),
         oth14 = rep(NA, nrow(df_jkt)),
         oth14dtc = rep(NA, nrow(df_jkt)),
         oth14stt = rep(NA, nrow(df_jkt)),
         
         oth15text = rep(NA, nrow(df_jkt)),
         oth15 = rep(NA, nrow(df_jkt)),
         oth15dtc = rep(NA, nrow(df_jkt)),
         oth15stt = rep(NA, nrow(df_jkt)),
         
         rifprobe_a = rep(NA, nrow(df_jkt)),
         rifprobe_b = rep(NA, nrow(df_jkt)),
         rifprobe_c = rep(NA, nrow(df_jkt)),
         rifprobe_d = rep(NA, nrow(df_jkt)),
         rifprobe_e = rep(NA, nrow(df_jkt)),
         ctval_a = rep(NA, nrow(df_jkt)),
         ctval_b = rep(NA, nrow(df_jkt)),
         ctval_c = rep(NA, nrow(df_jkt)),
         ctval_d = rep(NA, nrow(df_jkt)),
         ctval_e = rep(NA, nrow(df_jkt)),
         
         resmrin0 = rep(NA, nrow(df_jkt)),
         resmrin1 = rep(NA, nrow(df_jkt)),
         resmrin2 = rep(NA, nrow(df_jkt)),
         resmrin3 = rep(NA, nrow(df_jkt)),
         resmrin4 = rep(NA, nrow(df_jkt)),
         other1 = rep(NA, nrow(df_jkt)),
         other2 = rep(NA, nrow(df_jkt)),
         other3 = rep(NA, nrow(df_jkt)),
         xraynd = rep(NA, nrow(df_jkt)),
         xray_ores = rep(NA, nrow(df_jkt)), # Different from Excel!
         
         treatoth4 = rep(NA, nrow(df_jkt)),
         treatoth4dtc = rep(NA, nrow(df_jkt)),
         treatoth4stt = rep(NA, nrow(df_jkt)),
         treatoth4rs = rep(NA, nrow(df_jkt)),
         treatoth4spec = rep(NA, nrow(df_jkt)),
         
         treatoth5 = rep(NA, nrow(df_jkt)),
         treatoth5dtc = rep(NA, nrow(df_jkt)),
         treatoth5stt = rep(NA, nrow(df_jkt)),
         treatoth5rs = rep(NA, nrow(df_jkt)),
         treatoth5spec = rep(NA, nrow(df_jkt)),
         
         treatoth6 = rep(NA, nrow(df_jkt)),
         treatoth6dtc = rep(NA, nrow(df_jkt)),
         treatoth6stt = rep(NA, nrow(df_jkt)),
         treatoth6rs = rep(NA, nrow(df_jkt)),
         treatoth6spec = rep(NA, nrow(df_jkt)),
         
         treatoth7 = rep(NA, nrow(df_jkt)),
         treatoth7dtc = rep(NA, nrow(df_jkt)),
         treatoth7stt = rep(NA, nrow(df_jkt)),
         treatoth7rs = rep(NA, nrow(df_jkt)),
         treatoth7spec = rep(NA, nrow(df_jkt)),
         
         treatoth8 = rep(NA, nrow(df_jkt)),
         treatoth8dtc = rep(NA, nrow(df_jkt)),
         treatoth8stt = rep(NA, nrow(df_jkt)),
         treatoth8rs = rep(NA, nrow(df_jkt)),
         treatoth8spec = rep(NA, nrow(df_jkt)),
         
         treatoth9 = rep(NA, nrow(df_jkt)),
         treatoth9dtc = rep(NA, nrow(df_jkt)),
         treatoth9stt = rep(NA, nrow(df_jkt)),
         treatoth9rs = rep(NA, nrow(df_jkt)),
         treatoth9spec = rep(NA, nrow(df_jkt)),
         
         treatoth10 = rep(NA, nrow(df_jkt)),
         treatoth10dtc = rep(NA, nrow(df_jkt)),
         treatoth10stt = rep(NA, nrow(df_jkt)),
         treatoth10rs = rep(NA, nrow(df_jkt)),
         treatoth10spec = rep(NA, nrow(df_jkt)),
         
         treatoth11 = rep(NA, nrow(df_jkt)),
         treatoth11dtc = rep(NA, nrow(df_jkt)),
         treatoth11stt = rep(NA, nrow(df_jkt)),
         treatoth11rs = rep(NA, nrow(df_jkt)),
         treatoth11spec = rep(NA, nrow(df_jkt)),
         
         treatoth12 = rep(NA, nrow(df_jkt)),
         treatoth12dtc = rep(NA, nrow(df_jkt)),
         treatoth12stt = rep(NA, nrow(df_jkt)),
         treatoth12rs = rep(NA, nrow(df_jkt)),
         treatoth12spec = rep(NA, nrow(df_jkt)),
         
         treatoth13 = rep(NA, nrow(df_jkt)),
         treatoth13dtc = rep(NA, nrow(df_jkt)),
         treatoth13stt = rep(NA, nrow(df_jkt)),
         treatoth13rs = rep(NA, nrow(df_jkt)),
         treatoth13spec = rep(NA, nrow(df_jkt)),
         
         treatoth14 = rep(NA, nrow(df_jkt)),
         treatoth14dtc = rep(NA, nrow(df_jkt)),
         treatoth14stt = rep(NA, nrow(df_jkt)),
         treatoth14rs = rep(NA, nrow(df_jkt)),
         treatoth14spec = rep(NA, nrow(df_jkt)),
         
         treatoth15 = rep(NA, nrow(df_jkt)),
         treatoth15dtc = rep(NA, nrow(df_jkt)),
         treatoth15stt = rep(NA, nrow(df_jkt)),
         treatoth15rs = rep(NA, nrow(df_jkt)),
         treatoth15spec = rep(NA, nrow(df_jkt)),
         
         lvous = rep(NA, nrow(df_jkt)),
         death_day_sc = rep(NA, nrow(df_jkt))
         ) |> 
  
  # Make variables uniform across sites
  rename(monopare = monoparesis,
         suetiunk = syetiunk,
         xray = xray_x,
         tbdrug = tbdrug_x,
         pyrimedtc = pyrimedtc_x,
         pyrimestt = pyrimestt_x,
         flucodtc = flucodtc_x,
         flucostt = flucostt_x,
         amphodtc = amphodtc_x,
         amphostt = amphostt_x,
         ceftriadtc = ceftriadtc_x,
         ceftriastt = ceftriastt_x,
         acyclostt = acyclostt_x,
         valgandtc = valgandtc_x,
         valganstt = valganstt_x,
         methyldtc = methyldtc_x,
         methylstt = methylstt_x,
         furosedtc = furosedtc_x,
         furosestt = furosestt_x,
         deathdtc = deathdtc_x)

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
         lumpdur = rep(NA, nrow(df_bdg)),
         lumpdurhour = rep(NA, nrow(df_bdg)),
         mob = rep(NA, nrow(df_bdg)),
         yob = rep(NA, nrow(df_bdg)),
         covid19 = rep(NA, nrow(df_bdg)),
         covid19vac = rep(NA, nrow(df_bdg)),
         phyexamoth = rep(NA, nrow(df_bdg)),
         veruni = rep(NA, nrow(df_bdg)),
         verunirs = rep(NA, nrow(df_bdg)),
         fujilamdtc = rep(NA, nrow(df_bdg)),
         fujilamvalue = rep(NA, nrow(df_bdg)),
         aleredtc = rep(NA, nrow(df_bdg)),
         alerevalue = rep(NA, nrow(df_bdg)),
         sequence = rep(NA, nrow(df_bdg)),
         csfsample1 = csfsample,
         csfsample2 = rep(NA, nrow(df_bdg)),
         sampledtc1 = sampledtc,
         sampledtc2 = rep(NA, nrow(df_bdg)),
         sampletime1 = sampletime,
         sampletime2 = rep(NA, nrow(df_bdg)),
         sptype1 = sptype,
         sptype2 = rep(NA, nrow(df_bdg)),
         oppressure1 = oppressure,
         oppressure2 = rep(NA, nrow(df_bdg)),
         oppressunm1 = oppressunm,
         oppressunm2 = rep(NA, nrow(df_bdg)),
         appearance1 = appearance,
         appearance2 = rep(NA, nrow(df_bdg)),
         csf1 = csf,
         csf2 = rep(NA, nrow(df_bdg)),
         csfglucose1 = csfglucose,
         csfglucose2 = rep(NA, nrow(df_bdg)),
         gramstain1 = gramstain,
         gramstain2 = rep(NA, nrow(df_bdg)),
         znsmear1 = znsmear,
         znsmear2 = rep(NA, nrow(df_bdg)),
         ininksmear1 = ininksmear,
         ininksmear2 = rep(NA, nrow(df_bdg)),
         baccult1 = baccult,
         baccult2 = rep(NA, nrow(df_bdg)),
         pathongen1 = pathongen,
         pathongen2 = rep(NA, nrow(df_bdg)),
         crypcult1 = crypcult,
         crypcult2 = rep(NA, nrow(df_bdg)),
         mycocult1 = mycocult,
         mycocult2 = rep(NA, nrow(df_bdg)),
         csfnmdar1 = csfnmdar,
         csfnmdar2 = rep(NA, nrow(df_bdg)),
         hivviload1 = hivviload,
         hivviload2 = rep(NA, nrow(df_bdg)),
         hivviloadnd1 = hivviloadnd,
         hivviloadnd2 = rep(NA, nrow(df_bdg)),
         pcrtb1 = pcrtb,
         pcrtb2 = rep(NA, nrow(df_bdg)),
         gene1 = rep(NA, nrow(df_bdg)),
         gene2 = rep(NA, nrow(df_bdg)),
         pcrcov21 = rep(NA, nrow(df_bdg)),
         pcrcov22 = rep(NA, nrow(df_bdg)),
         cffoth1 = rep(NA, nrow(df_bdg)),
         cffoth2 = rep(NA, nrow(df_bdg)),
         mrinm = rep(NA, nrow(df_bdg)),
         mriabscess = rep(NA, nrow(df_bdg)),
         mrimyelitis = rep(NA, nrow(df_bdg)),
         mrispcol = rep(NA, nrow(df_bdg)),
         etiprctb = rep(NA, nrow(df_bdg)),
         veruni2 = rep(NA, nrow(df_bdg)),
         verunirs2 = rep(NA, nrow(df_bdg))
         ) |>
  
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
         etiohnmdar = etioth_spec___6,
         etioothspec = etiothspec,
         monopare = monoparesis,
         tbsttpul = type_tb___1,
         tbsttmen = type_tb___2,
         tbsttoth = type_tb___3,
         cnsmen = cnsmen___1,
         cnsenc = cnsmen___2,
         cnsbra = cnsmen___3,
         cnsmye = cnsmen___4,
         cnsmtub = cnsm_sp___1,
         cnstoxo = cnsm_sp___2,
         cnscryp = cnsm_sp___3,
         cnsoth = cnsm_oth,
         inidiamen = inidiamen___1,
         inidiaence = inidiamen___2,
         inidiamye = inidiamen___3,
         inidiabrain = inidiamen___4,
         inidiaenc = inidiamen___5,
         suetibac = suetibac___1,
         suetiviral = suetibac___2,
         suetitoxo = suetibac___3,
         suetiauto = suetibac___4,
         sueticryp = suetibac___5,
         suetimtube = suetibac___6,
         suetiunk = suetibac___7,
         xrayoth = xray_res___4,
         binormal1 = rest_rad1___0,
         biinfarct1 = rest_rad1___1,
         bituber1 = rest_rad1___2,
         bihydro1 = rest_rad1___4,
         biabscess1 = rest_rad1___5,
         bimenin1 = rest_rad1___6, 
         other1 = rest_rad1___9,
         other_specify1 = rest_rad1_ot,
         binormal2 = rest_rad2___0,
         biinfarct2 = rest_rad2___1,
         bituber2 = rest_rad2___2,
         bihernia2 = rest_rad2___3,
         bihydro2 = rest_rad2___4,
         biabscess2 = rest_rad2___5,
         bimenin2 = rest_rad2___6,
         bienceph2 = rest_rad2___7,
         other2 = rest_rad2___9,
         other_specify2 = rest_rad2_ot,
         binormal3 = rest_rad3___0,
         biinfarct3 = rest_rad3___1,
         bituber3 = rest_rad3___2,
         bihernia3 = rest_rad3___3,
         bihydro3 = rest_rad3___4,
         biabscess3 = rest_rad3___5,
         bimenin3 = rest_rad3___6,
         bienceph3 = rest_rad3___7,
         other3 = rest_rad3___9,
         other_specify3 = rest_rad3_ot,
         resmrin0 = resmrin___0,
         resmrin1 = resmrin___1,
         resmrin2 = resmrin___2,
         resmrin3 = resmrin___3,
         resmrin4 = resmrin___4,
         syndmen = syndiag___1,
         syndenc = syndiag___2,
         syndba = syndiag___3,
         syndmye = syndiag___4,
         syndence = syndiag___5,
         syndnonneu = syndiag___0) 

# Merge -------------------------------------------------------------------

# Select variables of interest
# When adding a variable of interest in the merged dataset, update:
# 1. `vars_of_interest`
# 2. `df_bdg_selected`, where necessary. Usually due to different classes.
#    If so, an error will pop up when merging the data

vars_of_interest <- c(
  
  # Screening (extended)
  'initial', 'iscnsinfec', 'is18', 'issigned', 'admisdtc', 'admistime',
  'neuroinfecdtc', 'neuroinfectime', 'fstdosend', 'fstdosedtc', 'fstdosetime',
  'lumpuncnd', 'lumpuncdtc', 'lunpunctime', 'lumpdur', 'lumpdurhour',
  'enrolldtc', 'enrolltime', 'isrecrbf', 'patientid1', 'patientid2',
  'patientid3', 'patientid4',
  
  # Baseline (main)
  'site', 'siteid', 'subjid',
  'age', 'sex',
  'symdays', 'feversym', 'feverday',
  'headsym', 'headday',
  'vomitsym', 'vomitday',
  'alconsym', 'alconday',
  'bechsym', 'bechday',
  'seizusym', 'seizuonset',
  'cough',
  'htemp',
  'gcs', 'palsy', 'papille', 'neckstiff',
    'motordef', 'hemipare', 'parapare', 'tetrapare', 'monopare',
  
  # Baseline (extended)
  # `initial` in Baseline not considered
  # 'hivstt' in Baseline not considered
  'dob', 'mob', 'yob', 'studypl', 'refstt', 'refplace', 'fstsymdtc',
  'fevercomp', 'headcomp', 'vomitcomp', 'alconcomp', 'lethsym', 'lethcomp',
  'lethday', 'bechcomp', 'moabsym', 'moabcomp', 'moabday', 'seabsym',
  'seabcomp', 'seabday', 'cranpsym', 'cranpcomp', 'cranpday', 'seizucomp',
  'seipattern', 'seiwitms', 'seizufre', 'othsym',
  'othcomp', 'othday', 'weightlost',
  'nsweat', 'bcg', 'treatedtb', 'timettb', 'pretb', 'tbfre', 'tbrecent',
  'tbsttrec', 'tbsttpul', 'tbsttmen', 'tbsttoth', 'tblatent', 'tbipt', 'precns',
  'cnsfre', 'cnsmen', 'cnsenc', 'cnsbra', 'cnsmye', 'cnsmtub', 'cnstoxo',
  'cnscryp', 'cnsoth', 'cnsepi', 'diabetes', 'diatherapy', 'corticocurt',
  'corticodur', 'piein30', 'piein30spec', 'pieanimal', 'pieanispec',
  'piefreshwt', 'piefrwtspec', 'pieplace', 'hissmoking', 'covid19',
  'covid19vac', 'loctemp', 'pulse', 'sbp', 'dbp', 'respiratory', 'oxysatu',
  'weight', 'weightmth', 'armcir', 'armcirmea', 'height', 'heightmth',
  'phyexamoth', 'eyesc', 'motoric', 'verbal', 'veruni', 'verunirs', 'nerve3rd',
  'nerve4th', 'nerve6th', 'nerve7th', 'nerve12th', 'pupisz', 'pupiref',
  'urinary', 'gasbleed',
  
  # Initial diagnosis (extended)
  # Here, `initial`, `siteid`, `subjid` not considered
  'inidiamen', 'inidiaence', 'inidiamye', 'inidiabrain', 'inidiaenc',
  'inidiaspec', 'suetibac', 'suetiviral', 'suetitoxo', 'suetitoxo', 'suetiauto',
  'sueticryp', 'suetimtube', 'suetiunk', 'suetioth', 'gradespec', 'cnsinfec',
  'othonset', 'onsetstt', 'evimales', 'csfabnor', 'hivresult', 'extraclues',
  'extracluesspec', 'xray', 'ranscale',
  
  # Initial treatment (extended)
  # Here, `initial`, `siteid`, `subjid` not considered
  'tbdrug', 'tbdrugspec', 'tbdrugdtc', 'tbdrugstt', 'levof', 'levofdtc',
  'levofstt', 'moxif', 'moxifdtc', 'moxifstt', 'cotrimo', 'cotrimodtc',
  'cotrimostt', 'metroni', 'metronidtc', 'metronistt', 'pyrime', 'pyrimedtc',
  'pyrimestt', 'clinda', 'clindastt', 'clindadtc', 'fluco', 'flucodtc',
  'flucostt', 'ampho', 'amphodtc', 'amphostt', 'ceftria', 'ceftriadtc',
  'ceftriastt', 'merope', 'meropedtc', 'meropestt', 'acyclo', 'acyclodtc',
  'acyclostt', 'valgan', 'valgandtc', 'valganstt', 'dexame', 'dexamedtc',
  'dexamestt', 'methyl', 'methyldtc', 'methylstt', 'mannitol', 'mannitoldtc',
  'mannitolstt', 'furose', 'furosedtc', 'furosestt', 'valproic', 'valproicdtc',
  'valproicstt', 'topira', 'topiradtc', 'topirastt', 'pheny', 'phenydtc',
  'phenystt', 'levitira', 'levitiradtc', 'levitirastt', 'ranppi', 'ranppidtc',
  'ranppistt', 'art', 'artspec', 'artdtc', 'artstt', 'oth1text', 'oth1',
  'oth1dtc', 'oth1stt', 'oth2text', 'oth2', 'oth2dtc', 'oth2stt',
  
  'oth3text', 'oth3', 'oth3dtc', 'oth3stt',
  'oth4text', 'oth4', 'oth4dtc', 'oth4stt',
  'oth5text', 'oth5', 'oth5dtc', 'oth5stt',
  'oth6text', 'oth6', 'oth6dtc', 'oth6stt',
  'oth7text', 'oth7', 'oth7dtc', 'oth7stt',
  'oth8text', 'oth8', 'oth8dtc', 'oth8stt',
  'oth9text', 'oth9', 'oth9dtc', 'oth9stt',
  'oth10text', 'oth10', 'oth10dtc', 'oth10stt',
  'oth11text', 'oth11', 'oth11dtc', 'oth11stt',
  'oth12text', 'oth12', 'oth12dtc', 'oth12stt',
  'oth13text', 'oth13', 'oth13dtc', 'oth13stt',
  'oth14text', 'oth14', 'oth14dtc', 'oth14stt',
  'oth15text', 'oth15', 'oth15dtc', 'oth15stt',
  
  # Blood
  'hemovalue', 'wcellcvalue', 'platevalue',
  'hivvalue', 'cd4value', 'antiigg_res',
  
  # Blood findings (extended)
  # Here, `initial`, `siteid`, `subjid` not considered
  'hemodtc', 'hemond', 'hemadtc', 'hemavalue', 'hemand', 'wcellcdtc',
  'wcellcnd', 'neudtc', 'neuvalue', 'neund', 'monodtc', 'monovalue', 'monond',
  'lympdtc',
  'lympvalue', 'lympnd', 'eosidtc', 'eosivalue', 'eosind', 'platedtc',
  'platend', 'altdtc', 'altvalue', 'altnd', 'creatdtc', 'creatvalue', 'creatnd',
  'sodiumdtc', 'sodiumvalue', 'sodiumnd', 'hivdtc', 'hbadtc', 'hbavalue',
  'hband', 'bilirudtc', 'biliruvalue', 'bilirund', 'hbsagdtc', 'hbsagvalue',
  'hcvdtc', 'hcvvalue', 'crpdtc', 'crpvalue', 'crpnd', 'procaldtc',
  'procalvalue', 'procalnd', 'cd4dtc', 'cd4nd', 'hivvldtc', 'hivvlvalue',
  'hivvlnd', 'antiiggdtc', 'vdrldtc', 'vdrlvalue', 'vdrltiter', 'tphadtc',
  'tphavalue', 'nmdardtc', 'nmdarvalue', 'fujilamdtc', 'fujilamvalue',
  'aleredtc', 'alerevalue', 'othrs1dtc', 'othrs1value', 'othrs2dtc',
  'othrs2value',
  
  # HIV
  'hivstt',
  
  # HIV findings (extended)
  # Here, `initial`, `siteid`, `subjid` not considered
  'hivmonth', 'hivyear', 'hivunk', 'unkmonth', 'arths', 'artdtc2', 'artyear',
  'artregimen', 'arteverm', 'artevery', 'artcurspec', 'cd4count', 'cd4stt',
  'cd4dtc2', 'viloadstt', 'viload', 'viloaddtc', 'hivdtc2', 'hivmusus',
  'hivmususstt', 'cotripro',
  
  # CSF
  'whcellc1', 'whcellc2', 'polycellc1', 'polycellc2', 'monocellc1',
    'monocellc2', 'protein1', 'protein2', 'pblglucose1', 'pblglucose2',
    'ratio_glucose1', 'ratio_glucose2',
  'crag1', 'crag2', 'xpert1', 'xpert2', 'xpertrif1', 'xpertrif2',
  'csfcmv1', 'csfcmv2', 'csfhsv1', 'csfhsv2', 'csfebv1', 'csfebv2', 'csfvzv1',
    'csfvzv2', 'csfvdrl1', 'csfvdrl2', 'csftpha1', 'csftpha2',
  
  # CSF findings (extended)
  # Here, `initial`, `siteid`, `subjid` not considered
  'sequence', 'csfsample1', 'csfsample2', 'sampledtc1', 'sampledtc2',
  'sampletime1', 'sampletime2', 'sptype1', 'sptype2', 'oppressure1',
  'oppressure2', 'oppressunm1', 'oppressunm2', 'appearance1', 'appearance2',
  'csf1', 'csf2', 'csfglucose1', 'csfglucose2', 'gramstain1', 'gramstain2',
  'znsmear1', 'znsmear2', 'ininksmear1', 'ininksmear2', 'rifprobe_a',
  'rifprobe_b', 'rifprobe_c', 'rifprobe_d', 'rifprobe_e', 'ctval_a',
  'ctval_b', 'ctval_c', 'ctval_d', 'ctval_e', 'baccult1', 'baccult2',
  'pathongen1', 'pathongen2', 'crypcult1', 'crypcult2', 'mycocult1',
  'mycocult2', 'csfnmdar1', 'csfnmdar2', 'hivviload1', 'hivviload2',
  'hivviloadnd1', 'hivviloadnd2', 'pcrtb1', 'pcrtb2', 'gene1', 'gene2',
  'pcrcov21', 'pcrcov22', 'cffoth1', 'cffoth2',
  
  # Radiologic
  'xraynm', 'xrayinf', 'xraymili', 'xraycav', 'xray_ores',
  'bihernia1','bihernia2','bihernia3', 'bienceph1', 'bienceph2', 'bienceph3',
  
  # Radiologic findings (extended)
  # Here, `initial`, `siteid`, `subjid` not considered
  'xraydtc', 'xraynd', 'xrayoth', 'binb1', 'bitype1', 'bidtc1', 'bind1',
  'bicontrast1', 'binormal1', 'binormal2', 'binormal3', 'biinfarct1',
  'biinfarct2', 'biinfarct3', 'bituber1', 'bituber2', 'bituber3', 'bihydro1',
  'bihydro2', 'bihydro3', 'biabscess1', 'biabscess2', 'biabscess3', 'bimenin1',
  'bimenin2', 'bimenin3', 'other1', 'other_specify1', 'other_specify2',
  'other_specify3', 'binb2', 'binb3', 'bitype2', 'bitype3', 'bidtc2', 'bidtc3',
  'bind2', 'bind3', 'bicontrast2', 'bicontrast3', 'other2', 'other3', 'mridtc',
  'mrind', 'resmrin0', 'resmrin1', 'resmrin2', 'resmrin3', 'resmrin4', 'mrinm',
  'mriabscess', 'mrimyelitis', 'mrispcol', 'mrioth',
  
  # Discharge-Death
  'staydur', 'outcome', 'nonneuspec', 'etiomtuber', 'etmtustt', 'etitoxoenc',
    'etitoxostt', 'eticryp', 'eticrypstt', 'etibac', 'etibacstt', 'etivence',
    'etivencestt', 'etiothba', 'etiothmuco', 'etiothbm', 'etiothneu',
    'etiohtlym', 'etiohnmdar', 'etioothspec',
  
  # Discharge-Death (extended)
  # Here, `initial`, `siteid`, `subjid` not considered
  'deathdtc', 'deathneu', 'deathnonneu', 'syndmen', 'syndenc', 'syndba',
  'syndmye', 'syndence', 'syndnonneu', 'tbmgrade', 'etipara', 'etiafb',
  'eticult', 'etixpert', 'etiprctb', 'etigstain', 'etigstainpos', 'etibaccult',
  'eticultpos', 'etivencedef', 'etivencespec', 'etioth', 'eyesc2', 'motoric2',
  'verbal2', 'veruni2', 'verunirs2', 'gcs2', 'extgocs', 'mrs', 'lupu2nd', 
  'mri2nd', 'neuproce', 'icu', 'meven', 'drugae', 'artstart',
  
  # Treatment during Hospitalisation (extended)
  # Here, `initial`, `siteid`, `subjid` not considered
  'rifamdtc', 'rifamstt', 'rifamrs', 'rifamspec',
  'isonidtc', 'isonistt', 'isonirs', 'isonispec',
  'pyradtc', 'pyrastt', 'pyrars', 'pyraspec',
  'ethamdtc', 'ethamstt', 'ethamrs', 'ethamspec',
  'strepdtc', 'strepstt', 'streprs', 'strepspec',
  'tbfdcdtc', 'tbfdcstt', 'tbfdcrs', 'tbfdcspec', 
  'levodtc', 'levostt', 'levors', 'levospec',
  'moxidtc', 'moxistt', 'moxirs', 'moxispec',
  'pyrimedtc2', 'pyrimestt2', 'pyrimers', 'pyrimespec',
  'clinddtc', 'clindstt', 'clindrs', 'clindspec',
  'cotridtc', 'cotristt', 'cotrirs', 'cotrispec',
  'metrodtc', 'metrostt', 'metrors', 'metrospec',
  'flucodtc2', 'flucostt2', 'flucors', 'flucospec',
  'amphodtc2', 'amphostt2', 'amphors', 'amphospec',
  'ceftriadtc2', 'ceftriastt2', 'ceftriars', 'ceftriaspec',
  'merodtc', 'merostt', 'merors', 'merospec',
  'acyclodtc2', 'acyclostt2', 'acyclors', 'acyclospec',
  'valgandtc2', 'valganstt2', 'valganrs', 'valganspec',
  'methyldtc2', 'methylstt2', 'methylrs', 'methylspec',
  'mannidtc', 'mannistt', 'mannirs', 'mannispec',
  'furosedtc2', 'furosestt2', 'furosers', 'furosespec', 
  'rantididtc', 'ranitidistt', 'ranitidirs', 'ranitidispec',
  'arvdtc', 'arvstt', 'arvrs', 'arvrsspec',
  'arvspec1', 'arvspec2', 'arvspec3', 'treatoth1',
  'treatoth1dtc', 'treatoth1stt', 'treatoth1rs', 'treatoth1spec',
  'treatoth2', 'treatoth2dtc', 'treatoth2stt', 'treatoth2rs', 'treatoth2spec',
  'treatoth3', 'treatoth3dtc', 'treatoth3stt', 'treatoth3rs', 'treatoth3spec',
  'treatoth4', 'treatoth4dtc', 'treatoth4stt', 'treatoth4rs', 'treatoth4spec',
  'treatoth5', 'treatoth5dtc', 'treatoth5stt', 'treatoth5rs', 'treatoth5spec',
  'treatoth6', 'treatoth6dtc', 'treatoth6stt', 'treatoth6rs', 'treatoth6spec',
  'treatoth7', 'treatoth7dtc', 'treatoth7stt', 'treatoth7rs', 'treatoth7spec',
  'treatoth8', 'treatoth8dtc', 'treatoth8stt', 'treatoth8rs', 'treatoth8spec',
  'treatoth9', 'treatoth9dtc', 'treatoth9stt', 'treatoth9rs', 'treatoth9spec',
  'treatoth10', 'treatoth10dtc', 'treatoth10stt', 'treatoth10rs',
    'treatoth10spec',
  'treatoth11', 'treatoth11dtc', 'treatoth11stt', 'treatoth11rs',
    'treatoth11spec',
  'treatoth12', 'treatoth12dtc', 'treatoth12stt', 'treatoth12rs',
    'treatoth12spec',
  'treatoth13', 'treatoth13dtc', 'treatoth13stt', 'treatoth13rs',
    'treatoth13spec',
  'treatoth14', 'treatoth14dtc', 'treatoth14stt', 'treatoth14rs',
    'treatoth14spec',
  'treatoth15', 'treatoth15dtc', 'treatoth15stt', 'treatoth15rs',
    'treatoth15spec',
  
  # Sample storage (extended)
  # Here, `initial`, `siteid`, `subjid` not considered
  'xray_s', 'ctscan', 'bmri', 'smri', 'bserum', 'cserum', 'bplasma', 'dna',
  'bcsf', 'fucsf', 'urine', 'isolate', 'isospec', 'othspec', 'oth',
  
  # Study Completion
  'paticond',
  
  # Study completion (extended)
  # Here, `initial`, `siteid`, `subjid` not considered
  'fudtc', 'moass', 'rehosp6m', 'rehospdtc', 'rehosprs', 'pacontdtc',
  'tbmedi_sc', 'tbspec',
  'antoxomedi', 'antoxospec', 'ancrypmedi', 'ancrypspec',
  'art_sc', 'artspec_sc', 'neurocase_sc',
  'neudiaspec', 'mtuber_sc', 'mtubestt', 'tbmgrade_sc', 'pararea_sc',
  'afb', 'mtubecult', 'mtubexpert', 'toxo', 'toxostt', 'crypto', 'cryptstt',
  'bacmen', 'bacmenstt', 'gstain', 'gstainpos', 'gstaincult', 'gstaincultpos',
  'vencep', 'vencepstt', 'vencepdef', 'vencepspec',
  'etiothba_sc', 'etiothmuco_sc', 'etiothbm_sc', 'etiothneu_sc',
  'etiothlym_sc', 'etiothnmdar_sc', 'etioth_sc', 'etiothspec_sc',
  'lvous', 'outcomesc', 'totalsc', 'egs', 'mrsd',
  'deathdtc_sc', 'death_day_sc', 'deathplace', 'tbdrug_sc', 'arv', 'deathrs',
  'deathspec'
)

# Subset the datasets and coerce into the character type where necessary
# If non-existent in one of the datasets, create an NA vector
df_jkt_selected <- jkt_raw |>
  select(all_of(vars_of_interest)) |> 
  mutate(
   # Extended
   ## Extended
   fstdosend = as.character(fstdosend),
   fstdosetime = as.character(fstdosetime),
   lumpuncnd = as.character(lumpuncnd),
   clindadtc = as.character(clindadtc),
   cd4count = as.character(cd4count),
   viload = as.character(viload),
   bidtc2 = as.character(bidtc2),
   bidtc3 = as.character(bidtc3),
    
   ## Baseline
   dob = as.character(dob)
  )

df_bdg_selected <- bdg_raw |>
  select(all_of(vars_of_interest)) |>
  mutate(
    # Main
    sex = as.character(sex),
    feversym = as.character(feversym),
    headsym = as.character(headsym),
    vomitsym = as.character(vomitsym),
    alconsym = as.character(alconsym),
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
    paticond = as.character(paticond),
    
    # Extended
    ## Screening
    iscnsinfec = as.character(iscnsinfec),
    is18 = as.character(is18),
    issigned = as.character(issigned),
    admistime = as.character(admistime),
    neuroinfectime = as.character(neuroinfectime),
    fstdosetime = as.character(fstdosetime),
    lumpuncnd = as.character(lumpuncnd),
    fstdosend = as.character(fstdosend),
    lunpunctime = as.character(lunpunctime),
    enrolltime = as.character(enrolltime),
    isrecrbf = as.character(isrecrbf),
    patientid1 = as.character(patientid1),
    patientid2 = as.character(patientid2),
    patientid3 = as.character(patientid3),
    patientid4 = as.character(patientid4),
    
    ## Baseline
    dob = as.character(dob),
    studypl = as.character(studypl),
    refstt = as.character(refstt),
    fevercomp = as.character(fevercomp),
    headcomp = as.character(headcomp),
    vomitcomp = as.character(vomitcomp),
    alconcomp = as.character(alconcomp),
    lethsym = as.character(lethsym),
    lethcomp = as.character(lethcomp),
    bechcomp = as.character(bechcomp),
    moabsym = as.character(moabsym),
    moabcomp = as.character(moabcomp),
    seabsym = as.character(seabsym),
    seabcomp = as.character(seabcomp),
    cranpsym = as.character(cranpsym),
    cranpcomp = as.character(cranpcomp),
    seizucomp = as.character(seizucomp),
    seipattern = as.character(seipattern),
    seiwitms = as.character(seiwitms),
    seizufre = as.character(seizufre),
    othcomp = as.character(othcomp),
    weightlost = as.character(weightlost),
    nsweat = as.character(nsweat),
    bcg = as.character(bcg),
    treatedtb = as.character(treatedtb),
    timettb = as.character(timettb),
    pretb = as.character(pretb),
    tbfre = as.character(tbfre),
    tbrecent = as.character(tbrecent),
    tbsttrec = as.character(tbsttrec),
    tblatent = as.character(tblatent),
    precns = as.character(precns),
    cnsfre = as.character(cnsfre),
    diabetes = as.character(diabetes),
    diatherapy = as.character(diatherapy),
    corticocurt = as.character(corticocurt),
    corticodur = as.character(corticodur),
    piein30 = as.character(piein30),
    pieanimal = as.character(pieanimal),
    piefreshwt = as.character(piefreshwt),
    hissmoking = as.character(hissmoking),
    loctemp = as.character(loctemp),
    weightmth = as.character(weightmth),
    armcirmea = as.character(armcirmea),
    heightmth = as.character(heightmth),
    nerve3rd = as.character(nerve3rd),
    nerve4th = as.character(nerve4th),
    nerve6th = as.character(nerve6th),
    nerve7th = as.character(nerve7th),
    nerve12th = as.character(nerve12th),
    pupisz = as.character(pupisz),
    pupiref = as.character(pupiref),
    urinary = as.character(urinary),
    gasbleed = as.character(gasbleed),
    gradespec = as.character(gradespec),
    cnsinfec = as.character(cnsinfec),
    onsetstt = as.character(onsetstt),
    evimales = as.character(evimales),
    csfabnor = as.character(csfabnor),
    hivresult = as.character(hivresult),
    extraclues = as.character(extraclues),
    xray = as.character(xray),
    tbdrug = as.character(tbdrug),
    tbdrugstt = as.character(tbdrugstt),
    levof = as.character(levof),
    levofstt = as.character(levofstt),
    moxif = as.character(moxif),
    cotrimo = as.character(cotrimo),
    cotrimostt = as.character(cotrimostt),
    metroni = as.character(metroni),
    metronistt = as.character(metronistt),
    pyrime = as.character(pyrime),
    pyrimestt = as.character(pyrimestt),
    clinda = as.character(clinda),
    clindastt = as.character(clindastt),
    clindadtc = as.character(clindadtc),
    fluco = as.character(fluco),
    flucostt = as.character(flucostt),
    ampho = as.character(ampho),
    amphostt = as.character(amphostt),
    ceftria = as.character(ceftria),
    ceftriastt = as.character(ceftriastt),
    merope = as.character(merope),
    meropestt = as.character(meropestt),
    acyclo = as.character(acyclo),
    acyclostt = as.character(acyclostt),
    valgan = as.character(valgan),
    dexame = as.character(dexame),
    dexamestt = as.character(dexamestt),
    methyl = as.character(methyl),
    methylstt = as.character(methylstt),
    mannitol = as.character(mannitol),
    mannitolstt = as.character(mannitolstt),
    furose = as.character(furose),
    valproic = as.character(valproic),
    topira = as.character(topira),
    pheny = as.character(pheny),
    phenystt = as.character(phenystt),
    levitira = as.character(levitira),
    levitirastt = as.character(levitirastt),
    ranppi = as.character(ranppi),
    ranppistt = as.character(ranppistt),
    art = as.character(art),
    artstt = as.character(artstt),
    oth1 = as.character(oth1),
    oth1stt = as.character(oth1stt),
    oth2 = as.character(oth2),
    oth2stt = as.character(oth2stt),
    hivdtc = as.character(hivdtc),
    hbsagvalue = as.character(hbsagvalue),
    hcvvalue = as.character(hcvvalue),
    vdrlvalue = as.character(vdrlvalue),
    tphavalue = as.character(tphavalue),
    nmdarvalue = as.character(nmdarvalue),
    arths = as.character(arths),
    cd4stt = as.character(cd4stt),
    cd4dtc2 = as.character(cd4dtc2),
    viloadstt = as.character(viloadstt),
    viloaddtc = as.character(viloaddtc),
    hivmususstt = as.character(hivmususstt),
    cotripro = as.character(cotripro),
    csfsample1 = as.character(csfsample1),
    sampletime1 = as.character(sampletime1),
    sptype1 = as.character(sptype1),
    appearance1 = as.character(appearance1),
    gramstain1 = as.character(gramstain1),
    znsmear1 = as.character(znsmear1),
    ininksmear1 = as.character(ininksmear1),
    baccult1 = as.character(baccult1),
    crypcult1 = as.character(crypcult1),
    mycocult1 = as.character(mycocult1),
    csfnmdar1 = as.character(csfnmdar1),
    pcrtb1 = as.character(pcrtb1),
    xrayoth = as.character(xrayoth),
    binb1 = as.character(binb1),
    bitype1 = as.character(bitype1),
    bicontrast1 = as.character(bicontrast1),
    binb2 = as.character(binb2),
    binb3 = as.character(binb3),
    bitype2 = as.character(bitype2),
    bitype3 = as.character(bitype3),
    bidtc2 = as.character(bidtc2),
    bidtc3 = as.character(bidtc3),
    bicontrast2 = as.character(bicontrast2),
    bicontrast3 = as.character(bicontrast3),
    tbmgrade = as.character(tbmgrade),
    etipara = as.character(etipara),
    etiafb = as.character(etiafb),
    eticult = as.character(eticult),
    etixpert = as.character(etixpert),
    etigstain = as.character(etigstain),
    etibaccult = as.character(etibaccult),
    etivencedef = as.character(etivencedef),
    lupu2nd = as.character(lupu2nd),
    mri2nd = as.character(mri2nd),
    neuproce = as.character(neuproce),
    icu = as.character(icu),
    meven = as.character(meven),
    drugae = as.character(drugae),
    artstart = as.character(artstart),
    rifamstt = as.character(rifamstt),
    rifamrs = as.character(rifamrs),
    isonistt = as.character(isonistt),
    isonirs = as.character(isonirs),
    pyrastt = as.character(pyrastt),
    pyrars = as.character(pyrars),
    ethamstt = as.character(ethamstt),
    ethamrs = as.character(ethamrs),
    strepstt = as.character(strepstt),
    streprs = as.character(streprs),
    tbfdcstt = as.character(tbfdcstt),
    tbfdcrs = as.character(tbfdcrs),
    levostt = as.character(levostt),
    pyrimestt2 = as.character(pyrimestt2),
    clindstt = as.character(clindstt),
    cotristt = as.character(cotristt),
    cotrirs = as.character(cotrirs),
    metrostt = as.character(metrostt),
    flucostt2 = as.character(flucostt2),
    amphostt2 = as.character(amphostt2),
    ceftriastt2 = as.character(ceftriastt2),
    merostt = as.character(merostt),
    acyclostt2 = as.character(acyclostt2),
    methylstt2 = as.character(methylstt2),
    mannistt = as.character(mannistt),
    furosestt2 = as.character(furosestt2),
    ranitidistt = as.character(ranitidistt),
    arvstt = as.character(arvstt),
    arvrs = as.character(arvrs),
    treatoth1stt = as.character(treatoth1stt),
    treatoth1rs = as.character(treatoth1rs),
    treatoth2stt = as.character(treatoth2stt),
    treatoth3stt = as.character(treatoth3stt),
    xray_s = as.character(xray_s),
    ctscan = as.character(ctscan),
    bmri = as.character(bmri),
    smri = as.character(smri),
    bserum = as.character(bserum),
    cserum = as.character(cserum),
    bplasma = as.character(bplasma),
    dna = as.character(dna),
    bcsf = as.character(bcsf),
    fucsf = as.character(fucsf),
    urine = as.character(urine),
    isolate = as.character(isolate),
    oth = as.character(oth),
    moass = as.character(moass),
    rehosp6m = as.character(rehosp6m),
    ancrypmedi = as.character(ancrypmedi),
    art_sc = as.character(art_sc),
    neurocase_sc = as.character(neurocase_sc),
    mtubestt = as.character(mtubestt),
    tbmgrade_sc = as.character(tbmgrade_sc),
    pararea_sc = as.character(pararea_sc),
    afb = as.character(afb),
    mtubecult = as.character(mtubecult),
    mtubexpert = as.character(mtubexpert),
    toxostt = as.character(toxostt),
    cryptstt = as.character(cryptstt),
    bacmenstt = as.character(bacmenstt),
    gstain = as.character(gstain),
    gstaincult = as.character(gstaincult),
    vencepstt = as.character(vencepstt),
    vencepdef = as.character(vencepdef),
    deathplace = as.character(deathplace),
    tbdrug_sc = as.character(tbdrug_sc),
    arv = as.character(arv),
    deathrs = as.character(deathrs)
  )

# Merge across sites
ibis_raw <- bind_rows(df_jkt_selected, df_bdg_selected)

# Check if there is any duplication
remaining_duplicated_rows <- table(ibis_raw$subjid) |>
  data.frame() |>
  arrange(desc(Freq)) |>
  filter(Freq >= 2) |>
  pull(Var1)

# Look at the remaining duplicated data, if any
duplicated <- ibis_raw |>
  arrange(subjid) |>
  filter(subjid %in% remaining_duplicated_rows)

if (dim(duplicated)[1] == 0) {
  print('You have no duplicated rows.')
} else {
  print('Duplicated rows found.')
}

# Take a glimpse of the merged data
dim(ibis_raw)
glimpse(ibis_raw)

# Save the dataset
ibis_raw |> write_rds(here('0-data', 'ibis_all-merged.rds')) # R

# Appendix ----------------------------------------------------------------

sessioninfo::platform_info()
