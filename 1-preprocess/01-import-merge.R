
# Preamble ----------------------------------------------------------------

# Code author  : Ihsan Fadilah
# Email        : ifadilah@eocru.org
# Last updated : Late December 2021
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
         
         # Extended
         ## Baseline (syntax format: new_name = old_name)
         ## Probably not valid this way
         # fstdosend = fstdosedtc,
         # fstdosedtc = fstdosetime,
         # fstdosetime = fstdosend,
         # lumpuncnd = lumpuncdtc,
         # lumpuncdtc = lunpunctime,
         # lunpunctime = lumpdur,
         # lumpdur = lumpdurhour,
         # lumpdurhour = lumpuncnd
         ) |> 
  
  # Add non-existent variables to match the other site
  mutate(motordef = rep(NA, nrow(df_jkt))) |>
  
  # Make variables uniform across sites
  rename(xray_ores = xrayoth,
         monopare = monoparesis,
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
         ceftriastt = ceftriastt_x)

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
         bienceph3 = rep(NA, nrow(df_bdg)),
         lumpdur = rep(NA, nrow(df_bdg)),
         lumpdurhour = rep(NA, nrow(df_bdg)),
         mob = rep(NA, nrow(df_bdg)),
         yob = rep(NA, nrow(df_bdg)),
         covid19 = rep(NA, nrow(df_bdg)),
         covid19vac = rep(NA, nrow(df_bdg)),
         phyexamoth = rep(NA, nrow(df_bdg)),
         veruni = rep(NA, nrow(df_bdg)),
         verunirs = rep(NA, nrow(df_bdg))) |>
  
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
         suetiunk = suetibac___7) 

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
  'seipattern', 'seiwitms', 'seizufre', 'othcomp', 'othday', 'weightlost',
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
  'ceftriastt', 'merope', 'meropedtc', 'meropestt',
  
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
    'etiohtlym', 'etiohnmdar', 'etioothspec',
  
  # Study Completion
  'paticond'
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
    meropestt = as.character(meropestt)
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
ibis_raw |> write_rds(here('0-data', 'ibis_merged.rds')) # R

# Appendix ----------------------------------------------------------------

sessioninfo::platform_info()