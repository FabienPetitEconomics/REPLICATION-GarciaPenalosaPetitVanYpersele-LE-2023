#### INITIALIZATION ####
source(file.path("_script", "init.R"))
#### LOAD DATA ####
### SOC90 Classification
soc90_class = file.path(loc_CAMSIS, "soc90_isco88.csv") %>% read.csv
### NCDS
# Age 11 from occupational coding
ncds2 = file.path(loc_BOTH_occ, "ncds2_occupation_coding_father.dta") %>% 
  read.dta() %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid",
    soc90 = "n2ssoc90", # NCDS 1969 Father: Semi auto SOC90
    sc = "n2srgsc", # NCDS 1969 Father: RGSC social class code SEMI processing
  )
# Age 11 from sweep 0 to 3
ncds16 = file.path(loc_NCDS_16, "ncds0123.dta") %>% 
  read.dta() %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    
    # "n492", # 0 Social class mother's husband (GRO 1951)
    # "n525", # 0  SEG maternal GPa-Miner,foreman code (GRO 1951)
    # "n526", # 0  Mothers fathers social class (GRO 1951)
    # "n571", # 0  Mums fathers soc class + miner codes (GRO 1951)
    father_00 = "n236", # 0P Social class of mother's husband (GRO 1951)
    # mother_00 = "n660", # 0 Mother's,father's social class 1958 (GRO 1951)
    
    out_07 = "n188", # 1P Unemployed,sick and retired (GRO 1960)
    father_07 = "n190", # 1P Social class of father,male head (GRO 1960)
    
    # "n1685", # 2PD Social class father,male head-groupd (GRO 1966)
    # "n1687", # 2PD Social class of father or male head (GRO 1966)
    out_11 = "n1172", # 2P Father,male head's occupation
    father_11 = "n1171", # 2P Social Class of father or male head (GRO 1966)
    # SEGfather_11 = "n1175", # 2P Father,male head's socio-economic grp (GRO 1966)
    
    out_16 = "n2383", # 3P Father or father figure's occupation (GRO 1970)
    father_16 = "n2384", # 3P Social class father,male head (GRO 1970)
    # SEGfather_16 = "n2385", # 3P Father,male heads socio-economic grp (GRO 1970)
    # mother_16 = "n2393", # 3P Mother-s social class,if works (GRO 1970)
    # SEGmother_16 = "n2394", # 3P Mothers Socio-economic group,if works (GRO 1970)
    
  )
### BCS
# Age 10 from occupation coding
bcs3 = file.path(loc_BOTH_occ, "bcs3_occupation_coding_father.dta") %>% 
  read_dta() %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid",
    soc90 = "b3fssoc90", # BCS 1980 Father: SEMI automatic SOC90   
    sc = "b3fsrgsc", # BCS 1980 Father: RGSC social class code SEMI processing 
  )
# Age 10
bcs10 = file.path(loc_BCS_10, "sn3723.dta") %>% 
  read.dta() %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    
    "c2_1", # FATHER'S EMPLOYMENT: REGULAR PAID JOB
    "c2_2", # FATHER'S EMPLOYMENT: WORKS OCCASIONALLY
    "c2_3", # FATHER'S EMPLOYMENT: SEEKING WORK
    "c2_4", # FATHER'S EMPLOYMENT: LOOKS AFTER HOME
    "c2_5", # FATHER'S EMPLOYMENT: NOT IN PAID JOB
    "c2_6", # FATHER'S EMPLOYMENT: REASON NOT PAID JOB
    "c2_7", # FATHER'S EMPLOYMENT: OTHER EMPLOYMENT SITUATION
    "c2_8", # FATHER'S EMPLOYMENT: NO MALE HEAD IN HOUSE
    
    # father_10 = "c3_4", # FATHER'S CORRECTED SOCIAL CLASS 1980
    father_10 = "c3_5", # FATHER'S CORRECTED SOCIAL CLASS 1970
    # mother_10 = "c3_11", # MOTHER'S CORRECTED SOCIAL CLASS 1980
    mother_10 = "c3_12", # MOTHER'S CORRECTED SOCIAL CLASS 1970
    
  )
#### TREATMENT ####
# NCDS
ncds = merge(ncds2, select(ncds16, id, out_11), by = "id", all = T) %>% 
  mutate(activ0 = factor(out_11, 
    levels = c("No male head", "Unemployed,sick", "Retired"),
    labels = c("No male head", "UnempSick", "Retired"))) %>% 
  mutate(
    activ = 
      case_when(
        !is.na(activ0) ~ as.character(activ0),
        !is.na(soc90) ~ "Work", 
        TRUE ~ NA_character_
        ),
    activ2 = factor(activ,
      levels = c("Work", "No male head", "Retired", "UnempSick"),
      labels = c("Work", rep("Out", 3)))) %>% 
  select(id, sc, soc90, activ2)
# BCS
bcs = merge(bcs3, bcs10, by = "id", all = T) %>% 
  # Rename "Not stated" into "Inactive"
  mutate(c2_6 = factor(c2_6, 
    levels = c("Not stated", "Retired", "Disabled", "Student", "Prison"),
    labels = c("Inactive", "Retired", "Disabled", "Student", "Prison"))) %>% 
  mutate(activ = case_when(
    c2_3 == "Yes" ~ "Unemp", # Unemployed
    c2_4 == "Yes" ~ "Home", # FT Home
    !is.na(c2_6) ~ as.character(c2_6), # Reason why not employed
    !is.na(soc90) | c2_1 == "Yes" | c2_2 == "Yes" ~ "Work", # Working
    TRUE ~ NA_character_ # NA
  )) %>% 
  # Regroup into Work versus Out-of-work
  mutate(activ2 = factor(activ,
    levels = c("Work", "Disabled", "Home", "Inactive", "Prison", "Retired", "Student", "Unemp"),
    labels = c("Work", rep("Out", 7)))) %>%
  select(id, sc, soc90, activ2)
#### RBIND ####
parent_occ = rbind(ncds, bcs) %>% 
  merge(soc90_class %>% select(soc90, isco88), by = "soc90", all.x = TRUE) %>%
  select(id, activ2, sc, soc90, isco88) %>% 
  arrange(id, isco88) %>% 
  group_by(id) %>% slice(1) %>% ungroup
### SAVE ####
write.csv(parent_occ, file.path(loc_cohort, "parent_occ.csv"), row.names = F)