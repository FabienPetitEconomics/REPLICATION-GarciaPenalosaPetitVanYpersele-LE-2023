#### 0 INITIALIZATION ####
source(file.path("_script","init.R"))
#### 1 LOAD DATA ####
### SOC Classification
soc90_class = read.csv(file.path(loc_CAMSIS, "soc90_isco88.csv"), stringsAsFactors = T)
soc00_class = read.csv(file.path(loc_CAMSIS, "soc00_isco88.csv"), stringsAsFactors = T)
### LAST SWEEP
if(!file.exists(file.path(loc_cohort, "sweep_last.csv"))){
  source(file.path(loc_cleaner, "_cohort", "sweep.R"))
}
## Load
sweep_last = read.csv(file.path(loc_cohort, "sweep_last.csv"), stringsAsFactors = T)
### ACTIVITY HISTORY
## Load NCDS
ncds_acthist = file.path(loc_NCDS_acthist, "ncds_activity_histories.dta") %>% 
  read_dta() %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    "numacts", # Total number of activities
    "actnum", # Activity number
    "jactiv", # Activity status
    "jempst", # Employment status
    "jftpt", # Full/Part time
    "jstmth", "jstyr", # Month/Year activity started
    "jendmth", "jendyr", # Month/Year activity ended
    # contains("jyleft"), # Reason why left
    "j91seg", # Socio Economic Group (1990 scheme)
    "j91sc", # Social Class (1990 scheme)
    "j90soc", # SOC90
    "j2ksoc", # Activity soc2000
  ) %>% 
  # Remove CM without any activity
  filter(!(actnum <= 0) & !is.na(actnum))
## Load BCS
bcs_acthist = file.path(loc_BCS_acthist, "bcs70_activity_histories.dta") %>% 
  read_dta() %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    "numacts", # Total number of activities
    "actnum", # Activity number
    "jactiv", # Activity status
    "jempst", # Employment status
    "jftpt", # Full/Part time
    "jstmth", "jstyr", # Month/Year activity started
    "jendmth", "jendyr", # Month/Year activity ended
    # contains("jyleft"), # Reason why left
    "j91seg", # Socio Economic Group (1990 scheme)
    "j91sc", # Social Class (1990 scheme)
    "j90soc", # SOC90
    "j2ksoc", # Activity soc2000
  ) %>% 
  # Remove CM without any activity
  filter(!(actnum <= 0) & !is.na(actnum))
### RBIND
acthist = rbind(ncds_acthist, bcs_acthist)
#### 2 TREATMENT ####
### 2.1 MERGE SOC ####
acthist2 = acthist %>% 
  merge(soc90_class %>% select(j90soc = soc90, isco88.90 = isco88), by = "j90soc", all = TRUE) %>% # Merge SOC90
  merge(soc00_class %>% select(j2ksoc = soc00, isco88.00 = isco88), by = "j2ksoc", all = TRUE) %>% # Merge SOC00
  mutate(isco88 = isco88.00 %>% ifelse(!is.na(.), ., isco88.90)) %>%  # Merge ISCO88
  select(id, numacts, actnum, jactiv, jempst, jftpt, jstmth, jstyr, jendmth, jendyr, isco88) %>% # Select variables
  arrange(id, actnum) %>% # Arrange by id and activity number
  subset(!duplicated(select(., -isco88))) # Remove duplicated due to isco88.90 + isco88.00
#### 2.2 DATE ####
acthist3 = acthist2 %>% 
  mutate_if(is.numeric, function(x){x %>% ifelse(. < 0, NA, .)}) %>% # Replace negative values with NA
  # Generate start/end date in character format
  mutate(
    # Concatenate date (character format), then Date format
    start = paste0("01", sprintf("%02d", jstmth), jstyr) %>% dmy(), # Start of the period
    end = paste0("01", sprintf("%02d", jendmth), jendyr) %>% ifelse(is.na(jendyr), NA, .) %>% dmy(), # End of the period
  ) %>%
  filter(!is.na(start)) %>% # Remove NA start
  select(id, numacts, actnum, start, end, everything()) %>% # Reorder variables
  select(-end, -jstmth, -jstyr, -jendmth, -jendyr) %>% # Remove intermediate variables
  arrange(id, start) # Arrange by ID and date of activity
#### 2.3 VARIABLE TREATMENT ####
acthist4 = acthist3 %>% 
  mutate(
    # Activity
    jactiv = jactiv %>% 
      factor(labels = c("Employee", "Employee", "Self employed", "Self employed", "Employee", "Self employed", 
                        "Voluntary work", "Employee", "Employee", "Employee", "Unemployed", "Education", 
                        "Education", "Training scheme", "Temporary sick", "Long term sick", "Long term sick",
                        "Home", "Retired", "Voluntary work", "Maternity leave", "Other", "Other")),
    # Employment status
    jempst = jempst %>% 
      factor(labels = c("Self-Large", "Self-Small", "Self-Alone", "Mana-Large", "Mana-Small",
                        "ForSup", "Emp")),
    # Full-time / Part-time
    jftpt = jftpt %>% 
      factor(labels = c("FT", "PT")),
    ) %>% 
  # Remove CM with only one unknown activity
  filter(!(numacts == 1 & is.na(jactiv))) %>% 
  # Change the first activity (when unknown) to FT Education
  mutate(jactiv = jactiv %>% as.character %>% 
           ifelse(is.na(.) & actnum == 1, "Education", .) %>% 
           factor(levels = c("Employee", "Self employed", "Voluntary work", "Unemployed", "Education",
                             "Training scheme", "Temporary sick", "Long term sick", "Home", "Retired",
                             "Maternity leave", "Other"))) %>% 
  # Remove the remaining Dont Know
  filter(!is.na(jactiv))
#### SAVE ####
write.csv(acthist4, file.path(loc_cohort, "acthist.csv"), row.names = FALSE)