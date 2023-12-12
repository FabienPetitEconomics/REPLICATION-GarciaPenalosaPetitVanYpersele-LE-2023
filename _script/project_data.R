#### PROJECT_DATA.R ####
# The purpose of this script is to prepare the final dataset for the analysis
##### 0 INITIALIZATION #####
source("./_script/init.R")
# Load data on inflation and function
source(file.path(loc_function, "compute_deflator.R"))
#### 1 FUNCTIONS ####
# Function to interpolate activity
interpolate_activity = function(df){
  df2 = df %>% 
    group_by(id) %>% # Group data by id
    mutate_at(vars(jactiv:isco88), function(x){x %>% na.locf(., na.rm = FALSE)}) %>% # Interpolate variables
    ungroup() %>% # Ungroup data
    mutate_if(is.character, ~replace(., . == "NA", NA)) %>% # Replace "NA" with NA
    mutate_at(vars(jactiv, jempst, jftpt), as.factor) %>% # jactiv, jempst, jftpt as factor
    mutate_at(vars(isco88), as.integer) # isco88 as integer
  return(df2)
}
# Function to isolate first job
find_firstjob = function(df){
  df2 = df %>% 
    subset(!is.na(jempst)) %>% # Remove NA employment status
    arrange(id, start) %>% # Arrange by id and starting date of activity
    group_by(id) %>% slice(1) %>% ungroup() %>% # Extract first observation for each CM (first job)
    select(cohort, id, start) %>% # Select variables
    mutate(first_job = TRUE) %>% # Boolean to define first job
    merge(df, ., by = c("cohort", "id", "start"), all = TRUE) %>% # Merge with initial dataframe
    mutate_at(vars(first_job), ~replace(., is.na(.), FALSE)) %>% # Replace NA in Boolean with FALSE
    group_by(id, start) %>% slice(1) %>% ungroup # Slice for duplicated activities (length less than 1 month)
  return(df2)
}
#### 2 LOAD DATA ####
## Deflator
def = compute_deflator(1970) # Reference year for pay
## Parental Income
parinc = read.csv(file.path(loc_cohort, "parent_income.csv"), stringsAsFactors = T)
## Parent Age at Birth
parage = read.csv(file.path(loc_cohort, "parent_age_at_birth.csv"), stringsAsFactors = T)
## Child Pay
pay = read.csv(file.path(loc_cohort, "work_pay.csv"), stringsAsFactors = T)
## Education
hqual = read.csv(file.path(loc_cohort, "educ_hqual.csv"), stringsAsFactors = T)
## Activity History
acthist = read.csv(file.path(loc_cohort, "acthist.csv"), stringsAsFactors = T)
## Gender
gender = read.csv(file.path(loc_cohort, "gender.csv"), stringsAsFactors = T)
## Last Sweep
sweep_last = read.csv(file.path(loc_cohort, "sweep_last.csv"), stringsAsFactors = T)
## Response Sweep
sweep_response = read.csv(file.path(loc_cohort, "sweep_response.csv"), stringsAsFactors = T)
## Family
family = read.csv(file.path(loc_cohort, "family.csv"), stringsAsFactors = T)
## Location
location = read.csv(file.path(loc_cohort, "location.csv"), stringsAsFactors = T)
## Parents education
parent_educ = read.csv(file.path(loc_cohort, "parent_education.csv"), stringsAsFactors = T)
## Parents occupation
parent_occ = read.csv(file.path(loc_cohort, "parent_occ.csv"), stringsAsFactors = T)
#### 3 TREATMENT ####
#### 3.0 Sweep ####
# Add 23 for BCS and 26 for NCDS to sweep (Robustness)
add2326 = data.frame(cohort = c("N", "B"), age = c(26, 23),
                     int_date = c(NA, NA), int_date.full = c("1984-09-01", "1993-09-01"), 
                     actdata = c(NA, NA), present = c(NA, NA))
sweep_response1 = sweep_response %>% 
  select(id) %>% mutate(cohort = substr(id, 1, 1)) %>% unique %>% 
  merge(add2326, by = "cohort", all.x = T) %>% select(-cohort) %>% 
  rbind(sweep_response) %>% arrange(id, age)
#### 3.1 Parental Income ####
parinc2 = parinc %>% 
  select(id, age, par_inc = income_def70) %>% # Parental income in £1970
  group_by(id, age) %>% slice(1) %>% ungroup() %>% filter(!duplicated(.)) %>% # Remove duplicates
  setDT %>% dcast(id ~ age, value.var = "par_inc", fill = NA) %>% # Cast data (from long to wide)
  rename(pinc10 = "10", pinc16 = "16") %>% # Rename variables
  mutate(pinc = rowMeans(.[, c("pinc10", "pinc16")], na.rm = TRUE)) # Take the average when two parental income
#### 3.2 Parent Age at Birth ####
# Nothing to do
#### 3.3 Pay ####
# Pay treatment
pay2 = pay %>% 
  select(id, age, year, gp_wk, np_wk, gn_ratio, ng_ratio) %>% # Select variables
  mutate(pay_wk = np_wk %>% ifelse(is.na(.), gp_wk, .)) %>% # If net pay is missing, take the gross pay
  # Correction
  mutate(
    pay_wk_c = pay_wk %>% 
      # Delete when gross/net ratio is greater than 3 or lower than .33
      ifelse((!is.na(gn_ratio))&((gn_ratio > 3)|(ng_ratio > 3)), NA, .) %>% 
      # Take the gross when net and gross have been inverted
      ifelse((!is.na(ng_ratio))&(ng_ratio <= 3)&(ng_ratio > 1), gp_wk, .)
  ) %>% 
  merge(def, by = "year") %>% # Merge Deflator
  select(id, age, year, deflator, pay_wk_c) %>% # Select variables
  mutate(pay_wk_c_def = pay_wk_c * deflator) %>% # Deflate to £1970
  arrange(id, age) %>% # Arrange by id and age
  select(id, age, pay = pay_wk_c_def) # Select and rename variables
# Assign to periods
pay3 = pay2 %>% 
  filter(age %in% c(23, 26, 33, 34, 42)) %>% # Ages used for smjp project
  # Create period
  mutate(
    period = age %>% 
      factor(levels = levels(factor(.)), labels = c(1, 1, 2, 2, 3)) %>% 
      as.integer(.)
  ) %>% 
  select(id, period, pay) %>% # Select variables
  setDT %>% dcast(id ~ period, value.var = "pay", fill = NA) %>% # Cast data (from long to wide)
  setNames(c("id", paste0("pay_p", names(.)[-1]))) # Rename variables
#### 3.4 Education ####
# Highest academic qualification
hqual2 = hqual %>% 
  select(id, educ = hqual_ac_num, hqual = hqual_num) %>% # Select variables in numeric format
  arrange(id, desc(educ)) %>% group_by(id) %>% slice(1) %>% ungroup() # Slice to take the highest educational qualification
#### 3.5 Activity History ####
# Prepare for interpolation
acthist2 = acthist %>% 
  mutate_all(~ as.character(.) %>% ifelse(is.na(.), "NA", .)) %>% # Change NA to "NA" (char) for interpolation
  merge(sweep_response1 %>% select(id, start = int_date.full, age, present), by = c("id", "start"), all = T) # Merge with response
# Interpolate activity
acthist3 = interpolate_activity(acthist2)
# Define cohort, birth and date formats
acthist4 = acthist3 %>% 
  mutate(
    cohort = ifelse(substr(id, 1, 1) == "B", "BCS", "NCDS") %>% factor(levels = c("NCDS", "BCS")), # Cohort
    birth = ifelse(cohort == "BCS", "19700401", "19580301") %>% ymd, # Birth
    start = start %>% as.character %>% ymd, # Start of the activity
    year = year(start) # Starting year of the activity
  )  %>% 
  select(cohort, id, age, present, start, jactiv, jempst, jftpt, isco88) %>% # Select variables
  find_firstjob(.) # Find first job
#### 3.6 First Job ####
first_job = acthist4 %>% filter(first_job == TRUE) %>% # Keep first job
  mutate(
    birth = ifelse(cohort == "NCDS", "01-03-1958", "01-04-1970") %>% dmy(.), # Birth date
    age = {interval(birth, start) / years(1)} %>% round # Compute age
  ) %>% 
  select(cohort, id, date_FJ = start, age_FJ = age, jempst_FJ = jempst, 
         jftpt_FJ = jftpt, isco88_FJ = isco88) %>% # Select and rename variables
  subset(!duplicated(.)) %>% 
  arrange(id) # Arrange by id
# Clean acthist
acthist5 = acthist4 %>% filter(!is.na(age)) %>% # Remove missing age
  select(cohort, id, date = start, age, present, jactiv, jempst, jftpt, isco88) # Select variables
# Merge with sweep last
acthist6 = acthist5 %>% 
  merge(sweep_last, by = "id", suffixes = c("", "_LastSweep")) %>% # Merge with last sweep
  filter(age <= age_LastSweep) %>% select(-age_LastSweep) %>% # Remove interpolation after last interview
  setDT %>% unique(by = c("id", "age")) %>% 
  arrange(id, age)# Remove duplicated at same period
#### 3.7 Siblings ####
family2 = family %>% 
  setDT %>% dcast(id ~ period, value.var = c("HHsize", "SIBpos", "SIBsize")) %>% # Cast data
  select(id, ends_with("cp1"), ends_with("cp2"), ends_with("cp3")) # Reorder variables
#### 3.8 Parents occupation ####
parent_occ2 = parent_occ %>% 
  setNames(c("id", "pactiv", "psc","psoc90", "pisco88"))
#### 3.9 Location ####
# Group regions according to Standard Statistical Regions (SSR)
location1 = location %>% 
  mutate(
    region = factor(region, 
                    labels = c("Abroad", "Yorkshire and Humberside", "East Anglia", "East Anglia", "East Midlands", 
                               "West Midlands", "North", "East Midlands", "North West", "Abroad", "Scotland", "South East", 
                               "South East", "South West", "Wales", "West Midlands", "Yorkshire and Humberside"))
  )
# Interpolate location
location2 = location1 %>% 
  select(id, age, region) %>% 
  group_by(id) %>%
  mutate_at(vars(region), function(x){x %>% na.locf(., na.rm = FALSE)}) %>% 
  ungroup() %>% 
  mutate_if(is.character, ~replace(., . == "NA", NA))
# Location at 16 years old
location3 = location2 %>% 
  subset(age == 16) %>% select(id, region_16yo = region)
#### MERGE ####
cohort_data = gender %>% 
  merge(., hqual2, by = "id", all = TRUE) %>% 
  merge(., parinc2, by = "id", all = TRUE) %>% 
  merge(., parent_educ, by = "id", all = TRUE) %>% 
  merge(., parent_occ2, by = "id", all = TRUE) %>% 
  merge(., parage, by = "id", all = TRUE) %>% 
  merge(., pay3, by = "id", all = TRUE) %>% 
  merge(., family2, by = "id", all = TRUE) %>% 
  merge(., location3, by = "id", all = TRUE) %>% 
  merge(., sweep_last, by = "id", all = TRUE) %>% 
  # Assign to cohort
  mutate(cohort = ifelse(substr(id, 1, 1) == "B", "BCS", "NCDS") %>% factor(levels = c("NCDS", "BCS"))) %>% 
  # Reorder variables
  select(
    cohort, id, age_LS = age,
    # Fixed
    sex, educ, hqual, 
    # Parents and Family
    pinc, pinc10, pinc16, pactiv, psc, pisco88,
    starts_with("age_lefteduc"), starts_with("educ_"),
    age_mother, age_father, starts_with("HH"), starts_with("SIB"),
    region_16yo,
    # Other periods
    contains("pay")
  )
#### SAVE ####
write.csv(cohort_data, file.path(loc_data, "cohort_data.csv"), row.names = FALSE)
write.csv(acthist6, file.path(loc_data, "cohort_acthist.csv"), row.names = FALSE)
write.csv(first_job, file.path(loc_data, "first_job.csv"), row.names = FALSE)
