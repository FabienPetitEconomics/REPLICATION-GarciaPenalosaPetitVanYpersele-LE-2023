#### 0 INITIALIZATION ####
source(file.path("_script","init.R"))
#### 1.0 ALL SWEEPS ####
#### 1.1 NCDS ####
# Load activity history
ncds_sweep = read_dta(file.path(loc_NCDS_acthist, "ncds_activity_histories.dta")) %>% # Read csv data
  setnames(., tolower(names(.))) %>% # All variable names in lowercase
  select(id = "ncdsid", contains("wsweep"), starts_with("intmon"), starts_with("intyr")) %>% # Select data
  unique # Remove duplicates
# Treatment
ncds_sweep1 = ncds_sweep %>% setDT %>% melt(id.var = "id") %>% # Melt data (from wide to long)
  tidyr::separate(variable, c("variable", "age"), sep = "(?<=[a-z])(?=[0-9])") %>% # Split variable and periods
  mutate(
    variable = factor(variable, labels = c("int_month", "int_year", "actdata")), # Variable are month, year and whether the individual was present
    age = factor(age, labels = c(23, 33, 42, 46, 50, 55, 37)) %>% as.character %>% as.integer # Age according to periods
    ) %>%
  setDT %>% dcast.data.table(id + age ~ variable) %>% # Cast data (from long to wide)
  mutate_at(vars(starts_with("int")), ~ ifelse(. < 0, NA, .)) %>% # Negative value for date is NA
  mutate(
    actdata = factor(actdata, labels = c("ActData", "No ActData", "Out")), # Factor if individual was present at the interview and whether activitity data are register or not
    present = ifelse(actdata == "Out", 0, 1), # Dummy variable whether the individual was present at the interview
    int_date = ifelse(is.na(int_month), NA, paste0("01-", sprintf("%02d", int_month),"-", int_year)) %>% dmy(.), # Interview date in dmy format (DD/MM/YYYY)
  ) %>% 
  select(id, age, int_date, actdata, present) # Select variables
# Construct full interview date that takes the average interview date when not interviewed
ncds_sweep2 = ncds_sweep1 %>%  group_by(age) %>% # Group by age
  mutate(int_date.mean = mean.Date(int_date, na.rm = T) %>% format("%Y-%m-01") %>% ymd(.)) %>% # Mean interview date
  mutate(int_date.full = as.character(int_date) %>%  ifelse(is.na(.), as.character(int_date.mean), .) %>% ymd(.)) %>% # Apply for NA date
  select(id, age, int_date, int_date.full, actdata, present) # Select variables
#### 1.2 BCS ####
# Load activity history
bcs_sweep = read_dta(file.path(loc_BCS_acthist,"bcs70_activity_histories.dta")) %>% # Read csv data
  setnames(., tolower(names(.))) %>% # All variable names in lowercase
  select(id = "bcsid", contains("wsweep"), starts_with("intmon"), starts_with("intyr")) %>% # Select data
  unique # Remove duplicates
# Treatment
bcs_sweep1 = bcs_sweep %>% setDT %>% melt(id.var = "id") %>% # Melt data (from wide to long)
  tidyr::separate(variable, c("variable", "age"), sep = "(?<=[a-z])(?=[0-9])") %>% # Split variable and periods
  mutate(
    variable = factor(variable, labels = c("int_month", "int_year", "actdata")), # Variable are month, year and whether the individual was present
    age = factor(age, labels = c(26, 30, 34, 38, 42)) %>% as.character %>% as.integer # Age according to periods
  ) %>% 
  setDT %>% dcast.data.table(id + age ~ variable) %>% # Cast data (from long to wide)
  mutate_at(vars(starts_with("int")), ~ ifelse(. < 0, NA, .)) %>% # Negative value for date is NA
  mutate(
    actdata = factor(actdata, labels = c("ActData", "No ActData", "Out")), # Factor if individual was present at the interview and whether activitity data are register or not
    present = ifelse(actdata == "Out", 0, 1), # Dummy variable whether the individual was present at the interview
    int_date = ifelse(is.na(int_month), NA, paste0("01-", sprintf("%02d", int_month),"-", int_year)) %>% dmy(.), # Interview date in dmy format (DD/MM/YYYY)
  ) %>% 
  select(id, age, int_date, actdata, present) # Select variables
# Construct full interview date that takes the average interview date when not interviewed
bcs_sweep2 = bcs_sweep1 %>%  group_by(age) %>% # Group by age
  mutate(int_date.mean = mean.Date(int_date, na.rm = T) %>% format("%Y-%m-01") %>% ymd(.)) %>% # Mean interview date
  mutate(int_date.full = as.character(int_date) %>%  ifelse(is.na(.), as.character(int_date.mean), .) %>% ymd(.)) %>% # Apply for NA date
  select(id, age, int_date, int_date.full, actdata, present) # Select variables
#### 2.0 LAST SWEEP ####
#### 2.1 NCDS ####
ncds_last = ncds_sweep2 %>% arrange(id, desc(age)) %>% # Arrange data by id and age (from the oldest to the youngest)
  filter(present == 1) %>% # Keep only when individuals were present
  group_by(id) %>% slice(1) %>% # Slice to keep only the last interview
  select(id, age) # Select id and age
#### 2.1 BCS ####
bcs_last = bcs_sweep2 %>% arrange(id, desc(age)) %>% # Arrange data by id and age (from the oldest to the youngest)
  filter(present == 1) %>% # Keep only when individuals were present
  group_by(id) %>% slice(1) %>% # Slice to keep only the last interview
  select(id, age) # Select id and age
#### 3 RBIND ####
last_sweep = rbind(ncds_last, bcs_last)
reponse_sweep = rbind(ncds_sweep2, bcs_sweep2)
#### 4 SAVE ####
### Last Sweep
write.csv(last_sweep, file.path(loc_cohort, "sweep_last.csv"), row.names = FALSE)
### Response Sweep
write.csv(reponse_sweep, file.path(loc_cohort, "sweep_response.csv"), row.names = FALSE)