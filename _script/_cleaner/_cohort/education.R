#### 0 INITIALIZATION ####
source(file.path("_script", "init.R"))
#### 1 LOAD DATA ####
#### 1.1 NCDS ####
ncds_educ = read.dta(file.path(loc_BOTH_educ, "ncdsqualificationshistory.dta")) %>% # Load data
  setnames(., tolower(names(.))) %>% # Variables names in lowercase
  select(id = ncdsid, everything()) %>% # Rename id
  select(-cmsex, -starts_with("ind_"), -starts_with("edflag"), -starts_with("misy")) # Remove useless variables
#### 1.2 BCS ####
bcs_educ = read.dta(file.path(loc_BOTH_educ, "bcs70qualificationshistory.dta")) %>% # Load data
  setnames(., tolower(names(.))) %>% # Variables names in lowercase
  select(id = bcsid, everything()) %>% # Rename id
  select(-cmsex, -starts_with("ind_"), -starts_with("edflag"), -starts_with("misy")) # Remove useless variables
#### 2 TREATMENT ####
#### 2.1 NCDS ####
#### 2.1.1 LEFT SCHOOL ####
ncds_lefteduc = ncds_educ %>% 
  select(id, contains("left")) %>% # Select left school variables 
  mutate(date_left_educ = dmy("01011900") + months(datelefted)) %>% # Compute date left school
  select(id, date_left_educ, age_left_educ = agelefted) # Select and rename variables
#### 2.1.2 HIGHEST QUALIFICATION ####
ncds_hqual = ncds_educ %>% 
  select(id, starts_with("hi")) %>% # Select highest qualifications
  mutate_if(is.factor, as.character) %>% # Change factor to character
  setDT %>% melt(id.var = "id") %>% # Melt data (from wide to long)
  tidyr::separate(variable, c("variable", "period"), "(?<=[a-z])(?=[0-9])") %>% # Split variable and period
  mutate(value = value %>% ifelse(. %in% c("-1", "missing"), NA, .)) %>% # NA values
  setDT %>% dcast(id + period ~ variable) %>% # Cast data (from long to wide)
  subset(!is.na(hialldate)) %>% # Remove NA
  mutate(hialldate = dmy("01011900") + months(as.numeric(hialldate))) %>% # Compute date
  select(id, date = hialldate, year = hiallyr,  hqual = hialltyp, hqual_ac = hiedtyp, hqual_oq = hioqtyp, hqual_vq = hivqtyp, hqual_vqoq = hivqoqtyp) %>% # Rename and select variables
  arrange(id, date) %>% # Arrange by id and date
  merge(ncds_lefteduc, ., all.x = TRUE) %>% # Merge with left educ
  mutate_at(vars(contains("date")), as.character) %>% # Change date to character
  mutate(
    date = date %>% ifelse(is.na(.), date_left_educ, .), # Assign left educ date when date is missing
    year = year %>% ifelse(is.na(.), year(date_left_educ), .), # Assign year left educ when year is missing
    hqual = hqual %>% ifelse(is.na(.), 1, .), # Missing highest qualification becomes no qualifications
    hqual_ac = hqual_ac %>% ifelse(is.na(.), "Less than O level", .), # Missing academic hqual becomes min hqual
    hqual_vq = hqual_vq %>% ifelse(is.na(.), "Level 0", .), # Missing vocational hqual becomes min hqual
    hqual_oq = hqual_oq %>% ifelse(is.na(.), "Level 0", .), # Missing occupational hqual becomes min hqual
    hqual_vqoq = hqual_vqoq %>% ifelse(is.na(.), "Level 0", .), # Missing vocational/occupational hqual becomes min hqual
    ) %>% 
  mutate_at(vars(contains("date")), ymd) %>% # Date in ymd format
  mutate(
    # Labels for highest qualifications
    hqual = factor(hqual, labels = c("No qualifications", "Sub secondary", "Lower secondary–low performance",
                                     "Lower secondary–high performance", "Higher secondary", "Lower tertiary", 
                                     "Higher tertiary", "Postgraduate")), 
    # Labels for highest academic qualifications
    hqual_ac = hqual_ac %>% 
      factor(levels = c("Less than O level", "Less than 5 O levels", "5+ O levels", "1 A level and less than 5 O levels",
                        "1 A level and 5+ O levels", "2+ A levels and less than 5 O levels", "2+ A levels and 5+ O levels", 
                        "Sub degrees", "Degree: lower grade", "Degree: first+upper second grade", "Higher degree")),
    hqual_vq = factor(hqual_vq), # Factor for highest vocational qualification
    hqual_oq = factor(hqual_oq), # Factor for highest occupational qualification
    hqual_vqoq = factor(hqual_vqoq), # Factor for highest vocational/occupational qualification
    ) %>%
  # Highest qualifications in numeric format
  mutate(
    hqual_num = as.numeric(hqual) -1,
    hqual_ac_num = as.numeric(hqual_ac) -1,
    hqual_vq_num = as.numeric(hqual_vq) -1,
    hqual_oq_num = as.numeric(hqual_oq) -1,
    hqual_vqoq_num = as.numeric(hqual_vqoq) -1,
    ) %>%
  select(-contains("left_educ")) # Remove left educ variable
#### 2.1.3 DIPLOMA ####
ncds_diploma = ncds_educ %>% 
  select(-contains("left"), -starts_with("hi")) %>% # Remove left and highest qual variables
  mutate_if(is.factor, as.character) %>%# Factor variables as character
  setDT %>% melt(id.var = "id") %>% # Melt data (from wide to long)
  tidyr::separate(variable, c("variable", "period"), "(?<=[a-z])(?=[0-9])") %>% # Split variable and period
  separate(variable, c("var1", "var2"), sep = c("(?=typ|yr|year|date)"), fill = "right") %>% # Separate variable 
  mutate(value = value %>% ifelse(. %in% c("-1", "missing"), NA, .)) %>% # Remove missing
  subset(complete.cases(.)) %>% # Complete cases
  setDT %>% dcast(id + period + var1 ~ var2) %>% # Cast data (from long to wide)
  mutate_at(vars(c("period", "date", "year")), as.numeric) %>% # Period, date and year in numeric
  mutate(date = dmy("01011900") + months(date)) %>% # Compute date
  subset(complete.cases(.)) %>% # Remove missing
  arrange(id, period) %>% # Arrange by id and period
  select(id, date, year, variable = var1, diploma = typ) %>% # Select and rename variables
  unique(.) %>% # Remove duplicates
  setDT %>% dcast(id + date + year ~ variable, value.var = "diploma", fill = NA_character_, fun.aggregate = head, n = 1) # Cast data (from long to wide)
#### 2.2 BCS ####
#### 2.2.1 LEFT SCHOOL ####
bcs_lefteduc = bcs_educ %>% 
  select(id, contains("left")) %>% # Select left school variables
  subset(datelefted > 0) %>% # Remove missing
  mutate(date_left_educ = dmy("01011900") + months(datelefted)) %>% # Compute date left school
  select(id, date_left_educ, age_left_educ = agelefted) # Select and rename variables
#### 2.2.2 HIGHEST QUALIFICATION ####
bcs_hqual = bcs_educ %>% 
  select(id, starts_with("hi")) %>% # Select highest qualifications
  mutate_if(is.factor, as.character) %>% # Change factor to character
  setDT %>% melt(id.var = "id") %>% # Melt data (from wide to long)
  tidyr::separate(variable, c("variable", "period"), "(?<=[a-z])(?=[0-9])") %>% # Split variable and period
  mutate(value = value %>% ifelse(. %in% c("-1", "missing"), NA, .)) %>% # NA values
  setDT %>% dcast(id + period ~ variable) %>% # Cast data (from long to wide)
  subset(!is.na(hialldate)) %>% # Remove NA
  mutate(hialldate = dmy("01011900") + months(as.numeric(hialldate))) %>% # Compute date
  select(id, date = hialldate, year = hiallyr,  hqual = hialltyp, hqual_ac = hiedtyp, hqual_oq = hioqtyp, hqual_vq = hivqtyp, hqual_vqoq = hivqoqtyp) %>% # Rename and select variables
  arrange(id, date) %>% # Arrange by id and date
  merge(bcs_lefteduc, ., all.x = TRUE) %>% # Merge with left educ
  mutate_at(vars(contains("date")), as.character) %>% # Change date to character
  mutate(
    date = date %>% ifelse(is.na(.), date_left_educ, .), # Assign left educ date when date is missing
    year = year %>% ifelse(is.na(.), year(date_left_educ), .), # Assign year left educ when year is missing
    hqual = as.character(hqual) %>% ifelse(is.na(.), "1", .), # Missing highest qualification becomes no qualifications
    hqual_ac = as.character(hqual_ac) %>% ifelse(is.na(.), "1", .), # Missing academic hqual becomes min hqual
    hqual_vq = as.character(hqual_vq) %>% ifelse(is.na(.), "Level 0", .), # Missing vocational hqual becomes min hqual
    hqual_oq = as.character(hqual_oq) %>% ifelse(is.na(.), "Level 0", .), # Missing occupational hqual becomes min hqual
    hqual_vqoq = as.character(hqual_vqoq) %>% ifelse(is.na(.), "Level 0", .), # Missing vocational/occupational hqual becomes min hqual
  ) %>% 
  mutate_at(vars(contains("date")), ymd) %>% # Date in ymd format
  mutate(
    hqual = factor(hqual,
      levels = c("1", "Sub secondary", "Lower secondary-low performance", "Lower secondary-high performance", 
                 "Higher secondary", "Lower tertiary", "Higher tertiary", "Postgraduate"),
      labels = c("No qualifications", "Sub secondary", "Lower secondary–low performance", "Lower secondary–high performance",
                 "Higher secondary", "Lower tertiary", "Higher tertiary", "Postgraduate")),
    # Labels for highest academic qualifications
    hqual_ac = factor(hqual_ac, 
      labels = c("Less than O level", "Less than 5 O levels", "5+ O levels", "1 A level and less than 5 O levels",
                 "1 A level and 5+ O levels", "2+ A levels and less than 5 O levels", "2+ A levels and 5+ O levels",
                 "Sub degrees", "Degree: lower grade", "Degree: first+upper second grade", "Higher degree")),
    hqual_vq = factor(hqual_vq), # Factor for highest vocational qualification
    hqual_oq = factor(hqual_oq), # Factor for highest occupational qualification
    hqual_vqoq = factor(hqual_vqoq), # Factor for highest vocational/occupational qualification
  ) %>% 
  # Highest qualifications in numeric format
  mutate(
    hqual_num = as.numeric(hqual) -1,
    hqual_ac_num = as.numeric(hqual_ac) -1,
    hqual_vq_num = as.numeric(hqual_vq) -1,
    hqual_oq_num = as.numeric(hqual_oq) -1,
    hqual_vqoq_num = as.numeric(hqual_vqoq) -1,
  ) %>%
  select(-contains("left_educ")) # Remove left educ variable
#### 2.2.3 DIPLOMA ####
bcs_diploma = bcs_educ %>% 
  select(-contains("left"), -starts_with("hi")) %>% # Remove left and highest qual variables
  mutate_if(is.factor, as.character) %>%# Factor variables as character
  setDT %>% melt(id.var = "id") %>% # Melt data (from wide to long)
  tidyr::separate(variable, c("variable", "period"), "(?<=[a-z])(?=[0-9])") %>% # Split variable and period
  separate(variable, c("var1", "var2"), sep = c("(?=typ|yr|year|date)"), fill = "right") %>% # Separate variable 
  mutate(value = value %>% ifelse(. %in% c("-1", "missing"), NA, .)) %>% # Remove missing
  subset(complete.cases(.)) %>% # Complete cases
  setDT %>% dcast(id + period + var1 ~ var2) %>% # Cast data (from long to wide)
  mutate_at(vars(c("period", "date", "year")), as.numeric) %>% # Period, date and year in numeric
  mutate(date = dmy("01011900") + months(date)) %>% # Compute date
  subset(complete.cases(.)) %>% # Remove missing
  arrange(id, period) %>% # Arrange by id and period
  select(id, date, year, variable = var1, diploma = typ) %>% # Select and rename variables
  unique(.) %>% # Remove duplicates
  setDT %>% dcast(id + date + year ~ variable, value.var = "diploma", fill = NA_character_, fun.aggregate = head, n = 1) # Cast data (from long to wide)
#### 3 MERGE ####
## Left Education
left_school = rbind(ncds_lefteduc, bcs_lefteduc)
## Highest Qualification
hqual = rbind(ncds_hqual, bcs_hqual)
## Diploma
diploma = rbind(ncds_diploma, bcs_diploma)
#### 4 SAVE ####
## Left Education
write.csv(left_school, file.path(loc_cohort, "educ_leftschool.csv"), row.names = FALSE)
## Highest Qualification
write.csv(hqual, file.path(loc_cohort, "educ_hqual.csv"), row.names = FALSE)
## Diploma
write.csv(diploma, file.path(loc_cohort, "educ_diploma.csv"), row.names = FALSE)
