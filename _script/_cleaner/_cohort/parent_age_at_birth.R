#### 0 INITIALIZATION ####
source(file.path("_script", "init.R"))
#### 1 LOAD DATA ####
#### 1.1 NCDS ####
## Age 00-16
ncds16 = read.dta(file.path(loc_NCDS_16, "ncds0123.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    age_mother = "n553", # 0 Mother's age last birthday,in years
    age_father = "n494", # 0 Husband's age in years,1958
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate_at(vars(starts_with("age")),
            function(x) ifelse(x  < 0, NA, x))

#### 1.2 BCS ####
## Age 05
bcs05 = read.dta(file.path(loc_BCS_05, "f699b.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    age_mother = "e008", # Age of Mother
    age_father = "e009", # Age of Father
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate_at(vars(starts_with("age")), function(x) ifelse((x-5) < 0, NA, x-5))
#### 2 RBIND ####
parent_age_at_birth = rbind(ncds16, bcs05)
#### 3 SAVE ####
write.csv(parent_age_at_birth, file.path(loc_cohort, "parent_age_at_birth.csv"), row.names = FALSE)