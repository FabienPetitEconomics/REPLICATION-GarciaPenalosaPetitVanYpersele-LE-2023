#### INITIALIZATION ####
# Init
source(file.path("_script", "init.R"))
#### LOAD DATA ####
### NCDS
## Age 00 to 16
ncds16 = read.dta(file.path(loc_NCDS_16, "ncds0123.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    sex = "n622", # 0-3D Sex of child
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 16, year = 1974) %>% 
  mutate(sex = sex %>% factor(levels = c("Male", "Female")))
## Age 23
ncds23 = read.dta(file.path(loc_NCDS_23, "ncds4.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    sex = "n622_4", # Sex of Cohort Member
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 23, year = 1981) %>% 
  mutate(sex = sex %>% factor(levels = c("Male", "Female")))
## Age 33
ncds33 = read.dta(file.path(loc_NCDS_33, "ncds5cmi.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    sex = "n622_5", # Sex of Cohort Member
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 33, year = 1991) %>% 
  mutate(sex = sex %>% factor(levels = c("Male", "Female")))
## Age 42
ncds42 = read.dta(file.path(loc_NCDS_42, "ncds6.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    sex = "n622_6", # Sex of Cohort Member
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 42, year = 2000) %>% 
  mutate(sex = sex %>% factor(levels = c("Male", "Female")))
## Age 47
ncds47 = read.dta(file.path(loc_NCDS_47, "ncds7.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    sex = "nd7sex", # Sex of Cohort Member
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 47, year = 2005) %>% 
  mutate(sex = sex %>% factor(levels = c("Male", "Female")))
## Age 50
ncds50 = read.dta(file.path(loc_NCDS_50, "ncds_2008_followup.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    sex = "n8cmsex", # [CMSEX] Cohort members current gender at swp8
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 50, year = 2008) %>% 
  mutate(sex = sex %>% factor(levels = c("Male", "Female")))
## Age 55
ncds55 = read.dta(file.path(loc_NCDS_55, "ncds_2013_flatfile.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    sex = "n9cmsex", # CM's sex
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 55, year = 2013) %>% 
  mutate(sex = sex %>% factor(levels = c("Male", "Female")))
### BCS
## Age 05
bcs05 = read.dta(file.path(loc_BCS_05, "bcs70_1975_developmental_history.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    sex = "var5503", # Sex
  ) %>% 
  subset(!(get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 05, year = 1975) %>% 
  select(id, age, year, sex) %>% 
  mutate(sex = sex %>% factor(labels = c("Male", "Female")))
## Age 10
bcs10 = read.dta(file.path(loc_BCS_10, "sn3723.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    sex = "sex10", # Child's sex
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 10, year = 1980) %>% 
  mutate(sex = sex %>% factor(levels = c("Male", "Female")))
## Age 16
bcs16 = read.dta(file.path(loc_BCS_16, "bcs7016x.dta")) %>%
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    sex = "sex86", # Sex of cohort member
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 16, year = 1986) %>% 
  mutate(sex = sex %>% factor(labels = c("Male", "Female")))
## Age 26
bcs26 = read.dta(file.path(loc_BCS_26, "bcs96x.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    "sex", # Sex from Address File
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 26, year = 1996) %>%
  mutate(sex = sex %>% factor(levels = c("Male", "Female")))
## Age 30
bcs30 = read.dta(file.path(loc_BCS_30, "bcs2000.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    sex = "cmsex", # Interviewer Check On CM Gender 
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 30, year = 2000) %>% 
  mutate(sex = sex %>% factor(levels = c("Male", "Female")))
## Age 34
bcs34 = read.dta(file.path(loc_BCS_34, "bcs_2004_followup.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    sex = "bd7sex", # (Derived) Cohort member's sex (checked against address database)
  )  %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 34, year = 2004) %>% 
  mutate(sex = sex %>% factor(levels = c("Male", "Female")))
## Age 38
bcs38 = read.dta(file.path(loc_BCS_38, "bcs_2008_followup.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    sex = "bd8sex", # (Derived) Cohort member's sex (final)
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 38, year = 2008) %>% 
  mutate(sex = sex %>% factor(levels = c("Male", "Female")))
## Age 42
bcs42 = read.dta(file.path(loc_BCS_42, "bcs70_2012_flatfile.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    sex = "b9cmsex",
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 42, year = 2012) %>% 
  mutate(sex = sex %>% factor(levels = c("Male", "Female")))
### MERGE
## NCDS
ncds_gender = Reduce(function(x,y) rbind(x, y), lapply(ls(pattern = "ncds"), get))
rm(ncds16, ncds23, ncds33, ncds42, ncds47, ncds50, ncds55)
## BCS
bcs_gender = Reduce(function(x,y) rbind(x, y), lapply(ls(pattern = "bcs"), get))
rm(bcs05, bcs10, bcs16, bcs26, bcs30, bcs34, bcs38, bcs42)
## Rbind BOTH
gender = rbind(ncds_gender, bcs_gender)
#### TREATMENT ####
### Slice
## Function
treat_gender = function(df){
  df2 = df %>% 
    arrange(id, desc(age)) %>% 
    subset(complete.cases(.)) %>% 
    group_by(id) %>% 
    slice(1) %>% 
    select(id, sex)
  return(df2)
}
## Apply function
gender = gender %>% treat_gender()
#### SAVE ####
write.csv(gender, file.path(loc_cohort, "gender.csv"), row.names = FALSE)