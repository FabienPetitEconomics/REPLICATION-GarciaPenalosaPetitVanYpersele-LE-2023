#### 0/ INITIALIZATION ####
source("_script/init.R")
# Function
attr_to_names <- function(df, var.exception = NULL){
  # Use labels as names
  
  if(!is.null(var.exception)){
    names.target <- names(df)[-var.exception]
    var.name.all <- names(df)[var.exception]
  } else {names.target = names(df)
  var.name.all <- c()}
  for(var.name in names.target){
    var.name.all <- c(var.name.all, attributes(df[[var.name]])$label)
  }
  var.name.all <- tolower(var.name.all)
  names(df) <- var.name.all
  # Remove attributes
  df <- data.frame(lapply(df, function(x) { attributes(x) <- NULL; x }))
  return(df)
}
#### 1/ LFS 1975-1991 ####

# #### LFS 1975 [OUT] ####
# lfs75 = read_dta(file.path(loc_LFS_1975, "lfs75.dta")) %>% 
#   attr_to_names %>% 
#   select(hhid, pid, #HH id and person id in HH
#        uresrega, # Region
#        sexa, # Sex
#        relationa, # Relation to head of HH
#        agea, # Age
#        econpoa, # Economic position and status
#        kosa, # Key occupations in main job
#        kosgroa, # Key occupations groups main job
#        kosonea, # Key occupations one year ago
# )
# #### LFS 1977 [OUT]####
# lfs77 = read_dta(file.path(loc_LFS_1977, "lfs77.dta")) %>% 
#   attr_to_names %>% 
#   select(hhid, pid, #HH id and person id in HH
#          uresregb, # Region
#          sexb, # Sex
#          relationb, # Relation to head of HH
#          ageb, # Age
#          econpob, # Economic position and status
#          kosb, # Key occupations in main job
#          kosgrob, # Key occupations groups main job
#          kosoneb, # Key occupations one year ago
#   )
#### LFS 1983 ####
lfs83 = read_dta(file.path(loc_LFS_1983, "lfs83.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  attr_to_names(var.exception = c(1:2)) %>% 
  select(hhid = ark1, pid = ark2,
         location = uresrgie, # Region
         sex = sexie, # Sex
         relation = relatie, # Relationship to head of HH
         age = ageie, # Age
         activity = econpoe, # Economic position
         hours = hrnormie, # Hours normally worked
         isco68 = ekos, # ISCO-68
         kos551 = kose, # Key occupations in main activity last week
         kos164 = koscone, # Key occupations (condensed) in main activity last week
         kos551_lastyear = kosonee, # Key occupations one year ago
         kos164_lastyear = koscnone, # Key occupations (condensed) one year ago
  ) %>% 
  # Remove FULL NA
  subset(!is.na(sex)) %>% 
  # Variable treatment
  mutate(
    year = 1983,
    location = factor(location,
                      labels = c("North", "Yorkshire and the Humber", "East Midlands", "East Anglia",
                                 "South East", "South West", "West Midlands",
                                 "North West", "Wales", "Scotland", "Northern Ireland")),
    sex = factor(sex, labels = c("Male", "Female")),
    relation = factor(relation,
                      labels = c("Head", "Spouse", "Child", "Ascendant ", 
                                 "Other relative", "Other relative", "Other relative", 
                                 "Other relative", "Other")),
    hours = ifelse(hours > 98, NA, hours),
    activity = factor(activity,
                      labels = c(rep("Self", 2), rep("Emp", 7), rep("Unemp", 2),
                                 "Sick", "Holiday", "Retired", "Home", rep("Scheme", 2),
                                 "Sick", rep("Inactive", 5), rep("Educ", 2), "Child")),
    kos551 = ifelse(kos551 == 551, NA, kos551),
    isco68 = ifelse(isco68 == 0, NA, isco68),
    kos550 = NA, kos1640 = NA, kos164 = NA, occ351 = NA,
    # kos164 = ifelse(kos164 == 164, NA, kos164),
    # kos551_lastyear = ifelse(kos551_lastyear == 551, NA, kos551_lastyear),
    # kos164_lastyear = ifelse(kos164_lastyear == 164, NA, kos164_lastyear),
  ) %>% 
  # Pick variables
  select(year, hhid, pid, location, relation, sex, age, activity, hours, 
         kos550, kos551, kos1640, kos164, occ351, isco68) %>% 
  arrange(hhid, pid, relation)
#### KOS551 to ISCO68 ####
# Translation table from KOS551 into ISCO68 using LFS83
# Pr(ISCO68 | Sex & KOS551)
kos551_isco68 = lfs83 %>% 
  subset(!is.na(kos551) & !is.na(isco68)) %>% 
  select(kos551, sex, isco68) %>% 
  count(kos551, sex, isco68, name = "n") %>% 
  add_count(sex, kos551, wt = n, name = "N") %>% 
  mutate(prop = n/N) %>% 
  arrange(kos551, sex, -prop) %>% 
  group_by(kos551, sex) %>% slice(1) %>% ungroup
#### KOS550 to ISCO68 ####
kos550_isco68 = kos551_isco68 %>% 
  subset(kos551 != 550) %>% 
  rename(kos550 = kos551)
#### LFS 1979 ####
lfs79 = read_dta(file.path(loc_LFS_1979, "lfs79.dta")) %>%
  attr_to_names(., var.exception = c(1,2)) %>% # Remove attributes
  select(hhid = ark1, pid = ark2, #HH id and person id in HH
         uresregc, # Region
         sexc, # Sex
         relationc, # Relation to head of HH
         agec, # Age
         econpoc, # Economic position and status
         hrsnormc, # Hours normally worked
         kosc, # Key occupations in main acivity last week
         koscondc, # Key occupations (condensed) in main activity last week
         kosonec, # Key occupations one year ago
         koscononc, # Key occupations (condensed) one year ago
  ) %>%
  mutate(
    year = 1979,
    location = factor(uresregc, levels = c(1:12),
      labels = c("North", "Yorkshire and the Humber", "North West", "East Midlands",
                 "West Midlands", "East Anglia", rep("South East", 2),
                 "South West", "Wales", "Scotland", "Northern Ireland")),
    sex = factor(sexc, labels = c("Male", "Female")),
    relation = factor(relationc,
      labels = c("Head", "Spouse", "Child", "Ascendant", "Other relative", "Other", NA)),
    age = agec,
    activity = factor(econpoc, labels = c("Self", "Self", "Emp", "Emp",
      "Emp", "Unemp", "Unemp", "Sick", "Retired", "Home",
      "Inactive", "Educ", "Child")),
    hours = ifelse(hrsnormc >= 99, NA, hrsnormc),
    kos550 = ifelse(kosc == 550, NA, kosc),
    kos164 = ifelse(koscondc == 164, NA, koscondc),
    kos551 = NA, kos1640 = NA, kos164 = NA, occ351 = NA,
    # kos550_lastyear = ifelse(kosonec == 550, NA, kosonec),
    # kos164_lastyear = ifelse(koscononc == 164, NA, koscononc),
  ) %>% 
  # Merge with translation tables to get ISCO68
  merge(select(kos550_isco68, kos550, sex, isco68), by = c("sex", "kos550"), all.x = T) %>% 
  # Pick variables
  select(year, hhid, pid, location, relation, sex, age, activity, hours, 
         kos550, kos551, kos1640, kos164, occ351, isco68) %>% 
  arrange(hhid, pid, relation)
#### LFS 1981 ####
lfs81 = read_dta(file.path(loc_LFS_1981, "lfs81.dta")) %>% 
  setnames(., tolower(names(.))) %>% # Names to lower
  select(location = uresreg, # Region
         sex, # Sex
         relation = relat, # Relationship to head of HH
         age, # Age
         activity = usitemp, # Usual situation with regard to economic 
         hours, # Hours normally worked
         kos551 = kos, # Key occupations in main activity last week
         # kos164 = koscon, # Key occupations (condensed) in main activity last week
         # isco68 = kosisco, # Key occupations main activity last week
         # kos551_lastyear = kosone, # Key occupations one year ago
         # kos164_lastyear = koscnon, # Key occupations (condensed) one year ago
  ) %>% 
  # Create ID for Household and Person in HH ID
  subset(relation != -99.99) %>% 
  mutate(id = seq.int(nrow(.))) %>% 
  merge(select(subset(., relation == 1), id) %>% 
          mutate(hhid = seq.int(nrow(.))), by = "id", all = T) %>% 
  select(-id) %>% 
  mutate(hhid = na.locf0(hhid)) %>% 
  group_by(hhid) %>% mutate(pid = 1:n()) %>% ungroup %>% 
  select(hhid, pid, everything()) %>% 
  # Variable treatment
  mutate(
    year = 1981,
    location = factor(location, 
      labels = c("North", "Yorkshire and the Humber", "East Midlands", "East Anglia",
                 rep("South East", 2), "South West", "West Midlands",
                 "North West", "Wales", "Scotland", "Northern Ireland")),
    sex = factor(sex, labels = c("Male", "Female")),
    relation = factor(relation,
      labels = c("Head", "Spouse", "Child", "Ascendant", "Ascendant", 
                 "Other relative", "Other relative", "Other")),
    hours = ifelse(hours > 98, NA, hours),
    activity = factor(activity, levels = c(1:8),
      labels = c("Self", "Emp", "Unemp", "Educ", 
                 "Educ", "Retired", "Home", "Sick")) %>% 
      as.character(.) %>% ifelse(age < 16, "Child", .) %>% factor(.),
    kos550 = NA, kos1640 = NA, kos164 = NA, occ351 = NA,
    kos551 = ifelse(kos551 == 551, NA, kos551),
    # isco68 = ifelse(isco68 == 83, NA, isco68),
    ) %>% 
  # Merge with translation tables to get ISCO68
  merge(select(kos551_isco68, kos551, sex, isco68), by = c("sex", "kos551"), all.x = T) %>% 
  # Pick variables
  select(year, hhid, pid, location, relation, sex, age, activity, hours, 
         kos550, kos551, kos1640, kos164, occ351, isco68) %>% 
  arrange(hhid, pid, relation)
# Scale on 83 for ISCO68. To implement the good scale:
# 1888_userguide_v2_p1.pdf in loc_LFS_1981 (pdf page 105)
#### LFS 1984 ####
lfs84 = read_dta(file.path(loc_LFS_1984, "lfs1984.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = freg, # Region
    sex = fsex, # Sex
    relation = frelat, # Relationship to head of HH
    age = fage, # Age
    activity = econpof , # Economic activity in reference week
    hours = totushrs, # Total number of hours usually worked
    # Doesn't have ISCO-68
    kos551 = kosf, # Key occupations in main activity last week
    kos551_lastyear = kosonef, # Key occupations one year ago
    ) %>% 
  # Create ID for Household and Person in HH ID
  mutate(id = seq.int(nrow(.))) %>% 
  merge(select(subset(., relation == 1), id) %>% 
          mutate(hhid = seq.int(nrow(.))), by = "id", all = T) %>% 
  select(-id) %>% 
  mutate(hhid = na.locf0(hhid)) %>% 
  group_by(hhid) %>% mutate(pid = 1:n()) %>% ungroup %>% 
  select(hhid, pid, everything()) %>% 
  # Variable treatment
  mutate(
    year = 1984,
    location = factor(location,
      labels = c("North", "Yorkshire and the Humber", "East Midlands", "East Anglia",
                 "South East", "South West", "West Midlands",
                 "North West", "Wales", "Scotland", "Northern Ireland")),
    sex = factor(sex, labels = c("Male", "Female")),
    relation = factor(relation,
      labels = c("Head", "Spouse", "Child", "Ascendant", "Other relative", "Other")),
    hours = ifelse(hours > 98 | hours < 0, NA, hours),
    activity = factor(activity,
      labels = c(rep("Emp", 3), rep("Self", 3), "Emp", rep("Scheme",2),
                 "Unemp", "Sick", "Inactive", rep("Unemp", 2), rep("Educ", 3),                   
                 "Sick", "Home", "Retired", rep("Inactive", 5), "Child")),
    kos551 = ifelse(kos551 == 551, NA, kos551), 
    kos550 = NA, kos1640 = NA, kos164 = NA, occ351 = NA,
    # kos551_lastyear = ifelse(kos551_lastyear == 551, NA, kos551_lastyear),
    ) %>% 
  # Merge with translation tables to get ISCO68
  merge(select(kos551_isco68, kos551, sex, isco68), by = c("sex", "kos551"), all.x = T) %>% 
  # Pick variables
  select(year, hhid, pid, location, relation, sex, age, activity, hours, 
         kos550, kos551, kos1640, kos164, occ351, isco68) %>% 
  arrange(hhid, pid, relation)
#### LFS 1985 ####
lfs85 = read_dta(file.path(loc_LFS_1985, "lfs1985.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = ereg, # Region
         sex = esex, # Sex
         relation = erelat, # Relationship to head of HH
         age = age, # Age
         activity = econacrg, # Economic activity in reference week
         hours = totushrs, # Total number of hours usually worked
         # kos551 = kosf, # Key occupations in main activity last week
         isco68 = ekos, # ISCO  68 Occupation
         kos164 = koscong, # Occupation (condensed) # 164
         kos1640 = kosg, # OPCS 1980 OCCUPATION CODES # 164.1
         occ351 = occcode, # OCCUPATION CODE // 351
    ) %>% 
  # Create ID for Household and Person in HH ID
  mutate(id = seq.int(nrow(.))) %>% 
  merge(select(subset(., relation == 1), id) %>% 
          mutate(hhid = seq.int(nrow(.))), by = "id", all = T) %>% 
  select(-id) %>% 
  mutate(hhid = na.locf0(hhid)) %>% 
  group_by(hhid) %>% mutate(pid = 1:n()) %>% ungroup %>% 
  select(hhid, pid, everything()) %>% 
  # Variable treatment
  mutate(
    year = 1985,
    location = factor(location,
      labels = c("North", "Yorkshire and the Humber", "East Midlands", "East Anglia",
                 "South East", "South West", "West Midlands", "North West", "Wales",
                 "Scotland", "Northern Ireland")),
    sex = factor(sex, labels = c("Male", "Female")),
    relation = factor(relation,
      labels = c("Head", "Spouse", "Child", "Ascendant", "Other relative", "Other")),
    hours = ifelse(hours > 98 | hours < 0, NA, hours),
    activity = factor(activity, levels = c(1:52),
      labels = c(rep("Emp", 9), rep("Self", 9), rep("Emp", 3), rep("Unemp", 15),
        rep("Educ", 6), "Retired", "Sick", "Home", "Retired", rep("Inactive", 5),
        "Child")),
    isco68 = ifelse(isco68 %in% c("", "00", "NA"), NA, isco68),
    kos550 = NA, kos551 = NA,
    kos164 = ifelse(kos164 < 0 | kos164 == 999, NA, kos164),
    kos1640 = round(kos1640, 1) %>% ifelse(. < 0 | . == 999, NA, .),
    occ351 = ifelse(occ351 < 0, NA, occ351),
  ) %>% 
  # Pick variables
  select(year, hhid, pid, location, relation, sex, age, activity, hours, 
         kos550, kos551, kos1640, kos164, occ351, isco68) %>% 
  arrange(hhid, pid, relation)
#### LFS 1986 ####
lfs86 = read_dta(file.path(loc_LFS_1986, "lfs1986.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = ereg, # Region
         sex = esex, # Sex
         relation = erelat, # Relationship to head of HH
         age = age, # Age
         activity = ecarh, # Economic activity in reference week
         hours = totushrs, # Total number of hours usually worked
         isco68 = ekos, # ISCO  68 Occupation
         kos164 = kosconh, # Occupation (condensed) # 164
         kos1640 = kosh, # OPCS 1980 OCCUPATION CODES # 164.1
         occ351 = occcode, # OCCUPATION CODE // 351
  ) %>% 
  # Create ID for Household and Person in HH ID
  mutate(id = seq.int(nrow(.))) %>% 
  merge(select(subset(., relation == 1), id) %>% 
          mutate(hhid = seq.int(nrow(.))), by = "id", all = T) %>% 
  select(-id) %>% 
  mutate(hhid = na.locf0(hhid)) %>% 
  group_by(hhid) %>% mutate(pid = 1:n()) %>% ungroup %>% 
  select(hhid, pid, everything()) %>% 
  # Variable treatment
  mutate(
    year = 1986,
    location = factor(location,
                      labels = c("North", "Yorkshire and the Humber", "East Midlands", "East Anglia",
                                 "South East", "South West", "West Midlands", "North West", "Wales",
                                 "Scotland", "Northern Ireland")),
    sex = factor(sex, labels = c("Male", "Female")),
    relation = factor(relation,
                      labels = c("Head", "Spouse", "Child", "Ascendant", "Other relative", "Other")),
    hours = ifelse(hours > 98 | hours < 0, NA, hours),
    activity = factor(activity, levels = c(1:28),
      labels = c("Emp", "Self", "Emp", rep("Scheme", 6), rep("Unemp", 5), rep("Educ", 4), 
                 "Retired", "Sick", "Home", "Retired", rep("Inactive", 5), "Child")),
    isco68 = ifelse(isco68 %in% c("", "00", "NA"), NA, isco68),
    kos550 = NA, kos551 = NA,
    kos164 = ifelse(kos164 < 0 | kos164 == 999, NA, kos164),
    kos1640 = round(kos1640, 1) %>% ifelse(. < 0 | . == 999, NA, .),
    occ351 = ifelse(occ351 < 0, NA, occ351),
  ) %>% 
  # Pick variables
  select(year, hhid, pid, location, relation, sex, age, activity, hours, 
         kos550, kos551, kos1640, kos164, occ351, isco68) %>% 
  arrange(hhid, pid, relation)
#### LFS 1987 ####
lfs87 = read_dta(file.path(loc_LFS_1987, "lfs1987.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = ereg, # Region
         sex = esex, # Sex
         relation = erelat, # Relationship to head of HH
         age = age, # Age
         activity = ecacj, # Economic activity in reference week
         hours = totushrs, # Total number of hours usually worked
         isco68 = ekos, # ISCO  68 Occupation
         kos164 = kosconj, # Occupation (condensed) # 164
         kos1640 = kosj, # OPCS 1980 OCCUPATION CODES # 164.1
         occ351 = occcode, # OCCUPATION CODE // 351
  ) %>% 
  # Create ID for Household and Person in HH ID
  mutate(id = seq.int(nrow(.))) %>% 
  merge(select(subset(., relation == 1), id) %>% 
          mutate(hhid = seq.int(nrow(.))), by = "id", all = T) %>% 
  select(-id) %>% 
  mutate(hhid = na.locf0(hhid)) %>% 
  group_by(hhid) %>% mutate(pid = 1:n()) %>% ungroup %>% 
  select(hhid, pid, everything()) %>% 
  # Variable treatment
  mutate(
    year = 1987,
    location = factor(location,
                      labels = c("North", "Yorkshire and the Humber", "East Midlands", "East Anglia",
                                 "South East", "South West", "West Midlands", "North West", "Wales",
                                 "Scotland", "Northern Ireland")),
    sex = factor(sex, labels = c("Male", "Female")),
    relation = factor(relation,
                      labels = c("Head", "Spouse", "Child", "Ascendant", "Other relative", "Other")),
    hours = ifelse(hours > 98 | hours < 0, NA, hours),
    activity = factor(activity, levels = c(1:28),
                      labels = c("Emp", "Self", "Emp", rep("Scheme", 6), rep("Unemp", 5), rep("Educ", 4), 
                                 "Retired", "Sick", "Home", "Retired", rep("Inactive", 5), "Child")),
    isco68 = ifelse(isco68 %in% c("", "00", "NA"), NA, isco68),
    kos550 = NA,
    kos551 = NA,
    kos164 = ifelse(kos164 < 0 | kos164 == 999, NA, kos164),
    kos1640 = round(kos1640, 1) %>% ifelse(. < 0 | . == 999, NA, .),
    occ351 = ifelse(occ351 < 0, NA, occ351),
  ) %>% 
  # Pick variables
  select(year, hhid, pid, location, relation, sex, age, activity, hours, 
         kos550, kos551, kos1640, kos164, occ351, isco68) %>% 
  arrange(hhid, pid, relation)
#### LFS 1988 ####
lfs88 = read_dta(file.path(loc_LFS_1988, "lfs1988.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = ereg, # Region
         sex = esex, # Sex
         relation = erelat, # Relationship to head of HH
         age = age, # Age
         activity = ecara, # Economic activity in reference week
         hours = totushrs, # Total number of hours usually worked
         isco68 = ekos, # ISCO  68 Occupation
         kos164 = kscona, # Occupation (condensed) # 164
         kos1640 = kosa, # OPCS 1980 OCCUPATION CODES # 164.1
         occ351 = occcode, # OCCUPATION CODE // 351
  ) %>% 
  # Create ID for Household and Person in HH ID
  mutate(id = seq.int(nrow(.))) %>% 
  merge(select(subset(., relation == 1), id) %>% 
          mutate(hhid = seq.int(nrow(.))), by = "id", all = T) %>% 
  select(-id) %>% 
  mutate(hhid = na.locf0(hhid)) %>% 
  group_by(hhid) %>% mutate(pid = 1:n()) %>% ungroup %>% 
  select(hhid, pid, everything()) %>% 
  # Variable treatment
  mutate(
    year = 1988,
    location = factor(location,
                      labels = c("North", "Yorkshire and the Humber", "East Midlands", "East Anglia",
                                 "South East", "South West", "West Midlands", "North West", "Wales",
                                 "Scotland", "Northern Ireland")),
    sex = factor(sex, labels = c("Male", "Female")),
    relation = factor(relation, levels = c(1:6),
                      labels = c("Head", "Spouse", "Child", "Ascendant", "Other relative", "Other")),
    hours = ifelse(hours > 98 | hours < 0, NA, hours),
    activity = factor(activity, levels = c(1:28),
                      labels = c("Emp", "Self", "Emp", rep("Scheme", 6), rep("Unemp", 5), rep("Educ", 4), 
                                 "Retired", "Sick", "Home", "Retired", rep("Inactive", 5), "Child")),
    isco68 = ifelse(isco68 %in% c("", "00", "NA"), NA, isco68),
    kos550 = NA, kos551 = NA,
    kos164 = ifelse(kos164 < 0 | kos164 == 999, NA, kos164),
    kos1640 = round(kos1640, 1) %>% ifelse(. < 0 | . == 999, NA, .),
    occ351 = ifelse(occ351 < 0, NA, occ351),
  ) %>% 
  # Pick variables
  select(year, hhid, pid, location, relation, sex, age, activity, hours, 
         kos550, kos551, kos1640, kos164, occ351, isco68) %>% 
  arrange(hhid, pid, relation)
#### LFS 1989 ####
lfs89 = read_dta(file.path(loc_LFS_1989, "lfs1989.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = ereg, # Region
         sex = esex, # Sex
         relation = erelat, # Relationship to head of HH
         age = age, # Age
         activity = ecarb, # Economic activity in reference week
         hours = tothrsb, # Total number of hours usually worked
         isco68 = ekos, # ISCO  68 Occupation
         kos164 = kscona, # Occupation (condensed) # 164
         kos1640 = kosb, # OPCS 1980 OCCUPATION CODES # 164.1
         occ351 = occcode, # OCCUPATION CODE // 351
  ) %>% 
  # Create ID for Household and Person in HH ID
  mutate(id = seq.int(nrow(.))) %>% 
  merge(select(subset(., relation == 1), id) %>% 
          mutate(hhid = seq.int(nrow(.))), by = "id", all = T) %>% 
  select(-id) %>% 
  mutate(hhid = na.locf0(hhid)) %>% 
  group_by(hhid) %>% mutate(pid = 1:n()) %>% ungroup %>% 
  select(hhid, pid, everything()) %>% 
  # Variable treatment
  mutate(
    year = 1989,
    location = factor(location,
                      labels = c("North", "Yorkshire and the Humber", "East Midlands", "East Anglia",
                                 "South East", "South West", "West Midlands", "North West", "Wales",
                                 "Scotland", "Northern Ireland")),
    sex = factor(sex, labels = c("Male", "Female")),
    relation = factor(relation, levels = c(1:6),
                      labels = c("Head", "Spouse", "Child", "Ascendant", "Other relative", "Other")),
    hours = ifelse(hours > 98 | hours < 0, NA, hours),
    activity = factor(activity, levels = c(1:26),
                      labels = c("Emp", "Self", "Emp", rep("Scheme", 5), rep("Unemp", 5), rep("Educ", 3), 
                                 "Retired", "Sick", "Home", "Retired", rep("Inactive", 5), "Child")),
    isco68 = ifelse(isco68 %in% c("", "00", "NA"), NA, isco68),
    kos164 = ifelse(kos164 < 0 | kos164 == 999, NA, kos164),
    kos1640 = round(kos1640, 1) %>% ifelse(. < 0 | . == 999, NA, .),
    occ351 = ifelse(occ351 < 0, NA, occ351),
    kos550 = NA, kos551 = NA,
  ) %>% 
  # Pick variables
  select(year, hhid, pid, location, relation, sex, age, activity, hours, 
         kos550, kos551, kos1640, kos164, occ351, isco68) %>% 
  arrange(hhid, pid, relation)
#### LFS 1990 ####
lfs90 = read_dta(file.path(loc_LFS_1990, "lfs1990.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = ereg, # Region
         sex = esex, # Sex
         relation = erelat, # Relationship to head of HH
         age = age, # Age
         activity = ecar, # Economic activity in reference week
         hours = totact2, # TOTAL ACTUAL HOURS WORKED
         isco68 = ekos, # ISCO  68 Occupation
         kos164 = kscon, # Occupation (condensed) # 164
         kos1640 = kos, # OPCS 1980 OCCUPATION CODES # 164.1
         occ351 = occcode, # OCCUPATION CODE // 351
  ) %>% 
  # Create ID for Household and Person in HH ID
  mutate(id = seq.int(nrow(.))) %>% 
  merge(select(subset(., relation == 1), id) %>% 
          mutate(hhid = seq.int(nrow(.))), by = "id", all = T) %>% 
  select(-id) %>% 
  mutate(hhid = na.locf0(hhid)) %>% 
  group_by(hhid) %>% mutate(pid = 1:n()) %>% ungroup %>% 
  select(hhid, pid, everything()) %>% 
  # Variable treatment
  mutate(
    year = 1990,
    location = factor(location,
                      labels = c("North", "Yorkshire and the Humber", "East Midlands", "East Anglia",
                                 "South East", "South West", "West Midlands", "North West", "Wales",
                                 "Scotland", "Northern Ireland")),
    sex = factor(sex, labels = c("Male", "Female")),
    relation = factor(relation, levels = c(1:6),
                      labels = c("Head", "Spouse", "Child", "Ascendant", "Other relative", "Other")),
    hours = ifelse(hours > 98 | hours < 0, NA, hours),
    activity = factor(activity, levels = c(1:26),
                      labels = c("Emp", "Self", "Emp", rep("Scheme", 5), rep("Unemp", 5), rep("Educ", 3), 
                                 "Retired", "Sick", "Home", "Retired", rep("Inactive", 5), "Child")),
    isco68 = ifelse(isco68 %in% c("", "00", "NA"), NA, isco68),
    kos164 = ifelse(kos164 < 0 | kos164 == 999, NA, kos164),
    kos1640 = round(kos1640, 1) %>% ifelse(. < 0 | . == 999, NA, .),
    occ351 = ifelse(occ351 < 0, NA, occ351),
    kos550 = NA, kos551 = NA,
  ) %>% 
  # Pick variables
  select(year, hhid, pid, location, relation, sex, age, activity, hours, 
         kos550, kos551, kos1640, kos164, occ351, isco68) %>% 
  arrange(hhid, pid, relation)
#### LFS 1991 ####
lfs91 = read.dta(file.path(loc_LFS_1991, "lfs1991.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = ereg, # Region
         sex = esex, # Sex
         relation = erelat, # Relationship to head of HH
         age = age, # Age
         activity = ecar, # Economic activity in reference week
         hours = acthr, # Total actual hours worked
         isco68 = ekos, # ISCO  68 Occupation
         # kos164 = kscon, # Occupation (condensed) # 164
         kos1640 = kos, # OPCS 1980 OCCUPATION CODES # 164.1
         occ351 = occcode, # OCCUPATION CODE // 351
         soc90 = soc, # Occupation in main activity (using soc)
  ) %>% 
  # Create ID for Household and Person in HH ID
  mutate(id = seq.int(nrow(.))) %>% 
  merge(select(subset(., relation == "head of household"), id) %>% 
          mutate(hhid = seq.int(nrow(.))), by = "id", all = T) %>% 
  select(-id) %>% 
  mutate(hhid = na.locf0(hhid)) %>% 
  group_by(hhid) %>% mutate(pid = 1:n()) %>% ungroup %>% 
  select(hhid, pid, everything()) %>% 
  # Variable treatment
  mutate(
    year = 1991,
    location = factor(location,
                      labels = c("North", "Yorkshire and the Humber", "East Midlands", "East Anglia",
                                 "South East", "South West", "West Midlands", "North West", "Wales",
                                 "Scotland", "Northern Ireland")),
    sex = factor(sex, labels = c("Male", "Female")),
    relation = factor(relation,
                      labels = c("Head", "Spouse", "Child", "Ascendant", "Other relative", "Other")),
    hours = ifelse(hours > 98 | hours < 0, NA, hours),
    activity = factor(activity, levels = c(1:26),
                      labels = c("Emp", "Self", "Emp", rep("Scheme", 5), rep("Unemp", 5), rep("Educ", 3), 
                                 "Retired", "Sick", "Home", "Retired", rep("Inactive", 5), "Child")),
    isco68 = ifelse(isco68 %in% c("", "00", "NA"), NA, isco68),
    # kos164 = ifelse(kos164 < 0 | kos164 == 999, NA, kos164),
    kos1640 = round(kos1640, 1) %>% ifelse(. < 0 | . == 999, NA, .),
    occ351 = ifelse(occ351 < 0, NA, occ351),
    kos550 = NA, kos551 = NA, kos164 = NA
  ) %>% 
  # Pick variables
  select(year, hhid, pid, location, relation, sex, age, activity, hours, 
         kos550, kos551, kos1640, kos164, occ351, isco68) %>% 
  arrange(hhid, pid, relation)
#### ISCO68 to ISCO88 ####
# Read conversion table from CAMSIS
isco68_isco88 <- read.csv(file.path(loc_CAMSIS, "isco68_isco88.csv")) %>% 
  arrange(isco68, -prop) %>% 
  group_by(isco68) %>% slice(1) %>% ungroup %>% 
  select(isco68, isco88)
#### MERGE 1975-91 ####
lfs7991 <- do.call("rbind", 
  list(lfs79, lfs81, lfs83, lfs84, lfs85, lfs86, lfs87, lfs88, lfs89, lfs90, lfs91)) %>% 
  mutate(isco68 = as.integer(isco68)) %>% 
  merge(isco68_isco88, by = "isco68", all.x = T) %>% 
  mutate(soc90 = NA, soc00 = NA) %>% 
  select(year, hhid, pid, location, relation, sex, age, activity, hours, 
         # kos550, kos551, kos1640, kos164, occ351,
         isco68, soc90, soc00, isco88) %>% 
  arrange(year, hhid, pid, relation)
#### 2/ LFS 1992 - 2000 ####
# Function to clean data from 1992 to 2010
clean9210 <- function(df, year){
  
  # Value for Head of Household to create ID
  if(year < 1996){headHH = 1} else {headHH = 0}
  
  df0 = df %>% 
    # Create ID for Household and Person in HH ID
    mutate(id = seq.int(nrow(.))) %>% 
    merge(select(subset(., relation == headHH), id) %>% 
            mutate(hhid = seq.int(nrow(.))), by = "id", all = T) %>% 
    select(-id) %>% 
    mutate(hhid = na.locf0(hhid)) %>% 
    group_by(hhid) %>% mutate(pid = 1:n()) %>% ungroup %>% 
    select(hhid, pid, everything()) %>% 
    # Variable treatment
    mutate(
      'year' = year,
      location = factor(location, levels = c(1:20),
                        labels = c(rep("North", 2), rep("Yorkshire and the Humber", 3), "East Midlands", "East Anglia", 
                                   rep("South East", 3), "South West", rep("West Midlands", 2), 
                                   rep("North West", 3), "Wales",  rep("Scotland", 2), "Northern Ireland")),
      sex = factor(sex, levels = c(1,2), labels = c("Male", "Female")),
      age = as.integer(age),
      hours = as.numeric(as.character(hours)) %>% ifelse(. > 97 | . < 0, NA, .),)
  
  ## Change the definition of Economic activity in 2005
  if(year < 2005){
    df1 = df0 %>% 
      mutate(activity = factor(activity, levels = c(1:30),
        labels = c("Emp", "Self", "Scheme", "Home", "Unemp", "Educ", "Home",
                   rep("Sick", 2), rep("Inactive", 3), "Educ", "Home", 
                   rep("Sick", 2), rep("Inactive", 5), "Educ", "Home", 
                   rep("Sick", 2), "Inactive", "Retired", rep("Inactive", 2), 
                   "Child")))
  }
  if(year >= 2005){
    df1 = df0 %>% 
      mutate(activity = factor(activity, levels = c(1:34),
        labels = c("Emp", "Self", "Scheme", "Home", "Unemp", "Educ", "Home",
                   rep("Sick", 2), rep("Inactive", 3), "Educ", "Home", 
                   rep("Sick", 2), rep("Inactive", 3), "Retired", rep("Inactive", 3),
                   "Educ", "Home",  rep("Sick", 2), rep("Inactive", 3), "Retired", 
                   rep("Inactive", 2), "Child")))
  }
  
  ## Change the SOC classification in 2001
  if(year < 2001){
    df2 = df1 %>% 
      mutate(soc90 = as.integer(soc90) %>% ifelse(. < 0, NA, .), soc00 = NA) %>% 
      # Pick variables
      select('year', hhid, pid, location, relation, sex, age, activity, hours, soc90, soc00) %>% 
      arrange(hhid, pid, relation)
  }
  if(year >= 2001){
    df2 = df1 %>% 
      mutate(soc00 = as.integer(soc00) %>% ifelse(. < 0, NA, .), soc90 = NA) %>% 
      # Pick variables
      select('year', hhid, pid, location, relation, sex, age, activity, hours, soc90, soc00) %>% 
      arrange(hhid, pid, relation)
  }
  
  ## Change the definition of relationship to head of HH in 1996 and 2006
  if(year < 1996){
    df3 = df2 %>% 
      mutate(relation = factor(relation, levels = c(1:10),
        labels = c("Head", "Spouse", "Child", "Ascendant", 
                   rep("Other relative", 3), rep("Other", 3))),)
  }
  if(year >= 1996 & year < 2006){
    df3 = df2 %>% 
      mutate(relation = factor(relation, levels = c(0:8, 10:21),
        labels = c("Head", "Spouse", "Other", rep("Child",  4),
                   rep("Ascendant", 4), rep("Other relative", 6), rep("Other", 4))),)
  }
  if(year >= 2006){
    df3 = df2 %>% 
      mutate(relation = factor(relation, levels = c(0:8, 10:22),
        labels = c("Head", "Spouse", "Other", rep("Child",  4),
                   rep("Ascendant", 4), rep("Other relative", 6),
                   rep("Other", 2), "Civil Partner", rep("Other", 2))),)
  }
  
  
  return(df3)
      
}
#### LFS 1992 ####
lfs92q2 = read_dta(file.path(loc_LFS_1992Q2, "qlfsaj92.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = uresmc, # Region of usual residence
         sex = sex, # Sex
         relation = relhoh, # Relationship to head of HH
         age = age, # Age
         activity = inecacr, # Economic activity (reported)
         hours = tothrs, # Total hrs worked in reference wk
         soc90 = socmain, # Occupation (main job) [3 digits]
  ) %>% 
  clean9210(year = 1992)
#### LFS 1993 ####
lfs93q2 = read_dta(file.path(loc_LFS_1993Q2, "qlfsaj93.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = uresmc, # Region of usual residence
         sex = sex, # Sex
         relation = relhoh, # Relationship to head of HH
         age = age, # Age
         activity = inecacr, # Economic activity (reported)
         hours = tothrs, # Total hrs worked in reference wk
         soc90 = socmain, # Occupation (main job) [3 digits]
  ) %>% 
  clean9210(year = 1993)
#### LFS 1994 ####
lfs94q2 = read_dta(file.path(loc_LFS_1994Q2, "qlfsaj94.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = uresmc, # Region of usual residence
         sex = sex, # Sex
         relation = relhoh, # Relationship to head of HH
         age = age, # Age
         activity = inecacr, # Economic activity (reported)
         hours = tothrs, # Total hrs worked in reference wk
         soc90 = socmain, # Occupation (main job) [3 digits]
  ) %>% 
  clean9210(year = 1994)
#### LFS 1995 ####
lfs95q2 = read_dta(file.path(loc_LFS_1995Q2, "qlfsaj95.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = uresmc, # Region of usual residence
         sex = sex, # Sex
         relation = relhoh, # Relationship to head of HH
         age = age, # Age
         activity = inecacr, # Economic activity (reported)
         hours = tothrs, # Total hrs worked in reference wk
         soc90 = socmain, # Occupation (main job) [3 digits]
  ) %>% 
  clean9210(year = 1995)
#### LFS 1996 ####
lfs96q2 = read_dta(file.path(loc_LFS_1996Q2, "qlfsaj96.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = uresmc, # Region of usual residence
         sex = sex, # Sex
         relation = relh96, # Relationship to head of household
         age = age, # Age
         activity = inecacr, # Economic activity (reported)
         hours = tothrs, # Total hrs worked in reference wk
         soc90 = socmain, # Occupation (main job) [3 digits]
  ) %>% 
  clean9210(year = 1996)
#### LFS 1997 ####
lfs97q2 = read_dta(file.path(loc_LFS_1997Q2, "qlfsaj97.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = uresmc, # Region of usual residence
         sex = sex, # Sex
         relation = relh96, # Relationship to head of HH
         age = age, # Age
         activity = inecacr, # Economic activity (reported)
         hours = tothrs, # Total hrs worked in reference wk
         soc90 = socmain, # Occupation (main job) [3 digits]
  ) %>% 
  clean9210(year = 1997)
#### LFS 1998 ####
lfs98q2 = read_dta(file.path(loc_LFS_1998Q2, "qlfsaj98.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = uresmc, # Region of usual residence
         sex = sex, # Sex
         relation = relh96, # Relationship to head of HH
         age = age, # Age
         activity = inecacr, # Economic activity (reported)
         hours = tothrs, # Total hrs worked in reference wk
         soc90 = socmain, # Occupation (main job) [3 digits]
  ) %>% 
  clean9210(year = 1998)
#### LFS 1999 ####
lfs99q2 = read_dta(file.path(loc_LFS_1999Q2, "qlfsaj99.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = uresmc, # Region of usual residence
         sex = sex, # Sex
         relation = relh96, # Relationship to head of HH
         age = age, # Age
         activity = inecacr, # Economic activity (reported)
         hours = tothrs, # Total hrs worked in reference wk
         soc90 = socmain, # Occupation (main job) [3 digits]
  ) %>% 
  clean9210(year = 1999)
#### LFS 2000 ####
lfs00q2 = read_dta(file.path(loc_LFS_2000Q2, "qlfsaj00.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = uresmc, # Region of usual residence
         sex = sex, # Sex
         relation = relh96, # Relationship to head of HH
         age = age, # Age
         activity = inecacr, # Economic activity (reported)
         hours = tothrs, # Total hrs worked in reference wk
         soc90 = socmain, # Occupation (main job) [3 digits]
  ) %>% 
  clean9210(year = 2000)
#### SOC90 to ISCO88 ####
# Read conversion table from CAMSIS
soc90_isco88 <- read.csv(file.path(loc_CAMSIS, "soc90_isco88.csv"))
#### MERGE 1992-00 ####
lfs9200 <- do.call("rbind", 
                   list(lfs92q2, lfs93q2, lfs94q2, lfs95q2, lfs96q2, lfs97q2, lfs98q2, lfs99q2, lfs00q2)) %>% 
  merge(soc90_isco88, by = "soc90", all.x = T) %>% 
  mutate(isco68 = NA) %>% 
  select(year, hhid, pid, location, relation, sex, age, activity, hours, isco68,
         soc90, soc00, isco88) %>% 
  arrange(year, hhid, pid, relation)
#### 3/ LFS 2001 - 2010 ####
#### LFS 2001 ####
lfs01q2 = read_dta(file.path(loc_LFS_2001Q2, "qlfsaj01.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = uresmc, # Region of usual residence
         sex = sex, # Sex
         relation = relh96, # Relationship to head of HH
         age = age, # Age
         activity = inecacr, # Economic activity (reported)
         hours = tothrs, # Total hrs worked in reference wk
         soc00 = soc2km, # Occupation (main job) SOC2000 [4 digits]
  ) %>% 
  clean9210(year = 2001)
#### LFS 2002 ####
lfs02q2 = read_dta(file.path(loc_LFS_2002Q2, "lfsp_aj02_end_user.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = uresmc, # Region of usual residence
         sex = sex, # Sex
         relation = relh96, # Relationship to head of HH
         age = age, # Age
         activity = inecacr, # Economic activity (reported)
         hours = tothrs, # Total hrs worked in reference wk
         soc00 = soc2km, # Occupation (main job) SOC2000 [4 digits]
  ) %>% 
  clean9210(year = 2002)
#### LFS 2003 ####
lfs03q2 = read_dta(file.path(loc_LFS_2003Q2, "lfsp_aj03_end_user.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = uresmc, # Region of usual residence
         sex = sex, # Sex
         relation = relh96, # Relationship to head of HH
         age = age, # Age
         activity = inecacr, # Economic activity (reported)
         hours = tothrs, # Total hrs worked in reference wk
         soc00 = soc2km, # Occupation (main job) SOC2000 [4 digits]
  ) %>% 
  clean9210(year = 2003)
#### LFS 2004 ####
lfs04q2 = read_dta(file.path(loc_LFS_2004Q2, "lfsp_aj04_end_user.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = uresmc, # Region of usual residence
         sex = sex, # Sex
         relation = relh96, # Relationship to head of HH
         age = age, # Age
         activity = inecacr, # Economic activity (reported)
         hours = tothrs, # Total hrs worked in reference wk
         soc00 = soc2km, # Occupation (main job) SOC2000 [4 digits]
  ) %>% 
  clean9210(year = 2004)
#### LFS 2005 ####
lfs05q2 = read_dta(file.path(loc_LFS_2005Q2, "lfsp_aj05_end_user.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = uresmc, # Region of usual residence
         sex = sex, # Sex
         relation = relh96, # Relationship to head of HH
         age = age, # Age
         activity = inecac05, # Economic activity (reported)
         hours = tothrs, # Total hrs worked in reference wk
         soc00 = soc2km, # Occupation (main job) SOC2000 [4 digits]
  ) %>% 
  clean9210(year = 2005)
#### LFS 2006 ####
lfs06q2 = read_dta(file.path(loc_LFS_2006Q2, "lfsp_aj06_end_user.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = uresmc, # Region of usual residence
         sex = sex, # Sex
         relation = relh06, # Relationship to head of HH
         age = age, # Age
         activity = inecac05, # Economic activity (reported)
         hours = tothrs, # Total hrs worked in reference wk
         soc00 = soc2km, # Occupation (main job) SOC2000 [4 digits]
  ) %>% 
  clean9210(year = 2006)
#### LFS 2007 ####
lfs07q2 = read_dta(file.path(loc_LFS_2007Q2, "lfsp_aj07_end_user.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = uresmc, # Region of usual residence
         sex = sex, # Sex
         relation = relh06, # Relationship to head of HH
         age = age, # Age
         activity = inecac05, # Economic activity (reported)
         hours = tothrs, # Total hrs worked in reference wk
         soc00 = soc2km, # Occupation (main job) SOC2000 [4 digits]
  ) %>% 
  clean9210(year = 2007)
#### LFS 2008 ####
lfs08q2 = read_dta(file.path(loc_LFS_2008Q2, "lfsp_aj08_end_user.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = uresmc, # Region of usual residence
         sex = sex, # Sex
         relation = relh06, # Relationship to head of HH
         age = age, # Age
         activity = inecac05, # Economic activity (reported)
         hours = tothrs, # Total hrs worked in reference wk
         soc00 = soc2km, # Occupation (main job) SOC2000 [4 digits]
  ) %>% 
  clean9210(year = 2008)
#### LFS 2009 ####
lfs09q2 = read_dta(file.path(loc_LFS_2009Q2, "lfsp_aj09_end_user.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = uresmc, # Region of usual residence
         sex = sex, # Sex
         relation = relh06, # Relationship to head of HH
         age = age, # Age
         activity = inecac05, # Economic activity (reported)
         hours = tothrs, # Total hrs worked in reference wk
         soc00 = soc2km, # Occupation (main job) SOC2000 [4 digits]
  ) %>% 
  clean9210(year = 2009)
#### LFS 2010 ####
lfs10q2 = read_dta(file.path(loc_LFS_2010Q2, "lfsp_aj10_end_user.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = uresmc, # Region of usual residence
         sex = sex, # Sex
         relation = relh06, # Relationship to head of HH
         age = age, # Age
         activity = inecac05, # Economic activity (reported)
         hours = tothrs, # Total hrs worked in reference wk
         soc00 = soc2km, # Occupation (main job) SOC2000 [4 digits]
  ) %>% 
  clean9210(year = 2010)
#### SOC00 to ISCO88 ####
# Read conversion table from CAMSIS
soc00_isco88 <- read.csv(file.path(loc_CAMSIS, "soc00_isco88.csv"))
#### MERGE 2001-10 ####
lfs0110 <- do.call("rbind", 
                   list(lfs01q2, lfs02q2, lfs03q2, lfs04q2, lfs05q2, lfs06q2, lfs07q2, 
                        lfs08q2, lfs09q2, lfs10q2)) %>% 
  merge(soc00_isco88, by = "soc00", all.x = T) %>% 
  mutate(isco68 = NA) %>% 
  select(year, hhid, pid, location, relation, sex, age, activity, hours, isco68, soc90,
         soc00, isco88) %>% 
  arrange(year, hhid, pid, relation)
#### 4/ LFS 2011-20 ####
#### LFS 2011 [Manual] ####
lfs11q2 = read_dta(file.path(loc_LFS_2011Q2, "lfsp_aj11_end_user.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = uresmc, # Region of usual residence
         sex = sex, # Sex
         relation = relh06, # Relationship to head of HH
         age = age, # Age
         activity = inecac05, # Economic activity (reported)
         hours = tothrs, # Total hrs worked in reference wk
         soc00 = sc102km, # SOC2010 mapped to SOC2000 Main Job Unit
         soc10 = soc10m, # Occupation (main job) SOC2010
  ) %>% 
  # Create ID for Household and Person in HH ID
  mutate(id = seq.int(nrow(.))) %>% 
  merge(select(subset(., relation == 0), id) %>% 
          mutate(hhid = seq.int(nrow(.))), by = "id", all = T) %>% 
  select(-id) %>% 
  mutate(hhid = na.locf0(hhid)) %>% 
  group_by(hhid) %>% mutate(pid = 1:n()) %>% ungroup %>% 
  select(hhid, pid, everything()) %>% 
  # Variable treatment
  mutate(
    'year' = 2011,
    location = factor(location, levels = c(1:20),
                      labels = c(rep("North", 2), rep("Yorkshire and the Humber", 3), "East Midlands", "East Anglia", 
                                 rep("South East", 3), "South West", rep("West Midlands", 2), 
                                 rep("North West", 3), "Wales",  rep("Scotland", 2), "Northern Ireland")),
    sex = factor(sex, levels = c(1,2), labels = c("Male", "Female")),
    age = as.integer(age),
    hours = as.numeric(as.character(hours)) %>% ifelse(. > 97 | . < 0, NA, .),
    activity = factor(activity, levels = c(1:34),
      labels = c("Emp", "Self", "Scheme", "Home", "Unemp", "Educ", "Home",
                 rep("Sick", 2), rep("Inactive", 3), "Educ", "Home", 
                 rep("Sick", 2), rep("Inactive", 3), "Retired", rep("Inactive", 3),
                 "Educ", "Home",  rep("Sick", 2), rep("Inactive", 3), "Retired", 
                 rep("Inactive", 2), "Child")),
    soc90 = NA, soc00 = as.integer(soc00) %>% ifelse(. < 0, NA, .),
    soc10 = as.integer(soc10) %>% ifelse(. < 0, NA, .),
    relation = factor(relation, levels = c(0:8, 10:22),
      labels = c("Head", "Spouse", "Other", rep("Child",  4), rep("Ascendant", 4), 
                 rep("Other relative", 6), rep("Other", 2), "Civil Partner", rep("Other", 2))),
    ) %>% 
  select(year, hhid, pid, location, relation, sex, age, activity, hours, soc90, soc00, soc10)
#### SOC10 to SOC00 ####
# Translation table from SOC10 into SOC00 using LFS11Q2
# Pr(SOC00 | Sex & SOC10)
soc10_soc00 = lfs11q2 %>% 
  subset(!is.na(soc10) & !is.na(soc00)) %>% 
  select(soc10, sex, soc00) %>% 
  count(soc10, sex, soc00, name = "n") %>% 
  add_count(sex, soc10, wt = n, name = "N") %>% 
  mutate(prop = n/N) %>% 
  arrange(soc10, sex, -prop) %>% 
  group_by(soc10, sex) %>% slice(1) %>% ungroup
#### LFS 2012 [Manual] ####
lfs12q2 = read_dta(file.path(loc_LFS_2012Q2, "lfsp_aj12_end_user.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(location = uresmc, # Region of usual residence
         sex = sex, # Sex
         relation = relh06, # Relationship to head of HH
         age = age, # Age
         activity = inecac05, # Economic activity (reported)
         hours = tothrs, # Total hrs worked in reference wk
         soc10 = soc10m, # Occupation (main job)
  ) %>% 
  # Create ID for Household and Person in HH ID
  mutate(id = seq.int(nrow(.))) %>% 
  merge(select(subset(., relation == 0), id) %>% 
          mutate(hhid = seq.int(nrow(.))), by = "id", all = T) %>% 
  select(-id) %>% 
  mutate(hhid = na.locf0(hhid)) %>% 
  group_by(hhid) %>% mutate(pid = 1:n()) %>% ungroup %>% 
  select(hhid, pid, everything()) %>% 
  # Variable treatment
  mutate(
    'year' = 2012,
    location = factor(location, levels = c(1:20),
                      labels = c(rep("North", 2), rep("Yorkshire and the Humber", 3), "East Midlands", "East Anglia", 
                                 rep("South East", 3), "South West", rep("West Midlands", 2), 
                                 rep("North West", 3), "Wales",  rep("Scotland", 2), "Northern Ireland")),
    sex = factor(sex, levels = c(1,2), labels = c("Male", "Female")),
    age = as.integer(age),
    hours = as.numeric(as.character(hours)) %>% ifelse(. > 97 | . < 0, NA, .),
    activity = factor(activity, levels = c(1:34),
                      labels = c("Emp", "Self", "Scheme", "Home", "Unemp", "Educ", "Home",
                                 rep("Sick", 2), rep("Inactive", 3), "Educ", "Home", 
                                 rep("Sick", 2), rep("Inactive", 3), "Retired", rep("Inactive", 3),
                                 "Educ", "Home",  rep("Sick", 2), rep("Inactive", 3), "Retired", 
                                 rep("Inactive", 2), "Child")),
    soc90 = NA, 
    soc10 = as.integer(soc10) %>% ifelse(. < 0, NA, .),
    relation = factor(relation, levels = c(0:8, 10:22),
                      labels = c("Head", "Spouse", "Other", rep("Child",  4), rep("Ascendant", 4), 
                                 rep("Other relative", 6), rep("Other", 2), "Civil Partner", rep("Other", 2))),
  ) %>% 
  # Merge with translation table to get SOC00
  merge(select(soc10_soc00, soc10, sex, soc00), by = c("sex", "soc10"), all.x = T) %>%  
  # Pick variables
  select(year, hhid, pid, location, relation, sex, age, activity, hours, 
         soc90, soc00, soc10) %>% 
  arrange(hhid, pid, relation)
#### MERGE 2011-20 ####
lfs1120 <- do.call("rbind", list(lfs11q2, lfs12q2)) %>% 
  merge(soc00_isco88, by = "soc00", all.x = T) %>% 
  mutate(isco68 = NA) %>% 
  select(year, hhid, pid, location, relation, sex, age, activity, hours, isco68, soc90,
         soc00, isco88) %>% 
  arrange(year, hhid, pid, relation)
#### 5/ WRITE ALL ####
write.csv(lfs7991, file.path(loc_data, "_clean", "_lfs", "_decade", "lfs7991.csv"), row.names = F)
write.csv(lfs9200, file.path(loc_data, "_clean", "_lfs", "_decade", "lfs9200.csv"), row.names = F)
write.csv(lfs0110, file.path(loc_data, "_clean", "_lfs", "_decade", "lfs0110.csv"), row.names = F)
write.csv(lfs1120, file.path(loc_data, "_clean", "_lfs", "_decade", "lfs1120.csv"), row.names = F)
