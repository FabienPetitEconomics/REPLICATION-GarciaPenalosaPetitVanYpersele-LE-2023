#### 0 INITIALIZATION ####
source(file.path("_script", "init.R"))
### REGION CLASSIFICATION
region_ante1965 = c("E & W.Riding", "East", "Midlands", "North", "North Midlands", 
                   "North West", "Scotland", "South", "South East", "South West", "Wales")
region_post1965 = c("North", "Yorkshire & Humberside", "East Midlands", "East Anglia", 
                    "South East", "South West", "West Midlands", "North West", "Wales",
                    "Scotland")
#### 1 LOAD DATA ####
#### 1.1 NCDS ####
## Age 00 to 16
ncds00 = read.dta(file.path(loc_NCDS_16, "ncds0123.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    "n0region", # Region at PMS (1958) - Birth
    "n1region", # Region at NCDS1 (1965) - 7 years
    "n2region", # Region at NCDS2 (1969) - 11 years
    "n3region", # Region at NCDS3 (1974) - 16 years
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  reshape2::melt(id.var = "id") %>% 
  mutate(age = case_when(variable == "n0region" ~ 0,
                         variable == "n1region" ~ 7,
                         variable == "n2region" ~ 11,
                         variable == "n3region" ~ 16),
         year = age + 1958,
         region = value) %>%  
  mutate(region = ifelse(region %in% c("No imformation", "Not in PMS"), NA, region),
         country = ifelse(region %in% region_ante1965[-c(7, 11)], "England", region),
         gov_rgn = NA) %>% 
  select(id, age, year, region, country, gov_rgn)
## Age 23
ncds23 = read.dta(file.path(loc_NCDS_23, "ncds4.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    region = "n4region", # Region at NCDS4 (1981) - 23 years
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 23, year = 1981) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(region = ifelse(region == "Unknown", NA, region),
         country = ifelse(region %in% region_post1965[-c(9,10)], "England", region),
         gov_rgn = NA) %>% 
  select(id, age, year, region, country, gov_rgn)
## Age 33
ncds33 = read.dta(file.path(loc_NCDS_33, "ncds5cmi.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    region = "n5region", # Standard region at NCDS5
    gov_rgn = "n5gor", # NCDS5 Government Office Region
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 33, year = 1991) %>% 
  mutate_if(is.factor, as.character) %>%
  mutate_at(vars(c("region", "gov_rgn")), function(x) x %>%ifelse(. == "Unknown", NA, .)) %>% 
  mutate(country = ifelse(region %in% region_post1965[-c(9,10)], "England", region)) %>% 
  select(id, age, year, region, country, gov_rgn)
## Age 42
ncds42 = read.dta(file.path(loc_NCDS_42, "ncds6.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    region = "n6region", # Standard region at NCDS6
    gov_rgn = "n6gor", # NCDS6 Government Office Region
  ) %>%
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 42, year = 2000) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate_at(vars(c("region", "gov_rgn")), function(x) x %>%ifelse(. == "Unknown", NA, .)) %>% 
  mutate(country = ifelse(region %in% region_post1965[-c(9,10)], "England", region)) %>% 
  select(id, age, year, region, country, gov_rgn)
## Age 47
ncds47 = read.dta(file.path(loc_NCDS_47, "ncds7.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    country = "n7cntry", # Country at Interview
    region = "n7region", # Standard (Statistical) Region (SSR) at Interview
    gov_rgn = "n7gor", # Government Office Region at Interview
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 47, year = 2005) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate_at(vars(c("country", "region", "gov_rgn")), 
            function(x) x %>%ifelse(. == "Unknown", NA, .)) %>% 
  select(id, age, year, region, country, gov_rgn)
## Age 50
ncds50 = read.dta(file.path(loc_NCDS_50, "ncds_2008_followup.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    country = "n8cntry", # Country at Interview
    region = "n8region", # Standard (Statistical) Region (SSR) at Interview
    gov_rgn = "n8gor", # Government Office Region at Interview
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 50, year = 2008) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate_at(vars(c("country", "region", "gov_rgn")), 
            function(x) x %>%ifelse(. == "Unknown", NA, .)) %>% 
  select(id, age, year, region, country, gov_rgn)
## Age 55
ncds55 = read.dta(file.path(loc_NCDS_55, "ncds_2013_derived.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    country = "nd9cntry", # Country of Interview  (2013)
    region = "nd9regn", # Standard Statistical Region (2013) 
    gov_rgn = "nd9gor", # Government Office Region of residence (2013)
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 55, year = 2013) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate_at(vars(c("country", "region", "gov_rgn")), 
            function(x) x %>%ifelse(. == "Not applicable (not resident in GB)", "Abroad", .)) %>% 
  # Correction below: some individuals are considered as non-resident in "region" variable, 
  # but they are either in North West or in South East on government region
  # Thus, based on the classification map, we correct this
  mutate(
    region = case_when(
      region == "Not resident in GB" & country == "England" & gov_rgn == "North West" ~ "North",
      region == "Not resident in GB" & country == "England" & gov_rgn == "South East" ~ "South East",
      region == "Not resident in GB" & country == "Abroad" & gov_rgn == "Abroad" ~ "Abroad",
      TRUE ~ region,
    )
  ) %>% 
  select(id, age, year, region, country, gov_rgn)
#### 1.2 BCS ####
## Age 05
bcs05 = read.dta(file.path(loc_BCS_05, "bcs2derived.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    country = "bd2cntry", # 1975: Country of Interview
    region = "bd2regn", # 1975: Standard Region of residence
  ) %>% 
  subset(!(get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 05, year = 1975) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate_at(vars(c("region", "country")), function(x) x %>% ifelse(. == "Overseas", "Abroad", .)) %>% 
  mutate(gov_rgn = NA) %>% 
  select(id, age, year, region, country, gov_rgn)
## Age 10
bcs10 = read.dta(file.path(loc_BCS_10, "bcs3derived.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    country = "bd3cntry", # 1980: Country of Interview
    region = "bd3regn", # 1980: Standard Region of residence
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 10, year = 1980) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate_at(vars(c("region", "country")), function(x) x %>% ifelse(. == "Unknown", NA, .)) %>% 
  mutate(gov_rgn = NA) %>% 
  select(id, age, year, region, country, gov_rgn) %>% 
  # Remove NA duplicates
  subset(!(is.na(region) & is.na(country)))
## Age 16
bcs16 = read.dta(file.path(loc_BCS_16, "bcs4derived.dta")) %>%
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    country = "bd4cntry", # 1986: Country of Interview
    region = "bd4regn", # 1986: Standard Region of residence
    gov_rgn = "bd4gor", # 1986: Government Office Region of residence
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 16, year = 1986) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate_at(vars(c("region", "country", "gov_rgn")), 
            function(x) x %>% ifelse(. == "Unknown", NA, .)) %>% 
  select(id, age, year, region, country, gov_rgn)
## Age 26
bcs26 = read.dta(file.path(loc_BCS_26, "bcs5derived.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    "country" = "bd5cntry", # 1996: Country of Interview
    "region" = "bd5regn", # 1996: Standard Region of residence
    "gov_rgn" = "bd5gor", # 1996: Government Office Region of residence
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 26, year = 1996) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate_at(vars(c("region", "country", "gov_rgn")), 
            function(x) x %>% ifelse(. == "Unknown", NA, .)) %>% 
  select(id, age, year, region, country, gov_rgn)
## Age 30
bcs30 = read.dta(file.path(loc_BCS_30, "bcs6derived.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    country = "bd6cntry", # 2000: Country of Interview
    region = "bd6regn", # 2000: Standard Region of residence
    gov_rgn = "bd6gor", # 2000: Government Office Region of residence
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 30, year = 2000) %>% 
  mutate_if(is.factor, as.character) %>% 
  select(id, age, year, region, country, gov_rgn)
## Age 34
bcs34 = read.dta(file.path(loc_BCS_34, "bcs_2004_followup.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    country = "b7cntry", # 2004: Country of Interview
    region = "b7region", # 2004: Standard Region of residence
    gov_rgn = "b7gor", # 2004: Government Office Region of residence
    )  %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 34, year = 2004) %>%  
  mutate_if(is.factor, as.character) %>% 
  mutate_at(vars(c("region", "country", "gov_rgn")), 
            function(x) x %>% ifelse(. == "Refusal", NA, .)) %>% 
  select(id, age, year, country, region, gov_rgn)
## Age 38
bcs38 = read.dta(file.path(loc_BCS_38, "bcs8derived.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    country = "bd8cntry", # 2008: Country of Interview
    region = "bd8regn", # 2008: Standard Region of residence
    gov_rgn = "bd8gor", # 2008: Government Office Region of residence
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 38, year = 2008) %>% 
  mutate_if(is.factor, as.character) %>% 
  select(id, age, year, country, region, gov_rgn)
## Age 42
bcs42 = read.dta(file.path(loc_BCS_42, "bcs70_2012_derived.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    country = "bd9cntry", # 2012: Country of Interview
    region = "bd9regn", # 2012: Standard Region of residence
    gov_rgn = "bd9gor", # 2012: Government Office Region of residence
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == "")) %>% 
  mutate(age = 42, year = 2012) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate_at(vars(c("region", "country", "gov_rgn")), 
            function(x) x %>% ifelse(. == "Not applicable (not resident in UK)", "Abroad", .)) %>% 
  select(id, age, year, country, region, gov_rgn)
#### 2 MERGE ####
#### 2.1 NCDS ####
## Reduce Rbind
ncds_location = Reduce(function(x,y) rbind(x, y), lapply(ls(pattern = "ncds"), get))
rm(ncds00, ncds23, ncds33, ncds42, ncds47, ncds50, ncds55)
## Empty + Merge
ncds_location = data.frame(
  id = rep(levels(factor(ncds_location$id)), 
           each = length(as.numeric(levels(factor(ncds_location$age))))),
  age = rep(as.numeric(levels(factor(ncds_location$age))),
            length(levels(factor(ncds_location$id)))),
  year = rep(as.numeric(levels(factor(ncds_location$year))),
             length(levels(factor(ncds_location$id))))) %>% 
  merge(., ncds_location, by = c("id", "age", "year"), all.x = TRUE)
#### 2.2 BCS ####
## Reduce Rbind
bcs_location = Reduce(function(x,y) rbind(x, y), lapply(ls(pattern = "bcs"), get))
rm(bcs05, bcs10, bcs16, bcs26, bcs30, bcs34, bcs38, bcs42)
## Empty + Merge
bcs_location = data.frame(
  id = rep(levels(factor(bcs_location$id)), 
           each = length(as.numeric(levels(factor(bcs_location$age))))),
  age = rep(as.numeric(levels(factor(bcs_location$age))),
            length(levels(factor(bcs_location$id)))),
  year = rep(as.numeric(levels(factor(bcs_location$year))),
             length(levels(factor(bcs_location$id))))) %>% 
  merge(., bcs_location, by = c("id", "age", "year"), all.x = TRUE)
#### 3 TREATMENT ####
### Correction for Yorkshire and the Humberside
## NCDS
ncds_location = ncds_location %>% 
  mutate_at(vars(c("region", "gov_rgn")), 
          function(x) x %>% 
            ifelse(. %in% c("Yorkshire and Humberberside", "Yorkshire and Humberside", 
                            "Yorkshire and The Humber", "Yorkshire & Humberside",
                            "Yorks and Humberside"),
                   "Yorkshire and the Humber", .))
## BCS
bcs_location = bcs_location %>% 
  mutate_at(vars(c("region", "gov_rgn")), 
            function(x) x %>% 
              ifelse(. %in% c("Yorkshire and Humberberside", "Yorkshire and Humberside", 
                              "Yorkshire and The Humber", "Yorkshire & Humberside",
                              "Yorks and Humberside"),
                     "Yorkshire and the Humber", .))
#### 4 RBIND ####
location = rbind(ncds_location, bcs_location)
#### 5 SAVE ####
write.csv(location, file.path(loc_cohort, "location.csv"), row.names = FALSE)