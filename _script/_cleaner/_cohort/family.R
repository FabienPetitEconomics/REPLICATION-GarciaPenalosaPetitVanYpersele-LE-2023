#### 0 INITIALIZATION ####
source(file.path("_script", "init.R"))
#### 1 LOAD DATA ####
#### 1.1 NCDS ####
## Age 00-16
ncds16 = read.dta(file.path(loc_NCDS_16, "ncds0123.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    
    # SIBposition_00 = "n550", # 0  Birth order - mums live siblings
    
    SIBsize_07 = "n295", # 1P Total number of births to own mother
    SIBpos_07 = "n297", # 1P Child's position in birth order
    HHsize_07 = "n419", # 1P Number of people in the household
    
    HHsize_11 = "n1116", # 2P Number living in child's household
    # "n1119", # 2P No. Childn under 21yrs not in hsehld
    SIBsize_11 = "n1117", # 2P No. kids undr 21 living in household
    SIBpos_11 = "n1118", # 2P Child's posn hhld, inc liv away
    # "n1120", # 2P No. brths to own mum aftr study child
    # "n1147", # 2P Dads role in management of child
    # "n1157", # 2P How many people share childs bedroom
    # "n1158", # 2P Does child share bed with anyone else
    
    HHsize_16 = "n2360", # 3P Number of people in household
    
    BROolder_16 = "n2363", # 3P Boys undr 21 hhld oldr than NCDS chld
    BROyounger_16 = "n2364", # 3P Boys in hhld yngr than NCDS child
    SISolder_16 = "n2365", # 3P Girls undr 21 hhld oldr thn NCDS chld
    SISyounger_16 = "n2366", # 3P Girls in hhld yngr than NCDS child
    # 
    # "n2367", # 3P Has NCDS child any older brothers
    # "n2368", # 3P Has NCDS child any younger brothers
    # "n2369", # 3P Has NCDS child any older sisters
    # "n2370", # 3P Has NCDS child any younger sisters
    
    # "n2479", # 3P How many people share child-s bedroom
    # "n2480", # 3P How many people share child-s bed
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == ""))
#### 1.2 BCS ####
## Age 05
bcs05 = read.dta(file.path(loc_BCS_05, "f699b.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    
    HHsize_05 = "e005", # Number Of Persons in Household
    SIBolder_05 = "e006", # Number Of Children Older Than Child
    SIByounger_05 = "e007", # Number of Children Younger Than Child
    # "e013", # Number of Subsequent Children
    
    # starts_with("e014"), # Child 2
    # starts_with("e015"), # Child 3
    # starts_with("e016"), # Child 4
    # starts_with("e017"), # Child 5
    # starts_with("e018"), # Child 6
    
    # "e228c", # N Share Bedroom
    # "e228e", # Share a Bed
    
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == ""))
## Age 10
bcs10 = read.dta(file.path(loc_BCS_10, "sn3723.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    
    HHsize_10 = "a4a_41", # NUMBER OF PERSONS IN HOUSEHOLD  
    SIBsize_10 = "a4a_42", # NUMBER OF CHILDREN IN HOUSEHOLD 
    
    # starts_with("a4a"), # PERSONS IN HOUSE
    # starts_with("a4b"), # PERSONS NOT IN HOUSE
    # 
    # starts_with("c1_"), # FATHER'S and MOTHER's QUALIFICATIONS
    
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == ""))
## Age 16
bcs16 = read.dta(file.path(loc_BCS_16, "bcs7016x.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    
    # "c6_2", # Teenager singleton or twin
    SIBolder_16 = "c6_3a", # No.of brother-sister in same house older
    SIByounger_16 = "c6_3b", # No.of brother-sister in same house young
    # "c6_4a", # No.of brother-sister elsewhere older
    # "c6_4b", # No.of brother-sister elsewhere younger
    # HHother_16 = "c6_5", # No.of other relatves in same house young
    SIBsize_16 = "f7", # Number of children in the family
    SIBpos_16 = "f8" , # Family position of teenager
    
    # starts_with("oa7"), # Relatives to study teen
    
    HHYounger = "oa8a_1", # No. in household younger than teenager
    HHSame = "oa8a_2", # No. in household same age as teenager
    HHOlder21less = "oa8a_3", # No. in household older but under 21
    HHOlder21plus = "oa8a_4", # No. in household older but over 21
    SIBYounger = "oa8b_1", # Blood brothers,sisters younger than teen
    SIBSame = "oa8b_2", # Blood brothers,sisters exactly same age
    SIBOlder21less = "oa8b_3", # Blood brothers, sisters older under 21
    SIBOlder21plus = "oa8b_4", # Blood brothers, sisters older over 21
  ) %>% 
  subset(!(get(vars_select(names(.), contains("id"))) == ""))

#### 2 TREATMENT - SIBLING ####
#### 2.1 NCDS ####
ncds = ncds16 %>% 
  mutate_if(is.numeric, function(x){ifelse(x < 0, NA, x)}) %>% 
  mutate(
    SIBsize_16 = BROolder_16 + BROyounger_16 + SISolder_16 + SISyounger_16 +1,
    SIBpos_16 = BROolder_16 + SISolder_16 +1,
  ) %>% 
  select(id, starts_with("HH"), starts_with("SIB")) %>% setDT %>% 
  melt(id.vars = "id") %>% 
  tidyr::separate(col = "variable", into = c("variable", "age")) %>% 
  mutate(age = as.integer(age), variable = factor(variable)) %>% 
  select(id, age, variable, value) %>% 
  arrange(id, age, variable) %>% 
  setDT %>% dcast(id + age ~ variable, value.var = "value")
### 2.2 BCS ####
bcs = bcs05 %>% merge(bcs10, by = "id", all = T) %>% 
  merge(bcs16, by = "id", all = T) %>% 
  mutate_if(is.numeric, function(x){ifelse(x < 0, NA, x)}) %>% 
  mutate(
    # Age 05
    SIBsize_05 = SIBolder_05 + SIByounger_05 +1,
    SIBpos_05 = SIBolder_05 +1,
    # Age 10
    SIBpos_10 = NA,
    # Age 16
    SIBsize_16 = SIBsize_16 %>% 
      ifelse(!is.na(.), ., SIBYounger + SIBSame + SIBOlder21less + SIBOlder21plus + 1),
    SIBpos_16 = SIBpos_16 %>% 
      ifelse(!is.na(.), ., SIBOlder21less + SIBOlder21plus + 1),
    HHsize_16 = HHYounger + HHSame + HHOlder21less + HHOlder21plus + 1,
  ) %>% 
  select(id, starts_with("HHsize"), starts_with("SIBsize"), starts_with("SIBpos")) %>% setDT %>% 
  melt(id.vars = "id") %>% 
  tidyr::separate(col = "variable", into = c("variable", "age")) %>% 
  mutate(age = as.integer(age), variable = factor(variable)) %>% 
  select(id, age, variable, value) %>% 
  arrange(id, age, variable) %>% 
  setDT %>% dcast(id + age ~ variable, value.var = "value")
#### 3 MERGE ####
family = rbind(ncds, bcs) %>% 
  mutate(period = factor(age, labels = c("cp1", "cp1", "cp2", "cp2", "cp3"))) %>% 
  select(id, period, age, HHsize, SIBpos, SIBsize)
#### 4 SAVE ####
write.csv(family, file.path(loc_cohort, "family.csv"), row.names = FALSE)
