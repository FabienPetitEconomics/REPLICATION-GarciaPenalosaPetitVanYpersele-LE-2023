#### INITIALIZATION ####
source(file.path("_script", "init.R"))
#### LOAD DATA ####
### NCDS
## Age 16
ncds16 = file.path(loc_NCDS_16, "ncds0123.dta") %>% 
  read.dta() %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    
    # Age 7
    # educ_int_mother1 = "n43", # 1S Mothers' interest in childs education
    # educ_int_father1 = "n44", # 1S Fathers' interest in childs education
    # Age 11
    educ_int_father = "n851", # 2S Fathers' interest in childs education
    educ_int_mother = "n852", # 2S Mothers' interest in childs education
    # Age 16
    # educ_int_father3 = "n2324", # 3S Fathers' interest in childs education
    # educ_int_mother3 = "n2325", # 3S Mothers' interest in childs education
    
    # educ_wish3 = "n2407", # 3P Parents wishes about childs education
    # educ_expect3 = "n2408", # 3P Parents expectations childs education
    
    educ_left_father = "n2396", # 3P Age father figr left full-time educ
    educ_left_mother = "n2397", # 3P Age mother figr left full-time educ
    
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == ""))
### BCS
## Age 05
bcs05 = file.path(loc_BCS_05, "f699b.dta") %>% 
  read.dta %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    # educ_heduc_mother = "e189a", # Mothers Highest Educ Qualification
    # educ_heduc_father = "e189b", # Fathers Highest Educ Qualification
    # educ_heduc_parents = "e190", # Highest Known Qualification of Parents
    educ_leftschool_father = "e192", # Age Father Left School
    educ_leftschool_mother = "e191", # Age Mother Left School
    educ_afterschool_father = "e194", # Years of Ft Educ After School-Father
    educ_afterschool_mother = "e193", # Years of Ft Educ After School-Mother
    educ_after15_father = "e196", # Years of Ft Educ After Age 15-Father
    educ_after15_mother = "e195", # Years of Ft Educ After Age 15-Mother
    ) %>% 
  subset(!(get(vars_select(names(.), contains("id"))) == ""))
## Age 10
bcs10 = file.path(loc_BCS_10, "sn3723.dta") %>% 
  read.dta() %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    educ_int_father = "j098", # A32-FATHER'S INTEREST IN CHILD'S EDUCATION  
    educ_int_mother = "j097", # A32-MOTHER'S INTEREST IN CHILD'S EDUCATION
  ) %>% 
  subset(!(get(vars_select(names(.), contains("id"))) == ""))
## Age 16
# bcs16 = file.path(loc_BCS_16, "bcs7016x.dta") %>% 
#   read.dta() %>% 
#   setnames(., tolower(names(.))) %>% 
#   select(
#     id  = "bcsid", # ID
#     age_lefteduc_father = "t7_1", # Age father finished full-time education
#     age_lefteduc_mother = "t7_2", # Age mother finished full-time education
#   ) %>% 
#   subset(! (get(vars_select(names(.), contains("id"))) == ""))
#### TREATMENT ####
### NCDS
ncds1 = ncds16 %>% 
  select(id, educ_int_father, educ_int_mother, educ_left_father, educ_left_mother) %>% 
  mutate_at(vars(starts_with("educ_left")),
            ~ factor(., labels = c(NA, "-13", "13-14", "14-15", "15-16", "16-17", "17-18", 
                                   "18-19", "19-21", "21-23", "23+", NA)) %>% 
              as.character(.) %>% 
              factor(levels = c("-13", "13-14", "14-15", "15-16", "16-17", "17-18", 
                                "18-19", "19-21", "21-23", "23+"))
            ) %>% 
  mutate_at(vars(starts_with("educ_int")),
            ~ as.character(.) %>% 
              factor(., levels = c("Over concerned", "Very interested", "Some interest",
                                   "Little interest", "Cant say")))
## BCS
# BCS05 : compute left FT educ
bcs05 = bcs05 %>% 
  mutate_if(is.numeric, ~ ifelse(. < 0, NA, .)) %>% 
  mutate(
    educ_left_father = educ_leftschool_father + educ_afterschool_father,
    educ_left_mother = educ_leftschool_mother + educ_afterschool_mother,
  ) %>% 
  select(id, educ_left_father, educ_left_mother)
## MERGE BCS
bcs1 = merge(bcs05, bcs10, by = "id", all = TRUE) %>% 
  select(id, educ_int_father, educ_int_mother, educ_left_father, educ_left_mother) %>% 
  mutate_at(vars(starts_with("educ_int")),
            ~ as.character(.) %>% 
              factor(levels = c("Very Interested", "Moderate Interest", "Very Little Interest",
                                "Uninterested", "Cannot Say"))
  ) %>% 
  mutate_at(vars(starts_with("educ_left")), ~ ifelse(. < 0, NA, .)) %>% 
  mutate_at(vars(starts_with("educ_left")),
            ~ case_when(
              . < 13 ~ "-13",
              . < 14 ~ "13-14",
              . < 15 ~ "14-15",
              . < 16 ~ "15-16",
              . < 17 ~ "16-17",
              . < 18 ~ "17-18",
              . < 19 ~ "18-19",
              . < 21 ~ "19-21",
              . < 23 ~ "21-23",
              . >= 23 ~ "23+"
            ) %>% factor(levels = c("-13", "13-14", "14-15", "15-16", "16-17", "17-18", 
                                    "18-19", "19-21", "21-23", "23+")))
#### MERGE ####
parent_educ = rbind(ncds1, bcs1)
# Normalize scales
parent_educ2 = parent_educ %>% 
  mutate_at(vars(starts_with("educ_int")),
            ~ factor(., labels = c("Very interested", "Very interested", 
                                   "Moderate interest", "Little interest", 
                                   "Cannot say", "Very interested", "Moderate interest", 
                                   "Little interest", "Little interest", "Cannot say")))
#### SAVE ####
write.csv(parent_educ2, file.path(loc_cohort, "parent_education.csv"), row.names = FALSE)
