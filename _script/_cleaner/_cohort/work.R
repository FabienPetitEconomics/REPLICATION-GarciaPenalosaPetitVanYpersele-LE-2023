#### 0 INITIALIZATION ####
source(file.path("_script", "init.R"))
# Load data on inflation and function
source(file.path(loc_function, "compute_deflator.R"))
#### 1 LOAD DATA ####
#### 1.1 NCDS58 ####
#### First Job ####
ncds_fj = read.dta(file.path(loc_NCDS_23, "ncds4.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    
    fjob_mo = "n4145", # 1st job: start date - month
    fjob_yr = "n4147", # 1st job: start date - year
    
    pay = "n4223", # Pay in first job
    prd = "n4227", # Period of pay in first job
    paytype = "n4228", # Deductions in first job pay
  )
#### Age 23 ####
ncds23 = read.dta(file.path(loc_NCDS_23, "ncds4.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    
    hours = "n4331", # Current job hours of work
    # hours_suit = "n4335", # Suitability of hours
    # hours_constraint = "n4336", # Constraints on hours
    # tradeoff_hourspay= "n4337", # Trade off hours and pay
    
    work_evening = "n4326", # Evening work
    work_night  = "n4327", # Night work
    work_early = "n4328", # Early morning work
    work_saturday = "n4329", # Saturday work
    work_sunday = "n4330", # Sunday work
    
    gropay = "n4269", # Current job gross pay
    groprd = "n4274", # Current job gross pay period
    gropay_hr = "n4269ph", # Gross earnings per hour
    gropay_wk = "n4269pw", # Current job - gross pay per week
    
    netpay = "n4262", # Current job net pay
    netprd = "n4267", # Current job net pay period
    netpay_hr = "n4262ph", # Net earnings per hour
    netpay_wk = "n4262pw", # Current job - net pay per week
  ) %>% 
  # Change missing values to NA
  mutate_if(is.numeric, function(x){ifelse(x < 0, NA, x)}) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(work_weekend = ifelse(work_saturday == "YES" | work_sunday ==  "YES",
                               "YES", "NO")) %>%
  mutate(age = 23) %>%  
  select(id, age, hours, work_evening, work_night, work_early, work_weekend,
         gropay, groprd, netpay, netprd)
#### Age 33 ####
ncds33 = read.dta(file.path(loc_NCDS_33, "ncds5cmi.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    
    chours1 = "n500550", # CURRENT/MOST RECENT JOB: Usual hours/week for usual pay
    chours2 = "n500552", # CURRENT/MOST RECENT JOB: NO USUAL HOURS-Work 30/+ hrs/wk
    chours3 = "n500567", # CURRENT/MOST RECENT JOB: Hours/week worked for last pay
    chours4 = "n500569", # CURRENT/MOST RECENT JOB: Are/were hours usually worked
    chours5 = "n500570", # CURRENT/MOST RECENT JOB: Hours/week usually worked
    
    work_evening = "n500572", # CURRENT/MOST RECENT JOB: Work 6pm-10pm 1/+ times/month
    work_night = "n500573", # CURRENT/MOST RECENT JOB: Work 10pm-4am 1/+ times/month
    work_early = "n500574", # CURRENT/MOST RECENT JOB: Work 4am-7am 1/+ times/month
    work_saturday = "n500613", # CURRENT/MOST RECENT JOB: Work Saturdays 1/+ times/month
    work_sunday = "n500614", # CURRENT/MOST RECENT JOB: Work Sundays 1/+ times/month
    
    gropay = "n500543", # CURRENT/MOST RECENT JOB: Last gross pay before
    groprd = "n500549", # CURRENT/MOST RECENT JOB: Period for last gross pay
    gropay2 = "n500560", # CURRENT/MOST RECENT JOB: Last gross pay before
    groprd2 = "n500566", # CURRENT/MOST RECENT JOB: Period for last gross pay
    
    netpay = "n500536", # CURRENT/MOST RECENT JOB: Last take home pay
    netprd = "n500542", # CURRENT/MOST RECENT JOB: Period for last take home pay
    netpay2 = "n500553", # CURRENT/MOST RECENT JOB: Last take home pay
    netprd2 = "n500559", # CURRENT/MOST RECENT JOB: Period for last take home pay
    
  ) %>% 
  # Change missing values to NA
  mutate_if(is.numeric, function(x) ifelse(x < 0, NA, x)) %>% 
  mutate_if(is.factor, as.character) %>% 
  # Remove NA for pay
  mutate_at(vars(gropay, netpay), function(x) ifelse(x %in% c(999999, 999996, 0), NA, x)) %>% 
  mutate(
    # Remove Period when Pay is NA
    groprd = groprd %>% ifelse(is.na(gropay), NA, .),
    netprd = netprd %>% ifelse(is.na(netpay), NA, .),
    
    # Replace
    gropay = gropay %>% ifelse(is.na(gropay) & !is.na(gropay2), gropay2, .),
    groprd = groprd %>% ifelse(is.na(groprd) & !is.na(groprd2), groprd2, .),
    netpay = netpay %>% ifelse(is.na(netpay) & !is.na(netpay2), netpay2, .),
    netprd = netprd %>% ifelse(is.na(netprd) & !is.na(netprd2), netprd2, .),
    
    # Compute hours
    hours = case_when(
      !is.na(chours1) ~ chours1,
      !is.na(chours3) ~ chours3,
      !is.na(chours5) ~ chours5
    ),
    
    # Working conditions
    work_weekend = ifelse(work_saturday == "Yes" | work_sunday == "Yes", "Yes", "No"),
  ) %>%
  mutate(age = 33) %>% 
  select(id, age, hours, work_evening, work_night, work_early, work_weekend,
         gropay, groprd, netpay, netprd)
#### Age 42 ####
ncds42 = read.dta(file.path(loc_NCDS_42, "ncds6.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    
    chours1 = "chours1", # Usual hrs worked p/wk  (excl. meal breaks)
    chours1_se = "cjsehrs", # Self employed: usual hours worked p/wk
    chours2 = "chours2", # Usual hours worked p/wk (excl. meal breaks and overtime)
    chours3 = "chours3", # Usual hrs worked p/wk as paid overtime
    chours4 = "chours4", # Usual hrs worked p/wk as unpaid overtime
    chours5 = "chours5", # INTERVIEWER confirms total hrs p/wk CM usually works
    
    work_evening = "cshifts1", # How often works after 6pm up to 10pm
    work_evening_se = "cseshft1", # Self emp: how often work after 6pm up to 10pm
    work_night = "cshifts2", # How often works after 10pm up to 4am
    work_night_se = "cseshft2", # Self emp: how often work after 10pm up to 4am
    work_early = "cshifts3", # How often works after 4am up to 7am
    work_early_se = "cseshft3", # Self emp: how often work after 4am up to 7am
    work_weekend = "cshifts4", # How often works at weekends
    work_weekend_se = "cseshft4", # Self emp: how often work at weekends
    
    gropay = "cgropay", # Gross pay: amount
    groprd = "cgroprd", # Gross pay: period amount covered
    gropred = "cgropred", # BACKCODE from CGroPrd
    
    netpay = "cnetpay", # Take-home [net] pay: amount
    netprd = "cnetprd", # Take-home pay: period amount covered
    netpred = "cnetpred", # BACKCODE from CNerPrd
    
  ) %>% 
  # Change missing values to NA
  mutate_if(is.numeric, function(x){ifelse(x < 0, NA, x)}) %>% 
  mutate_if(is.factor, as.character) %>% 
  rowwise() %>% 
  mutate(
    # Hours per week
    hours = ifelse(!is.na(chours1), chours1, chours2) %>% 
      ifelse(is.na(.), chours1_se, .) %>% 
      as.numeric() %>% 
      ifelse(. %in% c(0, 999, 998, 99, 98), NA, .),
    # Working conditions
    work_evening = ifelse(work_evening == "Item not applicable", work_evening_se, work_evening),
    work_night = ifelse(work_evening == "Item not applicable", work_night_se, work_night),
    work_early = ifelse(work_early == "Item not applicable", work_early_se, work_early),
    work_weekend = ifelse(work_weekend == "Item not applicable", work_weekend_se, work_weekend),
  ) %>% 
  ungroup() %>% 
  # Missing values
  mutate_at(
    vars(netpay, gropay), 
    function(x) x %>% ifelse(. %in% c(1, 9999998, 9999999), NA, .) %>% as.numeric()
  ) %>% 
  # Other periods
  mutate(
    netprd = netprd %>% ifelse(. == "Other period", netpred, .),
    groprd = groprd %>% ifelse(. == "Other period", gropred, .),
  ) %>% 
  mutate(age = 42) %>% 
  select(id, age, hours, work_evening, work_night, work_early, work_weekend,
         gropay, groprd, netpay, netprd)
#### Age 47 ####
ncds47 = read.dta(file.path(loc_NCDS_47, "ncds7.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    
    hours = "n7chour1", # Number of hours worked per week, excluding meal breaks
    hours_se = "n7cjsehs", # Number of hours worked per week
    
    # No hours constraints data
    
    gropay = "n7cgropy", # Total gross pay
    groprd = "n7cgropd", # Period gross pay covered
    
    netpay = "n7cnetpy", # Total take-home pay
    netprd = "n7cnetpd", # Period take-home pay covered
    
  ) %>% 
  # Change missing values to NA
  mutate_if(is.numeric, function(x){ifelse(x < 0, NA, x)}) %>% 
  mutate_if(is.factor, as.character) %>%
  mutate(work_evening = NA, work_night = NA, work_early = NA, work_weekend = NA) %>% 
  mutate(age = 47) %>% 
  select(id, age, hours, work_evening, work_night, work_early, work_weekend,
         gropay, groprd, netpay, netprd)
#### Age 50 ####
ncds50 = read.dta(file.path(loc_NCDS_50, "ncds_2008_followup.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    chours1 = "n8chour1", # [CHOURS1] Curr Job Emp: Num hours worked per week, excl. breaks (no otime)
    chours1_se = "n8cjsehs", # [CJSEHRS] Curr Job S/e:Number of hours worked per week
    chours2 = "n8chour2", # [CHOURS2] Curr Job Emp: Num hours worked/week, excl. overtime (works otime)
    chours3 = "n8chour3", # [CHOURS3] Curr Job Emp: Num hours of paid overtime per week (works otime) 
    chours4 = "n8chour4", # [CHOURS4] Curr Job Emp: Num hours of unpaid overtime per week (works otime)
    chours5 = "n8chour5", # [CHOURS5] Curr Job Emp: Is tot num hours worked/week ~right? (works otime)
    
    work_evening = "n8chour6", # [CHOURS6] Curr Job Emp: Freq CM works in the evening between 6-10pm 
    work_evening_se = "n8csest1", # [CSESHFT1] Curr Job S/e:Freq CM works in the evening between 6-10pm 
    work_night = "n8chour7", # [CHOURS7] Curr Job Emp: Freq CM works at night between 10pm-4am 
    work_night_se = "n8csest2", # [CSESHFT2] Curr Job S/e:Freq CM works at night between 10pm-4am 
    work_early = "n8chour8", # [CHOURS8] Curr Job Emp: Freq CM works early morning between 4am-7am 
    work_early_se = "n8csest3", # [CSESHFT3] Curr Job S/e:Freq CM works early morning between 4am-7am
    work_weekend = "n8chour9", # [CHOURS9] Curr Job Emp: Freq CM works at weekends
    work_weekend_se = "n8csest4", # [CSESHFT4] Curr Job S/e:Freq CM works at weekends
    
    gropay = "n8cgropy", # [CGROPAY] Curr Job Emp: Total gross pay 
    groprd = "n8cgropd", # [CGROPRD] Curr Job Emp: Period gross pay covered
    
    netpay = "n8cnetpy", # [CNETPAY] Curr Job Emp: Total take-home pay
    netprd = "n8cnetpd", # [CNETPRD] Curr Job Emp: Period take-home pay covered
  ) %>% 
  # Change missing values to NA
  mutate_if(is.numeric, function(x){ifelse(x < 0, NA, x)}) %>% 
  mutate_if(is.factor, as.character) %>% 
  # Hours per week / Working conditions
  rowwise() %>% 
  mutate(
    hours = ifelse(!is.na(chours1), chours1, chours2) %>%
      ifelse(!is.na(.), ., chours1_se) %>% 
      as.numeric(),
    work_evening = ifelse(work_evening == "Item not applicable", work_evening_se, work_evening),
    work_night = ifelse(work_evening == "Item not applicable", work_night_se, work_night),
    work_early = ifelse(work_early == "Item not applicable", work_early_se, work_early),
    work_weekend = ifelse(work_weekend == "Item not applicable", work_weekend_se, work_weekend),
  ) %>% 
  ungroup() %>% 
  mutate(age = 50) %>% 
  select(id, age, hours, work_evening, work_night, work_early, work_weekend,
         gropay, groprd, netpay, netprd)
#### 1.2 BCS70 ####
#### Age 26 ####
bcs26 = read.dta(file.path(loc_BCS_26, "bcs96x.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    hours = "b960277", # Number of hours usually works
    
    # No data about working conditions
    
    netpay = "b960312", # Net pay
    netprd = "b960318", # Period pay covers
    netprd_other = "q19other", # Q19 Other pay period     
  ) %>% 
  # Change missing values to NA
  mutate_if(is.numeric, function(x){ifelse(x < 0, NA, x)}) %>% 
  mutate_if(is.factor, as.character) %>%
  mutate(work_evening = NA, work_night = NA, work_early = NA, work_weekend = NA) %>% 
  mutate(age = 26, gropay = NA, groprd = NA) %>% 
  select(id, age, hours, work_evening, work_night, work_early, work_weekend,
         gropay, groprd, netpay, netprd)
#### Age 30 ####
bcs30 = read.dta(file.path(loc_BCS_30, "bcs2000.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    
    chours1 = "chours1", # Usual hrs worked p/wk  (excl. meal breaks)
    chours1_se = "cjsehrs", # Self employed: usual hours worked p/wk
    chours2 = "chours2", # Usual hours worked p/wk (excl. meal breaks and overtime)
    chours3 = "chours3", # Usual hrs worked p/wk as paid overtime
    chours4 = "chours4", # Usual hrs worked p/wk as unpaid overtime
    chours5 = "chours5", # INTERVIEWER confirms total hrs p/wk CM usually works
    
    work_evening = "cshifts1", # How often works after 6pm up to 10pm
    work_evening_se = "cseshft1", # Self emp: how often work after 6pm up to 10pm
    work_night = "cshifts2", # How often works after 10pm up to 4am
    work_night_se = "cseshft2", # Self emp: how often work after 10pm up to 4am
    work_early = "cshifts3", # How often works after 4am up to 7am
    work_early_se = "cseshft3", # Self emp: how often work after 4am up to 7am
    work_weekend = "cshifts4", # How often works at weekends
    work_weekend_se = "cseshft4", # Self emp: how often work at weekends
    
    gropay = "cgropay", # Gross pay: amount
    groprd = "cgroprd", # Gross pay: period amount covered
    gropred = "cgropred", # BACKCODE from CGroPrd
    
    netpay = "cnetpay", # Take-home [net] pay: amount
    netprd = "cnetprd", # Take-home pay: period amount covered
    netpred = "cnetpred", # BACKCODE from CNerPrd
    
    # seprofit, # Own bus: how much profit/loss did CM make
    # seprfsyr, # Own bus: year accounting period started
    # seprfsmo, # Own bus: month accounting period started
    # seprfeyr, # Own bus: year accounting period ended
    # seprfemo, # Own bus: month accounting period ended
    # 
    # seearn, # Self acc: earnings before tax in last yr
    # seernsyr, # Self acc: year accounting period started
    # seernsmo, # Self acc: month accounting period started
    # seerneyr, # Self acc: year accounting period ended
    # seernemo, # Self acc: month accounting period ended
    
  ) %>% 
  # Change missing values to NA
  mutate_if(is.numeric, function(x){ifelse(x < 0, NA, x)}) %>% 
  mutate_if(is.factor, as.character) %>%
  # Missing values
  mutate(
    netpay = netpay %>% 
      ifelse(. %in% c(1, 9999998, 9999999), NA, .) %>% 
      as.numeric(),
    gropay = gropay %>% 
      ifelse(. %in% c(1, 9999998, 9999999), NA, .) %>% 
      as.numeric(),
    chours1 = chours1 %>% ifelse(. %in% c(98, 99), NA, .),
    chours2 = chours2 %>% ifelse(. %in% c(999, 998), NA, .),
    chours3 = chours3 %>% ifelse(. %in% c(999, 998), NA, .),
    chours4 = chours4 %>% ifelse(. %in% c(999, 998), NA, .),
  ) %>% 
  # Hours per week
  rowwise() %>% 
  mutate(
    # Hours per week
    hours = ifelse(!is.na(chours1), chours1, chours2) %>% 
      ifelse(is.na(.), chours1_se, .) %>% 
      as.numeric() %>% 
      ifelse(. %in% c(0, 999, 998, 99, 98), NA, .),
    # Working conditions
    work_evening = ifelse(work_evening == "Item not applicable", work_evening_se, work_evening),
    work_night = ifelse(work_evening == "Item not applicable", work_night_se, work_night),
    work_early = ifelse(work_early == "Item not applicable", work_early_se, work_early),
    work_weekend = ifelse(work_weekend == "Item not applicable", work_weekend_se, work_weekend),
  ) %>% 
  ungroup() %>% 
  # Other periods
  mutate(
    netprd = netprd %>% ifelse(. == "Other period", netpred, .),
    groprd = groprd %>% ifelse(. == "Other period", gropred, .),
  ) %>% 
  mutate(age = 30) %>% 
  select(id, age, hours, work_evening, work_night, work_early, work_weekend,
         gropay, groprd, netpay, netprd)
#### Age 34 ####
bcs34 = read.dta(file.path(loc_BCS_34, "bcs_2004_followup.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    
    netpay = "b7cnetpy", # Total take-home pay (pounds)
    netprd = "b7cnetpd", # Period take-home pay covered
    
    gropay = "b7cgropy", # Total gross pay (pounds)
    groprd = "b7cgropd", # Period gross pay covered
    
    # Hours
    chours1 = "b7chour1", # Number of hours worked per week (if employee) - not working overtime
    chours1_se = "b7cjsehs", # Number of hours worked per week (if self-employed)
    chours2 = "b7chour2", # Number of hours worked per week, excluding overtime - works overtime
    chours3 = "b7chour3", # Number of hours of paid overtime per week - works overtime
    chours4 = "b7chour4", # Number of hours of unpaid overtime per week - works overtime
    
    # Working conditions (self only)
    work_evening = "b7csest1", # Whether works evening shift
    work_night = "b7csest2", # Whether works night shift
    work_early = "b7csest3", # Whether works early morning shift
    work_weekend = "b7csest4", # Whether works in the weekends
    
  ) %>% 
  # Change negative missing values to NA for numeric variables
  mutate_if(is.numeric, function(x){ifelse(x < 0, NA, x)}) %>% 
  mutate_if(is.factor, as.character) %>%
  # Missing values
  mutate(
    netpay = netpay %>% as.numeric() %>% ifelse(. == 1, NA, .),
    gropay = gropay %>% as.numeric() %>% ifelse(. == 1, NA, .),
  ) %>% 
  # Hours per week
  rowwise() %>% 
  mutate(
    hours = ifelse(!is.na(chours1), chours1, chours2) %>%
      ifelse(!is.na(.), ., chours1_se) %>% 
      as.numeric() %>% ifelse(. <= 1, NA, .),
  ) %>% 
  ungroup() %>% 
  mutate(age = 34) %>% 
  select(id, age, hours, work_evening, work_night, work_early, work_weekend,
         gropay, groprd, netpay, netprd)
#### Age 38 ####
bcs38 = read.dta(file.path(loc_BCS_38, "bcs_2008_followup.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    
    netpay = "b8cnetpy", # Total Take-Home Pay
    netprd = "b8cnetpd", # Period Take-Home Pay Covered
    
    gropay = "b8cgropy", # Total Gross Pay
    groprd = "b8cgropd", # Period Gross Pay Covered
    
    hours = "b8chour1", # Number Of Hours Worked Per Week, Excluding Meal Breaks And Overtime
    hours_se = "b8cjsehs", # S/E Number Of Hours Worked Per Week
    
    # No data on working conditions
    
  ) %>% 
  # Change negative missing values to NA for numeric variables
  mutate_if(is.numeric, function(x){ifelse(x < 0, NA, x)}) %>% 
  mutate_if(is.factor, as.character) %>%
  mutate(hours = ifelse(!is.na(hours), hours, hours_se)) %>% 
  mutate(work_evening = NA, work_night = NA, work_early = NA, work_weekend = NA) %>% 
  mutate(age = 38) %>% 
  select(id, age, hours, work_evening, work_night, work_early, work_weekend,
         gropay, groprd, netpay, netprd)
#### Age 42 ####
bcs42 = read.dta(file.path(loc_BCS_42, "bcs70_2012_flatfile.dta")) %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    
    netpay = "b9neta", # Net pay- amount 
    netprd = "b9netp", # Net pay- period
    
    gropay = "b9groa", # Gross pay- amount
    groprd = "b9grop", # Gross pay- period
    
    # takehomeinc = "b9sepa", # Take home income in last year
    
    # Usual net pay
    usual = "b9pusl", # Whether net pay given is usual take home pay
    netpay_usual = "b9usla", # Usual net pay- amount
    netprd_usual = "b9uslp", # Usual net pay- period
    
    # Hours
    chours1 = "b9chour1", # Usual weekly working hours
    chours1_se = "b9cjsehs", # Usual weekly working hours
    chours2 = "b9chour2", # Usual weekly working hours not including overtime 
    chours3 = "b9chour3", # Number of hours paid overtime usually works per week
    chours4 = "b9chour4", # Number of hours unpaid overtime usually works per week
    
    # Working conditions
    work_evening = "b9chour6", # Frequency of working in the evening between 6 and 10pm
    work_night = "b9chour7", # Frequency of working at night between 10pm and 4am  
    work_early = "b9chour8", # Frequency of working in the early morning between 4 and 7am 
    work_weekend = "b9chour9", # Frequency of working at weekends
    
  ) %>% 
  # Change negative missing values to NA for numeric variables
  mutate_if(is.numeric, function(x){ifelse(x < 0, NA, x)}) %>% 
  mutate_if(is.factor, as.character) %>%
  rowwise() %>% 
  mutate(
    # Hours per week
    hours = ifelse(!is.na(chours1), chours1, chours2) %>%
      ifelse(is.na(.), chours1_se, .) %>% 
      as.numeric() %>% ifelse(. == 0, NA, .),
    # # Take home pay with self-employed
    # netpay = ifelse(!is.na(takehomeinc), takehomeinc, netpay),
    # netprd = ifelse(!is.na(takehomeinc), "One year/12 months/52 weeks", netprd),
  ) %>% 
  ungroup() %>% 
  # Usual take home
  mutate(
    netpay = ifelse(usual == "No", netpay_usual, netpay),
    netprd = ifelse(usual == "No", netprd_usual, netprd),
  ) %>% 
  mutate(age = 42) %>% 
  select(id, age, hours, work_evening, work_night, work_early, work_weekend,
         gropay, groprd, netpay, netprd)
#### 2 RBIND ####
#### NCDS58 ####
ncds = ncds23 %>% rbind(ncds33) %>% rbind(ncds42) %>% rbind(ncds47) %>% rbind(ncds50) %>% 
  arrange(id, age) %>% 
  mutate(year = age + 1958)
#### BCS70 ####
bcs = bcs26 %>% rbind(bcs30) %>% rbind(bcs34) %>% rbind(bcs38) %>% rbind(bcs42) %>% 
  arrange(id, age) %>% 
  mutate(year = age + 1970)
#### BOTH ####
work_pay = rbind(ncds, bcs) %>% 
  select(id, age, year, hours, gropay, groprd, netpay, netprd)
work_cdt = rbind(ncds, bcs) %>% 
  select(id, age, year, starts_with("work"))
#### 3 TREATMENT ####
#### 3.1 PAY ####
# Reference all levels
period_levels = c("A calendar month", "Calendar month", "Month", "PER MONTH", 
                  "Two calendar months", 
                  "A year", "Year", "A year or", "One year/12 months/52 weeks", "Lump sum/one off", "PER YEAR",
                  "Eight times a year", "Nine times a year", "Ten times a year",
                  "Hourly", "Hour", 
                  "Day",  "Daily", "PER DAY",
                  "Less than one week", 
                  "One Week", "One week", "Week", "1 week", "PER WEEK",
                  "Two weeks", "A fortnight", "Fortnight", "PER TWO WEEKS",
                  "Three weeks", "Four weeks", "Five weeks", "Six weeks", "Seven weeks",
                  "Three months/13 weeks", "Three months", "PER 3  MONTHS", "Six months", "PER SIX MONTHS") %>% 
  setNames(c(rep("Month", 4), 
             "Two months", 
             rep("Year", 6), 
             "Eight times a year", "Nine times a year", "Ten times a year",
             rep("Hour", 2), 
             rep("Day", 3),
             "Less one week",
             rep("Week", 5),
             rep("Two weeks", 4), 
             "Three weeks", "Four weeks", "Five weeks", "Six weeks", "Seven weeks", 
             rep("Three months", 3), rep("Six months", 2)))
# Coefficients
period_coefs = names(period_levels) %>% 
  unique(.) %>% 
  setNames(c(7/30, 7/30/2, 1/52, 8/52, 9/52, 10/52, NA, 5, 5/2, 1, 1/2, 1/3, 1/4,
             1/5, 1/6, 1/7, 1/13, 1/26))
# Use levels
work_pay2 = work_pay %>% 
  mutate(
    groprd = groprd %>%
      factor(levels = period_levels, labels = names(period_levels)),
    netprd = netprd %>% 
      factor(levels = period_levels, labels = names(period_levels))
  ) %>% 
  mutate(
    grocoef = groprd %>% 
      factor(levels = period_coefs, labels = names(period_coefs)) %>% 
      as.character() %>% 
      as.numeric(),
    netcoef = netprd %>% 
      factor(levels = period_coefs, labels = names(period_coefs)) %>% 
      as.character() %>% 
      as.numeric(),
  ) %>% 
  mutate(
    grocoef = grocoef %>% ifelse(groprd == "Hour", hours, .),
    netcoef = netcoef %>% ifelse(netprd == "Hour", hours, .),
  ) %>% 
  mutate(
    gropay = gropay %>% ifelse(. == 0, NA, .),
    netpay = netpay %>% ifelse(. == 0, NA, .),
  ) %>% 
  mutate(
    # Per week
    gp_wk = gropay*grocoef,
    np_wk = netpay*netcoef,
    # Per hour
    gp_hr = gp_wk/hours,
    np_hr = np_wk/hours,
    # Gross/Net ratio and Net/Gross ratio
    gn_ratio = gp_wk/np_wk,
    ng_ratio = np_wk/gp_wk,
  )
#### 3.2 WORK CONDITIONS ####
# Reference all levels
condition_levels = c("At least once a month", "At least once a week", "Don't know",
                     "Don't Know", "Dont know", "Every day", "Every week", 
                     "Item not applicable", "Less often than once a month", 
                     "Less often than once a month, or", "Never", "No", "NO", 
                     "Not answered", "Not applicable", "Not Applicable", "Refusal",
                     "Refused", "Yes", "YES") %>% 
  setNames(c(rep("Yes", 2), rep("No", 3), rep("Yes", 2), NA, rep("No", 5),
             rep(NA, 5), rep("Yes", 2))) 
work_cdt2 = work_cdt %>% 
  mutate_at(vars(starts_with("work_")), ~ factor(., 
    levels = condition_levels, labels = names(condition_levels)))
#### 4 SAVE ####
write.csv(work_pay2, file.path(loc_cohort, "work_pay.csv"), row.names = FALSE)
write.csv(work_cdt2, file.path(loc_cohort, "work_conditions.csv"), row.names = FALSE)
