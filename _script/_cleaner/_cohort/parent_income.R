#### INITIALIZATION ####
source(file.path("_script", "init.R"))
# Load data on inflation and function
source(file.path(loc_function, "compute_deflator.R"))
#### LOAD DATA ####
### NCDS
## First Job
## Age 16
ncds16 = file.path(loc_NCDS_16, "ncds0123.dta") %>% 
  read.dta() %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "ncdsid", # ID
    
    # Parents' income
    inc16_wk_father = "n2462", # 3P Fathers weekly net pay
    inc16_wk_mother = "n2463", # 3P Mothers weekly net pay
    inc16_wk_other = "n2464", # 3P All other sources of net weekly income
    inc16_mt_father = "n2465", # 3P Fathers monthly net pay
    inc16_mt_mother = "n2466", # 3P Mothers monthly net pay
    inc16_mt_other = "n2467", # 3P All other sources of net monthly income
    
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == ""))
### BCS
## Age 10
bcs10 = file.path(loc_BCS_10, "sn3723.dta") %>% 
  read.dta() %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id = "bcsid", # ID
    contains("c9"),
    -c9_8,
  ) %>% 
  subset(!(get(vars_select(names(.), contains("id"))) == ""))
## Age 16
bcs16 = file.path(loc_BCS_16, "bcs7016x.dta") %>% 
  read.dta() %>% 
  setnames(., tolower(names(.))) %>% 
  select(
    id  = "bcsid", # ID
    inc16_group = "oe2", # Combined income of parents per wk/mth
  ) %>% 
  subset(! (get(vars_select(names(.), contains("id"))) == ""))
#### TREATMENT ####
### NCDS
## Age 16
# Wide to long
ncds16 = ncds16 %>% 
  mutate_if(is.factor, 
            function(x) x %>% as.integer %>% {.-1} %>% 
              ifelse(. == 0, NA, .)) %>% 
  reshape2::melt(id.vars = "id") %>% 
  filter(complete.cases(.)) %>% 
  tidyr::separate(col = "variable", into = c("rm1", "rm2", "from")) %>% 
  select(id, from, group = value) %>% 
  arrange(id, from)
# Levels income NCDS
levels_inc_NCDS = c(mean(c(0,4)), mean(c(5,9)), mean(c(10,14)), mean(c(15,19)), 
                    mean(c(20,24)), mean(c(25,29)), mean(c(30,34)), mean(c(35,39)), 
                    mean(c(40,44)), mean(c(45,49)), mean(c(50,59)), mean(c(60,70)))
# Apply mean category
# For the eleven first income categories, we take the mean of both boundaries. 
# However, for the last one, we decide to take 65 which corresponds to the net weekly income 
# of the entry in the last bracket of income tax in 1974, 
# i.e. 20000 £ annual income with a tax rate of 83%.
ncds16 = ncds16 %>% 
  mutate(income = group %>% factor(labels = levels_inc_NCDS) %>% as.character %>% as.numeric) %>% 
  select(id, from, income) %>% 
  reshape2::dcast(id ~ from, fill = 0, value.var = "income") %>% 
  mutate(total = father + mother + other, 
         age = 16,
         year = 1974,) %>% 
  select(id, age, year, income = total)
### BCS
## Age 10
# Wide to long
bcs10 = bcs10 %>% 
  reshape2::melt(id.vars = "id") %>% 
  filter(complete.cases(.)) %>% 
  tidyr::separate(variable, into = c("rm", "group")) %>% 
  select(id, group)
# Levels income BCS16
levels_inc_BCS10 = c(mean(c(0,34)), mean(c(35,49)), mean(c(50,99)), 
                    mean(c(100,149)), mean(c(150,199)), mean(c(200,249)), 
                    mean(c(250,27750/52+250)))
# We assume that the maximum available weekly income is about £783.65. 
# This value is computed such that the average with 500 is equal to the last threshold 
# of income taxation in 1986, i.e. 27750/52. We assign each individual to one of the 
# seven income groups and compute the mean between both boundaries of each group.
bcs10 = bcs10 %>% 
  mutate(income = group %>% factor(labels = levels_inc_BCS10) %>% as.character %>% as.numeric) %>% 
  select(id, income)
# Thus, we obtain the gross weekly parental income in 1980. 
# The next step is to compute the net income. 
# To do so, we use data on the income tax rates provided by the UK government national archives
# (https://webarchive.nationalarchives.gov.uk/20100202151414/http://www.hmrc.gov.uk/stats/tax_structure/incometaxrates_1974to1990.pdf).
# In 1980, the structure of the income tax, based on the annual taxable income, 
# was the following:
#
# * Up to 11,250: 30 %;
# * 11,250 - 13,250: 40 %;
# * 13,250 - 16,750: 45 %;
# * 16,750 - 22,250: 50 %;
# * 22,250 - 27,750: 55 %;
# * Over 27,750: 60 %.
# Since the income data are expressed in weekly terms, 
# we multiply them by 52 to obtain the annual income and 
# determine which tax rate should be applied.
bcs10 = bcs10 %>% 
  mutate(
    income_annual = income * 52,
    tax_rate = case_when(
      income_annual < 11250 ~ 0.3,
      income_annual < 13250 ~ 0.4,
      income_annual < 16750 ~ 0.45,
      income_annual < 22250 ~ 0.5,
      income_annual < 27750 ~ 0.55,
      income_annual >= 27750 ~ 0.6,
    ),
    income_net = income*(1-tax_rate),
    age = 10,
    year = 1980,
  ) %>% 
  select(id, age, year, income = income_net)
### Age 16
# Wide to long
bcs16 = bcs16 %>% 
  mutate_at(vars(inc16_group),
            function(x) x %>% 
              ifelse(. %in% c("Not asked", "Not stated", "No questionnaire", 
                              "Refuse to Answer", "Uncertain"), NA, .) %>% 
              {.-3}) %>% 
  filter(complete.cases(.))
# Levels income BCS16
levels_inc_BCS16 = c(mean(c(0,49)), mean(c(50,99)), mean(c(100,149)), mean(c(150,199)),
                     mean(c(200,249)), mean(c(250,299)), mean(c(300,349)), mean(c(350,399)),
                     mean(c(400,449)), mean(c(450,499)), mean(c(500,41200/52+500)))
# We assume that the maximum available weekly income is about £1292.31. 
# This value is computed such that the average with 500 is equal to the last threshold 
# of income taxation in 1986, i.e. 41200/52. We assign each individual to one of the 
# seven income groups and compute the mean between both boundaries of each group.
bcs16 = bcs16 %>% 
  mutate(income = inc16_group %>% 
           factor(labels = levels_inc_BCS16) %>% as.character %>% as.numeric) %>% 
  select(id, income)
# Thus, we obtain the gross weekly parental income in 1980. 
# The next step is to compute the net income. 
# To do so, we use data on the income tax rates provided by the UK government national archives
# (https://webarchive.nationalarchives.gov.uk/20100202151414/http://www.hmrc.gov.uk/stats/tax_structure/incometaxrates_1974to1990.pdf)
# In 1986, the structure of the income tax, based on the annual taxable income, 
# was the following:
#   
# * Up to 17,200: 29 %;
# * 17,200 - 20,200: 40 %;
# * 20,200 - 25,400: 45 %;
# * 25,400 - 33,330: 50 %;
# * 33,330 - 41,200: 55 %;
# * Over 41,200: 60 %.
# 
# Since the income data are expressed in weekly terms, 
# we multiply them by 52 to obtain the annual income and 
# determine which tax rate should be applied.
bcs16 = bcs16 %>% 
  mutate(
    income_annual = income * 52,
    tax_rate = case_when(
      income_annual < 17200 ~ 0.3,
      income_annual < 20200 ~ 0.4,
      income_annual < 25400 ~ 0.45,
      income_annual < 33300 ~ 0.5,
      income_annual < 41200 ~ 0.55,
      income_annual >= 41200 ~ 0.6,
    ),
    income_net = income*(1-tax_rate),
    age = 16,
    year = 1986,
  ) %>% 
  select(id, age, year, income = income_net)
### RBIND
## Parental Income
parent_income = ncds16 %>% rbind(bcs10) %>% rbind(bcs16)
### Deflator
def = compute_deflator(1970)
### Merge Parental Income w/ Deflator
parent_income = parent_income %>% 
  merge(def, by = "year") %>% 
  select(id, age, year, income, deflator) %>% 
  mutate(
    income_def70 = income*deflator,
  )
#### SAVE ####
write.csv(parent_income, file.path(loc_cohort, "parent_income.csv"), row.names = FALSE)