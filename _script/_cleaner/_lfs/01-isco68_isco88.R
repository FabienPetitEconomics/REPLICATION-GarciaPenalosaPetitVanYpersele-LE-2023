#### ISCO-68 to ISCO-88 (2 digits)
#### INITIALIZATION ####
source("_script/init.R")
# Load Translation ISCO68 to ISCO88 from http://www.harryganzeboom.nl/
isco6888 <- read.csv(file.path(loc_CAMSIS, "isco68", "isco6888.sps")) %>% 
  .[-c(1,2),] %>% as.data.frame %>% setNames("translate")
# Script file in SPS => Extract isco68 and isco88 for each column
isco6888A = isco6888 %>% 
  mutate(
    isco68 = str_extract(translate, "(\\d)+(?==)"),
    isco88 = str_extract(translate, "(?<==)(\\d)+")) %>% 
  select(isco68, isco88)
# Occupations in four digits change into two digits
# Compute the conditional probabilities for each ISCO-88
# Pr(ISCO-88 | ISCO-68) // Reoder by proportion of occ
isco68_isco88 = isco6888A %>% 
  mutate_at(vars(isco68, isco88), ~ substr(., 1,2)) %>% 
  count(isco68, isco88, name = "n") %>% 
  add_count(isco68, wt = n, name = "N") %>% 
  mutate(prop = n/N) %>% 
  arrange(isco68, -prop)
# Ranking is consistent with ISCO-68 occcupations (major groups)
# https://www.ilo.org/public/english/bureau/stat/isco/isco68/major.htm
write.csv(isco68_isco88, file.path(loc_CAMSIS, "isco68_isco88.csv"), row.names = F)
