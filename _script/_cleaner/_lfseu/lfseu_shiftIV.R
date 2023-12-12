# Initialization
source("_script/init.R")
# Read data
eulfs <- read_csv(file.path(loc_data, "_raw/LFS-EU/lfsa_egais_linear.csv"))
# Treatment
eulfs1 <- eulfs %>% 
  subset(sex == "T" & age == "Y25-49" & wstatus == "EMP") %>% 
  select(country = geo, year = TIME_PERIOD, isco08, value = OBS_VALUE) %>% 
  subset(isco08 %in% paste0("OC", 1:9)) %>% 
  mutate(isco08 = as.integer(str_remove(isco08, "OC"))) %>% 
  mutate(occ_group = factor(isco08, levels = c(1:3, 4, 6:8, 5, 9),
                            labels = c(rep("High", 3),
                                       rep("Mid", 4),
                                       rep("Low", 2)))) %>% 
  arrange(country, year) %>% 
  subset(year %in% c(1992, 2004)) %>% 
  group_by(country, year) %>% 
  mutate(value = value/sum(value)) %>% 
  as.data.table %>% 
  dcast(country + occ_group + isco08 ~ year, value.var = "value") %>% 
  rename(s92 = `1992`, s04 = `2004`)
# Preview of remaining countries
eulfs1 %>% 
  subset(!is.na(s92)) %>% 
  pull(country) %>% unique()
# Compute shift
eulfs2 <- eulfs1 %>% 
  subset(!is.na(s92)) %>%
  # subset(country %in% c("DE", "FR", "UK")) %>%
  # subset(country %in% c("DE", "ES", "FR", "IT", "NL", "UK")) %>%
  mutate(var = ifelse(country == "UK", "UK", "EU")) %>% 
  group_by(var, occ_group, isco08) %>% 
  summarize_at(vars("s92", "s04"), ~ mean(.)) %>% 
  mutate(shift = (s04-s92)*100) %>% 
  as.data.table %>% 
  dcast(occ_group + isco08 ~ var, 
        value.var = c("s92", "s04", "shift"), sep = "")
# Save
write.csv(eulfs2, file.path(loc_data, "_clean", "_lfseu", "lfseu_shiftIV.csv"), row.names = F)
