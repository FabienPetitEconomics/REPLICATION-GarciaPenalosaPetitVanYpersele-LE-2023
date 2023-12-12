#### 0/ INITIALIZATION ####
source("_script/init.R")
# Load data
lfs7991 <- read.csv(file.path(loc_LFS_decade, "lfs7991.csv"), stringsAsFactors = T)
lfs9200 <- read.csv(file.path(loc_LFS_decade, "lfs9200.csv"), stringsAsFactors = T)
lfs0110 <- read.csv(file.path(loc_LFS_decade, "lfs0110.csv"), stringsAsFactors = T)
lfs1120 <- read.csv(file.path(loc_LFS_decade, "lfs1120.csv"), stringsAsFactors = T)
# Merge all
lfs <- do.call("rbind", list(lfs7991, lfs9200, lfs0110, lfs1120))
# Write all
write.csv(lfs, file.path(loc_data, "_clean", "_lfs", "lfs.csv"), row.names = F)
