#### LOCATION ####
# Standard file location
loc_data <- file.path(getwd(), "_data")
loc_function <- file.path(getwd(), "_function")
loc_graphic <- file.path(getwd(), "_graphic")
loc_script <- file.path(getwd(), "_script")
loc_tabular <- file.path(getwd(), "_tabular")

#### PACKAGES ####
# List all packages
packages <- c(
  # Load Data
  "foreign", "haven", "readr",
  # Deal with Data
  "dplyr", "reshape2", "tidyr", "tidyselect", "data.table",
  "stringr", # String
  "lubridate", # Date format
  "zoo", # Time series
  # Graphics
  "ggplot2", "ggrepel", "RColorBrewer", "grid", "ggpubr", "gridExtra",
  # Tables
  "texreg",
  # Knitr and Kable
  "kableExtra", "knitr",
  # Analysis
  "nnet", "effects")
# Load packages
lapply(packages, require, character.only = TRUE)
# Clear
rm(packages)

# knitr options
knitr::opts_chunk$set(echo = T, warning = F, message = F)
options(kableExtra.html.bsTable = T)
options(knitr.kable.NA = '')

#### FUNCTIONS ####
# Standardize
standardize <- function(x){(x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)}
# Peer-Inclusive Ranking (PIR)
PIRanking <- function(x){rank(x, ties.method = "max", na.last = "keep")/sum(!is.na(x))}
# Tabular to generate tables in TeX format
source(file.path(loc_function, "tabular.R"))
# Compute deflator
source(file.path(loc_function, "compute_deflator.R"))

# LFS
# Cleaned
loc_LFS_decade <- "_data/_clean/_lfs/_decade/"

# 1975 to 1991 (every Year)
loc_LFS_1975 <- "_data/_raw/LFS/UKDA-1758-stata6/stata6/"
loc_LFS_1977 <- "_data/_raw/LFS/UKDA-1757-stata6/stata6/"
loc_LFS_1979 <- "_data/_raw/LFS/UKDA-1756-stata11/stata11/"
loc_LFS_1981 <- "_data/_raw/LFS/UKDA-1888-stata11/stata11/"
loc_LFS_1983 <- "_data/_raw/LFS/UKDA-2029-stata6/stata6/"
loc_LFS_1984 <- "_data/_raw/LFS/UKDA-2143-stata6/stata6/"
loc_LFS_1985 <- "_data/_raw/LFS/UKDA-2265-stata6/stata6/"
loc_LFS_1986 <- "_data/_raw/LFS/UKDA-2360-stata6/stata6/"
loc_LFS_1987 <- "_data/_raw/LFS/UKDA-2720-stata6/stata6/"
loc_LFS_1988 <- "_data/_raw/LFS/UKDA-2721-stata6/stata6/"
loc_LFS_1989 <- "_data/_raw/LFS/UKDA-2722-stata6/stata6/"
loc_LFS_1990 <- "_data/_raw/LFS/UKDA-2839-stata6/stata6/"
loc_LFS_1991 <- "_data/_raw/LFS/UKDA-2875-stata6/stata6/"

# 1992 to 2001 (every quarter)
loc_LFS_1992Q2 <- "_data/_raw/LFS/UKDA-5887-stata8/stata8"
loc_LFS_1992Q3 <- "_data/_raw/LFS/UKDA-5888-stata8/stata8"
loc_LFS_1992Q4 <- "_data/_raw/LFS/UKDA-5889-stata8/stata8"

loc_LFS_1993Q2 <- "_data/_raw/LFS/UKDA-5884-stata8/stata8"
loc_LFS_1993Q3 <- "_data/_raw/LFS/UKDA-5885-stata8/stata8"
loc_LFS_1993Q4 <- "_data/_raw/LFS/UKDA-5886-stata8/stata8"

loc_LFS_1994Q1 <- "_data/_raw/LFS/UKDA-5879-stata8/stata8"
loc_LFS_1994Q2 <- "_data/_raw/LFS/UKDA-5880-stata8/stata8"
loc_LFS_1994Q3 <- "_data/_raw/LFS/UKDA-5881-stata8/stata8"
loc_LFS_1994Q4 <- "_data/_raw/LFS/UKDA-5882-stata8/stata8"

loc_LFS_1995Q1 <- "_data/_raw/LFS/UKDA-5875-stata8/stata8"
loc_LFS_1995Q2 <- "_data/_raw/LFS/UKDA-5876-stata8/stata8"
loc_LFS_1995Q3 <- "_data/_raw/LFS/UKDA-5877-stata8/stata8"
loc_LFS_1995Q4 <- "_data/_raw/LFS/UKDA-5878-stata8/stata8"

loc_LFS_1996Q1 <- "_data/_raw/LFS/UKDA-5871-stata8/stata8"
loc_LFS_1996Q2 <- "_data/_raw/LFS/UKDA-5872-stata8/stata8"
loc_LFS_1996Q3 <- "_data/_raw/LFS/UKDA-5873-stata8/stata8"
loc_LFS_1996Q4 <- "_data/_raw/LFS/UKDA-5874-stata8/stata8"

loc_LFS_1997Q1 <- "_data/_raw/LFS/UKDA-5869-stata8/stata8"
loc_LFS_1997Q2 <- "_data/_raw/LFS/UKDA-5414-stata8/stata8"
loc_LFS_1997Q3 <- "_data/_raw/LFS/UKDA-5870-stata8/stata8"
loc_LFS_1997Q4 <- "_data/_raw/LFS/UKDA-5415-stata8/stata8"

loc_LFS_1998Q1 <- "_data/_raw/LFS/UKDA-5865-stata8/stata8"
loc_LFS_1998Q2 <- "_data/_raw/LFS/UKDA-5866-stata8/stata8"
loc_LFS_1998Q3 <- "_data/_raw/LFS/UKDA-5867-stata8/stata8"
loc_LFS_1998Q4 <- "_data/_raw/LFS/UKDA-5868-stata8/stata8"

loc_LFS_1999Q1 <- "_data/_raw/LFS/UKDA-5863-stata8/stata8"
loc_LFS_1999Q2 <- "_data/_raw/LFS/UKDA-5416-stata8/stata8"
loc_LFS_1999Q3 <- "_data/_raw/LFS/UKDA-5864-stata8/stata8"
loc_LFS_1999Q4 <- "_data/_raw/LFS/UKDA-5417-stata8/stata8"

loc_LFS_2000Q1 <- "_data/_raw/LFS/UKDA-5856-stata8/stata8"
loc_LFS_2000Q2 <- "_data/_raw/LFS/UKDA-5857-stata8/stata8"
loc_LFS_2000Q3 <- "_data/_raw/LFS/UKDA-5858-stata8/stata8"
loc_LFS_2000Q4 <- "_data/_raw/LFS/UKDA-5859-stata8/stata8"

loc_LFS_2001Q1 <- "_data/_raw/LFS/UKDA-5854-stata8/stata8"
loc_LFS_2001Q2 <- "_data/_raw/LFS/UKDA-5418-stata8/stata8"
loc_LFS_2001Q3 <- "_data/_raw/LFS/UKDA-5855-stata11/stata11"
loc_LFS_2001Q4 <- "_data/_raw/LFS/UKDA-5419-stata11/stata11"

# 2002
loc_LFS_2002Q1 <- "_data/_raw/LFS/UKDA-5846-stata11/stata11"
loc_LFS_2002Q2 <- "_data/_raw/LFS/UKDA-5420-stata11/stata11"
loc_LFS_2002Q3 <- "_data/_raw/LFS/UKDA-5847-stata11/stata11"
loc_LFS_2002Q4 <- "_data/_raw/LFS/UKDA-5421-stata11/stata11"

loc_LFS_2003Q1 <- "_data/_raw/LFS/UKDA-5844-stata11/stata11"
loc_LFS_2003Q2 <- "_data/_raw/LFS/UKDA-5422-stata11/stata11"
loc_LFS_2003Q4 <- "_data/_raw/LFS/UKDA-5423-stata11/stata11"

loc_LFS_2004Q1 <- "_data/_raw/LFS/UKDA-5844-stata11/stata11"
loc_LFS_2004Q2 <- "_data/_raw/LFS/UKDA-5424-stata11/stata11"
loc_LFS_2004Q3 <- "_data/_raw/LFS/UKDA-5845-stata11/stata11"
loc_LFS_2004Q4 <- "_data/_raw/LFS/UKDA-5425-stata11/stata11"

loc_LFS_2005Q1 <- "_data/_raw/LFS/UKDA-5426-stata11/stata11"
loc_LFS_2005Q2 <- "_data/_raw/LFS/UKDA-5427-stata11/stata11"
loc_LFS_2005Q3 <- "_data/_raw/LFS/UKDA-5428-stata11/stata11"
loc_LFS_2005Q4 <- "_data/_raw/LFS/UKDA-5429-stata11/stata11"

loc_LFS_2006Q1 <- "_data/_raw/LFS/UKDA-5369-stata11/stata11"
loc_LFS_2006Q2 <- "_data/_raw/LFS/UKDA-5466-stata11/stata11"
loc_LFS_2006Q3 <- "_data/_raw/LFS/UKDA-5547-stata11/stata11"
loc_LFS_2006Q4 <- "_data/_raw/LFS/UKDA-5609-stata11/stata11"

loc_LFS_2007Q1 <- "_data/_raw/LFS/UKDA-5657-stata11/stata11"
loc_LFS_2007Q2 <- "_data/_raw/LFS/UKDA-5715-stata11/stata11"
loc_LFS_2007Q3 <- "_data/_raw/LFS/UKDA-5763-stata11/stata11"
loc_LFS_2007Q4 <- "_data/_raw/LFS/UKDA-5796-stata11/stata11"

loc_LFS_2008Q1 <- "_data/_raw/LFS/UKDA-5851-stata11/stata11"
loc_LFS_2008Q2 <- "_data/_raw/LFS/UKDA-6013-stata11/stata11"
loc_LFS_2008Q3 <- "_data/_raw/LFS/UKDA-6074-stata11/stata11"
loc_LFS_2008Q4 <- "_data/_raw/LFS/UKDA-6119-stata11/stata11"

loc_LFS_2009Q2 <- "_data/_raw/LFS/UKDA-6276-stata11/stata11"
loc_LFS_2009Q3 <- "_data/_raw/LFS/UKDA-6334-stata11/stata11"
loc_LFS_2009Q4 <- "_data/_raw/LFS/UKDA-6404-stata11/stata11"

loc_LFS_2010Q1 <- "_data/_raw/LFS/UKDA-6457-stata11/stata11"
loc_LFS_2010Q2 <- "_data/_raw/LFS/UKDA-6548-stata11/stata11"
loc_LFS_2010Q3 <- "_data/_raw/LFS/UKDA-6632-stata11/stata11"
loc_LFS_2010Q4 <- "_data/_raw/LFS/UKDA-6715-stata11/stata11"

loc_LFS_2011Q1 <- "_data/_raw/LFS/UKDA-6782-stata11/stata11"
loc_LFS_2011Q2 <- "_data/_raw/LFS/UKDA-6851-stata11/stata11"
loc_LFS_2011Q3 <- "_data/_raw/LFS/UKDA-6906-stata/stata/stata11"
loc_LFS_2011Q4 <- "_data/_raw/LFS/UKDA-6975-stata/stata/stata11"

loc_LFS_2012Q1 <- "_data/_raw/LFS/UKDA-7037-stata/stata/stata11"
loc_LFS_2012Q2 <- "_data/_raw/LFS/UKDA-7108-stata/stata/stata11"
loc_LFS_2012Q3 <- "_data/_raw/LFS/UKDA-7174-stata/stata/stata11"
loc_LFS_2012Q4 <- "_data/_raw/LFS/UKDA-7220-stata/stata/stata11"

loc_LFS_2013Q1 <- "_data/_raw/LFS/UKDA-7277-stata/stata/stata11"
loc_LFS_2013Q2 <- "_data/_raw/LFS/UKDA-7376-stata/stata/stata11"
loc_LFS_2013Q3 <- "_data/_raw/LFS/UKDA-7452-stata/stata/stata11"
loc_LFS_2013Q4 <- "_data/_raw/LFS/UKDA-7468-stata/stata/stata11"

loc_LFS_2014Q1 <- "_data/_raw/LFS/UKDA-7501-stata/stata/stata11"
loc_LFS_2014Q2 <- "_data/_raw/LFS/UKDA-7557-stata/stata/stata11"
loc_LFS_2014Q3 <- "_data/_raw/LFS/UKDA-7570-stata/stata/stata11"
loc_LFS_2014Q4 <- "_data/_raw/LFS/UKDA-7664-stata/stata/stata11"

loc_LFS_2015Q1 <- "_data/_raw/LFS/UKDA-7725-stata/stata/stata11"
loc_LFS_2015Q2 <- "_data/_raw/LFS/UKDA-7781-stata/stata/stata11"
loc_LFS_2015Q3 <- "_data/_raw/LFS/UKDA-7842-stata/stata/stata11"
loc_LFS_2015Q4 <- "_data/_raw/LFS/UKDA-7902-stata/stata/stata11"

loc_LFS_2016Q1 <- "_data/_raw/LFS/UKDA-7985-stata/stata/stata11"
loc_LFS_2016Q2 <- "_data/_raw/LFS/UKDA-8039-stata/stata/stata11"
loc_LFS_2016Q3 <- "_data/_raw/LFS/UKDA-8104-stata/stata/stata11"
loc_LFS_2016Q4 <- "_data/_raw/LFS/UKDA-8145-stata/stata/stata11"

loc_LFS_2017Q1 <- "_data/_raw/LFS/UKDA-8195-stata/stata/stata11"
loc_LFS_2017Q2 <- "_data/_raw/LFS/UKDA-8235-stata/stata/stata11"
loc_LFS_2017Q3 <- "_data/_raw/LFS/UKDA-8292-stata/stata/stata11"
loc_LFS_2017Q4 <- "_data/_raw/LFS/UKDA-8326-stata/stata/stata11"

loc_LFS_2018Q1 <- "_data/_raw/LFS/UKDA-8343-stata/stata/stata11"
loc_LFS_2018Q2 <- "_data/_raw/LFS/UKDA-8381-stata/stata/stata11"
loc_LFS_2018Q3 <- "_data/_raw/LFS/UKDA-8407-stata/stata/stata11"
loc_LFS_2018Q4 <- "_data/_raw/LFS/UKDA-8447-stata/stata/stata11"

loc_LFS_2019Q1 <- "_data/_raw/LFS/UKDA-8485-stata/stata/stata11"
loc_LFS_2019Q2 <- "_data/_raw/LFS/UKDA-8563-stata/stata/stata11"
loc_LFS_2019Q3 <- "_data/_raw/LFS/UKDA-8588-stata/stata/stata11"
loc_LFS_2019Q4 <- "_data/_raw/LFS/UKDA-8614-stata/stata/stata11"

# NCDS
loc_NCDS_acthist <- "_data/_raw/NCDS - GN 33004/UKDA-6942-stata11/stata11/"
loc_NCDS_response <- "_data/_raw/NCDS - GN 33004/UKDA-5560-stata13/stata13/"
loc_NCDS_16 <- "_data/_raw/NCDS - GN 33004/UKDA-5565-stata11/stata11/"
loc_NCDS_23 <- "_data/_raw/NCDS - GN 33004/UKDA-5566-stata9/stata9/"
loc_NCDS_33 <- "_data/_raw/NCDS - GN 33004/UKDA-5567-stata11_se/stata11_se/"
loc_NCDS_37 <- "_data/_raw/NCDS - GN 33004/UKDA-4992-stata8/stata8/"
loc_NCDS_42 <- "_data/_raw/NCDS - GN 33004/UKDA-5578-stata9_se/stata9_se/"
loc_NCDS_47 <- "_data/_raw/NCDS - GN 33004/UKDA-5579-stata9/stata9/"
loc_NCDS_50 <- "_data/_raw/NCDS - GN 33004/UKDA-6137-stata9_se/stata9_se/"
loc_NCDS_55 <- "_data/_raw/NCDS - GN 33004/UKDA-7669-stata11/stata11"

# BCS
loc_BCS_acthist <- "_data/_raw/BCS - GN 33229/UKDA-6943-stata11/stata11/"
loc_BCS_response <- "_data/_raw/BCS - GN 33229/UKDA-5641-stata11/stata11/"
loc_BCS_05 <- "_data/_raw/BCS - GN 33229/UKDA-2699-stata11/stata11/"
loc_BCS_10 <- "_data/_raw/BCS - GN 33229/UKDA-3723-stata11_se/stata11_se"
loc_BCS_16 <- "_data/_raw/BCS - GN 33229/UKDA-3535-stata11_se/stata11_se/"
loc_BCS_21 <- "_data/_raw/BCS - GN 33229/UKDA-4715-stata11/stata11/"
loc_BCS_26 <- "_data/_raw/BCS - GN 33229/UKDA-3833-stata11/stata11/"
loc_BCS_30 <- "_data/_raw/BCS - GN 33229/UKDA-5558-stata11_se/stata11_se/"
loc_BCS_34 <- "_data/_raw/BCS - GN 33229/UKDA-5585-stata11_se/stata11_se/"
loc_BCS_38 <- "_data/_raw/BCS - GN 33229/UKDA-6557-stata11_se/stata11_se/"
loc_BCS_42 <- "_data/_raw/BCS - GN 33229/UKDA-7473-stata11/stata11/"
loc_BCS_46 <- "_data/_raw/BCS - GN 33229/UKDA-8547-stata/stata/stata11/"

# BOTH (NCDS and BCS)

loc_BOTH_educ <- "_data/_raw/BOTH/UKDA-8127-stata11/stata11/"
loc_BOTH_occ <- "_data/_raw/BOTH/UKDA-7023-stata9/stata9/"

# CAMSIS Project
loc_CAMSIS <- "_data/_raw/CAMSIS"

# Cohort (cleaned)
loc_cleaner <- "_data/_script/_cleaner"
loc_cohort <- "_data/_clean/_cohort"
