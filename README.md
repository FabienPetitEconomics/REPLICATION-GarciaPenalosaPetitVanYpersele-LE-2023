# García-Peñalosa, Petit and van Ypersele (2023). "Can workers still climb the social ladder as middling jobs become scarce? Evidence from two British cohorts". Labour Economics.

<p align="justify"><b>Abstract</b>: The increase in employment polarization observed in several high-income economies has coincided with a reduction in inter-generational mobility. This paper argues that the disappearance of middling jobs can drive changes in mobility, notably by removing a stepping stone towards high-paying occupations for those from less well-off family backgrounds. Using data from two British cohorts who entered the labour market at two points in time with very different degrees of employment polarization, we examine how parental income affects both entry occupations and occupational upgrading over careers. We find that transitions across occupations are key to mobility and that the impact of parental income has grown over time. At regional level, using a shift-share IV-strategy, we show that the impact of parental income has increased the most in regions experiencing the greatest increase in polarisation. This indicates that the disappearance of middling jobs played a role in the observed decline in mobility.</p>

# Data Availability Statements

All data used to support the findings of this study have been deposited in `_data/`. The raw data are provided by [UKDATA](https://ukdataservice.ac.uk/) and require an authorization.

# Dataset list

This list contains the final datasets used in the `main.Rmd` notebook. Original data are available in `_data/_raw/`. Cleaned data in csv format are available in `_data/_clean/`. Cleaning of data are performed with scripts in `_script/_cleaner/`. Aggregation of the cleaned data are performed with the script `_script/project_data.R`.

| DATA FILE               | SOURCE                    | NOTES               | PROVIDED |
| ----------------------- | ------------------------- | ------------------- | -------- |
| `_data/classification_isco88.csv`  | [UKDATA](https://ukdataservice.ac.uk/) | As per terms of use | No      |
| `_data/cohort_acthist.csv`  | [UKDATA](https://ukdataservice.ac.uk/) | As per terms of use | No      |
| `_data/cohort_data.csv`  | [UKDATA](https://ukdataservice.ac.uk/) | As per terms of use | No      |
| `_data/first_job.csv`   | [UKDATA](https://ukdataservice.ac.uk/) | As per terms of use | No      |
| `_data/series-180322.csv`   | [ONS](https://www.ons.gov.uk/economy/grossdomesticproductgdp/timeseries/abmi/pn2) | As per terms of use | Yes      |

# Computational requirements

## Software requirements (R Session info)

R version 4.2.2 (2022-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 22621)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
[3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.utf8    

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] effects_4.2-2      carData_3.0-5      nnet_7.3-18        knitr_1.42         kableExtra_1.3.4  
 [6] texreg_1.38.6      gridExtra_2.3      ggpubr_0.5.0       RColorBrewer_1.1-3 ggrepel_0.9.3     
[11] ggplot2_3.4.0      zoo_1.8-11         lubridate_1.9.1    stringr_1.5.0      data.table_1.14.6 
[16] tidyselect_1.2.0   tidyr_1.3.0        reshape2_1.4.4     dplyr_1.1.0        readr_2.1.3       
[21] haven_2.5.1        foreign_0.8-84    

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.10       svglite_2.1.1     lattice_0.20-45   digest_0.6.31     utf8_1.2.3        R6_2.5.1         
 [7] plyr_1.8.8        backports_1.4.1   survey_4.1-1      evaluate_0.20     httr_1.4.4        pillar_1.8.1     
[13] rlang_1.0.6       minqa_1.2.5       rstudioapi_0.14   nloptr_2.0.3      car_3.1-1         Matrix_1.5-1     
[19] rmarkdown_2.20    splines_4.2.2     lme4_1.1-31       webshot_0.5.4     munsell_0.5.0     broom_1.0.3      
[25] compiler_4.2.2    xfun_0.37         pkgconfig_2.0.3   systemfonts_1.0.4 mitools_2.4       htmltools_0.5.4  
[31] insight_0.19.0    tibble_3.1.8      fansi_1.0.4       viridisLite_0.4.1 tzdb_0.3.0        withr_2.5.0      
[37] MASS_7.3-58.1     DBI_1.1.3         nlme_3.1-160      gtable_0.3.1      lifecycle_1.0.3   magrittr_2.0.3   
[43] scales_1.2.1      cli_3.6.0         stringi_1.7.12    ggsignif_0.6.4    xml2_1.3.3        ellipsis_0.3.2   
[49] generics_0.1.3    vctrs_0.5.2       boot_1.3-28       tools_4.2.2       forcats_1.0.0     glue_1.6.2       
[55] purrr_1.0.1       hms_1.1.2         survival_3.4-0    abind_1.4-5       fastmap_1.1.0     yaml_2.3.7       
[61] timechange_0.2.0  colorspace_2.1-0  rstatix_0.7.2     rvest_1.0.3 

- the file `_script/init.R` will install all dependencies (latest version), and should be run once prior to running other programs.

## Descriptions of programs

- Scripts in `_script/_cleaner/` clean all the data used in the analysis from `_data/_raw/`.
- The script `project_data.R` generates the final datasets used in the analysis.
- Notebook `main.Rmd` generates all graphs and tables of this study.

## Device specifications

The code was last run on a laptop with Processor 12th Gen Intel(R) Core(TM) i7-1265U and Windows 11 Enterprise Version	22H2 (OS build	22621.1848).

# Instructions

- Get access to the same datasets from [UKDATA](https://ukdataservice.ac.uk/).
- Download them and organize them in the folder `_data`, see `_data/README.md` for more details.
- Run all the scripts `_script/` to clean the data.
- Run `main.Rmd` to generates all graphs and tables of this study.

# List of directories and files

Each directory contains a README file describing each file and subdirectory.

| DATA FILE               | DESCRIPTION                                                    |
| ----------------------- | -------------------------------------------------------------- |
| `_data`                 | All the data (raw, cleaned and final datasets) |
| `_function`             | All the functions |
| `_graphic`              | All the graphics |
| `_script`               | All the scripts |
| `_tabular`              | All the tabulars |
| `main.Rmd`              | Main notebook (R code) |

# Acknowledgements

Structure of this file was copied from the [Template README and Guidance of The Review of Economic Studies](https://restud.github.io/data-editor/template-README/).
