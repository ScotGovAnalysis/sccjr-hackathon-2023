library(haven)
library(tidyverse)

# setwd(project_path)

#### Read in SPSS datasets ####

# Non-victim form (NVF)
df_0809 <- read_spss("./data/scjs_s2_rf_091214.sav")
df_0910 <- read_spss("./data/scjs_s3_rf_ukda_110120.sav")
df_1011 <- read_spss("./data/scjs_s4_2010-11_rf_ukda_130115.sav")
df_1213 <- read_spss("./data/scjs_main_2012_13_5.sav")
df_1415 <- read_spss("./data/new_main_2014_15_2_main_dataset.sav")
df_1617 <- read_spss("./data/scjs1617_nvf-main_y1_eul.sav")
df_1718 <- read_spss("./data/scjs1718__nvf-main_y2_eul_20190508.sav")
df_1819 <- read_spss("./data/scjs1819_nvf-main_y3_eul-safeguarded_20210316_nvf.sav")
df_1819_cyber <- read_spss("./data/scjs1819_nvf-main_y3_eul-safeguarded_20210316_cyber.sav")
df_1920 <- read_spss("./data/scjs1920_nvf-main_y4_eul-safeguarded_20210322_nvf.sav")
df_1920_cyber <- read_spss("./data/scjs1920_nvf-main_y4_eul-safeguarded_20210322_cyber.sav")

# merge cyber on to 1819 and 1920 data with natural join
df_1819 <- left_join(df_1819, df_1819_cyber)
df_1920 <- left_join(df_1920, df_1920_cyber)

# remove cyber datasets now they are merged
rm(list = c("df_1819_cyber", "df_1920_cyber"))

# Create a list of dataframe

data_frames_list <- list(df_0809, df_0910, df_1011, 
                         df_1213, df_1415, df_1617, 
                         df_1718, df_1819, df_1920)


# Self-completion data (SC)
df_0809_sc <- read_spss("./data/scjs2_sc_091209.sav")
df_0910_sc <- read_spss("./data/scjs_s3_scf_110808.sav")
df_1011_sc <- read_spss("./data/scjs_s4_2010-11_sc_ukda_130115.sav")