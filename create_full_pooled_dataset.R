library(tidyverse)
library(haven)
library(sjlabelled)

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

# make all column names lower case
df_0809 <- df_0809 %>% rename_all(., .funs = tolower)
df_0910 <- df_0910 %>% rename_all(., .funs = tolower)
df_1011 <- df_1011 %>% rename_all(., .funs = tolower)
df_1213 <- df_1213 %>% rename_all(., .funs = tolower)
df_1415 <- df_1415 %>% rename_all(., .funs = tolower)
df_1617 <- df_1617 %>% rename_all(., .funs = tolower)
df_1718 <- df_1718 %>% rename_all(., .funs = tolower)
df_1819 <- df_1819 %>% rename_all(., .funs = tolower)
df_1920 <- df_1920 %>% rename_all(., .funs = tolower)


if("serial2" %in% colnames(df_0809)) {
  df_0809 <- 
    df_0809 %>% 
    rename(serial = serial2)
}

if("serial2" %in% colnames(df_0910)) {
  df_0910 <- 
    df_0910 %>% 
    rename(serial = serial2)
}

if("serial2" %in% colnames(df_1011)) {
  df_1011 <- 
    df_1011 %>% 
    rename(serial = serial2)
}

if("serial2" %in% colnames(df_1213)) {
  df_1213 <- 
    df_1213 %>% 
    rename(serial = serial2)
}

if("serial2" %in% colnames(df_1415)) {
  df_1415 <- 
    df_1415 %>% 
    rename(serial = serial2)
}

if("serial2" %in% colnames(df_1617)) {
  df_1617 <- 
    df_1617 %>% 
    rename(serial = serial2)
}

if("serial2" %in% colnames(df_1718)) {
  df_1718 <- 
    df_1718 %>% 
    rename(serial = serial2)
}

if("serial2" %in% colnames(df_1819)) {
  df_1819 <- 
    df_1819 %>% 
    rename(serial = serial2)
}

if("serial2" %in% colnames(df_1920)) {
  df_1920 <- 
    df_1920 %>% 
    rename(serial = serial2)
}

df_0809 <- df_0809 %>% 
  mutate(case_id = str_pad(serial, 
                            width = 10, # this is the maximum size of the case_id variable across all datsets
                            side = "left",
                            pad = "0"),
          survey_year = "2008_09",
          year_case_id = paste(survey_year, case_id, sep = "-")) %>% 
  select(serial, case_id, survey_year,year_case_id, everything())

df_0910 <- df_0910 %>% 
  mutate(case_id = str_pad(serial, 
                           width = 10, # this is the maximum size of the case_id variable across all datsets
                           side = "left",
                           pad = "0"),
         survey_year = "2009_10",
         year_case_id = paste(survey_year, case_id, sep = "-")) %>% 
  select(serial, case_id, survey_year,year_case_id, everything())

df_1011 <- df_1011 %>% 
  mutate(case_id = str_pad(serial, 
                           width = 10, # this is the maximum size of the case_id variable across all datsets
                           side = "left",
                           pad = "0"),
         survey_year = "2010_11",
         year_case_id = paste(survey_year, case_id, sep = "-")) %>% 
  select(serial, case_id, survey_year,year_case_id, everything())

df_1213 <- df_1213 %>% 
  mutate(case_id = str_pad(serial, 
                           width = 10, # this is the maximum size of the case_id variable across all datsets
                           side = "left",
                           pad = "0"),
         survey_year = "2012_13",
         year_case_id = paste(survey_year, case_id, sep = "-")) %>% 
  select(serial, case_id, survey_year,year_case_id, everything())

df_1415 <- df_1415 %>% 
  mutate(case_id = str_pad(serial, 
                           width = 10, # this is the maximum size of the case_id variable across all datsets
                           side = "left",
                           pad = "0"),
         survey_year = "2014_15",
         year_case_id = paste(survey_year, case_id, sep = "-")) %>% 
  select(serial, case_id, survey_year,year_case_id, everything())

df_1617 <- df_1617 %>% 
  mutate(case_id = str_pad(serial, 
                           width = 10, # this is the maximum size of the case_id variable across all datsets
                           side = "left",
                           pad = "0"),
         survey_year = "2016_17",
         year_case_id = paste(survey_year, case_id, sep = "-")) %>% 
  select(serial, case_id, survey_year,year_case_id, everything())

df_1718 <- df_1718 %>% 
  mutate(case_id = str_pad(serial, 
                           width = 10, # this is the maximum size of the case_id variable across all datsets
                           side = "left",
                           pad = "0"),
         survey_year = "2017_18",
         year_case_id = paste(survey_year, case_id, sep = "-")) %>% 
  select(serial, case_id, survey_year,year_case_id, everything())

df_1819 <- df_1819 %>% 
  mutate(case_id = str_pad(serial, 
                           width = 10, # this is the maximum size of the case_id variable across all datsets
                           side = "left",
                           pad = "0"),
         survey_year = "2018_19",
         year_case_id = paste(survey_year, case_id, sep = "-")) %>% 
  select(serial, case_id, survey_year,year_case_id, everything())

df_1920 <- df_1920 %>% 
  mutate(case_id = str_pad(serial, 
                           width = 10, # this is the maximum size of the case_id variable across all datsets
                           side = "left",
                           pad = "0"),
         survey_year = "2019_20",
         year_case_id = paste(survey_year, case_id, sep = "-")) %>% 
  select(serial, case_id, survey_year,year_case_id, everything())

### Tidying annoying variables
df_0910 <- df_0910 %>% mutate(nmotthef = as.factor(nmotthef))

df_0809 <- df_0809 %>% mutate(nbikthef = as.factor(nbikthef))
df_1011 <- df_1011 %>% mutate(nbikthef = as.factor(nbikthef))
df_1213 <- df_1213 %>% mutate(nbikthef = as.factor(nbikthef))

df_0809 <- df_0809 %>% mutate(npersth = as.factor(npersth))
df_1011 <- df_1011 %>% mutate(npersth = as.factor(npersth))
df_1213 <- df_1213 %>% mutate(npersth = as.factor(npersth))
df_1415 <- df_1415 %>% mutate(npersth = as.factor(npersth))


### Pooling info ###
year <- c("2008_09", "2009_10", "2010_11", "2012_13", "2014_15", "2016_17",
          "2017_18", "2018_19", "2019_20")
dfactor <- c(1.5, 1.5, 1.5, 1.3, 1.2, 1.34, 1.22, 1.17, 1.21)
n <- c(nrow(df_0809), nrow(df_0910), nrow(df_1011),
       nrow(df_1213), nrow(df_1415), nrow(df_1617),
       nrow(df_1718), nrow(df_1819), nrow(df_1920))

pooling_info <- tibble(year, dfactor, n) %>% 
  mutate(deff = dfactor ^ 2) %>% 
  mutate(neff = n / deff) %>% 
  mutate(pool_neff = neff / sum(neff))

total_rows <- sum(nrow(df_0809), nrow(df_0910), nrow(df_1011),
                  nrow(df_1213), nrow(df_1415), nrow(df_1617),
                  nrow(df_1718), nrow(df_1819), nrow(df_1920))
total_rows

#### Combine years ####

scjs_combined_full <- dplyr::bind_rows(df_0809, df_0910, df_1011,
                                df_1213, df_1415, df_1617,
                                df_1718, df_1819, df_1920, .ptype = df_0809)

#### Add pooling info ####

df_pool2 <- left_join(scjs_combined_full, pooling_info, by = c("survey_year" = "year"))

summary_info <- df_pool2 %>% 
  select(pool_year, wgtgindiv) %>% 
  group_by(pool_year) %>% 
  summarise(sum_weight = sum(wgtgindiv))

df_pool3 <- left_join(df_pool2, summary_info, by = "pool_year")

df_pool3 <- df_pool3 %>% 
  mutate(weight_scale = wgtgindiv / (sum(wgtgindiv) / n)) %>% 
  mutate(weight_scale_pool = weight_scale * pool_neff)

df_pool <- df_pool3

####

class(df_0809$nmotthef)
class(df_0910$nmotthef) #num
class(df_1011$nmotthef)
class(df_1213$nmotthef)
class(df_1415$nmotthef)
class(df_1617$nmotthef)
class(df_1718$nmotthef)
class(df_1819$nmotthef)
class(df_1920$nmotthef)

class(df_0809$nbikthef) #
class(df_0910$nbikthef) 
class(df_1011$nbikthef) #
class(df_1213$nbikthef) #
class(df_1415$nbikthef)
class(df_1617$nbikthef)
class(df_1718$nbikthef)
class(df_1819$nbikthef)
class(df_1920$nbikthef)

class(df_0809$npersth) #
class(df_0910$npersth) 
class(df_1011$npersth) #
class(df_1213$npersth) #
class(df_1415$npersth) #
class(df_1617$npersth)
class(df_1718$npersth)
class(df_1819$npersth)
class(df_1920$npersth)

class(df_0809$noththef) 
class(df_0910$noththef) 
class(df_1011$noththef) 
class(df_1213$noththef) 
class(df_1415$noththef) 
class(df_1617$noththef)
class(df_1718$noththef)
class(df_1819$noththef)
class(df_1920$noththef)
