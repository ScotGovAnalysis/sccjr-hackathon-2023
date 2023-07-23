library(tidyverse)
library(haven)
library(sjlabelled)
library(readr)

# setwd(project_path)

# Functions

# Transform the column names to lowercase to avoid case sensitive issues
rename_dataframe_columns_to_lowercase <- function(df) {
  colnames(df) <- tolower(colnames(df))
  return(df)
}

# Produce a consistently named serial column
rename_serial2 <- function(df){
  if("serial2" %in% colnames(df)) {
    df <- 
      df %>% 
      rename(serial = serial2)
  }
  return(df)
}

# Add unique identifiers to all observations
add_scjs_ids <- function(df, srv_year) {
 df <- df %>%  mutate(case_id = str_pad(serial, 
                           width = 10, # this is the maximum size of the case_id variable across all datsets
                           side = "left",
                           pad = "0"),
         survey_year = srv_year,
         year_case_id = paste(survey_year, case_id, sep = "-")) %>% 
  select(serial, case_id, survey_year,year_case_id, everything())
 return(df)
}

# Apply functions

df_0809 <- rename_dataframe_columns_to_lowercase(df_0809)
df_0910 <- rename_dataframe_columns_to_lowercase(df_0910)
df_1011 <- rename_dataframe_columns_to_lowercase(df_1011)
df_1213 <- rename_dataframe_columns_to_lowercase(df_1213)
df_1415 <- rename_dataframe_columns_to_lowercase(df_1415)
df_1617 <- rename_dataframe_columns_to_lowercase(df_1617)
df_1718 <- rename_dataframe_columns_to_lowercase(df_1718)
df_1819 <- rename_dataframe_columns_to_lowercase(df_1819)
df_1920 <- rename_dataframe_columns_to_lowercase(df_1920)

df_0809 <- rename_serial2(df_0809)
df_0910 <- rename_serial2(df_0910)
df_1011 <- rename_serial2(df_1011)
df_1213 <- rename_serial2(df_1213)
df_1415 <- rename_serial2(df_1415)
df_1617 <- rename_serial2(df_1617)
df_1718 <- rename_serial2(df_1718)
df_1819 <- rename_serial2(df_1819)
df_1920 <- rename_serial2(df_1920)

df_0809 <- add_scjs_ids(df_0809, "2008_09")
df_0910 <- add_scjs_ids(df_0910, "2009_10")
df_1011 <- add_scjs_ids(df_1011, "2010_11")
df_1213 <- add_scjs_ids(df_1213, "2012_13")
df_1415 <- add_scjs_ids(df_1415, "2014_15")
df_1617 <- add_scjs_ids(df_1617, "2016_17")
df_1718 <- add_scjs_ids(df_1718, "2017_18")
df_1819 <- add_scjs_ids(df_1819, "2018_19")
df_1920 <- add_scjs_ids(df_1920, "2019_20")


# The bind_rows function no longer automatically coerces data types to 
# be compatible, so one workaround is to force every column to a character
# then perform the join
df_0809char <- df_0809 %>% mutate(across(.fns = as.character))
df_0910char <- df_0910 %>% mutate(across(.fns = as.character))
df_1011char <- df_1011 %>% mutate(across(.fns = as.character))
df_1213char <- df_1213 %>% mutate(across(.fns = as.character))
df_1415char <- df_1415 %>% mutate(across(.fns = as.character))
df_1617char <- df_1617 %>% mutate(across(.fns = as.character))
df_1718char <- df_1718 %>% mutate(across(.fns = as.character))
df_1819char <- df_1819 %>% mutate(across(.fns = as.character))
df_1920char <- df_1920 %>% mutate(across(.fns = as.character))


# Bind_rows on character datasets
scjs_combined_full_char <- bind_rows(df_0809char, df_0910char, df_1011char,
                                     df_1213char, df_1415char, df_1617char,
                                     df_1718char, df_1819char, df_1920char)



# Use readr package to re-interpret the datatypes.
# This produces the fullest pooled dataset, where no modifications have been
# done to the columns.
scjs_combined_full_type <- type_convert(scjs_combined_full_char)


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


scjs_pool_full <- left_join(scjs_combined_full_type, pooling_info, 
                      by = c("survey_year" = "year"))

summary_info <- scjs_pool_full %>% 
  select(survey_year, wgtgindiv) %>% 
  group_by(survey_year) %>% 
  summarise(sum_weight = sum(wgtgindiv))

scjs_pool_full <- left_join(scjs_pool_full, summary_info, by = "survey_year")

scjs_pool_full <- scjs_pool_full %>% 
  mutate(weight_scale = wgtgindiv / (sum(wgtgindiv) / n)) %>% 
  mutate(weight_scale_pool = weight_scale * pool_neff)

# Tidy up environment a bit
rm(list = c("df_0809char", "df_0910char", "df_1011char",
            "df_1213char", "df_1415char", "df_1617char",
            "df_1718char", "df_1819char", "df_1920char",
            "scjs_combined_full_char", "scjs_combined_full_type"))






