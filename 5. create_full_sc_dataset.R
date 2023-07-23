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

df_0809_sc <- rename_dataframe_columns_to_lowercase(df_0809_sc)
df_0910_sc <- rename_dataframe_columns_to_lowercase(df_0910_sc)
df_1011_sc <- rename_dataframe_columns_to_lowercase(df_1011_sc)

df_0809_sc <- rename_serial2(df_0809_sc)
df_0910_sc <- rename_serial2(df_0910_sc)
df_1011_sc <- rename_serial2(df_1011_sc)

df_0809_sc <- add_scjs_ids(df_0809_sc, "2008_09")
df_0910_sc <- add_scjs_ids(df_0910_sc, "2009_10")
df_1011_sc <- add_scjs_ids(df_1011_sc, "2010_11")


# The bind_rows function no longer automatically coerces data types to 
# be compatible, so one workaround is to force every column to a character
# then perform the join
df_0809_sc_char <- df_0809_sc %>% mutate(across(.fns = as.character))
df_0910_sc_char <- df_0910_sc %>% mutate(across(.fns = as.character))
df_1011_sc_char <- df_1011_sc %>% mutate(across(.fns = as.character))



# Bind_rows on character datasets
scjs_sc_combined_full_char <- bind_rows(df_0809_sc_char, df_0910_sc_char,
                                        df_1011_sc_char)
                                     
# Use readr package to re-interpret the datatypes.
# This produces the fullest pooled dataset, where no modifications have been
# done to the columns.
scjs_sc_combined_full_type <- type_convert(scjs_sc_combined_full_char)


#### Pooling info ####

year_sc <- c("2008_09", "2009_10", "2010_11")
time_sc <- c(1, 2, 3)
dfactor_sc <- c(1.5, 1.5, 1.5)
n_sc <- c(nrow(df_0809_sc), nrow(df_0910_sc), nrow(df_1011_sc))

pooling_info_sc <- tibble(year_sc, time_sc, dfactor_sc, n_sc) %>% 
  mutate(deff = dfactor_sc ^ 2) %>% 
  mutate(neff = n_sc / deff) %>% 
  mutate(pool_neff = neff / sum(neff))



scjs_pool_full_sc <- left_join(scjs_sc_combined_full_type, pooling_info_sc, 
                         by = c("survey_year" = "year_sc"))

summary_info_sc <- scjs_pool_full_sc %>% 
  select(survey_year, wgtgindiv_sc) %>% 
  group_by(survey_year) %>% 
  summarise(sum_weight = sum(wgtgindiv_sc))

scjs_pool_full_sc <- left_join(scjs_pool_full_sc, summary_info_sc,
                               by = "survey_year")

scjs_pool_full_sc <- scjs_pool_full_sc %>% 
  mutate(weight_scale_sc = wgtgindiv_sc / (sum_weight / n_sc)) %>% 
  mutate(weight_scale_pool_sc = weight_scale_sc * pool_neff) %>% 
  select(-starts_with("qdreaddy"))

# This show which years have missing data for each question in the dataset
year_base_counts_sc <- scjs_pool_full_sc %>% group_by(survey_year) %>% summarise_all(
  ~ sum(!is.na(.))) %>%
  gather(., key="variable",value="number_obs",-survey_year) 

# This can be run to show which columns have any missing values at all
variable_across_years_sc <- year_base_counts_sc %>% group_by(variable) %>%
  summarise(
    no_na = !(any(number_obs==0))
  )

