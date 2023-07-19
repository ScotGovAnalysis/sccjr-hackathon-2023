library(tidyverse)
library(haven)
library(sjlabelled)

# setwd(project_path)


# note: do not attempt to view this dataset, will crash R in my experience
combined_data <- 
  tibble(
    year = c("2008_09",
             "2009_10",
             "2010_11",
             "2012_13",
             "2014_15",
             "2016_17",
             "2017_18",
             "2018_19",
             "2019_20"),
    data = list(
      
      df_0809,
      df_0910,
      df_1011,
      df_1213,
      df_1415,
      df_1617,
      df_1718,
      df_1819,
      df_1920
      
    )
  )


# vars_to_keep <- c("serial|case|wgtg|prev|qpolconf|qs2area|qsfdark|qsfnigh|qratpol|polop|compol|polpres|qworr|numcar|qaco_|lcpeop|qhworr|qswem|dconf|pcon")
# broken_vars <- c("nummot|polpatr")

### Functions
df_names_lower <- function(df, srv_year){
  df %>% rename_all(., .funs = tolower)
}

add_scjs_ids <- function(df, srv_year){
  
  if("serial2" %in% colnames(df)) {
    df <- 
      df %>% 
      rename(serial = serial2)
  }
  
  df %>% 
    mutate(case_id = str_pad(serial, 
                             width = 10, # this is the maximum size of the case_id variable across all datsets
                             side = "left",
                             pad = "0"),
           survey_year = srv_year,
           year_case_id = paste(survey_year, case_id, sep = "-")) %>% 
    select(serial, case_id, survey_year,year_case_id, everything())
  
}

tidy_vars <- function(df, srv_year){

}

scjs_clean_and_standardize <- function(df, year){
  
  
  # defne all the sub-functions
  
  clean_scjs_names <- function(df, year){
    
    if (year == "2008_09"){ 
      df %>% 
        select(-tenure) %>% 
        rename(gender = qdgen,
               tenure = qdtenur,
               disability = qdisab,
               limiting_disability = qdisab2,
               when_left_paid_job = qd1last,
               nsse_sub = nssecsub,
               nssec_analytic = nsseca,
               income = qdinc,
               ethnicity = qdeth,
               flat_type = qflat,
               simd_top_15 = simd_top,
               n_children = nchil,
               n_adults = nadults,
               hundred_pounds = qdi100,
               n_cars = numcar
        )
    } else if (year == "2009_10" | year == "2010_11"){
      
      df %>% 
        select(-tenure) %>% 
        rename(gender = qdgen,
               tenure = qdtenur,
               when_left_paid_job = qd1last,
               nsse_sub = nssecsub,
               nssec_analytic = nsseca,
               income = qdinc,
               ethnicity = qdeth2,
               flat_type = qflat,
               simd_top_15 = simd_top,
               n_children = nchil,
               n_adults = nadults,
               hundred_pounds = qdi100,
               n_cars = numcar
        )
    } else if (year == "2012_13" |
               year == "2014_15" |
               year == "2016_17" |
               year == "2017_18"){
      
      df %>% 
        rename(gender = qdgen,
               income = qdinc2,
               ethnicity = qdeth3,
               disability = qcondit,
               simd_top_15 = simd_top,
               n_adults = qnadults,
               n_children = qnchild,
               hundred_pounds = qdi100,
               n_cars = numcar)
      
    } else if (year == "2018_19" |
               year == "2019_20"){
      df %>% 
        rename(gender = qdgen,
               income = qdinc2,
               ethnicity = qdeth3,
               disability = qcondit,
               simd_top_15 = simd_15most,
               n_adults = qnadults,
               n_children = qnchild,
               hundred_pounds = qdi100,
               n_cars = numcar)
      
    }
  }
  
  
  recode_single_parents <- function(df){
    
    df %>% 
      mutate(single_parent = case_when(
        n_adults == 1 & n_children > 0 ~ "Yes",
        TRUE ~ "No"
      ))
  }
  
  # recode age
  
  recode_age <- function(df, year) {
    if (year == "2008_09" | year == "2009_10" | year == "2010_11") {
      df %>%
        mutate(age = case_when(
          qdage >= 16 & qdage < 25 ~ "16-24",
          qdage >= 25 & qdage < 45 ~ "25-44",
          qdage >= 45 & qdage < 65 ~ "45-64",
          qdage >= 65 ~ "65+",
          TRUE ~ NA_character_
        ))
    } else if (year == "2012_13" | year == "2014_15") {
      df %>%
        mutate(age = case_when(
          qdage >= 16 & qdage < 25 | tabage == 1 ~ "16-24",
          qdage >= 25 & qdage < 45 ~ "25-44",
          qdage >= 45 & qdage < 65 ~ "45-64",
          qdage >= 65 ~ "65+",
          TRUE ~ NA_character_
        ))
    } else if (year == "2016_17" | year == "2017_18" |
               year == "2018_19" | year == "2019_20") {
      
      df %>% 
        mutate(age = case_when(
          qdage2 == 1 | qdage2 == 2 | qdage2 == 3 ~ "16-24",
          qdage2 == 4 | qdage2 == 5 ~ "25-44",
          qdage2 == 6 | qdage2 == 7 | qdage2 == 8 ~ "45-64",
          qdage2 > 8 ~ "65+",
          TRUE ~ NA_character_
        ))
      
    }
    
  }
  # recode disbility
  
  
  recode_disability <- function(df, year){
    
    if(year == "2008_09" | year == "2012_13" | year == "2014_15" |
       year == "2016_17" | year == "2017_18" |
       year == "2018_19" | year == "2019_20"){
      
      df %>% 
        mutate(disability = case_when(
          disability == 1 ~ "Yes",
          disability == 2 ~ "No",
          TRUE ~ NA_character_
        ),
        disability = fct_relevel(disability))
      
    } else if (year == "2009_10" | year == "2010_11") {
      
      df %>% 
        mutate(disability = case_when(
          disabnew_01 == 1 | 
            disabnew_02 == 1  |
            disabnew_03 == 1  |
            disabnew_04 == 1  |
            disabnew_05 == 1  |
            disabnew_06 == 1  |
            disabnew_07 == 1  |
            disabnew_ot == 1  
          ~ "Yes",
          disabnew_rf == 1 | 
            disabnew_dk == 1 ~ NA_character_,
          TRUE ~ "No"
        ),
        disability = fct_relevel(disability))
    }
  }
  
  # recode marital
  
  recode_marital <- function(df, year){
    
    if (year == "2008_09" | year == "2009_10" | year == "2010_11"){
      
      df %>% 
        mutate(marital = case_when(
          qdlegs == 1 ~ "Single",
          qdlegs == 2 | qdlegs == 3 ~ "Married / in a civil partnership",
          qdlegs >= 4 & qdlegs<= 7 ~  "Divorced / separated",
          qdlegs == 8 | qdlegs == 9 ~ "Widowed",
          TRUE ~ NA_character_
        ))
    } else if (year == "2012_13" | year == "2014_15" |  year == "2016_17" | year == "2017_18" |
               year == "2018_19" | year == "2019_20"){
      
      df %>% 
        mutate(marital = case_when(
          qdlegs == 1 ~ "Single",
          qdlegs == 2 ~ "Married / in a civil partnership",
          qdlegs == 3 | qdlegs == 4 ~  "Divorced / separated",
          qdlegs == 5 ~ "Widowed",
          TRUE ~ NA_character_
        ))
    }
    
  }
  # recode income
  
  # recode_income <- function(df, year){
  #   
  #   if (year == "2008_09" | year == "2009_10"| year == "2010_11"){
  #    
  #      df %>% 
  #       mutate(income = sjlabelled::as_character(income),
  #              income = if_else(str_detect(income, "£"), 
  #                               str_sub(income, 4, str_length(income)),
  #                               income),
  #              income = if_else(str_detect(income, " £"), 
  #                               str_squish(income),
  #                               income),
  #              income = case_when(
  #                income == "£20,000 - £29,999" ~ "£20,000 - £49,999",
  #                income == "£30,000 - £39,999" ~ "£20,000 - £49,999",
  #                income == "£40,000 - £49,999" ~ "£20,000 - £49,999",
  #                income == "Refused" ~ NA_character_,
  #                income == "Don't know" ~ NA_character_,
  #                TRUE ~ income),
  #              income = as.factor(income),
  #              income = fct_relevel(
  #                income, "Less than £5,000", "£5,000 - £9,999", "£10,000 - £19,999",
  #                "£20,000 - £49,999", "£50,000 or more"
  #              ),
  #              income = fct_explicit_na(income, "Refused/Don't know/Missing"))
  #     } else if (year == "2012_13" | year == "2014_15" |  year == "2016_17" | year == "2017_18"){
  #     
  #     df %>% 
  #       mutate(income = as.numeric(income),
  #              income = case_when(
  #                income == 1 ~ "Less than £5,000",
  #                income == 2 ~ "£5,000 - £9,999",
  #                income == 3 | income == 4 ~ "£10,000 - £19,999",
  #                income == 5 | income == 6 | income == 7 ~ "£20,000 - £49,999",
  #                income == 8 | income == 9 ~ "£50,000 or more",
  #                income == -2 ~ NA_character_,
  #                income == -1 ~ NA_character_,
  #                TRUE ~ NA_character_
  #              ), 
  #              income = as.factor(income),
  #              income = fct_relevel(
  #                income, "Less than £5,000", "£5,000 - £9,999", "£10,000 - £19,999",
  #                "£20,000 - £49,999", "£50,000 or more"),
  #                income = fct_explicit_na(income, "Refused/Don't know/Missing"))
  #   }
  
  #}
  
  # recode ethnicity
  
  recode_ethnicity <- function(df, year){
    if (year == "2008_09") {
      df %>% 
        mutate(ethnicity = sjlabelled::as_character(ethnicity),
               ethnicity = case_when(
                 ethnicity == "Scottish" ~ "White Scottish",
                 ethnicity == "Other British" ~ "White British",
                 ethnicity == "Irish" | ethnicity == "Any other white background" ~ "White Other",
                 ethnicity == "Refused" ~ NA_character_,
                 ethnicity == "Don't know" ~ NA_character_,
                 TRUE ~ "Minority Ethnic"
               ))
    } else if (year == "2009_10" | year == "2010_11"){
      df %>% 
        mutate(ethnicity = case_when(
          ethnicity == 1 ~ "White Scottish",
          ethnicity == 2 | ethnicity == 3 | ethnicity == 4 | ethnicity == 5 ~ "White British",
          ethnicity == 6 | ethnicity == 8 | ethnicity == 9 ~ "White Other",
          ethnicity == -2 ~ NA_character_,
          ethnicity == -1 ~ NA_character_,
          TRUE ~ "Minority Ethnic"
        ))
    } else if (year == "2012_13" | year == "2014_15" | 
               year == "2016_17" | year == "2017_18" |
               year == "2018_19" | year == "2019_20"){
      
      df %>% 
        mutate(ethnicity = case_when(
          ethnicity == 1 ~ "White Scottish",
          ethnicity == 2 ~ "White British",
          ethnicity == 3 ~ "White Other",
          ethnicity == 4 ~ "Minority Ethnic",
          ethnicity == -2 ~ NA_character_,
          ethnicity == -1 ~ NA_character_,
          TRUE ~ "Minority Ethnic"
        ))  
      
    }
    
  }# recode accommodation
  
  recode_accommodation <- function(df, year) {
    if (year == "2008_09" | year == "2009_10" | year == "2010_11") {
      df %>%
        mutate(
          accom_type = case_when(
            qdetach == 1 | qdetach == 2 ~ "Detached or semi-detatched house",
            qdetach == 3 ~ "Terraced house",
            is.na(qdetach) & flat_type > 0 & flat_type <= 6 ~ "Flat or maisonette",
            TRUE ~ NA_character_
          ),
          accom_type = fct_relevel(
            accom_type,
            "Detached or semi-detatched house",
            "Terraced house",
            "Flat or maisonette"
          )
        )
    } else if (year == "2012_13" | year == "2014_15" |  year == "2016_17" | year == "2017_18") {
      df %>%
        mutate(
          accom_type = case_when(
            acctype == 1 ~ "Detached or semi-detatched house",
            acctype == 2 ~ "Terraced house",
            acctype == 3 ~ "Flat or maisonette",
            TRUE ~ NA_character_
          ),
          accom_type = fct_relevel(
            accom_type,
            "Detached or semi-detatched house",
            "Terraced house",
            "Flat or maisonette"
          )
        )
    }
  }
  # recode urban rural
  
  recode_urban_rural <- function(df, year){
    
    if (year == "2008_09" | year == "2009_10" | year == "2010_11") {
      df %>% 
        mutate(urban_rural = case_when(
          urbrur >= 1 & urbrur <= 5 ~ "Urban",
          urbrur >= 6 ~ "Not urban"
        ))
      
      
    } else if (year == "2012_13" | year == "2014_15" |
               year == "2016_17" | year == "2017_18" |
               year == "2018_19" | year == "2019_20"){
      
      df %>% 
        mutate(urban_rural = case_when(
          taburbrur == 1 ~ "Urban",
          taburbrur == 2 ~ "Not urban"
        ))
      
    }
    
    
  }
  
  # recode employment
  
  recode_employment <- function(df, year){
    if (year == "2008_09" | year == "2009_10" | year == "2010_11") {
      
      df %>% 
        mutate(employment = case_when(
          nssec_analytic >= 1 & nssec_analytic < 4 ~ "Managerial and professional",
          nssec_analytic >= 4 & nssec_analytic <= 5 ~ "Intermediate and small employers",
          nssec_analytic >= 6 & nssec_analytic <= 8 ~ "Routine and manual",
          TRUE ~ sjlabelled::as_character(nssec_analytic)
        ),
        employment = fct_relevel(
          employment, 
          "Managerial and professional",
          "Intermediate and small employers",
          "Routine and manual",
          "Never worked and long-term unemployed",
          "Not classified"
        ))
    } else if (year == "2012_13" | year == "2014_15" |  year == "2016_17" | year == "2017_18"){
      
      df %>% 
        mutate(employment = case_when(
          tabnssec == 1 ~ "Managerial and professional",
          tabnssec == 2 ~ "Intermediate and small employers",
          tabnssec == 3 ~ "Routine and manual",
          tabnssec == 4 ~ "Never worked and long-term unemployed",
          TRUE ~ "Not classified"
        ),
        employment = fct_relevel(
          employment, 
          "Managerial and professional",
          "Intermediate and small employers",
          "Routine and manual",
          "Never worked and long-term unemployed",
          "Not classified"
        ))
    }
  }
  # recode number of cars
  
  recode_number_of_cars <- function(df){
    df %>% 
      mutate(n_cars = case_when(
        n_cars == 1 ~ "1",
        n_cars == 2 ~ "2",
        n_cars > 2 ~ "3+",
        n_cars == -1 ~ NA_character_,
        TRUE ~ "0"
      ))
  }
  
  
  
  # list of variables to keep -
  
  # age
  # gender
  # ethnicity
  # marital status
  # social class
  # disability
  # hhgender (household gender mix)
  # n children
  # n adults
  # single parents household
  # single pensioner household
  # tenure
  # accomodation type
  # number of years lived in local area
  
  
  # single pensioner household
  
  recode_single_pensioner <- function(df){
    df %>% 
      mutate(
        single_pensioner = case_when(
          age == "65+" & n_adults == 1 & n_children == 0 ~ "Yes",
          TRUE ~ "No"
        )
      )
  }
  
  
  recode_tenure <- function(df, year){
    if (year == "2008_09" | year == "2009_10" | year == "2010_11") {
      
      df %>% 
        mutate(tenure = case_when(
          tenure == 1 | tenure == 2 | tenure == 3 ~ "Owned/mortgage",
          tenure == 5 ~ "Living rent free",
          tenure < 0 ~ NA_character_,
          tenure == 4 & qdrent == 1 | qdrent == 2 ~ "Social renting",
          tenure == 4 & qdrent != 1 & qdrent != 2  ~ "Private renting",
          TRUE ~ sjlabelled::as_character(tenure)
        ),
        tenure = fct_infreq(tenure))
      
    } else if (year == "2012_13" | year == "2014_15" |  year == "2016_17" | year == "2017_18" |
               year == "2018_19" | year == "2019_20"){
      
      df %>% 
        mutate(tenure = case_when(
          tenure == 1 ~ "Owned/mortgage",
          tenure == 2 ~ "Social renting",
          tenure == 3 ~ "Private renting",
          tenure == 4 ~ "Living rent free",
          TRUE ~ NA_character_
          
        ),
        tenure = fct_infreq(tenure))
      
    }
    
  }
  
  recode_time_in_area <- function(df){
    df %>% 
      mutate(time_in_area = case_when(
        qsyarea == 5 ~ ">=10 years",
        qsyarea == 4 ~ "5 < 10 years",
        qsyarea == 3 ~ "2 < 5 years",
        qsyarea == 2 ~ "1 < 2 years",
        qsyarea == 1 ~ "<1 year",
      ),
      time_in_area = fct_relevel(
        time_in_area,
        ">=10 years",
        "5 < 10 years",
        "2 < 5 years",
        "1 < 2 years",
        "<1 year"
      ))
  }
  
  
  recode_has_motorvehicle <- function(df){
    
    df %>% 
      mutate(has_motorvehicle = case_when(
        n_cars > 0 | sjlabelled::as_character(motorcyc) == "Yes" ~ "Yes", 
        is.na(n_cars) ~ "No",
        TRUE ~ "No"),
        has_mmotorvehicle = fct_relevel(has_motorvehicle, "Yes", "No"))
    
  }
  
  
  recode_gender <- function(df){
    df %>% 
      mutate(gender = case_when(
        gender == 1 ~ "Male",
        gender == 2 ~ "Female",
        TRUE ~ NA_character_
      ))
  }
  
  
  recode_hundred_pounds <-  function(df){
    df %>% 
      mutate(hundred_pounds = sjlabelled::as_character(hundred_pounds),
             hundred_pounds = case_when(
               hundred_pounds == "Don't know" ~ NA_character_,
               hundred_pounds == "Refused" ~ NA_character_,
               TRUE ~ hundred_pounds
             ),
             hundred_pounds = fct_infreq(hundred_pounds)
      )
  }
  
  tidy_factors <- function(df){
    df %>% 
      mutate(n_adults = as.numeric(n_adults)) %>% 
      mutate(n_children = as.numeric(n_children))
  }
  
  # apply all the sub-functions in sequence
  
  
  df <- 
    df %>% 
    clean_scjs_names(., year = year) %>% 
    recode_single_parents() %>% 
    recode_age(., year = year) %>% 
    recode_disability(., year = year) %>% 
    recode_marital(., year = year) %>% 
    #     recode_income(., year = year) %>% 
    recode_ethnicity(., year = year) %>% 
    #   recode_accommodation(., year = year) %>% 
    recode_urban_rural(., year = year) %>% 
    #   recode_employment(., year = year) %>% 
    recode_number_of_cars() %>% 
    recode_single_pensioner() %>% 
    recode_tenure(., year = year) %>% 
    recode_time_in_area() %>% 
    recode_has_motorvehicle() %>% 
    recode_gender() %>% 
    recode_hundred_pounds() %>% 
    tidy_factors
  
  
  # select the variables that we want to keep from the dataset
  
  ivs <- c(
    "case_id",
    "survey_year",
    "year_case_id",
    "age",
    "gender",
    "ethnicity",
    #   "income",
    "hundred_pounds",
    "marital",
    # "employment",
    "disability",
    "n_adults",
    "n_children",
    "single_parent",
    "single_pensioner",
    "tenure",
    #   "accom_type",
    "time_in_area",
    "n_cars",
    "has_motorvehicle",
    "simd_top_15",
    "simd_quint",
    "urban_rural"
  )
  
  
  df <- 
    df %>% 
    select(ivs,
           starts_with(c("wgtg", #weighting
                         "prev", #prevalence
                         "rep",
                         "qs2area", #crime rate
                         "qsfdark", #feeling of safety
                         "qsfnigh", #feeling of safety
                         "qworr", #worry of victimisation
                         "qhapp", #perceived likelihood of victimisation
                         "qdconf", #confidence in justice system
                         "qpolconf", #confidence in police
                         #"polpatr", #police visibility - for some reason this causes errors so excluding for now
                         "polpres", #police presence
                         "polop", #attitude to police
                         "qpcon", #police contact
                         # "cyber",
                         "qwall", #how react to wallet stolen
                         "lcpeop", #attitude to local government
                         "qaco", #perception of local crime/issues
                         "qhworr", #worry about harassment
                         "qswem"
                         
           ))) 
  
  # recode variables so that the factor orders run from largest to smallest
  
  df %>% 
    mutate_at(
      vars(age, gender, ethnicity, marital, n_cars, has_motorvehicle, single_parent, single_pensioner),
      fct_infreq
    )
  
  
}


### Apply functions

# Note: this will produce warnings when run
scjs_combined_subset <- 
  combined_data %>% 
  mutate(data = map2(data, year, df_names_lower)) %>% 
  mutate(data = map2(data, year, add_scjs_ids)) %>% 
  mutate(data = map2(data, year, scjs_clean_and_standardize)) %>% 
  unnest_legacy()

# Pooling info
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

scjs_combined_subset_pool <- left_join(scjs_combined_subset, pooling_info, 
                                by = c("survey_year" = "year"))

summary_info <- scjs_combined_subset_pool %>% 
  select(survey_year, wgtgindiv) %>% 
  group_by(survey_year) %>% 
  summarise(sum_weight = sum(wgtgindiv))

scjs_combined_pool <- left_join(scjs_combined_subset_pool, summary_info, 
                                by = c("survey_year"))

scjs_pool_subset <- scjs_combined_pool %>%
  mutate(weight_scale = wgtgindiv / (sum(wgtgindiv) / n)) %>% 
  mutate(weight_scale_pool = weight_scale * pool_neff)

### Testing if data looks ok

scjs_pool_subset %>% select(year, wgtgindiv) %>% 
  group_by(year) %>% 
  summarise(sum(wgtgindiv))

scjs_pool_subset %>% select(year, prevviolent, wgtgindiv) %>% 
  group_by(year, prevviolent) %>% 
  summarise(sum(wgtgindiv))

scjs_pool_subset %>% select(year, prevsurveycrime, wgtgindiv) %>% 
  group_by(year, prevsurveycrime) %>% 
  summarise(sum(wgtgindiv))



# This show which years have missing data for each question in the dataset
year_base_counts <- scjs_pool_subset %>% group_by(year) %>% summarise_all(
  ~ sum(!is.na(.))) %>%
  gather(., key="variable",value="number_obs",-year) 

# This can be run to show which columns have any missing values at all
# year_counts %>% group_by(variable) %>% 
#   summarise(
#     no_na = !(any(number_obs==0))
#   ) -> variable_across_years


# Tidy up environment a bit
rm(list = c("scjs_combined_pool", "scjs_combined_subset", 
            "scjs_combined_subset_pool"))


