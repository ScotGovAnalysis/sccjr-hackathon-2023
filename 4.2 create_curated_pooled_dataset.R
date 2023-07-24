library(tidyverse)
library(haven)
library(sjlabelled)

# setwd(project_path)

convert_columns_to_numeric <- function(df) {
  col_names <- colnames(df)
  inc_cols <- col_names[startsWith(col_names, "inc")]
  
  for (col in inc_cols) {
    df[[col]] <- as.numeric(as.character(df[[col]]))
  }
  
  return(df)
}

df_1617 <- convert_columns_to_numeric(df_1617)
df_1718 <- convert_columns_to_numeric(df_1718)

# df_1617 <- df_1617 %>%  mutate(incatttheftmv = as.numeric(incatttheftmv),
#                                inctheftofmv = as.numeric(inctheftofmv),
#                                incrob = as.numeric(incrob))
# df_1718 <- df_1718 %>%  mutate(incatttheftmv = as.numeric(incatttheftmv),
#                                inctheftofmv = as.numeric(inctheftofmv),
#                                incrob = as.numeric(incrob))

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

# tidy_vars <- function(df, srv_year){
#   if (year == "2016_17" | year == "2017_18"){
#     df %>%
#       mutate(incatttheftmv = as.numeric(incatttheftmv))
#   }
# }

scjs_clean_and_standardize <- function(df, year){
  
  
  # define all the sub-functions
  
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
    tidy_factors() 
  
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
                         "prev", #prevalence - a bit untidy to include
                         #"rep", #repeat victim
                         "inc", #incidence
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
                         "qwall", #how react to wallet stolen
                         "lcpeop", #attitude to local government
                         "qaco_", #perception of local crime/issues
                         "qhworr", #worry about harassment
                         "qswem", #mental wellbeing score
                         "laa", #local authority
                         "pfa", #police force area
                         "qdi100", #how easily can source £100
                         "qdcrime2", #contact with justice system
                         "qdbeen", #been given custodial sentence etc
                         "cyber2" #summary variables cyber crime
                         
           ))) 
  
  # recode variables so that the factor orders run from largest to smallest
  
  df <- df %>% 
    mutate_at(
      vars(age, gender, ethnicity, marital, n_cars, has_motorvehicle, single_parent, single_pensioner),
      fct_infreq
    )
  
  #drop some specific vars
  # df <- df %>% select(-c("prevsexoff"))
  
  
}


### Apply functions

# Note: this will produce warnings when run
scjs_combined_subset <- 
  combined_data %>% 
  mutate(data = map2(data, year, df_names_lower)) %>% 
  mutate(data = map2(data, year, add_scjs_ids)) %>% 
  #mutate(data = map2(data, year, tidy_vars)) %>% 
  mutate(data = map2(data, year, scjs_clean_and_standardize)) %>% 
  unnest_legacy()

# Tidy up some nuisance variables

scjs_combined_subset <- 
  scjs_combined_subset %>% 
  mutate(qdconf_01 = coalesce(qdconf_1, qdconf_01),
         qdconf_02 = coalesce(qdconf_2, qdconf_02),
         qdconf_03 = coalesce(qdconf_3, qdconf_03),
         qdconf_04 = coalesce(qdconf_4, qdconf_04),
         qdconf_05 = coalesce(qdconf_5, qdconf_05),
         qdconf_06 = coalesce(qdconf_6, qdconf_06),
         qpolconf_01 = coalesce(qpolconf_1, qpolconf_01),
         qpolconf_02 = coalesce(qpolconf_2, qpolconf_02),
         qpolconf_03 = coalesce(qpolconf_3, qpolconf_03),
         qpolconf_04 = coalesce(qpolconf_4, qpolconf_04),
         qpolconf_05 = coalesce(qpolconf_5, qpolconf_05),
         qpolconf_06 = coalesce(qpolconf_6, qpolconf_06),
         qworr_01 = coalesce(qworr_1, qworr_01),
         qworr_02 = coalesce(qworr_2, qworr_02),
         qworr_03 = coalesce(qworr_3, qworr_03),
         qworr_04 = coalesce(qworr_4, qworr_04),
         qworr_05 = coalesce(qworr_5, qworr_05),
         qworr_06 = coalesce(qworr_6, qworr_06),
         qworr_07 = coalesce(qworr_7, qworr_07),
         qworr_08 = coalesce(qworr_8, qworr_08),
         qworr_09 = coalesce(qworr_9, qworr_09),
         prevatttheftmv = coalesce(prevtatttheftmv, prevatttheftmv)) %>% 
  select(-c("qdconf_1", "qdconf_2", "qdconf_3",
            "qdconf_4", "qdconf_5", "qdconf_6",
            "qpolconf_1", "qpolconf_2", "qpolconf_3",
            "qpolconf_4", "qpolconf_5", "qpolconf_6",
            "qworr_1", "qworr_2", "qworr_3",
            "qworr_4", "qworr_5", "qworr_6",
            "qworr_7", "qworr_8", "qworr_9",
            "prevsexoff", "incsexoff",
            )) %>% 
  mutate(
    numcars = as.numeric(n_cars),
    anyvehicle = ifelse(numcars>0 , 1, 0),
    qworr_01 = ifelse(anyvehicle==1,qworr_01,NA),
    qworr_02 = ifelse(anyvehicle==1,qworr_02,NA),
    qworr_03 = ifelse(anyvehicle==1,qworr_03,NA),
    qworr_12 = ifelse(anyvehicle==1,qworr_12,NA),
    qworr_13 = ifelse(anyvehicle==1,qworr_13,NA),
    qworr_14 = ifelse(anyvehicle==1,qworr_14,NA)
  ) %>% 
  mutate(
    qworr_01 = ifelse(is.na(qworr_01) & !is.na(qworr_12), qworr_12, qworr_01),
    qworr_02 = ifelse(is.na(qworr_02) & !is.na(qworr_13), qworr_13, qworr_02),
    qworr_03 = ifelse(is.na(qworr_03) & !is.na(qworr_14), qworr_14, qworr_03)
  )

#Fixes across years
#the qworr_01,02,03 questions need to be set to NA when there is <1 motor vehicle in the househole
#(they were asked even to people who don't have cars)
#these were changed in wordings, and became 12,13,14. so we need to do it for them too.
#so, qworr_01,2,3 and 12,13,14 are the same. (basically, the filtering of cars is now done post-hoc, rather than questions being asked conditionally upon car ownership)
#wording is the same, so we'll collapse them.


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

year_base_counts_wider <- year_base_counts %>% 
  pivot_wider(names_from = "year", values_from = number_obs)

check_base <- year_base_counts_wider %>% 
  filter(str_detect(variable, "qaco_")) #qdconf prevm qworr

check_base <- year_base_counts_wider %>% 
  filter(str_detect(variable, "inc")) #qdconf prevm qworr


year_base_counts_wider %>% 
  filter(str_detect(variable, "theftmv")) 

# This can be run to show which columns have any missing values at all
# year_counts %>% group_by(variable) %>% 
#   summarise(
#     no_na = !(any(number_obs==0))
#   ) -> variable_across_years


# Tidy up environment a bit
rm(list = c("scjs_combined_pool", "scjs_combined_subset", 
            "scjs_combined_subset_pool"))


# write.csv(year_base_counts_wider, ".\\Handy resources\\year_base_size.csv", row.names=FALSE)

### Create subset of first 3 years with self-completion summary variables

scjs_sc_select <- 
  scjs_pool_full_sc %>% 
  select(year_case_id, survey_year,
         shsent, shcalls, shloit, shfoll, shdk, shnone, shref, #stalking and harassment summary variables
         starts_with("da_1i_"), #psychological partner abuse
         starts_with("da_1iii_"), #physical partner abuse
         svinex, svst, svts, svdk, svnone, svref, #less serious sexual assault
         safs, saafs, saos, saaos, sadk, sanone, saref #more serious sexual assault
        )

# looks like the data is in each year (for the most part, so fine to join)
year_base_counts_2 <- scjs_sc_select %>% group_by(survey_year) %>% summarise_all(
  ~ sum(!is.na(.))) %>%
  gather(., key="variable",value="number_obs",-survey_year) 

year_base_counts_wider_2 <- year_base_counts_2 %>% 
  pivot_wider(names_from = "survey_year", values_from = number_obs)

scjs_pool_subset_sc <- 
  scjs_pool_subset %>% 
  filter(year %in% c("2008_09", "2009_10", "2010_11"))

scjs_pool_subset_sc <- left_join(scjs_pool_subset_sc, scjs_sc_select, by = c("year_case_id", "year" = "survey_year"))
  
  
  
  
  
  
