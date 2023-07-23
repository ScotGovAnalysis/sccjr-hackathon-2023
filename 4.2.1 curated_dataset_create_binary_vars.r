# This script will convert most of the question variables into a binary format
# in order to increase the ease of analysis. I.e. will convert 
# strongly agree / somewhat agree into net: agree. Credit to Josiah King who
# originally wrote most of this code for the SCJS shiny app

# You will have to have run the previous script 4.2 in order to get this to work

# New dataset with binary variables with suffix '_bin'

# the recode function used does not work on the data type that some cols have
# so this converts them to a factor, which can be recoded below
convert_columns_to_factor <- function(df, pattern) {
  col_names <- colnames(df)
  target_cols <- col_names[startsWith(col_names, pattern)]
  
  for (col in target_cols) {
    df[[col]] <- as.factor(as.character(df[[col]]))
  }
  
  return(df)
}

scjs_pool_subset_bin <- scjs_pool_subset


#all prev variables (prevsureycrime, prevviolent etc) need to be coded such that 2's become 0's.
scjs_pool_subset_bin <- 
  scjs_pool_subset_bin %>% 
  mutate_at(vars(starts_with("prev")), ~ replace(.,.==2,0))



#same with the polpatr_0 (not currently in subset of vars)
# scjs_pool_subset_bin <- 
#   scjs_pool_subset %>% 
#   mutate_at(vars(starts_with("polatr_0")), ~ replace(.,.==2,0))


#QS2AREA: Perceived change in crime rate in local area in last two years
#QS2AREAS: Perceived change in crime rate in Scotland in last two years
# 3,4,5 = same or less
# -2,-1,1,2 = rf, dk, or more
scjs_pool_subset_bin <- convert_columns_to_factor(scjs_pool_subset_bin, "qs2area")
scjs_pool_subset_bin <- 
  scjs_pool_subset_bin %>%
  mutate_at(vars(c(matches("qs2area"))), ~ recode(.,'1'=0,'2'=0,'3'=1,'4'=1,'5'=1,'-2'=0,'-1'=0,.default=NA_real_))


#QPOLCONF_01...06 Confidence in ability of police in local area to: ...
#QDCONF_01...14 Confidence that the Scottish CJS: ...
# 1,2 = confident
# -2,-1,3,4 = rf, dk, not confident

#COMPOL: How important to you is it that there are local police officers who know and patrol in your local area?
# 1,2 = important
# -2,-1,3,4, = rf, dk, not important

#POLOPREL,POLOPRESP,POLOPFAIR,POLOPMAT,POLOPCON,POLOPCOM,POLOPOVER
#Agreement that: Police in this area....
#LCPEOP_01...07: Agreement that: People in my local area ...
# 1,2 = agree
# -2,-1,3,4,5 = dk, rf, neither, disagree

#QWORR_01...07 Extent of worry that: ...?
#QHWORR_01...07 How much, if at all, do you, personally, worry about being insulted, pestered or intimidated on the basis of ...
# 1,2 = worried
# -2,-1,3,4,5 = rf, dk, not worried, not appl.

#QACO_01...14 In local area how common is: ...
# 1,2 = common
# -2,-1,3,4 = rf, dk, not common

#QSFDARK: How safe respondent feels walking alone in local area after dark
#QSFNIGH: How safe respondent feels alone in home at night
# 1,2 = safe
# -1,3,4 = dk, unsafe

#these questions have responses 1,2 = 1, 3,4 = 0.
scjs_pool_subset_bin <- convert_columns_to_factor(scjs_pool_subset_bin, "qpolconf")
scjs_pool_subset_bin <- convert_columns_to_factor(scjs_pool_subset_bin, "qdconf")
scjs_pool_subset_bin <- convert_columns_to_factor(scjs_pool_subset_bin, "compol")
scjs_pool_subset_bin <- convert_columns_to_factor(scjs_pool_subset_bin, "lcpeop")
scjs_pool_subset_bin <- convert_columns_to_factor(scjs_pool_subset_bin, "polop")
scjs_pool_subset_bin <- convert_columns_to_factor(scjs_pool_subset_bin, "qworr")
scjs_pool_subset_bin <- convert_columns_to_factor(scjs_pool_subset_bin, "qhworr")
scjs_pool_subset_bin <- convert_columns_to_factor(scjs_pool_subset_bin, "qaco")
scjs_pool_subset_bin <- convert_columns_to_factor(scjs_pool_subset_bin, "qsfdark")
scjs_pool_subset_bin <- convert_columns_to_factor(scjs_pool_subset_bin, "qsfnigh")

recode_questions="qpolconf|qdconf|compol|lcpeop|polop|qworr|qhworr|qaco|qsfdark|qsfnigh"
scjs_pool_subset_bin <- 
  scjs_pool_subset_bin %>%
  mutate_at(vars(c(matches(recode_questions))), ~ recode(.,'1'=1,'2'=1,'3'=0,'4'=0,'5'=0,'-2'=0,'-1'=0,.default=NA_real_))

#polpatrf is 1,2,3,4 = 1, - not currently in the subset
# scjs_pool_subset_bin <- 
#   scjs_pool_subset_bin %>%
#   mutate_at(vars(c(matches("polpatrf"))), ~ recode(.,'1'=1,'2'=1,'3'=1,'4'=1,'5'=0,'6'=0,'-2'=0,'-1'=0,.default=NA_real_))


#QPCONINT: How much interest did the police show in what you had to say (most recent contact in last year)?
# 1 = as much as you thought they should
# -2,-1,2 = rf, dk, less
#POLPRES: Police presence in local area is:
# 1 = not enough
# -2,-1,2,3 = rf, dk, about right, too much
#QDCRIME2: Excluding motoring offences, have you ever been convicted of a crime?
# 1 = yes
# -2,-1,2 = rf, dk, no
scjs_pool_subset_bin <- convert_columns_to_factor(scjs_pool_subset_bin, "polpres")
scjs_pool_subset_bin <- convert_columns_to_factor(scjs_pool_subset_bin, "qdcrime")
scjs_pool_subset_bin <- convert_columns_to_factor(scjs_pool_subset_bin, "qpconint")

scjs_pool_subset_bin <- 
  scjs_pool_subset_bin %>%
  mutate_at(vars(matches("polpres|qdcrime|qpconint")), ~ recode(.,'1'=1,'2'=0,'3'=0,'-2'=0,'-1'=0,.default=NA_real_))

#QPCON: Have you PERSONALLY had any contact with the police service in the last year?
# 1 = yes
# -2,-1,2 = rf, dk, no
scjs_pool_subset_bin <- convert_columns_to_factor(scjs_pool_subset_bin, "qpcon")

scjs_pool_subset_bin <- 
  scjs_pool_subset_bin %>%
  mutate_at(vars(qpcon), ~ recode(.,'1'=1,'2'=0,'3'=0,'-2'=0,'-1'=0,.default=NA_real_))

#QPCONPOL: How polite were they in dealing with you (most recent contact in last year)?
# 1,2 = very fairly polite
# -2,-1,3,4 = rf, dk, fairly very impolite
#QPCONFAIR: How fairly would you say the police treated you on this occasion (most recent contact in last year)?
# 1,2 = very quite fairly
# -2,-1,3,4 = rf, dk, quite very unfairly
scjs_pool_subset_bin <- convert_columns_to_factor(scjs_pool_subset_bin, "qpconfair")
scjs_pool_subset_bin <- convert_columns_to_factor(scjs_pool_subset_bin, "qpconpol")

scjs_pool_subset_bin <- 
  scjs_pool_subset_bin %>%
  mutate_at(vars(matches("qpconfair|qpconpol")), ~ recode(.,'1'=1,'2'=1,'3'=0,'4'=0,'-2'=0,'-1'=0,.default=NA_real_))

#QPCONSAT: Overall, were you satisfied or dissatisfied with the way the police handled the matter (most recent contact in last year)? 
# 1,2 = very quite satisfied
# -2,-1,3,4,5,7 = rf, dk, quite very disatisfied, neither, too early
scjs_pool_subset_bin <- convert_columns_to_factor(scjs_pool_subset_bin, "qpconsat")

scjs_pool_subset_bin <- 
  scjs_pool_subset_bin %>%
  mutate_at(vars(qpconsat), ~ recode(.,'1'=1,'2'=1,'3'=0,'4'=0,'-2'=0,'5'=0,'7'=0,'-1'=0,.default=NA_real_))


#QPCONVIEW: Did this incident change your view of the police at all (most recent contact in last year)?  Did you view them: 
# 1,3 = no change, more favourable
# -2,-1,2 = rf, dk, less favourably
scjs_pool_subset_bin <- convert_columns_to_factor(scjs_pool_subset_bin, "qpconview")

scjs_pool_subset_bin <- 
  scjs_pool_subset_bin %>%
  mutate_at(vars(qpconview), ~ recode(.,'1'=1,'3'=1,'2'=0,'-2'=0,'-1'=0,.default=NA_real_))


#QRATPOL: Taking everything into account, how good a job do you think the police IN THIS AREA are doing?
# 1,2 = excellent or good
# -2,-1,3,4,5 = rf, dk, poor, fair
scjs_pool_subset_bin <- convert_columns_to_factor(scjs_pool_subset_bin, "qratpol")

scjs_pool_subset_bin <- 
  scjs_pool_subset_bin %>%
  mutate_at(vars(matches("qratpol")), ~ recode(.,'1'=1,'2'=1,'3'=0, '4'=0,'5'=0,'-2'=0,'-1'=0,.default=NA_real_))





