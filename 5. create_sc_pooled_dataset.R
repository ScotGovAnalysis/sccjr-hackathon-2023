require(tidyverse)
require(haven)

#### Pooling info ####

year_sc <- c(2008, 2009, 2010)
time_sc <- c(1, 2, 3)
dfactor_sc <- c(1.5, 1.5, 1.5)
n_sc <- c(nrow(df_0809_sc), nrow(df_0910_sc), nrow(df_1011_sc))

pooling_info_sc <- tibble(year_sc, time_sc, dfactor_sc, n_sc) %>% 
  mutate(deff = dfactor_sc ^ 2) %>% 
  mutate(neff = n_sc / deff) %>% 
  mutate(pool_neff = neff / sum(neff))

df_pool_sc <- bind_rows(df_0809_sc, df_0910_sc, df_1011_sc)

df_pool_sc2 <- left_join(df_pool_sc, pooling_info_sc, 
                         by = c("pool_year" = "year_sc"))

summary_info_sc <- df_pool_sc2 %>% 
  select(pool_year, wgtgindiv_sc) %>% 
  group_by(pool_year) %>% 
  summarise(sum_weight = sum(wgtgindiv_sc))

df_pool_sc3 <- left_join(df_pool_sc2, summary_info_sc, by = "pool_year")

df_pool_sc3 <- df_pool_sc3 %>% 
  mutate(weight_scale_sc = wgtgindiv_sc / (sum_weight / n_sc)) %>% 
  mutate(weight_scale_pool_sc = weight_scale_sc * pool_neff)

df_pool_sc <- df_pool_sc3
