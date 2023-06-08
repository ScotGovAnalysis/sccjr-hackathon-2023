#### Pooling info ####

year <- c(2008, 2009, 2010, 2012, 2014, 2016, 2017, 2018, 2019)
time <- c(1, 2, 3, 5, 7, 9, 10, 11, 12)
dfactor <- c(1.5, 1.5, 1.5, 1.3, 1.2, 1.34, 1.22, 1.17, 1.21)
n <- c(nrow(df_0809), nrow(df_0910), nrow(df_1011),
       nrow(df_1213), nrow(df_1415), nrow(df_1617),
       nrow(df_1718), nrow(df_1819), nrow(df_1920))

pooling_info <- tibble(year, time, dfactor, n) %>% 
  mutate(deff = dfactor ^ 2) %>% 
  mutate(neff = n / deff) %>% 
  mutate(pool_neff = neff / sum(neff))

total_rows <- sum(nrow(df_0809), nrow(df_0910), nrow(df_1011),
                  nrow(df_1213), nrow(df_1415), nrow(df_1617),
                  nrow(df_1718), nrow(df_1819), nrow(df_1920))
total_rows

#### Combine years ####

df_pool <- bind_rows(df_0809, df_0910, df_1011,
                      df_1213, df_1415, df_1617,
                      df_1718, df_1819, df_1920)

#### Add pooling info ####

df_pool2 <- left_join(df_pool, pooling_info, by = c("pool_year" = "year"))

summary_info <- df_pool2 %>% 
  select(pool_year, wgtgindiv) %>% 
  group_by(pool_year) %>% 
  summarise(sum_weight = sum(wgtgindiv))

df_pool3 <- left_join(df_pool2, summary_info, by = "pool_year")

df_pool3 <- df_pool3 %>% 
  mutate(weight_scale = wgtgindiv / (sum(wgtgindiv) / n)) %>% 
  mutate(weight_scale_pool = weight_scale * pool_neff)

df_pool <- df_pool3





