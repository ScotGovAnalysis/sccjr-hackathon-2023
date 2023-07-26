### Create subset of first 3 years with self-completion summary variables

scjs_sc_select_bespoke <-
  scjs_pool_full_sc %>%
  select(year_case_id, survey_year,
         starts_with(c("qeve", "qof2", "q12m", "q1m", "qdr", "qdep", "qcutd"))
        )

scjs_pool_subset_bespoke <- 
  scjs_pool_subset %>% 
  filter(year %in% c("2008_09", "2009_10", "2010_11")) %>% 
  select(year_case_id, year, weight_scale, weight_scale_pool,
         gender, age, ethnicity, simd_quint, simd_top_15, disability, qlimit)

# scjs_pool_subset_bespoke <- 
#   scjs_pool_subset %>% 
#   filter(year %in% c("2008_09", "2009_10", "2010_11")) %>% 
#   select(year_case_id, year, weight_scale, weight_scale_pool,
#          gender, age, ethnicity, simd_quint, simd_top_15, disability, qlimit)

scjs_combined_bespoke <- left_join(scjs_pool_subset_bespoke, scjs_sc_select_bespoke, by = c("year_case_id", "year" = "survey_year"))