#### Tidies up environment after data

# Keepos only the pooled datasets.
rm(list = setdiff(ls(), c("df_pool", "df_pool_sc")))

