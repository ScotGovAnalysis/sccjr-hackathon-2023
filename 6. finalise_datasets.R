#### Tidies up environment after data

# Keeps only the pooled datasets.
rm(list = setdiff(ls(), c("df_pool", "df_pool_sc")))

# Deletes the victim form (VF) data sets from your directory to save space
# feel free to not run this if you wish to look at them for your project

# unlink(c("./data/scjs_s3_vff_ukda_110207.sav", "./data/scjs_s2_vff_091210.sav", 
#          "./data/scjs_s4_2010-11_vf_ukda_130115.sav"))
