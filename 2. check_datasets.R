# The script will check if all of the correct SPSS datasets 
# are now present in your directory

setwd(project_path)

required_files <- c(
  "new_main_2014_15_2_main_dataset.sav",              
  "scjs_main_2012_13_5.sav",                   
  "scjs_s2_rf_091214.sav",                 
  "scjs_s3_rf_ukda_110120.sav",    
  "scjs_s3_scf_110808.sav",                            
  "scjs_s4_2010-11_rf_ukda_130115.sav",                     
  "scjs_s4_2010-11_sc_ukda_130115.sav", 
  "scjs1617_nvf-main_y1_eul.sav",    
  "scjs1718__nvf-main_y2_eul_20190508.sav",                 
  "scjs1819_nvf-main_y3_eul-safeguarded_20210316_cyber.sav",
  "scjs1819_nvf-main_y3_eul-safeguarded_20210316_nvf.sav",
  "scjs1920_nvf-main_y4_eul-safeguarded_20210322_cyber.sav",
  "scjs1920_nvf-main_y4_eul-safeguarded_20210322_nvf.sav",  
  "scjs2_sc_091209.sav")

if (length(setdiff(required_files, c(list.files("./data/")))) == 0){
  print("All files are in correct directory")
} else{
  print("Not all files are in the directory")
  print("The following files are not in the directory:")
  setdiff(required_files, c(list.files("./data/")))
}

rm("required_files")
