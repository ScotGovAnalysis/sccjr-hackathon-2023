### This script will extract the datsets from your download folder and place them into a new directory 

download_path <- "C:\\Users\\u449344\\downloads" # Enter your download file path

project_path <- "C:\\Users\\u449344\\OneDrive - SCOTS Connect\\SCCJR_hackathon\\sccjr-hackathon-2023" # Enter the file path where you want the project to sit

setwd(project_path) # sets your current working directory to the specified project path

# creates a new directory in your project path called 'data' if one does not already exist
if (file.exists("data")) {
  print("data directory already exists in project path")
} else {
  dir.create("data")
  print("Created 'data' directory in project path")
}

# a list of all the files downloaded from the UKDS project
zip_files <- c("6362spss_b0e85406b8db4312682a52d39d6c950a.zip", #2008-09
               "6685spss_38fbf2daa74fe2c7805adf18e843ada3.zip", #2009-10
               "7229spss_b5abb40d9fd551b8a5c819aa9909a793.zip", #2010-11
               "7543spss_2a88f8ecfef18aa3c6312ab5b5e51738.zip", #2012-13
               "8141spss_8a83a1aa2b2d425c3004d4f092ad0e45.zip", #2014-15
               "8365spss_7997EC99C91E83ACD8547C13A00C78B2_V1.zip", #2016-17
               "8498spss_20B11DC9D8F60F39866F0DABA7A36D0C_V1.zip", #2017-18
               "8795spss_4F3477A29CCACB5D611F0F3F984BD5121865A4FBCD4FC2BED5FA50D7DBA387A0_V1.zip", #2018-19
               "8799spss_8C7E02210BA3E728C72A6FB7DE2399925C335D8D5596D9BE59DF208602AD331E_V1.zip" #2019-20
               )

# extracts and unzips all the downloaded files to your project path
for (file in zip_files){
  zip_f <-  paste0(download_path, 
                   "\\",
                   file)
  out_f <-  paste0(project_path, "\\", "data")
  unzip(zip_f, exdir = out_f)
}

# saves a list of the names of all of the extracted folders, useful to delete after copying dataset files to tidy up and reduce size
unzipped_files <- c(paste0("data\\", list.files(paste0(project_path, "\\data"))))

# moves the SPSS datasets from their subfolders into your 'data' folder
spss_files <- list.files(pattern='*.sav', recursive = TRUE)
file.copy(from = spss_files,
          to = paste0(project_path, "\\", "data"), recursive = TRUE,
          overwrite = TRUE, copy.mode = TRUE, copy.date = FALSE)

# deletes excess files in data directory - if you wish to keep these for completeness's sake then comment this line out
unlink(unzipped_files, recursive = TRUE)

rm(list = c("download_path", "file", "out_f", "spss_files",
            "unzipped_files", "zip_f", "zip_files"))

