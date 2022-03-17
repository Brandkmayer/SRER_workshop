# Walkthrough to organize .xlsx file into CSV files for analysis in "Production_Processing.R" file.

# absolute path to folder with samples
# absol_path <- "C:/Users/brand/Dropbox/Project_Data/SRER/Production/SRERexl2020"
# absol_path <- "D:/KoweekData/Data_raw"
absol_path <- readline() # paste the location of the Raw or remove "readline()" and type in file location. 

# folder_files <- list.files("C:/Users/brand/Dropbox/Project_Data/SRER/Production/SRERexl2020",recursive = T, include.dirs = F, pattern = ".xlsx")
folder_files <- list.files(absol_path,recursive = T, include.dirs = F, pattern = ".xlsx")

# path to external hardrive
# folder_files<- list.files(paste("D:/SRER_monitoring",year,sep = "/"),recursive = T, include.dirs = F, pattern = ".xlsx")
# absol_path <- "D:/SRER_monitoring/2020"


# finalfolder <-"C:/Users/brand/Dropbox/Project_Data/SRER/Production/SRER2020_sheeted"
finalfolder <-paste0(getwd(),"/data")
for (i in folder_files) {
  path_to_xlsx <- paste(absol_path,i,sep="/")
  # path_to_xlsx <- "D:/KoweekData/Data_raw/FryPan_20201109_170936.xlsx"
  metadata <- readxl::read_excel(path_to_xlsx, sheet = 1)
  Pasture <- gsub(" ", "", sub('-.*', '\\1',sub('.*>', '\\1', metadata[1,1])), fixed = TRUE)
  Transect <- stringr::str_sub(metadata[1,1],-1,-1)
  # date <- format(strptime(as.character(sub('.*:', '\\1', metadata[2,1])), "%d/%m/%Y"), "%Y-%m-%d") #dont need date
  base <- paste0(Pasture,"_",Transect)
  sheet_names <- readxl::excel_sheets(path_to_xlsx)[2:3]
  # sheet_names <- readxl::excel_sheets(path_to_xlsx)[2:4]
  # base <- gsub("\\..*","",basename(path_to_xlsx))
  for (j in sheet_names) {
    # j <- sheet_names[2]
    data <- readxl::read_excel(path_to_xlsx, sheet=j)
    if (j == "Frequency (by quadrat)") {
      name <- "Freq"
    }else if (j == "Comparative Yield") {
      name <- "CY"
    }else{
      name <- "DWR"
    }
    write.csv(data, file=paste0(finalfolder,"/",base,"_",name,".csv"))
  }
}
