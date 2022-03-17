folder_files<- list.files("E:/SantaRita",recursive = T, include.dirs = F, pattern = ".xlsx")
folder_files <- 
# i<-"Summer 2013/Pasture 12B/Spring Grazing/12B_T10_2013.xlsx"
for (i in folder_files) {
  path_to_xlsx <- paste("E:/SantaRita",i,sep="/")
  #year <- stringr::str_sub(i,-9,-6)
  # path_to_xlsx <- "E:/SantaRita/GY13_14/2N_util_2014.xlsx"
  sheet_names <- readxl::excel_sheets(path_to_xlsx)
  date <- as.data.frame(purrr::map(sheet_names, ~ readxl::read_excel(path_to_xlsx, sheet = .x, col_names = F)[1,3]))
  date <- date %>% magrittr::extract2(1) %>% as.numeric()
  date <- gsub("-", "", as.Date(date, origin = "1899-12-30"))
  sheets <- purrr::map(sheet_names, ~ readxl::read_excel(path_to_xlsx, sheet = .x, skip = 5)[1:50,c(1,2,5,6,7,10)])
  sheet_names<- paste(date,gsubfn::gsubfn(".",list("-" = "", "T" = "", "_" = ""),sheet_names),sep = "_")
  base::names(sheets) <- sheet_names

  colnames <-c("No.",	"Species code",	"% Utilized",	"No.",	"Species code",	"% Utilized")
  sheets <- lapply(sheets, setNames, colnames)

  purrr::iwalk(sheets, ~ readr::write_csv(x = .x, path = paste0(.y, ".csv")))

}

file_depo<- "C:/Users/Kali/Dropbox/Project_Data/SRER/Utilization/RAW_Data"

filesstrings::file.move(list.files(pattern = "\\.csv$"), file_depo)

