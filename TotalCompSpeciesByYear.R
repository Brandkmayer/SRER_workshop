library(tidyverse)
library(lubridate)
# Join all years and convert to Wideform to beused in an excel
path <- "C:/Users/brand/Dropbox/Project_Data/SRER/Utilization/Processed_data"
path2 <- "C:/Users/brand/Dropbox/Project_Data/SRER/Utilization/"
RAW_all <- list.files(path,pattern = "TotalCompositionUse.csv$", recursive = TRUE, full.names = TRUE)
dat_csv = plyr::ldply(RAW_all, read_csv)
TopSiteComp<- dat_csv %>% mutate(Year = year(ymd(Date)),Month = month(Date)) %>%
  group_by(Year, Site, Month) %>%
  arrange(Site, Month, Year, desc(`Percent of Count (%)`)) %>%
  slice(1:3)
TopSiteComp$Pasture = TopSiteComp$Site %>% 
  gsub('(C).*', '\\1', .) %>%
  gsub('(A).*', '\\1', .) %>% 
  gsub('(S).*', '\\1', .) %>% 
  gsub('(N).*', '\\1', .) %>% 
  gsub('(E).*', '\\1', .) %>% 
  gsub('(D).*', '\\1', .) %>% 
  gsub('(B).*', '\\1', .) %>% 
  gsub('(M).*', '\\1', .) %>% 
  gsub('(o).*', '', .) 
TopSiteComp$Transect = TopSiteComp$Site %>% 
  gsub('.*(C)', '', .) %>%
  gsub('.*(A)', '', .) %>% 
  gsub('.*(S)', '', .) %>% 
  gsub('.*(N)', '', .) %>% 
  gsub('.*(E)', '', .) %>% 
  gsub('.*(D)', '', .) %>% 
  gsub('.*(B)', '', .) %>% 
  gsub('.*(M)', '', .) %>% 
  gsub('.*(o)', '', .) 
TopSiteComp<-TopSiteComp %>%
  group_by(Year,Site, Transect, Month) %>%
  mutate(Ranks = order(order(Site, `Percent of Count (%)`, decreasing=TRUE)))
aum <- read_csv("C:/Users/brand/Dropbox/Project_Data/SRER/YearlyAU_bysite.csv")
TopSiteComp <- merge(x = TopSiteComp, y = aum, by = c("Year", "Pasture" )) %>% group_by(Year, Pasture, Site) %>% arrange(Year, Pasture, as.numeric(Transect))
TopSiteComp_Organized<- TopSiteComp %>% select(Pasture, Transect, Site, Ranks, Year, Month, AUM, `Species/Category Name`, `Percent of Count (%)`, `Mean Use (%)`)
TopSiteComp_Organized$Site <- gsub(x=TopSiteComp_Organized$Site, pattern = "o",replacement = " ")

TopSiteComp_Organized<- TopSiteComp_Organized %>% group_by(Pasture, Transect, Year, Site) %>% filter (! duplicated(Ranks))


library(tidyr)

temp <- pivot_wider(data = TopSiteComp_Organized, 
            id_cols = c(Pasture, Transect, Site, Ranks), 
            names_from = Year,
            names_glue = "{Year}_{.value}",
            values_from = c("Month","Species/Category Name","AUM", "Percent of Count (%)", "Mean Use (%)"),
            names_sort = TRUE
            )
list(colnames(temp))

temp <- temp[, c(1, 2, 3, 4, 5, 13, 21, 29, 37, 6, 14, 22, 30, 38, 7, 15, 23, 31, 39, 8, 16, 24, 32, 40, 9, 17, 25, 33, 41, 10, 18, 26, 34, 42, 11, 19, 27, 35, 43, 12, 20, 28, 36, 44)]
temp <- temp %>% arrange(Pasture, Site, as.numeric(Transect), Ranks)
colnames(temp)
write.csv(x = temp, file = paste(path2, "YearByYearSpeciesUtilization.csv",sep = ""), row.names=FALSE)

xlsx::write.xlsx(temp, file = paste0(path2,"YearByYearSpeciesUtilization.xlsx"),sheetName = "TopCompositionUse", append = FALSE,row.names = FALSE)

merge_cells<- temp %>% group_by(Site) %>%
  count() %>% as.data.frame() %>% mutate(rollapply_sum = zoo::rollapplyr(n, sum(n), sum, partial = TRUE))%>% mutate(oneup = rollapply_sum-(n-1)) %>%
  mutate(mergercolumn1 = paste0(oneup+1,":",rollapply_sum+1))
merge_pasture<- temp %>% group_by(Pasture) %>%
  count() %>% as.data.frame() %>% mutate(rollapply_sum = zoo::rollapplyr(n, sum(n), sum, partial = TRUE))%>% mutate(oneup = rollapply_sum-(n-1)) %>%
  mutate(mergercolumn1 = paste0(oneup+1,":",rollapply_sum+1))

wb <- openxlsx::loadWorkbook(file = paste0(path2,"YearByYearSpeciesUtilization.xlsx"))

## Add a worksheet
centerStyle <- openxlsx::createStyle(halign = "center", valign = "center")
colnames(temp)
for (i in 1:length(merge_cells[,5])) {
  rows<-seq(from = merge_cells[i,4]+1, to = merge_cells[i,3]+1, by = 1)
  openxlsx::mergeCells(wb, "TopCompositionUse", cols = 2, rows = rows)
  openxlsx::addStyle(wb, "TopCompositionUse", centerStyle,rows = rows, cols = 2)
  openxlsx::mergeCells(wb, "TopCompositionUse", cols = 3, rows = rows)
  openxlsx::addStyle(wb, "TopCompositionUse", centerStyle,rows = rows, cols = 3)
  openxlsx::mergeCells(wb, "TopCompositionUse", cols = 5, rows = rows)
  openxlsx::addStyle(wb, "TopCompositionUse", centerStyle,rows = rows, cols = 5)
  openxlsx::mergeCells(wb, "TopCompositionUse", cols = 10, rows = rows)
  openxlsx::addStyle(wb, "TopCompositionUse", centerStyle,rows = rows, cols = 10)
  openxlsx::mergeCells(wb, "TopCompositionUse", cols = 15, rows = rows)
  openxlsx::addStyle(wb, "TopCompositionUse", centerStyle,rows = rows, cols = 15)
  openxlsx::mergeCells(wb, "TopCompositionUse", cols = 20, rows = rows)
  openxlsx::addStyle(wb, "TopCompositionUse", centerStyle,rows = rows, cols = 20)
  openxlsx::mergeCells(wb, "TopCompositionUse", cols = 25, rows = rows)
  openxlsx::addStyle(wb, "TopCompositionUse", centerStyle,rows = rows, cols = 25)
  openxlsx::mergeCells(wb, "TopCompositionUse", cols = 30, rows = rows)
  openxlsx::addStyle(wb, "TopCompositionUse", centerStyle,rows = rows, cols = 30)
  openxlsx::mergeCells(wb, "TopCompositionUse", cols = 35, rows = rows)
  openxlsx::addStyle(wb, "TopCompositionUse", centerStyle,rows = rows, cols = 35)
  openxlsx::mergeCells(wb, "TopCompositionUse", cols = 40, rows = rows)
  openxlsx::addStyle(wb, "TopCompositionUse", centerStyle,rows = rows, cols = 40)
}
for (i in 1:length(merge_pasture[,5])) {
  rows<-seq(from = merge_pasture[i,4]+1, to = merge_pasture[i,3]+1, by = 1)
  openxlsx::mergeCells(wb, "TopCompositionUse", cols = 1, rows = rows)
  openxlsx::addStyle(wb, "TopCompositionUse", centerStyle,rows = rows, cols = 1)
}

################# Save

openxlsx::saveWorkbook(wb, paste0(path2,"YearByYearSpeciesUtilization.xlsx"), overwrite = TRUE)



