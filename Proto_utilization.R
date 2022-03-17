library(tidyverse)
library(lubridate)
library(readr)
library(data.table)
library(dplyr)
library(DescTools) # Adds %like any% which im using instead of %like% from 
#the data.table package because you can have multiple values assigned to one change


year <- 'GY2019/' 
path <- paste("C:/Users/brand/Dropbox/Project_Data/SRER/Utilization/RAW_Data/", year, sep="")
path2 <- paste("C:/Users/brand/Dropbox/Project_Data/SRER/Utilization/Processed_data/", year, sep="")
dir.create(path2)
#brand

# path2 <- paste(path,"5N")
# filez <- list.files(path = path2, pattern = "*.csv", full.names = T, all.files = TRUE, recursive = TRUE)
# filedate <- "20200614"
# sapply(filez,FUN=function(eachPath){
#   file.rename(from=eachPath,to=paste(filedate ,basename(eachPath), sep = "_"))
# })
# sapply(files,FUN=function(eachPath){
#   file.rename(from=eachPath,to=sub(pattern="VGSExport_",replacement="",eachPath))
# })

files <- list.files(path = path, pattern = "*.csv", full.names = T, all.files = TRUE, recursive = TRUE)
tbl <- sapply(files, read_csv, simplify=FALSE)

colnames<- c(colnames(tbl[[1]]))
rm(files)

# filedcsv <- list.files(path = path, pattern = "*.csv", full.names = T, all.files = TRUE, recursive = TRUE) %>%
#   lapply(function(x) {read_csv(x, skip=3, col_names = TRUE)}) %>%
#   lapply(setNames, colnames)
filedcsv <- list.files(path = path, pattern = "*.csv", full.names = T, all.files = TRUE, recursive = TRUE) %>%
  lapply(function(x) {read_csv(x,skip = 3, col_names = TRUE)}) %>%
  lapply(setNames, colnames)
# building the final 
final <- lapply(seq_along(filedcsv), function(i){    
  x <- as.data.frame(filedcsv[[i]])
  x$Date <- strsplit(basename(names(tbl[i])), "[_]")[[1]][1]
  x$Site <- as.character(str_remove_all(strsplit(basename(names(tbl[i])), "[_]")[[1]][2],"[.csv]"))
  return(x)})  %>% bind_rows(.id = "id")

final$Pasture <- final$Site %>% 
  gsub('(C).*', '\\1', .) %>%
  gsub('(A).*', '\\1', .) %>% 
  gsub('(S).*', '\\1', .) %>% 
  gsub('(N).*', '\\1', .) %>% 
  gsub('(E).*', '\\1', .) %>% 
  gsub('(D).*', '\\1', .) %>% 
  gsub('(B).*', '\\1', .) %>% 
  gsub('(M).*', '\\1', .) %>% 
  gsub('(o).*', '', .) 

#Clean "final", replace NAs with 0 and check for spelling errors 
final[is.na(final)] <- 0
final<- data.table(final)
colnames(final)[3] <- "SpeciesCode"
unique(final$SpeciesCode)
# correct_values <- c("ARIST","DICA","ERLE","MUPO","SEMA","SPORO","BORO","HECO","BOER","PLMU","BOBA","HIBE","ERCU","PECI", "UNKN", "BOCU", "LEDU","BOHI","BOCH","BORE","MERE","PAMU","ENDE")
# for (i in 1:nrow(final)){ # i <- 2
#   string <- final[i,`SpeciesCode`]
#   max <- 0
#   similarity <- 0
#   for(j in correct_values){ # j <- "ARIST"
#     similarity <-   length(Reduce(intersect, strsplit(c(string, j), split = "")))
#     if(similarity > max){
#       max <- similarity
#       to_replace <- j
#     }
#   }
#   final[i,"SpeciesCode"] <- to_replace
# }

final[,`Species/Category Name`:=ifelse(SpeciesCode%like any%c("^ARIST", "^ARIS", "^SARIS", "^ARI", "^AIRS", "^AR","^ARES","^aris"),
                                       "Aristida sp.",
                       ifelse(SpeciesCode%like any%c("^DICA", "^DIC", "^DIA", "^ADICA","^DCA","^dica","^AR","^ARIZ"),
                              "Digitaria californica",
                              ifelse(SpeciesCode%like any%c("^ERLE", "^ER","^ERE","^erle"),
                                     "Eragrostis lehmanniana",
                                     ifelse(SpeciesCode%like any%c("^MUPO", "^MU", "^MUP","^muPO","^mupo","^MUTO"),
                                            "Muhlenbergia porteri",
                                            ifelse(SpeciesCode%like any%c("^SEMA","^SEM","^sema"),
                                                   "Setaria macrostachya",
                                                   ifelse(SpeciesCode%like any%c("^BRO","^BORO","^boro"),
                                                          "Bouteloua rothrockii",
                                                          ifelse(SpeciesCode%like any%c("^BOER"),
                                                                 "Bouteloua eriopoda",
                                                                 ifelse(SpeciesCode%like any%c("^PLMU"),
                                                                        "Hilaria mutica",
                                                                        ifelse(SpeciesCode%like any%c("^DAPU"),
                                                                               "Dasyochloa pulchella",
                                                                               ifelse(SpeciesCode%like any%c("^HIBE"),
                                                                                      "Hilaria belangeri",
                                                                                      ifelse(SpeciesCode%like any%c("^PECI"),
                                                                                             "Pennisetum ciliare",
                                                                                             ifelse(SpeciesCode%like any%c("^HECO"),
                                                                                                    "Hesperostipa comata",
                                                                                                    ifelse(SpeciesCode%like any%c("^SPOR","^SPORO"),
                                                                                                           "Sporobolus sp.",
                                                                                                           ifelse(SpeciesCode%like any%c("^PAMU","^AMU","^PAPS"),
                                                                                                                  "Pappophorum vaginatum",
                                                                                                                  ifelse(SpeciesCode%like any%c("^ERCU","^ERCO"),
                                                                                                                         "Eragrostis curvula",
                                                                                                                         ifelse(SpeciesCode%like any%c("^MERE", "^MER"),
                                                                                                                                "Melinis repens",
                                                                                                                                ifelse(SpeciesCode%like any%c("^BOBA"),
                                                                                                                                       "Bothriochloa barbinodis",
                                                                                                                                       ifelse(SpeciesCode%like any%c("^BOHI"),
                                                                                                                                              "Bouteloua hirsuta",
                                                                                                                                              ifelse(SpeciesCode%like any%c("^LEDU"),
                                                                                                                                                     "Leptochloa dubia",
                                                                                                                                                     ifelse(SpeciesCode%like any%c("^BOCH","^BOC","^BOF"),
                                                                                                                                                            "Bouteloua chondrosioides",
                                                                                                                                                            ifelse(SpeciesCode%like any%c("^ENDE"),
                                                                                                                                                                   "Enneapogon desvauxii",
                                                                                                                                                                   ifelse(SpeciesCode%like any%c("^BOCU","^BOCUBOCU","^BOCI"),
                                                                                                                                                                          "Bouteloua curtipendula",
                                                                                                                                                                          ifelse(SpeciesCode%like any%c("^SPCR","^SPCO"),
                                                                                                                                                                                 "Sporobolus cryptandrus",
                                                                                                                                                                                 ifelse(SpeciesCode%like any%c("^BORE"),
                                                                                                                                                                                        "Bouteloua repens",
                                                                                                                                                                                        ifelse(SpeciesCode%like any%c("^BOGR"),
                                                                                                                                                                                               "Bouteloua gracilis",
                                                                                                                                                                                               ifelse(SpeciesCode%like any%c("^COTTEA","^COPA","^HAPA"),
                                                                                                                                                                                                      "Cottea pappophoroides",
                                                                                                                                                                                                      ifelse(SpeciesCode%like any%c("^ERIN", "^PLAINS"),
                                                                                                                                                                                                             "Eragrostis intermedia",
                                                                                                                                                                                                             ifelse(SpeciesCode%like any%c("^CYDA"),
                                                                                                                                                                                                                    "Cynodon dactylon","Unknown Perennial Grass"))))))))))))))))))))))))))))]
unique(final$`Species/Category Name`)

# Percentage Use by site to join with Arcgis site use. 
colnames(final)[4] <- "Class End-Point"
final[is.na(final)] <- 0
useaverage <- final %>% group_by(Date, Site, Pasture) %>% summarize(`Percent Ungrazed Utilization` = round(79.9451-(0.8705*sum(`Class End-Point` < 1)), digits = 1),
                                                                    `Graze Class Utilization` = round(sum(`Class End-Point`)/sum(`Class End-Point` > 0), digits = 1))
useaverage$Year = substr(useaverage$Date, start = 0,stop = 4)
write.csv(x = useaverage, file = paste(path2, "SiteUse.csv",sep = ""), row.names=FALSE)


# Percentage of Use by Pasture

pasture_average <- useaverage %>% group_by(Year,Pasture) %>% summarize( `Percent Ungrazed Utilization`= round(mean(`Percent Ungrazed Utilization`)),
                                                             `Graze Class Utilization`= round(mean(`Graze Class Utilization`)))
pasture_average[is.na(pasture_average)] <- 0

write.csv(x = pasture_average, file = paste(path2, "PastureUse.csv",sep = ""), row.names=FALSE)

# top species use 
# colnames(final)[3] <- "Species/Category Name"
Speciesuseaverage <- final %>% group_by(Site, Date, `Species/Category Name`) %>%  summarize(count = n(), mean = round(mean(`Class End-Point`[`Class End-Point` > 0])))
Speciesuseaverage[is.na(Speciesuseaverage)] <- 0
write.csv(x = Speciesuseaverage, file = paste(path2, "AllSpeciesUse.csv",sep = ""), row.names=FALSE)


d <- data.table(Speciesuseaverage, key="Site")

d.out <-d[order(d$Site,desc(d$count)),]
setnames(d.out, old = 'mean', 'Mean Use (%)')
setnames(d.out, old = 'count', 'Percent of Count (%)')
TopCompositionUse <- d.out
remove(d,d.out)

TopCompositionUse$Date<- as.character(lubridate::ymd(TopCompositionUse$Date))

## Create a new workbook

xlsx::write.xlsx(TopCompositionUse, file = paste0(path2,"TotalCompositionUse.xlsx"),sheetName = "TopCompositionUse", append = FALSE,row.names = FALSE)

merge_cells<- TopCompositionUse %>% group_by(Site) %>%
  count() %>% as.data.frame() %>% mutate(rollapply_sum = zoo::rollapplyr(n, sum(n), sum, partial = TRUE))%>% mutate(oneup = rollapply_sum-(n-1)) %>%
  mutate(mergercolumn1 = paste0(oneup+1,":",rollapply_sum+1))

wb <- openxlsx::loadWorkbook(file = paste0(path2,"TotalCompositionUse.xlsx"))

## Add a worksheet
centerStyle <- openxlsx::createStyle(halign = "center", valign = "center")

for (i in 1:length(merge_cells[,5])) {
  rows<-seq(from = merge_cells[i,4]+1, to = merge_cells[i,3]+1, by = 1)
  openxlsx::mergeCells(wb, "TopCompositionUse", cols = 1, rows = rows)
  openxlsx::addStyle(wb, "TopCompositionUse", centerStyle,rows = rows, cols = 1)
  openxlsx::mergeCells(wb, "TopCompositionUse", cols = 2, rows = rows)
  openxlsx::addStyle(wb, "TopCompositionUse", centerStyle,rows = rows, cols = 2)
}

################# Save

openxlsx::saveWorkbook(wb, paste0(path2,"TotalCompositionUse.xlsx"), overwrite = TRUE)
# write.csv(x = TopSpeciesUse, file = paste(path2, "TopSpeciesUse.csv",sep = ""), row.names=FALSE)
TopCompositionUse$Date[which(TopCompositionUse$Date == "2019-12-28"|TopCompositionUse$Date == "2019-12-18")] = "2020-01-01"
TopCompositionUse$Date<- lubridate::ymd(TopCompositionUse$Date)

write.csv(x = TopCompositionUse, file = paste(path2, "TotalCompositionUse.csv",sep = ""), row.names=FALSE)




#####################################################################################

d <- data.table(Speciesuseaverage, key="Site")
# d.out <- d[, .SD[mean %in% tail(sort(unique(mean)), 3)], by=Site]
# d.out <-d.out[order(d.out$Site,desc(d.out$mean)),]
# setnames(d.out, old = 'mean', 'Mean Use (%)')
# setnames(d.out, old = 'count', 'Percent of Count (%)')
# TopSpeciesUse <- d.out
d.out <-d[order(d$Site,desc(d$count)),]
d.out <- d.out[, .SD[count %in% tail(sort(unique(count)), 3)], by=Site]
# d.out <-d.out[order(d.out$Site,desc(d.out$count)),]
setnames(d.out, old = 'mean', 'Mean Use (%)')
setnames(d.out, old = 'count', 'Percent of Count (%)')
TopCompositionUse <- d.out

#### Wanted to look for the most common species between all the sites but
#### on site was entirely composed of lenman lovegrass...  where there were
#### also sites without it.... 
# split_tibble <- function(tibble, column = 'col') {
#   tibble %>% split(., .[,column]) %>% lapply(., function(x) x[,setdiff(names(x),column)])
# }
# SiteList<- split_tibble(as.tibble(d), 'Site')
# 
# for (i in 1:length(SiteList)) {
#   j <-paste0("L",i)
#   ll <- c(ll, list(j = SiteList[[i]][[2]]))
# }
# Reduce(intersect,ll)













# count, mean - median, and proportion of the use at the site
group_by(Speciesuseaverage,Site) %>% 
  mutate(pmean=(count/100)*mean("Mean Use")) 

PastureSpeciesUse <- final %>% group_by(Pasture, `Species/Category Name`) %>% summarize(count = n(), mean = round(mean(`Class End-Point`[`Class End-Point` > 0])))

PastureSpeciesUse <- setDT(PastureSpeciesUse)[, list(Count = sum(count), Mean = paste0(mean,"%")), keyby = list(Pasture, `Species/Category Name`)][, 
                                Compostion:= paste0(round(Count/sum(Count), 2)*100, "%"), by = Pasture][]
PastureSpeciesUse[is.na(PastureSpeciesUse)] <- 0

P <- data.table(PastureSpeciesUse, key="Pasture")
P.out <- P[, .SD[Count %in% tail(sort(unique(Count)), 3)], by=Pasture] # can change Count to Mean to focus on the top three of either category
P.out <- P.out[order(P.out$Pasture,desc(P.out$Count)),]


setnames(P.out, old = 'Mean', 'Average Use (%)')
setnames(P.out, old = 'Count', 'Total Pature Count')
setnames(P.out, old = 'Compostion', 'Compostion (%)')

P.out <- P.out[,c(1,2,3,5,4)]
write.csv(x = P.out, file = paste(path2, "CommonSpeciesUse.csv",sep = ""), row.names=FALSE)







Arc_GIS <- read_csv("C:/Users/brand/Dropbox/Project_Data/SRER/Utilization/ArcGIS_TransectUTM_Usebyyear.csv", col_names = TRUE)
Pastureprocessed <- Arc_GIS %>% group_by(Pasture) %>% summarize(GY2019 = round(mean(as.numeric(GY2019))), GY2020 = round(mean(as.numeric(GY2020))))
PastureFinal<- Pastureprocessed %>% drop_na(GY2020)
write.csv(x = PastureFinal, file = paste(path2, "PastureYearCompare.csv",sep = ""), row.names=FALSE)
