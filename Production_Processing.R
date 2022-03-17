require(tidyverse)
# ---------------------------------------------------------- CY -------------------------------------------------------------
# weights<- read.csv("C:/Users/brand/Dropbox/Project_Data/SRER/Production/2020Production_Dryweights.csv")
weights<- read.csv("D:/KoweekData/Horseshoe_Production_Dryweights.csv")
sumarWeights<-weights %>% group_by(Pasture, Transect)%>% summarise(formula =(lm(c(0,Value.g.)~c(0,Bag)))$coefficients[-1])
# allCY <- list.files(path = "C:/Users/brand/Dropbox/Project_Data/SRER/Production/SRER2020_sheeted", pattern = "CY",full.names = T)
allCY <- list.files(path = "D:/KoweekData/Data_sheeted", pattern = "CY",full.names = T)

for (i in allCY) {
  # i <- "D:/KoweekData/Data_sheeted/HorseshoeFryPanCY1_2020-05-11_CY.csv" 
  file <- read.csv(i)
  base<- basename(i) # pasture_transect_method.csv
  file$Pasture <- gsub("_.*","",base)
  file$Transect <- stringr::str_match(base, ".*_([^\\.]*)\\_.*")[2]
  file<- file %>% select(Pasture, Transect, Yield = Rank)
  write.csv(file,i,row.names = F)
}
CYdf <- data.table::rbindlist(lapply(allCY, data.table::fread))

TranCY <- transform(merge(CYdf, sumarWeights, by=c("Pasture", "Transect")), 
                    Dry.wt = Yield * formula) %>% na.omit() 
Production<- TranCY %>% group_by(Pasture, Transect)%>% summarise("Production_kg/ha" = (mean(Dry.wt)*62.5), "Production_lbs/ac" = (round(mean(Dry.wt)*62.5*.9,digits = 2)))
write.csv(Production, "D:/KoweekData/CY2020.csv")
# -----------------------------------------------------------DWR--------------------------------------------------------------

# allDWR <- list.files(path = "C:/Users/brand/Dropbox/Project_Data/SRER/Production/SRER2020_sheeted", pattern = "DWR",full.names = T)
allDWR <- list.files(path = "D:/KoweekData/Data_sheeted", pattern = "DWR",full.names = T)

for (i in allDWR) {
  file <- read.csv(i)
  base<- basename(i) # pasture_transect_method.csv
  file$Pasture <- gsub("_.*","",base)
  file$Transect <- stringr::str_match(base, ".*_([^\\.]*)\\_.*")[2]
  file<- file %>% select(Pasture, Transect, Species.Category.Name, Species.Category.Code, Rank)
  write.csv(file,i,row.names = F)
}


DWRdf <- data.table::rbindlist(lapply(allDWR, data.table::fread))

SpecList <- read.csv("C:/Users/brand/Dropbox/Project_Data/SRER/Production/Specieslist.csv")

TranDWR <- merge(DWRdf, SpecList, by=c("Species.Category.Name"))
  TranDWR$Rank[TranDWR$Rank == 1] <- 7
  TranDWR$Rank[TranDWR$Rank == 2] <- 2
  TranDWR$Rank[TranDWR$Rank == 3] <- 1
  TranDWR$Rank <- as.numeric(TranDWR$Rank)
GroupCounts<- TranDWR %>% group_by(Pasture, Transect, Plant_type, Native) %>% summarise("TotalCounts" = sum(Rank))

CountPercent<- GroupCounts %>% group_by(Pasture, Transect) %>%mutate(TotalCounts/sum(TotalCounts)) %>% subset(Plant_type == "Perennial Grasses") %>% select(!TotalCounts)
CountPercent %>% spread(Native, `TotalCounts/sum(TotalCounts)`) %>% replace(is.na(.), 0) %>% mutate(Total = Native)
write.csv(CountPercent %>% spread(Native, `TotalCounts/sum(TotalCounts)`) 
          %>% replace(is.na(.), 0) %>% mutate(Total = round((Introduced+Native),2)) %>% select(Pasture, Transect, Total, Native, Introduced), 
          "D:/KoweekData/PerennialgrassCOMP.csv")


#----------------------------------------------------------------------- Finished Product --------------------------------------------------------------------------------

final <- transform(Production, "Perennial grass Production" = `Production_lbs/ac`*CountPercent$`TotalCounts/sum(TotalCounts)`)
write.csv(final, "D:/KoweekData/final_results.csv", row.names = F)


p2



# df1$ele <- df2[match(paste(df1$lat,df1$long),paste(df2$lat,df2$long)),"ele"]
