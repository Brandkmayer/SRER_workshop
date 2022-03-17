transects_unchanged<- readxl::read_excel("C:/Users/brand/Dropbox/Project_Data/SRER/Utilization/Transect Management/waypoints_TableToExcel.xlsx")
transects_unchanged$Transect <- gsub("(.+?)(\\ .*)", "\\1", transects_unchanged$Name)
transects_unchanged$location <- sapply(strsplit(transects_unchanged$Name, "\\s+"), "[", 2)

SpringTransects2021 <-transects_unchanged %>% 
  select(Name,"Pasture"=Descript,"Location"=location, Transect, X, Y) 

%>% 
  pivot_wider(names_from = Location, values_from = c(X,Y))

write.csv(SpringTransects2021,"C:/Users/brand/Dropbox/Project_Data/SRER/Utilization/Transect Management/waypoints_long.csv")


# need to merge results with point map
# join based on transect name. 
transects_points<- readxl::read_excel("C:/Users/brand/Dropbox/Project_Data/SRER/Utilization/Transect Management/PointsToLine_ToCenterPoint.xlsx")
transects_points <- transects_points %>% select("SiteID"=Transect, X, Y)
Use_To_Merge <- read.csv("C:/Users/brand/Dropbox/Project_Data/SRER/Utilization/Processed_data/GY2021/SiteUse.csv")
Final_Join<- left_join(transects_points, Use_To_Merge, by = "SiteID")
Final_Join<-Final_Join[complete.cases(Final_Join), ]
write.csv(Final_Join,"C:/Users/brand/Dropbox/Project_Data/SRER/Utilization/Processed_data/GY2021/Joined_Spring_SiteUse.csv" )

# merge pdfs together
# install.packages("qpdf")
qpdf::pdf_combine(c("C:/Users/brand/Dropbox/Project_Data/SRER/Utilization/Maps/GY2021_SpringHalfYear_UseMap.pdf",
                    "C:/Users/brand/Dropbox/Project_Data/SRER/Utilization/Processed_data/GY2021/CommonSpeciesUse.pdf",
                    "C:/Users/brand/Dropbox/Project_Data/SRER/Utilization/Maps/GY2021SiteOverview.pdf",
                    "C:/Users/brand/Dropbox/Project_Data/SRER/Utilization/Processed_data/GY2021/SiteUseByTransect.pdf"), "C:/Users/brand/Dropbox/Project_Data/SRER/Utilization/Maps/Final/UseMap.pdf")

