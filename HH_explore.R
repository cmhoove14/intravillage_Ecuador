require(rgdal)
require(maptools)

data<-read.csv("C:/Users/chris_hoover/Documents/RemaisWork/SurfaceH2O/Ecuador/IntraVillage/hh_condense.csv")  

#Get rid of hh-weeks where nobody was surveyed
  hh<-subset(data, n_people != 0)
  hhs<-unique(hh$HH)
  
#merge with household spatial data
gis<-read.csv("C:/Users/chris_hoover/Documents/RemaisWork/SurfaceH2O/Ecuador/IntraVillage/GIS_ID_Casa.csv")  
  gishhs<-unique(gis$ID_Casa)
  giscols<-colnames(gis)
#projection to use
  prj<-CRS("+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

#merge spatial data with surveillance data; looks like we lose 6125 records
hh2<-merge(x=hh, y=gis, by.x="HH", by.y="ID_Casa")
  
#Drop unwanted variables from spatial data frame (character vector below is ones to keep)
hh2<-hh2[, !names(hh2) %in% giscols[!giscols %in% c("Com", "Numero", "Ocupada", "Destruida", "POINT_X", "POINT_Y")]]
  hh2s<-unique(hh2$HH)
  
#vector of households in surveillance dataset that did not have GIS data
  droppedhhs<-hhs[-which(hhs %in% hh2s)]
  
#Export final dataset (NOTE: THIS IS SENSITIVE DATA)
  write.csv(hh2, "C:/Users/chris_hoover/Documents/RemaisWork/SurfaceH2O/Ecuador/IntraVillage/hh_xy_merge.csv",
            row.names = FALSE)
  
#Check out distribution of cases within households ############
  #Calculate incidence in cases/1000 person weeks on the hh level
  hh2$d_inc = (hh2$inc_diarrhea / hh2$n_drisk2)

#Check out distribution of incidence values  
  plot(hist(hh2$d_inc))                        
  plot(hist(log(hh2$d_inc+1)))
  plot(hist(log(hh2$d_inc[hh2$d_inc > 0]+1)))
  
#get household total number of cases  
  hh2_totals<-as.numeric()
  for(i in 1:length(hh2s)){
    hh2_totals[i] = sum(hh2$inc_diarrhea[hh2$HH == hh2s[i]])
  }
  plot(hist(hh2_totals)) 
  plot(x = hh2s, y = hh2_totals)
  
#A handful of houses seem to be way more infected than others, let's check them out 
  hh2_hi<-subset(hh2, HH == c(hh2s[which(hh2_totals >= 150)]))
  #All three houses are in village three and have high hh population that appears to be quite variable as
  #well; maybe something like a hostel that has highly transient population?
  
#What about distribution without those three hhs?
  plot(hist(hh2_totals[hh2_totals <= 150]))
  plot(hist(log(hh2_totals[hh2_totals <= 150]+1)))
  
#Scatterplot matrix of random variables (week, hh, village, x/y coord)
  pairs(hh2[,c(1,3:5,36:38)])
  
#Scatterplot matrix of age and gender variables
  pairs(hh2[,c(7:13,38)], cex=0.6, col ='blue')
  
#Scatterplot matrix of WASH and SES variables
  pairs(hh2[,c(20:29,38)], cex=0.6, col ='red')