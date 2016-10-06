require(rgdal)
require(maptools)
require(plyr)

setwd("C:/Users/chris_hoover//Documents/RemaisWork/SurfaceH2O/Ecuador/Data/Final/Case data")
dat<-read.csv("ActiveSurveillanceDataset_20120303.csv")

#Initial data cleaning #########
  #Convert from factor dates to dates class
    dat$Control_Date<-as.Date(dat$Control_Date, format = '%m/%d/%Y')
    dat$birthdate<-as.Date(dat$birthdate, format = '%m/%d/%Y')
    dat$DeathDate<-as.Date(dat$DeathDate, format = '%m/%d/%Y')
    
  #Make our own age variable because old one has negative values  
    dat$age2 = as.numeric(dat$Control_Date - dat$birthdate)/365
    
  varbs<-colnames(dat) #All the variables in the data set
  dates<-as.character(unique(dat$Control_Date)) #All the control dates in the dataset
  houses<-unique(dat$ID_casa)
    houses<-houses[!is.na(houses)] #all the household IDs in the dataset
  weeks<-unique(dat$WEEK, na.rm=T)
    weeks<-weeks[!is.na(weeks)] #all the week codes in the dataset
    
#Make dataset of individuals who died ##########
    dead<-subset(dat, DeathDate != "2099-09-09", 
                 select = c(ID_individuo, Control_Date, birthdate, DeathDate, DIARRHEA))
    dead$age = as.numeric(dead$DeathDate - dead$birthdate)/365

#Create HH-level dataframe #############

newdf<-data.frame(HH = rep(sort(houses), times = length(weeks)),
                  week = rep(sort(weeks), each = length(houses))) #new df with all the unique HH/week pairs

for(i in 1:nrow(newdf)){
  print(c(i))
  #Village membership of household
    newdf[i,3] = dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2], 
                     which('comunidad' == varbs)[[1]]][1]
  #mean age in each household per week
    newdf[i,4] = mean(dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2], 
                          which('age2' == varbs)[[1]]], na.rm = TRUE)
  #standard deviation of age in each household per week
    newdf[i,5] = sd(dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2], 
                          which('age2' == varbs)[[1]]], na.rm = TRUE)
  #number of people in the household that week
    newdf[i,6] = sum(!is.na(dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2], 3]))
  #number of people in age category 4 (>13)
    newdf[i,7] = sum(!is.na(dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2] & dat$age2 >= 13, 1]))
  #number of people in age category 3 (5-13)
    newdf[i,8] = sum(!is.na(dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2] & dat$age2 < 13 & dat$age2 >= 5, 1]))
  #number of people in age category 2 (1-5)
    newdf[i,9] = sum(!is.na(dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2] & dat$age2 < 5 & dat$age2 >= 1, 1]))
  #number of people in age category 1 (<1)
    newdf[i,10] = sum(!is.na(dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2] & dat$age2 < 1, 1]))
  #number of gender=1
    newdf[i,11] = sum(!is.na(dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2] & dat$Gender == 1, 1]))
  #number of gender=2
    newdf[i,12] = sum(!is.na(dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2] & dat$Gender == 2, 1]))  
  #number of people with diarrhea = 1 (i.e. both ongoing and incident)
    newdf[i,13] = sum(!is.na(dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2] & dat$DIARRHEA == 1, 1]))
  #number of people with *incident* diarrhea
    newdf[i,14] = sum(!is.na(dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2] & dat$diarrhea_inc == 1, 1]))
  #number of people at risk (i.e. without diarrhea)
    newdf[i,15] = sum(!is.na(dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2] & dat$DIARRHEA != 1, 1]))
  #number of people with fever = 1 (i.e. both ongoing and incident)
    newdf[i,16] = sum(!is.na(dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2] & dat$FEVER == 1, 1]))
  #number of people with *fever* diarrhea
    newdf[i,17] = sum(!is.na(dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2] & dat$fever_inc == 1, 1]))
  #number of people at risk (i.e. without fever)
    newdf[i,18] = sum(!is.na(dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2] & dat$FEVER != 1, 1]))
  #Access to improved sanitation
    newdf[i,19] = dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2], 
                      which('impr_sani' == varbs)[[1]]][1]
  #SLI fraction (index of ownership); SES variable  
    newdf[i,20] = dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2], 
                      which('SLI_fract' == varbs)[[1]]][1]
  #Job stability (binary on a hh level)
    newdf[i,21] = dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2], 
                      which('job_stable' == varbs)[[1]]][1]
  #Max education level for the hh
    newdf[i,22] = dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2], 
                      which('edcat' == varbs)[[1]]][1]
  #metric for construction quality of the house based on materials used
    newdf[i,23] = dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2], 
                      which('const_score' == varbs)[[1]]][1]
  #Passing time degree, measure of social connectivity at community level
    newdf[i,24] = dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2], 
                      which('PTDegree' == varbs)[[1]]][1]
  #Food sharing degree, another community-level metric of social connectivity
    newdf[i,25] = dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2], 
                      which('CDegree' == varbs)[[1]]][1]
  #community-level metric of sanitation
    newdf[i,26] = dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2], 
                      which('com_obssani' == varbs)[[1]]][1]
  #community-level metric of access to improved water source
    newdf[i,27] = dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2], 
                      which('com_watersource' == varbs)[[1]]][1]
  #community-level metric of water treatment
    newdf[i,28] = dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2], 
                      which('com_watertreat' == varbs)[[1]]][1]
  #number of people at risk (i.e. without diarrhea) ***Including people who just contracted it that week
    newdf[i,29] = (sum(!is.na(dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2] & dat$DIARRHEA != 1, 1])) +
                   sum(!is.na(dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2] & dat$diarrhea_inc == 1, 1])))

  #number of people at risk (i.e. without fever) ***Including people who just contracted it that week
    newdf[i,30] = (sum(!is.na(dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2] & dat$FEVER != 1, 1])) +
                   sum(!is.na(dat[dat$ID_casa == newdf[i,1] & dat$WEEK == newdf[i,2] & dat$fever_inc == 1, 1])))
    }
      
#Code variable (i.e. column) names
  colnames(newdf)[c(3:30)] = c("village", "mean_age", "sd_age", 
                             "n_people", "n_agecat4", "n_agecat3", "n_agecat2", "n_agecat1",
                             "n_gender1", "n_gender2", "n_diarrhea", "inc_diarrhea", "n_drisk",
                             "n_fever", "inc_fever", "n_frisk", "impr_sani", "SLI_fract",
                             "job_stable", "edcat", "const_score", "PTDegree", "CDegree",
                             "com_obssani", "com_watersource", "com_watertreat",
                             "n_drisk2", "n_frisk2")  
  
#Check to see how two different measures of at risk population compare
  plot(x = newdf$n_drisk, y = newdf$n_drisk2, xlab = "Incident cases NOT included",
       ylab = "Incident cases included", main = "Calculation of at risk population per week")
  
write.csv(newdf, "C:/Users/chris_hoover//Documents/RemaisWork/SurfaceH2O/Ecuador/IntraVillage/hh_condense.csv")  

#Merge with gis data ##########

data<-read.csv("C:/Users/chris_hoover/Documents/RemaisWork/SurfaceH2O/Ecuador/IntraVillage/hh_condense.csv")  

#Get rid of hh-weeks where nobody was surveyed
hh<-subset(data, n_people != 0)
hhs<-unique(hh$HH)

#merge with household spatial data
gis<-read.csv("C:/Users/chris_hoover/Documents/RemaisWork/SurfaceH2O/Ecuador/IntraVillage/GIS_ID_Casa.csv")  
gishhs<-unique(gis$ID_Casa)
giscols<-colnames(gis)

#merge spatial data with surveillance data; looks like we lose 6125 records
hh2<-merge(x=hh, y=gis, by.x="HH", by.y="ID_Casa")

#Drop unwanted variables from spatial data frame (character vector below is ones to keep)
hh2<-hh2[, !names(hh2) %in% giscols[!giscols %in% c("Com", "Numero", "Ocupada", "Destruida", "POINT_X", "POINT_Y")]]
hh2s<-unique(hh2$HH)

#Fill village id NAs  
hh2$village[is.na(hh2$village)]<-gis$Com[match(hh2$HH,gis$ID_Casa)][which(is.na(hh2$village))]      

#vector of households in surveillance dataset that did not have GIS data
droppedhhs<-hhs[-which(hhs %in% hh2s)]

#Calculate incidence in cases/1000 person weeks on the hh level
hh2$d_inc = (hh2$inc_diarrhea / hh2$n_drisk2)

#Export final dataset (NOTE: THIS IS SENSITIVE DATA)
write.csv(hh2, "C:/Users/chris_hoover/Documents/RemaisWork/SurfaceH2O/Ecuador/IntraVillage/hh_xy_merge.csv",
          row.names = FALSE)


#Scatterplot matrix of random variables (week, hh, village, x/y coord)
pairs(hh2[,c(1,3:5,36:38)], cex=0.6, col='grey50')

#Scatterplot matrix of age and gender variables
pairs(hh2[,c(7:13,38)], cex=0.6, col ='blue')

#Scatterplot matrix of WASH and SES variables
pairs(hh2[,c(20:29,38)], cex=0.6, col ='red')

#Make dataset to import into ArcGIS
h<-unique(hh2$HH)

fin<-data.frame(hh = h,
                com = 0,
                n_surv = 0,
                n_total = 0,
                d_total = 0,
                d_per_n = 0,
                f_total = 0,
                point_x = 0,
                point_y = 0)

for(i in 1:nrow(fin)){
  fin[i,2] = gis$Com[gis$ID_Casa == fin$hh[i]][1]
  fin[i,3] = sum(!is.na(hh2$HH[hh2$HH == fin$hh[i]]))
  fin[i,4] = sum(hh2$n_people[hh2$HH == fin$hh[i]])
  fin[i,5] = sum(hh2$inc_diarrhea[hh2$HH == fin$hh[i]])
  fin[i,6] = sum(hh2$inc_diarrhea[hh2$HH == fin$hh[i]]) / 
    sum(hh2$n_people[hh2$HH == fin$hh[i]])
  fin[i,7] = sum(hh2$inc_fever[hh2$HH == fin$hh[i]])
  fin[i,8] = gis$POINT_X[gis$ID_Casa == fin$hh[i]][1]
  fin[i,9] = gis$POINT_Y[gis$ID_Casa == fin$hh[i]][1]
}

write.csv(fin, "C:/Users/chris_hoover/Documents/RemaisWork/SurfaceH2O/Ecuador/IntraVillage/hh_xy_only.csv")

#Looks like there are some housholds that have multiple IDs in the dataset ###################
#Same XY coordinates, but different HH-IDs
dups<-duplicated(fin[,c(8,9)]) #Find duplicated xy coords

dupsy<-duplicated(fin[,9])     #check if can just use y coords, yes (only duplicated xy coord have duplicated y coords)
ys<-fin[dupsy,9]  
duphhs<-fin$hh[fin$point_y %in% ys]
fin2<-subset(fin, hh %in% duphhs)

#Save this data frame to show to people who know more about the data
write.csv(fin2, "C:/Users/chris_hoover/Documents/RemaisWork/SurfaceH2O/Ecuador/IntraVillage/hh_xy_duplicates.csv", row.names=FALSE)

#condense duplicated hhs into the lowest hh id    
dupids<-as.numeric()
dup1<-as.numeric()
dup2<-as.numeric()
dup3<-as.numeric()

for(i in 1:length(ys)){
  print(fin2$hh[fin2$point_y == ys[i]])            #Household groups that are duplicated
  dup1[i] = fin2$hh[fin2$point_y == ys[i]][1]      #hh id of first duplicate
  dup2[i] = fin2$hh[fin2$point_y == ys[i]][2]      #hh id of first duplicate
  dup3[i] = fin2$hh[fin2$point_y == ys[i]][3]      #hh id of first duplicate
  dupids[i] = sum(fin2$hh[fin2$point_y == ys[i]])  #hh id that would result if two data frames were added
}

med1<-subset(fin, !hh %in% duphhs)  
med2<-data.frame(hh = 0,
                 com = 0,
                 n_surv = 0,
                 n_total = 0,
                 d_total = 0,
                 d_per_n = 0,
                 f_total = 0,
                 point_x = 0,
                 point_y = 0)

for(i in 1:length(dup1)){
  med2[i,1] = dup1[i]
  med2[i,2] = gis$Com[gis$ID_Casa == dup1[i]][1]
  med2[i,3] = sum(!is.na(hh2$HH[hh2$HH == dup1[i]]),
                  !is.na(hh2$HH[hh2$HH == dup2[i]]),
                  !is.na(hh2$HH[hh2$HH == dup3[i]]))
  med2[i,4] = sum(hh2$n_people[hh2$HH == dup1[i]],
                  hh2$n_people[hh2$HH == dup2[i]],
                  hh2$n_people[hh2$HH == dup3[i]],
                  na.rm = TRUE)
  med2[i,5] = sum(hh2$inc_diarrhea[hh2$HH == dup1[i]],
                  hh2$inc_diarrhea[hh2$HH == dup2[i]],
                  hh2$inc_diarrhea[hh2$HH == dup3[i]],
                  na.rm = TRUE)
  med2[i,6] = sum(hh2$inc_diarrhea[hh2$HH == dup1[i]],
                  hh2$inc_diarrhea[hh2$HH == dup2[i]],
                  hh2$inc_diarrhea[hh2$HH == dup3[i]],
                  na.rm = TRUE) / 
    sum(hh2$n_people[hh2$HH == dup1[i]],
        hh2$n_people[hh2$HH == dup2[i]],
        hh2$n_people[hh2$HH == dup3[i]],
        na.rm = TRUE)
  med2[i,7] = sum(hh2$inc_fever[hh2$HH == dup1[i]],
                  hh2$inc_fever[hh2$HH == dup2[i]],
                  hh2$inc_fever[hh2$HH == dup3[i]],
                  na.rm = TRUE)
  med2[i,8] = gis$POINT_X[gis$ID_Casa == dup1[i]][1]
  med2[i,9] = gis$POINT_Y[gis$ID_Casa == dup1[i]][1]
}

med2<-med2[-6,]

finfin<-rbind(med1, med2)

write.csv(finfin, "C:/Users/chris_hoover/Documents/RemaisWork/SurfaceH2O/Ecuador/IntraVillage/hh_xy_only_dups_rmv.csv")

#Merge data from these same households in the temporal dataset as well NEED TO FIGURE OUT BEST WAY TO DO THIS################
dupdf<-subset(hh2, HH %in% duphhs)

#Get summary data for each community on the hh level ###################    
coms<-sort(unique(fin$com))
coms.sum<-data.frame(vid = 0,
                     n_hh = 0,
                     n_hhs = 0,
                     n_ppls = 0,
                     n_d = 0)
for(i in 1:length(coms)){
  coms.sum[i,1] = coms[i]
  coms.sum[i,2] = sum(!is.na(fin$hh[fin$com == coms[i]]))
  coms.sum[i,3] = sum(fin$n_surv[fin$com == coms[i]])
  coms.sum[i,4] = sum(fin$n_total[fin$com == coms[i]])
  coms.sum[i,5] = sum(fin$d_total[fin$com == coms[i]])
}

write.csv(coms.sum, "C:/Users/chris_hoover/Documents/RemaisWork/SurfaceH2O/Ecuador/IntraVillage/com_hh_sum.csv",
          row.names = FALSE)


#Plot time series of HH cases over time in each village
vils<-unique(hh2$village)

for(j in 1:length(vils)){
  print(j)
  hs = unique(hh2$HH[hh2$village == vils[j] & hh2$inc_diarrhea > 0]) #subset to all households within the village that had a case
  plot(x = hh2$week[hh2$HH == hs[1]], y = hh2$inc_diarrhea[hh2$HH == hs[1]], xlim = c(0,185), ylim = c(0,4.5),
       main = paste("Village ", vils[j], sep = ''), xlab = "Time (weeks)", ylab = "# Cases", pch = 16) #initialize the plot
  
  for(i in 2:length(hs)){
    points(x = hh2$week[hh2$HH == hs[i]], y = hh2$inc_diarrhea[hh2$HH == hs[i]], col = i, pch = 16) #add other hh time series
  }
}