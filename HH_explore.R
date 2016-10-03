require(rgdal)
require(maptools)
require(plyr)

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
  
#vector of households in surveillance dataset that did not have GIS data
  droppedhhs<-hhs[-which(hhs %in% hh2s)]
  
#Export final dataset (NOTE: THIS IS SENSITIVE DATA)
  write.csv(hh2, "C:/Users/chris_hoover/Documents/RemaisWork/SurfaceH2O/Ecuador/IntraVillage/hh_xy_merge.csv",
            row.names = FALSE)
  
#Check out distribution of cases within households ############
  #Calculate incidence in cases/1000 person weeks on the hh level
  hh2$d_inc = (hh2$inc_diarrhea / hh2$n_drisk2)

#Check out distribution of incidence values  
  hist(hh2$d_inc)                        
  hist(log(hh2$d_inc+1))
  hist(log(hh2$d_inc[hh2$d_inc > 0]+1))
  
#get household total number of cases  
  hh2_totals<-as.numeric()
  for(i in 1:length(hh2s)){
    hh2_totals[i] = sum(hh2$inc_diarrhea[hh2$HH == hh2s[i]])
  }
  hist(hh2_totals)
  plot(x = hh2s, y = hh2_totals)
  
#A couple of houses seem to be way more infected than others, let's check them out 
  hh2_hi<-subset(hh2, HH == c(hh2s[which(hh2_totals >= 20)]))
  #both houses have 10+ people, one in Vil17 one in Vil19
  
#What about distribution without those three hhs?
  hist(hh2_totals[hh2_totals <= 20])
  hist(log(hh2_totals[hh2_totals <= 20]+1))
  
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
  