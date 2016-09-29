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