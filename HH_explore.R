data<-read.csv("C:/Users/chris_hoover/Documents/RemaisWork/SurfaceH2O/Ecuador/IntraVillage/hh_xy_merge.csv")  

vils<-sort(unique(data$village))
weeks<-sort(unique(data$week))
hhs<-sort(unique(data$HH))

#Plot number of infected households and individuals in each village across time ##########
infhh = matrix(ncol = length(vils), nrow = length(weeks))
cases = matrix(ncol = length(vils), nrow = length(weeks))

for(j in 1:length(vils)){
  print(j)
  for(i in 1:length(weeks)){
    infhh[i,j] = sum(!is.na(data$HH[data$village == vils[j] & data$week == weeks[i] & data$inc_diarrhea > 0]))
    cases[i,j] = sum(data$inc_diarrhea[data$village == vils[j] & data$week == weeks[i]])
  }
}

for(i in 1:length(vils)){
  mypath <- file.path("C:","Users","chris_hoover","Documents","RemaisWork","SurfaceH2o","Ecuador",
                      "IntraVillage","intravillage_Ecuador","vil_level_infection",
                      paste("ts_vil", vils[i], ".jpg", sep = ""))
  
  jpeg(file=mypath, width = 675, height = 550, units = "px")
  mytitle = paste("village", vils[i])
  
  plot(x = weeks, y = infhh[,i], xlim = c(0, max(weeks+10)), ylim = c(0,max(cases+2)), 
       type = 'l', lwd = 2, ylab = 'hh level infection', xlab = 'time (weeks)', main = paste('Village ', vils[i]))
  lines(x = weeks, y = cases[,i], lwd = 1.5, lty = 2)
  legend('topright', cex = 0.8, lwd = c(2,1.5), lty = c(1,2), legend = c('Infected households', 'Infected individuals'))
  
  dev.off()
  
  
}

#Check out distribution of cases within households ############
  #Check out distribution of incidence values  
    hist(data$d_inc)                        
    hist(log(data$d_inc+1))
    hist(log(data$d_inc[data$d_inc > 0]+1))

#get household total number of cases  
  hh_totals<-data.frame('hhid' = hhs,
                        'cases' = 0)
  
  for(i in 1:nrow(hh_totals)){
    hh_totals[i,2] = sum(data$inc_diarrhea[data$HH == hh_totals[i,1]])
  }
  
    hist(hh_totals[,2], breaks = 20)
    plot(density(hh_totals[,2]))
    
  for(i in 1:nrow(hh_totals)){
    hh_totals[i,3] = data$village[data$HH == hh_totals[i,1]][1]
  }
    colnames(hh_totals)[3] = 'village'
    
  for(j in 1:length(vils)){
    mypath <- file.path("C:","Users","chris_hoover","Documents","RemaisWork","SurfaceH2o","Ecuador",
                        "IntraVillage","intravillage_Ecuador","vil_level_infection",
                        paste("hist_vil", vils[j], ".jpg", sep = ""))
    
    jpeg(file=mypath, width = 675, height = 550, units = "px")
    mytitle = paste("village", vils[j])
    hist(hh_totals$cases[hh_totals$village == vils[j]], breaks = 20, main = paste('village', vils[j]))
    dev.off()
  }  
    

