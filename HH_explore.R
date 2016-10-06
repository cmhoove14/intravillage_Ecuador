data<-read.csv("C:/Users/chris_hoover/Documents/RemaisWork/SurfaceH2O/Ecuador/IntraVillage/hh_xy_merge.csv")  

vils<-sort(unique(data$village))
weeks<-sort(unique(data$week))

#Plot number of infected households in each village across time ##########
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
  plot(x = weeks, y = infhh[,i], xlim = c(0, max(weeks+10)), ylim = c(0,max(cases+2)), 
       type = 'l', lwd = 2, ylab = 'hh level infection', xlab = 'time (weeks)', main = paste('Village ', vils[i]))
  lines(x = weeks, y = cases[,i], lwd = 1.5, lty = 2)
  legend('topright', cex = 0.8, lwd = c(2,1.5), lty = c(1,2), legend = c('Infected households', 'Infected individuals'))
}