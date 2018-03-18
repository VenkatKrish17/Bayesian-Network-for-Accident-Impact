
install.packages('mice')
library(mice)
vehicle<-read.csv('../vehicle_safety_NASS2010_2000_2012.csv',header=T,na.strings=c(""," ","NA"))

#remvoing the Na's in Target variable
print(nrow(vehicle[is.na(vehicle$OA_MAIS),]))
vehicle<-vehicle[!is.na(vehicle$OA_MAIS),]

#imputing curb weight value wiht mean for each type of vehicle category
aggregatedmeans<-aggregate(GV_CURBWGT~GV_WGTCDTR, data=vehicle, mean)
for (row in 1:nrow(vehicle)){
  if(is.na(vehicle[row,'GV_CURBWGT'])){
   vehiclecat<-toString(vehicle[row,'GV_WGTCDTR'])
   #print(vehiclecat)
  #print(aggregatedmeans[aggregatedmeans$GV_WGTCDTR==vehiclecat,]$GV_CURBWGT)
    vehicle[row,'GV_CURBWGT']<-
  aggregatedmeans[aggregatedmeans$GV_WGTCDTR==vehiclecat,]$GV_CURBWGT
  
  }
}
summary(vehicle$GV_CURBWGT)
#imputing wheel base value with mean for each type of vehicle category
aggregatedwheelmeans<-aggregate(VE_WHEELBAS~GV_WGTCDTR, data=vehicle, mean)
for (row in 1:nrow(vehicle)){
  if(is.na(vehicle[row,'VE_WHEELBAS'])){
    vehiclecat<-toString(vehicle[row,'GV_WGTCDTR'])
    #print(vehiclecat)
    #print(aggregatedmeans[aggregatedmeans$GV_WGTCDTR==vehiclecat,]$GV_CURBWGT)
    vehicle[row,'VE_WHEELBAS']<-
      aggregatedwheelmeans[aggregatedwheelmeans$GV_WGTCDTR==vehiclecat,]$VE_WHEELBAS
    
  }
}
summary(vehicle$VE_WHEELBAS)
#imputing missing value for GV-FOOTPRINT & VE_ORIGAVTW

summary(vehicle$GV_FOOTPRINT)
lmodel<-lm(formula=as.formula('GV_FOOTPRINT~VE_WHEELBAS'),data=vehicle)

print(lmodel)

count<<-0
for (row in 1:nrow(vehicle)){
  
  if(is.na(vehicle[row,'GV_FOOTPRINT'])){
    print(predict(lmodel,newdata=vehicle[row,] ))
    vehicle[row,'GV_FOOTPRINT']<-
      predict(lmodel,newdata =vehicle[row,] )
    count<<-count+1
  }
}
print(count)


#imputing missing value for GV-FOOTPRINT & VE_ORIGAVTW
summary(vehicle$VE_ORIGAVTW)
lmodel2<-lm(formula='VE_ORIGAVTW~GV_FOOTPRINT',data=vehicle)

print(lmodel2)

count<<-0
for (row1 in 1:nrow(vehicle)){
  
  if(is.na(vehicle[row1,'VE_ORIGAVTW'])){
    print(predict(lmodel2,newdata =vehicle[row1,] ))
    vehicle[row1,'VE_ORIGAVTW']<-
      predict(lmodel2,newdata =vehicle[row1,] )
    count<<-count+1
  }
}
print(count)
#find median angle for impact zone and fill VE_PDOF_TR

#removing empty records for Delta v, direction and angle.

#nrow(vehicle[is.na(vehicle$GV_DVLAT) & is.na(vehicle$VE_PDOF_TR) & !is.na(vehicle$VE_GAD1),])
#ignoring 579 records, where lat, long, angle, direction is empty
nrow(vehicle[is.na(vehicle$GV_DVLAT) & is.na(vehicle$VE_GAD1) & is.na(vehicle$VE_PDOF_TR),])
vehicle<-vehicle[!(is.na(vehicle$GV_DVLAT) & is.na(vehicle$VE_GAD1) & is.na(vehicle$VE_PDOF_TR)),]


table(vehicle[vehicle$VE_GAD1=='Left',]$VE_PDOF_TR)

#filling up median angle with direction of impact
aggregateangles<-aggregate(VE_PDOF_TR~VE_GAD1,data=vehicle,median)
print(aggregateangles) 

for (row in 1:nrow(vehicle)){
  if(is.na(vehicle[row,'VE_PDOF_TR']) & !is.na(vehicle[row,'VE_GAD1'])){
    dircat<-toString(vehicle[row,'VE_GAD1'])
    print(dircat)
    #print(aggregatedmeans[aggregatedmeans$GV_WGTCDTR==vehiclecat,]$GV_CURBWGT)
    vehicle[row,'VE_PDOF_TR']<-
      aggregateangles[aggregateangles$VE_GAD1==dircat,]$VE_PDOF_TR
    
  }
}


#filling up direction of impact with existing angle.. 1 record only 
min_max_angle_left=seq(0,90)
min_max_angle_front=seq(91,180)
min_max_angle_right=seq(181,270)
min_max_angle_rear=seq(271,360)

for (row in 1:nrow(vehicle)){
  if( is.na(vehicle[row,'VE_GAD1'])){
    if(vehicle[row,'VE_PDOF_TR'] %in% min_max_angle_front){
      vehicle[row,'VE_GAD1']<-'Front'
    }
    else if(vehicle[row,'VE_PDOF_TR'] %in% min_max_angle_left){
      vehicle[row,'VE_GAD1']<-'Left'
    }
    else if(vehicle[row,'VE_PDOF_TR'] %in% min_max_angle_right){
      vehicle[row,'VE_GAD1']<-'Right'
    }
    else if(vehicle[row,'VE_PDOF_TR'] %in% min_max_angle_rear){
      vehicle[row,'VE_GAD1']<-'Rear'
    }
  }
}


#imputing height 


men_mean_height=mean(vehicle[vehicle$OA_SEX=='Male',]$OA_HEIGHT, na.rm=TRUE)
print(men_mean_height)
women_mean_height=mean(vehicle[vehicle$OA_SEX=='Female',]$OA_HEIGHT, na.rm=TRUE)
print(women_mean_height)
nrow(vehicle[is.na(vehicle$OA_HEIGHT) & is.na(vehicle$OA_SEX),])
count<<-0
for (row in 1:nrow(vehicle)){

  if(is.na(vehicle[row,]$OA_HEIGHT)){
  if(!is.na(vehicle[row,]$OA_SEX) & vehicle[row,'OA_SEX']=='Male'){
    vehicle[row,]$OA_HEIGHT<-men_mean_height
  }
    else if(!is.na(vehicle[row,]$OA_SEX) & vehicle[row,'OA_SEX']=='Female'){
      vehicle[row,]$OA_HEIGHT<-women_mean_height
    }
   }
}
overall_mean_height<-mean(vehicle$OA_HEIGHT,na.rm = TRUE)
for (row in 1:nrow(vehicle)){
   if(is.na(vehicle[row,]$OA_HEIGHT)){
    vehicle[row,]$OA_HEIGHT<-overall_mean_height
  }
}

new_male_mean<-mean(vehicle[vehicle$OA_SEX=='Male',]$OA_HEIGHT, na.rm = TRUE)
print(new_male_mean)
new_female_mean<-mean(vehicle[vehicle$OA_SEX=='Female',]$OA_HEIGHT, na.rm = TRUE)
print(new_female_mean)

for (row in  1:nrow(vehicle)){
  if(is.na(vehicle[row,]$OA_SEX)){
    if(vehicle[row,]$OA_HEIGHT<=((new_male_mean+new_female_mean)/2)){
      vehicle[row,'OA_SEX']<-'Female'
    }
    else if(vehicle[row,]$OA_HEIGHT>((new_male_mean+new_female_mean)/2)){
      vehicle[row,'OA_SEX']<-'Male'
    }
  }
}

mean_weight<-mean(vehicle$OA_WEIGHT,na.rm = TRUE)
print(mean_weight)
for(row in 1:nrow(vehicle)){
  if(is.na(vehicle[row,]$OA_WEIGHT)){
    vehicle[row,]$OA_WEIGHT<-mean_weight
  }
}

#mean age
mean_age<-round(mean(vehicle$OA_AGE,na.rm = TRUE))
print(mean_age)
for(row in 1:nrow(vehicle)){
  if(is.na(vehicle[row,]$OA_AGE)){
    vehicle[row,]$OA_AGE<-mean_age
  }
}

#mean_lane
mean_lane<-round(mean(vehicle$GV_LANES,na.rm = TRUE))
print(mean_lane)
for(row in 1:nrow(vehicle)){
  if(is.na(vehicle[row,]$GV_LANES)){
    vehicle[row,]$GV_LANES<-mean_lane
  }
}
#mean_lane
mean_limit<-round(mean(vehicle$GV_SPLIMIT,na.rm = TRUE))
print(mean_limit)
for(row in 1:nrow(vehicle)){
  if(is.na(vehicle[row,]$GV_SPLIMIT)){
    vehicle[row,]$GV_SPLIMIT<-mean_limit
  }
}

nrow(vehicle[is.na(vehicle$GV_ENERGY) & is.na(vehicle$GV_OTVEHWGT) &is.na(vehicle$VE_GAD1),])



#md.pattern(vehicle)
imputeddata<-mice(data = vehicle, m=5, maxit=20, method = 'pmm')
completed<-complete(imputeddata,1)
write.csv(completed,file="../cleanseddata.csv")
summary(completed)


