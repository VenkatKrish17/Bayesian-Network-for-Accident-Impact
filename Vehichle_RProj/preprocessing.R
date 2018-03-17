


#imputing curb weight value wiht mean for each type of vehicle category
aggregatedmeans<-aggregate(GV_CURBWGT~GV_WGTCDTR, data=vehicle_safety_NASS2010_2000_2012, mean)
for (row in 1:nrow(vehicle_safety_NASS2010_2000_2012)){
  if(is.na(vehicle_safety_NASS2010_2000_2012[row,'GV_CURBWGT'])){
   vehiclecat<-toString(vehicle_safety_NASS2010_2000_2012[row,'GV_WGTCDTR'])
   #print(vehiclecat)
  #print(aggregatedmeans[aggregatedmeans$GV_WGTCDTR==vehiclecat,]$GV_CURBWGT)
    vehicle_safety_NASS2010_2000_2012[row,'GV_CURBWGT']<-
  aggregatedmeans[aggregatedmeans$GV_WGTCDTR==vehiclecat,]$GV_CURBWGT
  
  }
}

#imputing wheel base value with mean for each type of vehicle category
aggregatedwheelmeans<-aggregate(VE_WHEELBAS~GV_WGTCDTR, data=vehicle_safety_NASS2010_2000_2012, mean)
for (row in 1:nrow(vehicle_safety_NASS2010_2000_2012)){
  if(is.na(vehicle_safety_NASS2010_2000_2012[row,'VE_WHEELBAS'])){
    vehiclecat<-toString(vehicle_safety_NASS2010_2000_2012[row,'GV_WGTCDTR'])
    #print(vehiclecat)
    #print(aggregatedmeans[aggregatedmeans$GV_WGTCDTR==vehiclecat,]$GV_CURBWGT)
    vehicle_safety_NASS2010_2000_2012[row,'VE_WHEELBAS']<-
      aggregatedwheelmeans[aggregatedwheelmeans$GV_WGTCDTR==vehiclecat,]$VE_WHEELBAS
    
  }
}

#imputing missing value for GV-FOOTPRINT & VE_ORIGAVTW
lmodel<-lm(formula=as.formula('GV_FOOTPRINT~VE_WHEELBAS'),data=vehicle_safety_NASS2010_2000_2012)

print(lmodel)

count<<-0
for (row in 1:nrow(vehicle_safety_NASS2010_2000_2012)){
  
  if(is.na(vehicle_safety_NASS2010_2000_2012[row,'GV_FOOTPRINT'])){
    print(predict(lmodel,newdata =vehicle_safety_NASS2010_2000_2012[row,'VE_WHEELBAS'] ))
    vehicle_safety_NASS2010_2000_2012[row,'GV_FOOTPRINT']<-
      predict(lmodel,newdata =vehicle_safety_NASS2010_2000_2012[row,'VE_WHEELBAS'] )
    count<<-count+1
  }
}
print(count)


#imputing missing value for GV-FOOTPRINT & VE_ORIGAVTW
lmodel2<-lm(formula=as.formula('VE_ORIGAVTW~GV_FOOTPRINT'),data=vehicle_safety_NASS2010_2000_2012)

print(lmodel2)

count<<-0
for (row in 1:nrow(vehicle_safety_NASS2010_2000_2012)){
  
  if(is.na(vehicle_safety_NASS2010_2000_2012[row,'VE_ORIGAVTW'])){
    print(predict(lmodel2,newdata =vehicle_safety_NASS2010_2000_2012[row,'GV_FOOTPRINT'] ))
    vehicle_safety_NASS2010_2000_2012[row,'VE_ORIGAVTW']<-
      predict(lmodel2,newdata =vehicle_safety_NASS2010_2000_2012[row,'GV_FOOTPRINT'] )
    count<<-count+1
  }
}
print(count)
#find median angle for impact zone and fill VE_PDOF_TR

#removing empty records for Delta v, direction and angle.
nrow(vehicle_safety_NASS2010_2000_2012[!(is.na(vehicle_safety_NASS2010_2000_2012$GV_DVLAT) & is.na(vehicle_safety_NASS2010_2000_2012$VE_GAD1) & is.na(vehicle_safety_NASS2010_2000_2012$VE_PDOF_TR)),])
vehicle_safety_NASS2010_2000_2012<-vehicle_safety_NASS2010_2000_2012[!(is.na(vehicle_safety_NASS2010_2000_2012$GV_DVLAT) & is.na(vehicle_safety_NASS2010_2000_2012$VE_GAD1) & is.na(vehicle_safety_NASS2010_2000_2012$VE_PDOF_TR)),]


table(vehicle_safety_NASS2010_2000_2012[vehicle_safety_NASS2010_2000_2012$VE_GAD1=='Left',]$VE_PDOF_TR)

#filling up median angle with direction of impact
aggregateangles<-aggregate(VE_PDOF_TR~VE_GAD1,data=vehicle_safety_NASS2010_2000_2012,median)
print(aggregateangles) 

for (row in 1:nrow(vehicle_safety_NASS2010_2000_2012)){
  if(is.na(vehicle_safety_NASS2010_2000_2012[row,'VE_PDOF_TR']) & !is.na(vehicle_safety_NASS2010_2000_2012[row,'VE_GAD1'])){
    dircat<-toString(vehicle_safety_NASS2010_2000_2012[row,'VE_GAD1'])
    print(dircat)
    #print(aggregatedmeans[aggregatedmeans$GV_WGTCDTR==vehiclecat,]$GV_CURBWGT)
    vehicle_safety_NASS2010_2000_2012[row,'VE_PDOF_TR']<-
      aggregateangles[aggregateangles$VE_GAD1==dircat,]$VE_PDOF_TR
    
  }
}


#filling up direction of impact with existing angle.. 1 record only 
min_max_angle_left=seq(0,90)
min_max_angle_front=seq(91,180)
min_max_angle_right=seq(181,270)
min_max_angle_rear=seq(271,360)

for (row in 1:nrow(vehicle_safety_NASS2010_2000_2012)){
  if( is.na(vehicle_safety_NASS2010_2000_2012[row,'VE_GAD1'])){
    if(vehicle_safety_NASS2010_2000_2012[row,'VE_PDOF_TR'] %in% min_max_angle_front){
      vehicle_safety_NASS2010_2000_2012[row,'VE_GAD1']<-'Front'
    }
    else if(vehicle_safety_NASS2010_2000_2012[row,'VE_PDOF_TR'] %in% min_max_angle_left){
      vehicle_safety_NASS2010_2000_2012[row,'VE_GAD1']<-'Left'
    }
    else if(vehicle_safety_NASS2010_2000_2012[row,'VE_PDOF_TR'] %in% min_max_angle_right){
      vehicle_safety_NASS2010_2000_2012[row,'VE_GAD1']<-'Right'
    }
    else{
      vehicle_safety_NASS2010_2000_2012[row,'VE_GAD1']<-'Rear'
    }
  }
}






summary(vehicle_safety_NASS2010_2000_2012$GV_CURBWGT)
newaggregatedmeans<-aggregate(GV_CURBWGT~GV_WGTCDTR, data=vehicle_safety_NASS2010_2000_2012, mean)
print(table(is.na(vehicle_safety_NASS2010_2000_2012$GV_CURBWGT)))
summary(vehicle_safety_NASS2010_2000_2012)
nrow(vehicle_safety_NASS2010_2000_2012[is.na(vehicle_safety_NASS2010_2000_2012$VE_GAD1),])


