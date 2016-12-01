#############################################
#This Code is used to perform Normalization, 
#Principal Component Analysis
#to calculate
#Road Safety Index
############################################


#Reading the Accident Dataset, Accident points and the Traffic Volume
setwd("C:/Users/Raghu/Documents/Predictive Analytics")
accds<-read.csv('NYPD_Motor_Vehicle_Collisions.csv',header=T,sep=',')
ds<-read.csv('Capi.txt',header=T,sep=',')
vol<-read.csv('Volume.csv',header=T,sep=',')

library(plyr)
vol<-rename(vol, c("From"="Start", "To"="End"))

#Retrieving the Streetnames from the traffic volume dataset
q<-"select distinct(Streetname) from vol order by Streetname"
res3<- sqldf(q)

#Calculating the mean of the traffic for each hour for each street
k=0
for(i in 8: ncol(vol)){
  res4<-aggregate(vol[,i] ~ STREETNAME, vol, mean)
  res4[,2]<-round(res4[,2],digits=2)
  res4<- res4[order(res4[,1]),]
  res3<-cbind(res3, res4[,2])
  names(res3)[ncol(res3)]<-paste(k,":00",sep="")
  k=k+1
}



#Exploring the Accident Coordinates and rounding them to 4 decimals
names(ds[,1])<-"Latitude"
names(ds[,2])<-"Longitude"
ds$Location <- "x"
ds[,1]<-round(ds[,1],digits=4)
ds[,2]<-round(ds[,2],digits=4)
ds[,3]<-paste(ds[,1],",",ds[,2],sep="")
names(ds[,3])<-"Location"
ds <- ds[order(ds[,3]),]

#DOing the same for the location coordinates in the Accident Dataset
accds1<-accds[,-c(20:23,26:29)]
accds1<-accds1[!is.na(accds1$LONGITUDE),]
accds1[,5]<-round(accds1[,5],digits=4)
accds1[,6]<-round(accds1[,6],digits=4)
accds1[,7]<-paste(accds1[,5],",",accds1[,6],sep="")
accds1 <- accds1[order(accds1$LOCATION),]

#Subsetting the accident dataset based on accident coordinates
library(sqldf)
q<-"Select a.* from accds1 a  join ds b on a.Location = b.Location"
res1<- sqldf(q)

#Calculating the count of accidents per each accident location
q<-"Select Location, count(Location) from res1 group by Location"
res2<- sqldf(q)


#Transforming the Subset Dataset to our needs
out <- strsplit(as.character(res1$TIME),':') 
out<-do.call(rbind, out)
out<-data.frame(out)
res1<-cbind(res1,out[,1])
names(res1)[ncol(res1)]<-"Hour"
names(res1)[8]<-"Street_Name"

#Extracting the Street names and the time
q<-"Select * from res1 Order by Street_Name,Hour"
res1<-sqldf(q)

#Selecting the data based on the streetnames in the earlier dataset
q<-"Select * from res3 where STREETNAME in (Select distinct Street_Name from res1) "
streets<-sqldf(q)

#Transposing the streetnames
temp<-streets[,-1]
temp<-t(temp)
temp<-data.frame(temp)
col_temp=ncol(temp)
for(i in 1:24){
  temp[i,col_temp+1] <-i-1
  
}

names(temp)[ncol(temp)]<-"Hour"

#Selecting the Streetnames from the resultant dataset
q<-"Select STREETNAME from res3 where STREETNAME in (Select distinct Street_Name from res1) "
streetnames<-sqldf(q)


temp_base<-data.frame()
z=1
for(x in 1:nrow(streetnames)){
  for(y in 1:nrow(temp) ){
    temp_base[z,1]<-streetnames[x,1]
    temp_base[z,2]<-temp[y,ncol(temp)]
    temp_base[z,3]<-temp[y,x]
    z=z+1
  }
}

#Writing the resultant dataset to a temporary file
write.csv(res1,"temp.csv")
####################################################
#Reading the temporary file
res1<-read.csv('temp.csv',header=T,sep=',')
res1<-res1[,-c(1:7)]

#Merging the datasets based on Streetnames
q<-"Select a.*,b.V3 from res1 a inner join temp_base b on a.Street_name=b.V1 and a.Hour=b.V2 "
res8<-sqldf(q)

#Calculating the total injured
res8[,18]<-res8[,5]+res8[,7]+res8[,9]+res8[,11]
names(res8)[18]<-"Total Injured"

#Calculating the total killed
res8[,19]<-res8[,6]+res8[,8]+res8[,10]+res8[,12]
names(res8)[19]<-"Total Killed"

#Writing it to a temporary file
write.csv(res8,"temp1.csv")

#Calculating Overall mean volume, Total Accidents and no of accident points
res9<-res8[,c(13,15:19)]
Mean_Vol<-mean(res9$V3)
Tot_Acc<-nrow(res9)
colnames(res9)<-c("Cont_Vehicle","Veh_Type","Hour","Mean_Vol","Total_Injured","Total_Killed")
Acc_points_T<-sqldf("Select count(distinct(LOCATION)) from res8 where hour = 19")

#Subsetting the dataset at time 7 PM and calculating the above parameters
res9<-sqldf("Select * from res9 where hour = 19")
Mean_Vol_at_T<-mean(res9[,4])
Tot_Acc_at_T<-nrow(res9)

#Determining the Accident density
Acc_density_at_T<-round((Tot_Acc_at_T * Mean_Vol)/(Mean_Vol_at_T * Tot_Acc ),digits=2)

names(res1)[13]<-"Cont_Vehicle"
names(res1)[15]<-"Veh_Type"

#Calculating the overall and at 7 PM , the mean frequency of 'Contributing Factor'
Veh1<-sqldf("Select Cont_Vehicle,count(Cont_Vehicle) from res9 group by Cont_Vehicle order by Cont_Vehicle")
Veh2<-sqldf("Select Cont_Vehicle,count(Cont_Vehicle) from res1 group by Cont_Vehicle order by Cont_Vehicle")
Veh3<-sqldf("Select * from Veh1 a inner join Veh2 b on a.Cont_Vehicle=b.Cont_Vehicle")
Veh3<-Veh3[,-3]
Veh3[,4]<-round((Veh3[,2]/Veh3[,3]),digits=2)
Veh3<-Veh3[,-c(2,3)]
names(Veh3)[2]<-"Cont_Vehicle_Frequency"
res9<-res9[order(res9$Cont_Vehicle),]
res9<-sqldf("Select a.* ,b.Cont_Vehicle_Frequency from res9 a inner join Veh3 b where a.Cont_Vehicle=b.Cont_Vehicle")

#Calculating the overall and at 7 PM , the mean frequency of 'Vehicle Type'
Veh1<-sqldf("Select Veh_Type,count(Veh_Type) from res9 group by Veh_Type order by Veh_Type")
Veh2<-sqldf("Select Veh_Type,count(Veh_Type) from res1 group by Veh_Type order by Veh_Type")
Veh3<-sqldf("Select * from Veh1 a inner join Veh2 b on a.Veh_Type=b.Veh_Type")
Veh3<-Veh3[,-3]
Veh3[,4]<-round((Veh3[,2]/Veh3[,3]),digits=2)
Veh3<-Veh3[,-c(2,3)]
names(Veh3)[2]<-"Veh_Type_Frequency"
res9<-res9[order(res9$Veh_Type),]
res9<-sqldf("Select a.* ,b.Veh_Type_Frequency from res9 a inner join Veh3 b where a.Veh_Type=b.Veh_Type")

#Removing Unwanted Variables
rm(Veh1,Veh2,Veh3)

res9<-res9[,-c(1:3,5:6)]

#Normalizing the variables
for(i in 1:nrow(res9)){
  res9[i, 1] <- (res9[i,1]-min(res9[,1:3]))/ max(res9[,1:3]) - (min(res9[,1:3]))
}

#Prinicipal Component Analysis
pca_list<-vector("list",nrow(res9))
pca_list[[i]]<-princomp(res9[, 1:3])
loadings_df <- pca_list[[length(pca_list)]]$loadings[,]
loadings_df[,]<-abs(loadings_df[,])

#Calculating the RSI's for each prinicpal component
RSI<-data.frame()

for(i in 1:3){
  x<-as.matrix(loadings_df[,i])
  for(j in 1: nrow(res9)){
    y<-as.matrix(res9[j,])
    
    z<-y %*% x
    
    
    RSI[j,i]<-Acc_density_at_T + z[1,1]
  }
  
}

#The mean RSI for the route
RSI<-c(mean(RSI[,1]),mean(RSI[,2]),mean(RSI[,3]))
