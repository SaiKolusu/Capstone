setwd("C:\\xampp\\htdocs\\maps")

library("rjson")
json_file <- "data_route_file.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=","))
f<-as.data.frame(json_data)
f<-t(f)

if(f[1,1]=="Route 1"){
  file_name='Capi_1.txt'
}else if(f[1,1]=="Route 2"){
  file_name='Capi_2.txt'
}else{
  file_name='Capi_3.txt'
}
######################################################################
ds<-read.csv(file_name,sep=',')
names(ds)[1]<-"Latitude"
names(ds)[2]<-"Longitude"
ds$Location <- "x"
ds[,1]<-round(ds[,1],digits=4)
ds[,2]<-round(ds[,2],digits=4)
ds[,3]<-paste(ds[,1],",",ds[,2],sep="")
names(ds[,3])<-"Location"
ds <- ds[order(ds[,3]),]
library(sqldf)


rs1<-read.csv('datafile.csv',header=T,sep=',')
rs1<-rs1[,-1]
rs1[,3]<-sample(30:80,size=nrow(rs1),replace=T)
rs1<-rs1[order(rs1$Location),]

q<-"Select distinct(Location) from rs1"
locs<-sqldf(q)

locs[1:28,2]<-c(1:28)
names(locs)[2]<-"STREETNAME"

q<-"Select a.*, b.STREETNAME from rs1 a inner join locs b on a.Location=b.Location"
rs4<-sqldf(q)

vol<-read.csv('Volume.csv',header=T,sep=',')

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

res3<-res3[1:nrow(locs),]
res3[,1]<-c(1:28)


q<-"Select * from res3 where STREETNAME in (Select distinct STREETNAME from rs1) "
streets<-sqldf(q)

temp<-streets[,-1]
temp<-t(temp)
temp<-data.frame(temp)
col_temp=ncol(temp)
for(i in 1:24){
  temp[i,col_temp+1] <-i-1
  
}

names(temp)[ncol(temp)]<-"Hour"

q<-"Select STREETNAME from res3 where STREETNAME in (Select distinct STREETNAME from rs1) "
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

#######################################################################################
rs4<-rs4[,-c(1:2)]
names(rs4)[5]<-"Hour"



q<-"Select * from rs4 Order by STREETNAME, Hour"
rs4<-sqldf(q)

q<-"Select a.*,b.V3 from rs4 a inner join temp_base b on a.STREETNAME=b.V1 and a.Hour=b.V2 "
res8<-sqldf(q)

res9<-res8[order(res8$Location),]

q<-"Select a.* from res9 a inner join ds b on a.Location=b.Location"
res8<-sqldf(q)

res8<-res8[,-4]

write.csv(res8,"temp1.csv")


res9<-res8[,c(1:3,4:9,13)]
Mean_Vol<-mean(res9$V3)
Tot_Acc<-nrow(res9)
Acc_points_T<-sqldf("Select count(distinct(LOCATION)) from res8 where hour = 2")
res9<-sqldf("Select * from res9 where hour = 2")

res9<-res9[,-2]

Veh1<-sqldf("Select atmcond,count(atmcond) from res9 group by atmcond order by atmcond")
Veh2<-sqldf("Select atmcond,count(atmcond) from rs1 group by atmcond order by atmcond")
Veh3<-sqldf("Select * from Veh1 a inner join Veh2 b on a.atmcond=b.atmcond")
Veh3<-Veh3[,-3]
Veh3[,4]<-round((Veh3[,2]/Veh3[,3]),digits=2)
Veh3<-Veh3[,-c(2,3)]
names(Veh3)[2]<-"atmcond_Frequency"
res9<-res9[order(res9$atmcond),]
res9<-sqldf("Select a.* ,b.atmcond_Frequency from res9 a inner join Veh3 b where a.atmcond=b.atmcond")

Veh1<-sqldf("Select alcres,count(alcres) from res9 group by alcres order by alcres")
Veh2<-sqldf("Select alcres,count(alcres) from rs1 group by alcres order by alcres")
Veh3<-sqldf("Select * from Veh1 a inner join Veh2 b on a.alcres=b.alcres")
Veh3<-Veh3[,-3]
Veh3[,4]<-round((Veh3[,2]/Veh3[,3]),digits=2)
Veh3<-Veh3[,-c(2,3)]
names(Veh3)[2]<-"alcres_Frequency"
res9<-res9[order(res9$alcres),]
res9<-sqldf("Select a.* ,b.alcres_Frequency from res9 a inner join Veh3 b where a.alcres=b.alcres")

Veh1<-sqldf("Select sex,count(sex) from res9 group by sex order by sex")
Veh2<-sqldf("Select sex,count(sex) from rs1 group by sex order by sex")
Veh3<-sqldf("Select * from Veh1 a inner join Veh2 b on a.sex=b.sex")
Veh3<-Veh3[,-3]
Veh3[,4]<-round((Veh3[,2]/Veh3[,3]),digits=2)
Veh3<-Veh3[,-c(2,3)]
names(Veh3)[2]<-"sex_Frequency"
res9<-res9[order(res9$sex),]
res9<-sqldf("Select a.* ,b.sex_Frequency from res9 a inner join Veh3 b where a.sex=b.sex")

Veh1<-sqldf("Select body,count(body) from res9 group by body order by body")
Veh2<-sqldf("Select body,count(body) from rs1 group by body order by body")
Veh3<-sqldf("Select * from Veh1 a inner join Veh2 b on a.body=b.body")
Veh3<-Veh3[,-3]
Veh3[,4]<-round((Veh3[,2]/Veh3[,3]),digits=2)
Veh3<-Veh3[,-c(2,3)]
names(Veh3)[2]<-"body_Frequency"
res9<-res9[order(res9$body),]
res9<-sqldf("Select a.* ,b.body_Frequency from res9 a inner join Veh3 b where a.body=b.body")

rm(Veh1,Veh2,Veh3)

res9<-res9[,c(1,4,5,9:13)]

#Normalizing the variables
for(i in 1:nrow(res9)){
  for(j in 1 :ncol(res9)){
    res9[i, j] <- (res9[i,j]-min(res9[,1:8]))/ max(res9[,1:8]) - (min(res9[,1:8]))
  }
  
}
library(caret)
split <- createDataPartition(res9$age, p = 1/2, list = FALSE)
train <- res9[split, ]
test <- res9[-split, ]

Mean_Vol_at_T<-mean(train[,4])
Tot_Acc_at_T<-nrow(train)
Acc_density_at_T<-round((Tot_Acc_at_T * Mean_Vol)/(Mean_Vol_at_T * Tot_Acc ),digits=2)

Mean_Vol_at_T_test<-mean(test[,4])
Tot_Acc_at_T_test<-nrow(test)
Acc_density_at_T_test<-round((Tot_Acc_at_T_test * Mean_Vol)/(Mean_Vol_at_T_test * Tot_Acc ),digits=2)

rows<-nrow(train)+1
for( k in 1:ncol(train)){
  for (i in rows:ncol(train)){
    d<-train[1:i-1,k]
    train[i,k]<-mean(d)
  }
}




pca_list<-vector("list",nrow(train))

pca_list[[i]]<-princomp(train[, 1:8])

#################################################
loadings_df <- pca_list[[length(pca_list)]]$loadings[,]
loadings_df[,]<-abs(loadings_df[,])
RSI<-data.frame()

for(i in 1:ncol(train)){
  x<-as.matrix(loadings_df[,i])
  for(j in 1: nrow(train)){
    y<-as.matrix(train[j,])
    
    z<-y %*% x
    
    
    RSI[j,i]<-Acc_density_at_T + z[1,1]
  }
  
}

int_value<-function(num){
  v<-nchar(round(num))
  num<-(num)/(10^v)
  num<-round(num,digits=3)
  return (num)
}
RSI<-c(mean(RSI[,1]),mean(RSI[,2]),mean(RSI[,3]),mean(RSI[,4]),mean(RSI[,5]),mean(RSI[,6]),mean(RSI[,7]),mean(RSI[,8]))
RSI_Train<-min(RSI)

RSI_Train<-int_value(RSI_Train)

RSI_Test<-data.frame()

for(i in 1:ncol(test)){
  x<-as.matrix(loadings_df[,i])
  for(j in 1: nrow(test)){
    y<-as.matrix(test[j,])
    
    z<-y %*% x
    
    
    RSI_Test[j,i]<-Acc_density_at_T_test + z[1,1]
  }
  
}

RSI<-c(mean(RSI_Test[,1]),mean(RSI_Test[,2]),mean(RSI_Test[,3]),mean(RSI_Test[,4]),mean(RSI_Test[,5]),mean(RSI_Test[,6]),mean(RSI_Test[,7]),mean(RSI_Test[,8]))
RSI_Test<-min(RSI_Test)

RSI_Test<-int_value(RSI_Test)

if(RSI_Train>RSI_Test){
  divisor<-RSI_Train
}else{
  divisor<-RSI_Test
}
divisor
accuracy<-(RSI_Train-RSI_Test)/divisor

accuracy<-(1-abs(accuracy))*100
Tot_Acc_at_T<-nrow(res9)

list1 <- vector(mode="list", length=4)
list1[[1]] <- c("Number of accidents", Tot_Acc_at_T)
list1[[2]] <- c("Road Safety Index(Train)", RSI_Train)
list1[[3]] <- c("Road Safety Index(Test)", RSI_Test)
list1[[4]] <- c("Accuracy", accuracy)

library(rjson)
x <- toJSON(list1)
write(x, "C:\\xampp\\htdocs\\maps\\export.JSON")

