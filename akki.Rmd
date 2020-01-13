
setwd("C:/Users/aksha/Downloads/cse5334_project2_TX_weather_datasets/datasets_all")

# year 2008


first_year = read.csv(file="hourly_2008.g",head = FALSE, sep="" , check.names=FALSE)

first_year <- first_year[-1,]
first_year <- first_year[,-23]
first_year <- first_year[,-22]
first_year <- first_year[,-21]
first_year <- first_year[,-20]
colnames(first_year) <- c("STN","WBAN","yearModa_hr","Temp","DewP","Count","SLP","Count","STP","Count","Visib","Count","WDSP","Count","MXSDP","Gust","PRCP","SNDP","FRSHIFT")


library(stringr)
yearModa_hr <- str_split_fixed(first_year$yearModa_hr, "_", 2)
date <- as.Date(yearModa_hr, format("%Y%m%d"))

month <- substr(date,6,7)
year <- substr(date,1,4)
day <- substr(date,9,10)

first_year <- cbind(month, first_year)

first_year <- cbind(year, first_year)
first_year <- cbind(day, first_year)


first_year = first_year[, c("STN","WBAN","yearModa_hr","Temp","DewP","Count","SLP","Count","STP","Count","Visib","Count","WDSP","Count","MXSDP","Gust","PRCP","SNDP","FRSHIFT","year","month","day")]




# year 2009

second_year = read.csv(file="hourly_2009.g",head = FALSE, sep="")
second_year <- second_year[-1,]
second_year <- second_year[,-23]
second_year <- second_year[,-22]
second_year <- second_year[,-21]
second_year <- second_year[,-20]
colnames(second_year) <- c("STN","WBAN","yearModa_hr","Temp","DewP","Count","SLP","Count","STP","Count","Visib","Count","WDSP","Count","MXSDP","Gust","PRCP","SNDP","FRSHIFT")

library(stringr)
yearModa_hr <- str_split_fixed(second_year$yearModa_hr, "_", 2)
date <- as.Date(yearModa_hr, format("%Y%m%d"))
month <- substr(date,6,7)
year <- substr(date,1,4)
day<- substr(date,9,10)

second_year <- cbind(month, second_year)
second_year <- cbind(year, second_year)
second_year <- cbind(day, second_year)

second_year = second_year[, c("STN","WBAN","yearModa_hr","Temp","DewP","Count","SLP","Count","STP","Count","Visib","Count","WDSP","Count","MXSDP","Gust","PRCP","SNDP","FRSHIFT","month","year","day")]



# year 2010

third_year = read.csv(file="hourly_2010.g",head = FALSE, sep=",")

third_year <- third_year[-1,]
third_year <- third_year[,-21]
third_year <- third_year[,-20]
third_year <- third_year[,-19]
third_year <- third_year[,-15]

library(tidyr)
third_year$V3 <- trimws(third_year$V3, which = c("left"))
third_year$V3 <- trimws(third_year$V3, which = c("right"))
third_year = separate(third_year, V3, into = c("new","DewP"), sep = " (?=[^ ]+$)")
third_year$new <- trimws(third_year$new, which = c("right"))

third_year = separate(third_year, new, into = c("new", "temp"), sep = " (?=[^ ]+$)")
third_year$new <- trimws(third_year$new, which = c("right"))



colnames(third_year) <- c("STN","WBAN","yearModa_hr","Temp","DewP","Count","SLP","Count","STP","Count","Visib","Count","WDSP","Count","MXSDP","Gust","PRCP","SNDP","FRSHIFT")

library(stringr)
date <- str_split_fixed(third_year$yearModa_hr, "_", 2)

date <- as.Date(date, format("%Y%m%d"))
month <- substr(date,6,7)
year <- substr(date,1,4)
day<- substr(date,9,10)

third_year <- cbind(month, third_year)
third_year <- cbind(year, third_year)
third_year <- cbind(day, third_year)

third_year = third_year[, c("STN","WBAN","yearModa_hr","Temp","DewP","Count","SLP","Count","STP","Count","Visib","Count","WDSP","Count","MXSDP","Gust","PRCP","SNDP","FRSHIFT","month","year","day")]

march_2008<-subset(first_year,month=="03")
march_2009<-subset(second_year,month=="03")
march_2010<-subset(third_year,month=="03")

march_2008 <- march_2008[, c("Temp", "DewP", "STP", "WDSP","STN","year","month","day")]
march_2009 <- march_2009[, c("Temp", "DewP", "STP", "WDSP","STN","year","month","day")]
march_2010 <- march_2010[, c("Temp", "DewP", "STP", "WDSP","STN","year","month","day")]

march_2008 <- march_2008[march_2008$Temp != 9999.9, ]
march_2008 <- march_2008[march_2008$DewP != 9999.9, ]
march_2008 <- march_2008[march_2008$STP != 9999.9, ]
march_2008 <- march_2008[march_2008$WDSP != 999.9, ]

march_2009 <- march_2009[march_2009$Temp != 9999.9, ]
march_2009 <- march_2009[march_2009$DewP != 9999.9, ]
march_2009 <- march_2009[march_2009$STP != 9999.9, ]
march_2009 <- march_2009[march_2009$WDSP != 999.9, ]

march_2010 <- march_2010[march_2010$Temp != 9999.9, ]
march_2010 <- march_2010[march_2010$DewP != 9999.9, ]
march_2010 <- march_2010[march_2010$STP != 9999.9, ]
march_2010 <- march_2010[march_2010$WDSP != 999.9, ]

march_08 <- lapply(march_2008, function(x) as.numeric(as.character(x)))
march_09 <- lapply(march_2009, function(x) as.numeric(as.character(x)))
march_10 <- lapply(march_2010, function(x) as.numeric(as.character(x)))

#Monthly mean of 2008



#find mean for 2008 aggregate--------------------------------
length(unique(march_2008[["STN"]]))

df_monthly2008 <- data.frame(
                 Temp=numeric(),
                 DewP=numeric(),
                 STP=numeric(),
                 WDSP=numeric(),
                 STN=character(),
                 stringsAsFactors=FALSE)

str(df_monthly2008 )

#--
             
for( i in unique(march_2008$STN ) ){
  
  df1<- subset(march_2008,march_2008$STN == i)
  
  march_08Temp<- subset(df1,df1$Temp!="9999.9")
  march_08Temp<- lapply(march_08Temp, function(x)   as.numeric(as.character(x)))
  
  agg_mean2008Temp <-   aggregate(march_08Temp$Temp,by=list(march_08Temp$day),FUN=mean,   na.rm=TRUE)
  
  march_08Dewp<- subset(df1,df1$DewP!="9999.9")
  march_08Dewp<- lapply(march_08Dewp, function(x)   as.numeric(as.character(x)))
  agg_mean2008DewP<-   aggregate(march_08Dewp$DewP,by=list(march_08Dewp$day),FUN=mean,   na.rm=TRUE)
  
  march_08STP<- subset(df1,df1$STP!="9999.9")
  march_08STP<- lapply(march_08STP, function(x)   as.numeric(as.character(x)))
  agg_mean2008STP<-   aggregate(march_08STP$STP,by=list(march_08STP$day),FUN=mean,   na.rm=TRUE)
  
  march_08WDSP<- subset(df1,df1$WDSP!="999.9")
  march_08WDSP<- lapply(march_08WDSP, function(x)   as.numeric(as.character(x)))
  agg_mean2008WDSP<-   aggregate(march_08WDSP$WDSP,by=list(march_08WDSP$day),FUN=mean,   na.rm=TRUE)
 
 
 monthlyTemp08<-sum(agg_mean2008Temp)/31
 
 monthlyDewP08<-sum(agg_mean2008DewP)/31
 monthlySTP08<-sum(agg_mean2008STP)/31
 monthlyWDSP08<- sum(agg_mean2008WDSP)/31
  
  de<-list(Temp=monthlyTemp08 ,DewP=monthlyDewP08 ,STP=monthlySTP08 ,WDSP=monthlyWDSP08,STN=i)
  df_monthly2008 <- rbind(df_monthly2008 ,de,stringsAsFactors=FALSE)
 
  
}

  

#find mean for 2009 aggregate--------------------------------

length(unique(march_2009[["STN"]]))
df_monthly2009 <- data.frame(
                 Temp=numeric(),
                 DewP=numeric(),
                 STP=numeric(),
                 WDSP=numeric(),
                 STN=character(),
                 stringsAsFactors=FALSE)

str(df_monthly2009 )

#--
                 
for(i in unique(march_2009$STN ) ){
  df1<- subset(march_2009,march_2009$STN == i)

  march_09Temp<- subset(df1,df1$Temp!="99999.9")
  march_09Temp<- lapply(march_09Temp, function(x)   as.numeric(as.character(x)))
  agg_mean2009Temp <-   aggregate(march_09Temp$Temp,by=list(march_09Temp$day),FUN=mean,   na.rm=TRUE)
  
  march_09Dewp<- subset(df1,df1$DewP!="99999.9")
  march_09Dewp<- lapply(march_09Dewp, function(x)   as.numeric(as.character(x)))
  agg_mean2009DewP<-   aggregate(march_09Dewp$DewP,by=list(march_09Dewp$day),FUN=mean,   na.rm=TRUE)
  
  march_09STP<- subset(df1,df1$STP!="99999.9")
  march_09STP<- lapply(march_09STP, function(x)   as.numeric(as.character(x)))
  agg_mean2009STP<-   aggregate(march_09STP$STP,by=list(march_09STP$day),FUN=mean,   na.rm=TRUE)
  
  march_09WDSP<- subset(df1,df1$WDSP!="99999.9")
  march_09WDSP<- lapply(march_09WDSP, function(x)   as.numeric(as.character(x)))
  agg_mean2009WDSP<-   aggregate(march_09WDSP$WDSP,by=list(march_09WDSP$day),FUN=mean,   na.rm=TRUE)
 
 monthlyTemp09<-sum(agg_mean2009Temp)/31
 monthlyDewP09<-sum(agg_mean2009DewP)/31
 monthlySTP09<-sum(agg_mean2009STP)/31
 monthlyWDSP09<- sum(agg_mean2009WDSP)/31
 
  de<-list(Temp=monthlyTemp09 ,DewP=monthlyDewP09 ,STP=monthlySTP09 ,WDSP=monthlyWDSP09,STN=i)
  df_monthly2009 = rbind(df_monthly2009 ,de,stringsAsFactors=FALSE)
 
}
  
#find mean for 2010 aggregate--------------------------------
length(unique(march_2010[["STN"]]))
df_monthly2010 <- data.frame(
                 Temp=numeric(),
                 DewP=numeric(),
                 STP=numeric(),
                 WDSP=numeric(),
                 STN=character(),
                 stringsAsFactors=FALSE)

str(df_monthly2010 )
for(i in unique(march_2010$STN ) ){
  df10<- subset(march_2010,march_2010$STN == i)

march_10Temp<- subset(df10,df10$Temp!="99999.9")
march_10Temp<- lapply(march_10Temp, function(x) as.numeric(as.character(x)))
agg_mean2010Temp <- aggregate(march_10Temp$Temp,by=list(march_10Temp$day),FUN=mean, na.rm=TRUE)

march_10Dewp<- subset(df10,df10$DewP!="99999.9")
march_10Dewp<- lapply(march_10Dewp, function(x) as.numeric(as.character(x)))
agg_mean2010DewP<- aggregate(march_10Dewp$DewP,by=list(march_10Dewp$day),FUN=mean, na.rm=TRUE)

march_10STP<- subset(df10,df10$STP!="99999.9")
march_10STP<- lapply(march_10STP, function(x) as.numeric(as.character(x)))
agg_mean2010STP<- aggregate(march_10STP$STP,by=list(march_10STP$day),FUN=mean, na.rm=TRUE)

march_10WDSP<- subset(df10,df10$WDSP!="999.9")
march_10WDSP<- lapply(march_10WDSP, function(x) as.numeric(as.character(x)))
agg_mean2010WDSP<- aggregate(march_10WDSP$WDSP,by=list(march_10WDSP$day),FUN=mean, na.rm=TRUE)

mon10w<- sum(agg_mean2010WDSP)/31
mon10t<- sum(agg_mean2010Temp)/31
mon10d<- sum(agg_mean2010DewP)/31
mon10s<- sum(agg_mean2010STP)/31
de<-list(Temp=mon10t ,DewP=mon10d ,STP=mon10s ,WDSP=mon10w,STN=i)
df_monthly2010 = rbind(df_monthly2010 ,de,stringsAsFactors=FALSE)
}

#-----attached data with lat log-------------
dfLatLog=read.csv("C:/Users/aksha/Downloads/stations.csv",head = TRUE, sep="," , check.names=FALSE)
dfLatLog <- subset(dfLatLog, select = c(StationNumber, Lat,	Lon))
names(dfLatLog) <- c("STN","Lat","Lon")
library(dplyr)
dfLatLog<-dfLatLog %>% distinct(STN, .keep_all = TRUE)

library(plyr)
df_monthly2008new<-join(df_monthly2008, dfLatLog, by="STN", type="left")

df_monthly2009new<-join(df_monthly2009, dfLatLog, by="STN", type="left")

df_monthly2010new<-join(df_monthly2010, dfLatLog, by="STN", type="left")


# set seed
# df_monthly2008new<-df_monthly2008[,-c(5)]
# df_monthly2009new<-df_monthly2009[,-c(5)]
# df_monthly2010new<-df_monthly2010[,-c(5)]


set.seed(20)

#---------------K-mean clusters = 10-----------------------
#2008 cluster
cluster1 <- kmeans(df_monthly2008new[,c(1,2,3,4)], centers = 10, nstart = 25)

df_monthly2008new$zone1 <- as.factor(cluster1$cluster)

str(cluster1)
cluster1witness<-cluster1$withinss

summary(df_monthly2008new)

#2009 cluster
cluster2 <- kmeans(df_monthly2009new[,c(1,2,3,4)], centers = 10, nstart = 25)

df_monthly2009new$zone2 <- as.factor(cluster2$cluster)

str(cluster2)

summary(df_monthly2009new)

#2010 cluster
cluster3 <- kmeans(df_monthly2010new[,c(1,2,3,4)], centers = 10, nstart = 25)

df_monthly2010new$zone3 <- as.factor(cluster3$cluster)

str(cluster3)

summary(df_monthly2010new)
library(tidyverse)
library(factoextra)
chng2008kmean<-df_monthly2008new
chng2008kmean$STN<- as.numeric(as.factor(chng2008kmean$STN))
chng2008kmean$zone1<- as.numeric(as.factor(chng2008kmean$zone1))
chng2008kmean$zoneP1<- as.numeric(as.factor(chng2008kmean$zoneP1))
chng2008kmean$zoneE1<- as.numeric(as.factor(chng2008kmean$zoneE1))
chng2008kmean <- scale(chng2008kmean)
C<-fviz_cluster(cluster1,data=chng2008kmean)
plot(C)


chng2009kmean<-df_monthly2009new
chng2009kmean$STN<- as.numeric(as.factor(chng2009kmean$STN))
chng2009kmean$zone2<- as.numeric(as.factor(chng2009kmean$zone2))
chng2009kmean$zoneP2<- as.numeric(as.factor(chng2009kmean$zoneP2))
chng2009kmean$zoneE2<- as.numeric(as.factor(chng2009kmean$zoneE2))
chng2009kmean <- scale(chng2009kmean)
C1<-fviz_cluster(cluster2,data=chng2009kmean)
plot(C1)


chng2010kmean<-df_monthly2010new
chng2010kmean$STN<- as.numeric(as.factor(chng2010kmean$STN))
chng2010kmean$zone3<- as.numeric(as.factor(chng2010kmean$zone3))
chng2010kmean$zoneP3<- as.numeric(as.factor(chng2010kmean$zoneP3))
chng2010kmean$zoneE3<- as.numeric(as.factor(chng2010kmean$zoneE3))
chng2010kmean <- scale(chng2010kmean)
C2<-fviz_cluster(cluster3,data=chng2010kmean)
plot(C2)
#4########################3

#data<- c(1,1,2,0,1,3,4,5,6,7,8)
##frame1<-as.data.frame(data)
#frame$twohouses <- ifelse(frame1$data>1, 2, 1)
#--------------------Euclidean--------------------------------
#2008


library(cluster)
# Loads the cluster library.
#fannyy <- fanny(df_monthly2008new, k=10, metric = "euclidean", memb.exp = 2)
#str(fannyy)
library(amap)
clusterE1 <- Kmeans(df_monthly2008new[,c(1,2,3,4)],centers = 10,method='euclidean')

df_monthly2008new$zoneE1 <- as.factor(clusterE1$cluster)
#e<-dist(df_monthly2008new,method = "euclidean")
#str(e)
str(clusterE1)

#2009
clusterE2 <- Kmeans(df_monthly2009new[,c(1,2,3,4)], centers = 10,method = "euclidean")
df_monthly2009new$zoneE2 <- as.factor(clusterE2$cluster)

str(clusterE2)

#2010
clusterE3 <- Kmeans(df_monthly2010new[,c(1,2,3,4)], centers = 10,method = "euclidean")

df_monthly2010new$zoneE3 <- as.factor(clusterE3$cluster)
#Kmeans(x, centers, iter.max = 10, nstart = 1,
         method = "euclidean")
str(clusterE3)


chng2008euc<-df_monthly2008new
chng2008euc$STN<- as.numeric(as.factor(chng2008euc$STN))
chng2008euc$zone1<- as.numeric(as.factor(chng2008euc$zone1))
chng2008euc$zoneP1<- as.numeric(as.factor(chng2008euc$zoneP1))
chng2008euc$zoneE1<- as.numeric(as.factor(chng2008euc$zoneE1))
chng2008euc <- scale(chng2008euc)
C3<-fviz_cluster(clusterE1,data=chng2008euc)
plot(C3)


chng2009euc<-df_monthly2009new
chng2009euc$STN<- as.numeric(as.factor(chng2009euc$STN))
chng2009euc$zone2<- as.numeric(as.factor(chng2009euc$zone2))
chng2009euc$zoneP2<- as.numeric(as.factor(chng2009euc$zoneP2))
chng2009euc$zoneE2<- as.numeric(as.factor(chng2009euc$zoneE2))
chng2009euc <- scale(chng2009euc)
C4<-fviz_cluster(clusterE2,data=chng2009euc)
plot(C4)


chng2010euc<-df_monthly2010new
chng2010euc$STN<- as.numeric(as.factor(chng2010euc$STN))
chng2010euc$zone3<- as.numeric(as.factor(chng2010euc$zone3))
chng2010euc$zoneP3<- as.numeric(as.factor(chng2010euc$zoneP3))
chng2010euc$zoneE3<- as.numeric(as.factor(chng2010euc$zoneE3))
chng2010euc <- scale(chng2010euc)
C5<-fviz_cluster(clusterE3,data=chng2010euc)
plot(C5)









#----------------pearson-------------------
#2008
clusterP1 <- Kmeans(df_monthly2008new[,c(1,2,3,4)],10,nstart = 25, method="pearson")


df_monthly2008new$zoneP1 <- as.factor(clusterP1$cluster)

str(clusterP1)

#2009
clusterP2 <- Kmeans(df_monthly2009new[,c(1,2,3,4)],10,nstart = 25, method="pearson")


df_monthly2009new$zoneP2 <- as.factor(clusterP2$cluster)

str(clusterP2)

#2010
clusterP3 <- Kmeans(df_monthly2010new[,c(1,2,3,4)],centers =10,nstart = 25, method="pearson")


df_monthly2010new$zoneP3 <- as.factor(clusterP3$cluster)

str(clusterP3)



chng2008pear<-df_monthly2008new
chng2008pear$STN<- as.numeric(as.factor(chng2008pear$STN))
chng2008pear$zone1<- as.numeric(as.factor(chng2008pear$zone1))
chng2008pear$zoneP1<- as.numeric(as.factor(chng2008pear$zoneP1))
chng2008pear$zoneE1<- as.numeric(as.factor(chng2008pear$zoneE1))
chng2008pear <- scale(chng2008pear)
C6<-fviz_cluster(clusterP1,data=chng2008pear)
plot(C6)


chng2009pear<-df_monthly2009new
chng2009pear$STN<- as.numeric(as.factor(chng2009pear$STN))
chng2009pear$zone2<- as.numeric(as.factor(chng2009pear$zone2))
chng2009pear$zoneP2<- as.numeric(as.factor(chng2009pear$zoneP2))
chng2009pear$zoneE2<- as.numeric(as.factor(chng2009pear$zoneE2))
chng2009pear <- scale(chng2009pear)
C7<-fviz_cluster(clusterP2,data=chng2009euc)
plot(C7)


chng2010pear<-df_monthly2010new
chng2010pear$STN<- as.numeric(as.factor(chng2010pear$STN))
chng2010pear$zone3<- as.numeric(as.factor(chng2010pear$zone3))
chng2010pear$zoneP3<- as.numeric(as.factor(chng2010pear$zoneP3))
chng2010pear$zoneE3<- as.numeric(as.factor(chng2010pear$zoneE3))
chng2010pear <- scale(chng2010pear)
C8<-fviz_cluster(clusterP3,data=chng2010pear)
plot(C8)







#-------------------------------Jaccard compare two cluster-------
#Compute the change in clusters from year to year using the Jaccard coefficient for each distance metric and provide analysis of (Y1, Y2), (Y2, Y3), and (Y1, Y2, Y3)
#library(clusteval)
#jacc0809<-jaccard_indep(cluster1, cluster2)
#
#source("http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/My_R_Script#s/clusterIndex.R") 
#library(cluster)
#ci <- cindex(clV1=cluster2, clV2=cluster3, self=FALSE, minSZ=1, #method="jaccard")
#ci[2:3]

library(maps)

jaccardCalcu<-function(s1,s2){
  inlen=length(intersect(s1,s2))
  unlength=length(union(s1,s2))
  jaccard =inlen/unlength
  return(jaccard)
}
maxSim<-function(v1,v2){
  sim=-1
  sum<-0
  for(i in 1:10){
    for(j in 1:10){
      k<-jaccardCalcu(v1[[i]],v2[[j]])
      if(k>sim){
        sim<-k
      }
    }
      print(sim)
      sum<-sum+sim
      sim=-1
    }
    return(sum/10)
}
 EClusters=c()
 PClusters=c()

for(i in 1:10) {
  EClusters[[i]] = which(clusterE1$cluster %in% c(i))
  PClusters[[i]] = which(clusterP1$cluster %in% c(i))
}
print(maxSim(EClusters, PClusters))
#--------------------J-E2-P2_---
library(maps)

jaccardCalcu<-function(s1,s2){
  inlen=length(intersect(s1,s2))
  unlength=length(union(s1,s2))
  jaccard =inlen/unlength
  return(jaccard)
}
maxSim<-function(v1,v2){
  sim=-1
  sum<-0
  for(i in 1:10){
    for(j in 1:10){
      k<-jaccardCalcu(v1[[i]],v2[[j]])
      if(k>sim){
        sim<-k
      }
    }
      print(sim)
      sum<-sum+sim
      sim=-1
    }
    return(sum/10)
}
 EClusters=c()
 PClusters=c()

for(i in 1:10) {
  EClusters[[i]] = which(clusterE2$cluster %in% c(i))
  PClusters[[i]] = which(clusterP2$cluster %in% c(i))
}
print(maxSim(EClusters, PClusters))


#--------------------J-E3-P3_---
library(maps)

jaccardCalcu<-function(s1,s2){
  inlen=length(intersect(s1,s2))
  unlength=length(union(s1,s2))
  jaccard =inlen/unlength
  return(jaccard)
}
maxSim<-function(v1,v2){
  sim=-1
  sum<-0
  for(i in 1:10){
    for(j in 1:10){
      k<-jaccardCalcu(v1[[i]],v2[[j]])
      if(k>sim){
        sim<-k
      }
    }
      print(sim)
      sum<-sum+sim
      sim=-1
    }
    return(sum/10)
}
 EClusters=c()
 PClusters=c()

for(i in 1:10) {
  EClusters[[i]] = which(clusterE3$cluster %in% c(i))
  PClusters[[i]] = which(clusterP3$cluster %in% c(i))
}
print(maxSim(EClusters, PClusters))

#--------------------J-E1-E2_---
library(maps)

jaccardCalcu<-function(s1,s2){
  inlen=length(intersect(s1,s2))
  unlength=length(union(s1,s2))
  jaccard =inlen/unlength
  return(jaccard)
}
maxSim<-function(v1,v2){
  sim=-1
  sum<-0
  for(i in 1:10){
    for(j in 1:10){
      k<-jaccardCalcu(v1[[i]],v2[[j]])
      if(k>sim){
        sim<-k
      }
    }
      print(sim)
      sum<-sum+sim
      sim=-1
    }
    return(sum/10)
}
 EClusters=c()
 PClusters=c()

for(i in 1:10) {
  EClusters[[i]] = which(clusterE1$cluster %in% c(i))
  PClusters[[i]] = which(clusterE2$cluster %in% c(i))
}
print(maxSim(EClusters, PClusters))

#--------------------J-E1-E3_---
library(maps)

jaccardCalcu<-function(s1,s2){
  inlen=length(intersect(s1,s2))
  unlength=length(union(s1,s2))
  jaccard =inlen/unlength
  return(jaccard)
}
maxSim<-function(v1,v2){
  sim=-1
  sum<-0
  for(i in 1:10){
    for(j in 1:10){
      k<-jaccardCalcu(v1[[i]],v2[[j]])
      if(k>sim){
        sim<-k
      }
    }
      print(sim)
      sum<-sum+sim
      sim=-1
    }
    return(sum/10)
}
 EClusters=c()
 PClusters=c()

for(i in 1:10) {
  EClusters[[i]] = which(clusterE1$cluster %in% c(i))
  PClusters[[i]] = which(clusterE3$cluster %in% c(i))
}
print(maxSim(EClusters, PClusters))
#--------------------J-E1-E3_---
library(maps)

jaccardCalcu<-function(s1,s2){
  inlen=length(intersect(s1,s2))
  unlength=length(union(s1,s2))
  jaccard =inlen/unlength
  return(jaccard)
}
maxSim<-function(v1,v2){
  sim=-1
  sum<-0
  for(i in 1:10){
    for(j in 1:10){
      k<-jaccardCalcu(v1[[i]],v2[[j]])
      if(k>sim){
        sim<-k
      }
    }
      print(sim)
      sum<-sum+sim
      sim=-1
    }
    return(sum/10)
}
 EClusters=c()
 PClusters=c()

for(i in 1:10) {
  EClusters[[i]] = which(clusterE1$cluster %in% c(i))
  PClusters[[i]] = which(clusterE3$cluster %in% c(i))
}
print(maxSim(EClusters, PClusters))

#--------------------J-E2-E3_---
library(maps)

jaccardCalcu<-function(s1,s2){
  inlen=length(intersect(s1,s2))
  unlength=length(union(s1,s2))
  jaccard =inlen/unlength
  return(jaccard)
}
maxSim<-function(v1,v2){
  sim=-1
  sum<-0
  for(i in 1:10){
    for(j in 1:10){
      k<-jaccardCalcu(v1[[i]],v2[[j]])
      if(k>sim){
        sim<-k
      }
    }
      print(sim)
      sum<-sum+sim
      sim=-1
    }
    return(sum/10)
}
 EClusters=c()
 PClusters=c()

for(i in 1:10) {
  EClusters[[i]] = which(clusterE2$cluster %in% c(i))
  PClusters[[i]] = which(clusterE3$cluster %in% c(i))
}
print(maxSim(EClusters, PClusters))

#--------------------J-P1-P2_---
library(maps)

jaccardCalcu<-function(s1,s2){
  inlen=length(intersect(s1,s2))
  unlength=length(union(s1,s2))
  jaccard =inlen/unlength
  return(jaccard)
}
maxSim<-function(v1,v2){
  sim=-1
  sum<-0
  for(i in 1:10){
    for(j in 1:10){
      k<-jaccardCalcu(v1[[i]],v2[[j]])
      if(k>sim){
        sim<-k
      }
    }
      print(sim)
      sum<-sum+sim
      sim=-1
    }
    return(sum/10)
}
 EClusters=c()
 PClusters=c()

for(i in 1:10) {
  EClusters[[i]] = which(clusterP1$cluster %in% c(i))
  PClusters[[i]] = which(clusterP2$cluster %in% c(i))
}
print(maxSim(EClusters, PClusters))

#--------------------J-P1-P3_---
library(maps)

jaccardCalcu<-function(s1,s2){
  inlen=length(intersect(s1,s2))
  unlength=length(union(s1,s2))
  jaccard =inlen/unlength
  return(jaccard)
}
maxSim<-function(v1,v2){
  sim=-1
  sum<-0
  for(i in 1:10){
    for(j in 1:10){
      k<-jaccardCalcu(v1[[i]],v2[[j]])
      if(k>sim){
        sim<-k
      }
    }
      print(sim)
      sum<-sum+sim
      sim=-1
    }
    return(sum/10)
}
 EClusters=c()
 PClusters=c()

for(i in 1:10) {
  EClusters[[i]] = which(clusterP1$cluster %in% c(i))
  PClusters[[i]] = which(clusterP3$cluster %in% c(i))
}
print(maxSim(EClusters, PClusters))

#--------------------J-P2-P3_---
library(maps)

jaccardCalcu<-function(s1,s2){
  inlen=length(intersect(s1,s2))
  unlength=length(union(s1,s2))
  jaccard =inlen/unlength
  return(jaccard)
}
maxSim<-function(v1,v2){
  sim=-1
  sum<-0
  for(i in 1:10){
    for(j in 1:10){
      k<-jaccardCalcu(v1[[i]],v2[[j]])
      if(k>sim){
        sim<-k
      }
    }
      print(sim)
      sum<-sum+sim
      sim=-1
    }
    return(sum/10)
}
 EClusters=c()
 PClusters=c()

for(i in 1:10) {
  EClusters[[i]] = which(clusterP2$cluster %in% c(i))
  PClusters[[i]] = which(clusterP3$cluster %in% c(i))
}
print(maxSim(EClusters, PClusters))



#-------------SSE Calculation--------------------

cluster1witness<-cluster1$withinss
cluster2witness<-cluster2$withinss
cluster3witness<-cluster3$withinss

clusterE1witness<-clusterE1$withinss
clusterE2witness<-clusterE2$withinss
clusterE3witness<-clusterE3$withinss
clusterE1tot.withinss<-sum(clusterE1witness)
clusterE2tot.withinss<-sum(clusterE2witness)
clusterE3tot.withinss<-sum(clusterE3witness)

clusterP1witness<-clusterP1$withinss
clusterP2witness<-clusterP2$withinss
clusterP3witness<-clusterP3$withinss
clusterP1tot.withinss<-sum(clusterP1witness)
clusterP2tot.withinss<-sum(clusterP2witness)
clusterP3tot.withinss<-sum(clusterP3witness)


print(clusterE1witness)
print(clusterE2witness)
print(clusterE3witness)
print(clusterE1tot.withinss)
print(clusterE2tot.withinss)
print(clusterE3tot.withinss)
print(clusterP1tot.withinss)
print(clusterP2tot.withinss)
print(clusterP3tot.withinss)
print(clusterP1witness)
print(clusterP2witness)
print(clusterP3witness)
#------------------Plot---------
#install.packages("ggmap")

#devtools::install_github("dkahle/ggmap", ref = "tidyup")
#library(ggplot2)
library(ggmap)



#register_google(key = "AIzaSyCOeKKikRDrpoYeVmNoRk0Newhhz-Fev24")

#register_google(key = "AIzaSyAI80it1h-3ml3hb_nlgyjp_9PQmjhNA_4", account_type = "standard")

TxMap <- get_map("Texas",zoom = 6 ,source="google",maptype="terrain")
ggmap(TxMap) + geom_point(aes(x = Lon, y = Lat, colour = as.factor(zone1)),data = df_monthly2008new) +
  ggtitle("2008 KMean cluster")
  
# kmean 2009
library(ggmap)
TxMap <- get_map("Texas",zoom = 6 ,source="google",maptype="terrain")
ggmap(TxMap) + geom_point(aes(x = Lon, y = Lat, colour = as.factor(zone2)),data = df_monthly2009new) +
  ggtitle("2009 KMean cluster")
  
# kmean 2010
library(ggmap)
TxMap <- get_map("Texas",zoom = 6 ,source="google",maptype="terrain")
ggmap(TxMap) + geom_point(aes(x = Lon, y = Lat, colour = as.factor(zone3)),data = df_monthly2010new) +
  ggtitle("2010 KMean cluster")
  
# Euc 2008
library(ggmap)
TxMap <- get_map("Texas",zoom = 6 ,source="google",maptype="terrain")
ggmap(TxMap) + geom_point(aes(x = Lon, y = Lat, colour = as.factor(zoneE1)),data = df_monthly2008new) +
  ggtitle("2008 Euclidean cluster")
  
# Euc 2009
library(ggmap)
TxMap <- get_map("Texas",zoom = 6 ,source="google",maptype="terrain")
ggmap(TxMap) + geom_point(aes(x = Lon, y = Lat, colour = as.factor(zoneE2)),data = df_monthly2009new) +
  ggtitle("2009 Euclidean cluster")
  
# Euc 2010
library(ggmap)
TxMap <- get_map("Texas",zoom = 6 ,source="google",maptype="terrain")
ggmap(TxMap) + geom_point(aes(x = Lon, y = Lat, colour = as.factor(zoneE3)),data = df_monthly2010new) +
  ggtitle("2010 Euclidean cluster")
  
# Pearson 2008
library(ggmap)
TxMap <- get_map("Texas",zoom = 6 ,source="google",maptype="terrain")
ggmap(TxMap) + geom_point(aes(x = Lon, y = Lat, colour = as.factor(zoneP1)),data = df_monthly2008new) +
  ggtitle("2008 Pearson cluster")
  
# Pearson 2009
library(ggmap)
TxMap <- get_map("Texas",zoom = 6 ,source="google",maptype="terrain")
ggmap(TxMap) + geom_point(aes(x = Lon, y = Lat, colour = as.factor(zoneP2)),data = df_monthly2009new) +
  ggtitle("2009 Pearson cluster")
  
# Pearson 2010
library(ggmap)
TxMap <- get_map("Texas",zoom = 6 ,source="google",maptype="terrain")
ggmap(TxMap) + geom_point(aes(x = Lon, y = Lat, colour = as.factor(zoneP3)),data = df_monthly2010new) +
  ggtitle("2010 Pearson cluster")
