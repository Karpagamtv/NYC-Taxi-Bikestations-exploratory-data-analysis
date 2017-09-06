##Exploratory Geo-spatial analysis

##Reading the May 2016 citibike and yellow taxi NYC trip cleansed data
##Data cleansing was done using SAS JMP

##Taxi Trip distance was present in the database 
##For citibike, trip distance is calculated by the below function
##calculate the distance between pickup and drop off points of citi bike

earthDist <- function (lon1, lat1, lon2, lat2){
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- lon1 * rad
  b1 <- lat2 * rad
  b2 <- lon2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- round(R * c* 0.62137119,2);
  return(d)
}

##adding a column "distance(in miles)" for the citi bike data
citibike$distance= earthDist(citibike$start.station.longitude, citibike$start.station.latitude,citibike$end.station.longitude, citibike$end.station.latitude)

##Checking the count citibike trip distance is less than or equal to 0 (Trip didn't occur)

length(which(citibike$distance<=0.00))

##Checking the count of the citibike trip distance is greater than 5 miles 

length(which(citibike$distance>=5.00))

##Eliminated over 100000 records in over 1 mil records with the below code

for (i in 1:length(citibike$distance))
{
  temp = which(citibike$distance <=0.00 | citibike$distance > 5.00)
  citibike$distance[temp] = NA
  
}

##Reading the May 2016 citibike and yellow taxi NYC trip cleansed data

citi_clean=read.csv("C:/Users/Karpagam/Desktop/tripdata_cleansed/citibike_cleansed.csv");
taxi_clean=read.csv("C:/Users/Karpagam/Desktop/tripdata_cleansed/tripdata_cleansed.csv");


##Installing data.table package for faster data manipulation 

install.packages("sqldf")
library(sqldf)
install.packages("data.table")
library(data.table)

## The data has citibike end and start station IDs and to compare against the taxi rides

## Storing the unique pick up and dropoff bike station ID from the data

a<- as.data.frame(citi_clean$start.station.id)
uniquea=(unique(a));
b<- as.data.frame(citi_clean$end.station.id)
uniqueb=(unique(b));

##Using the row count to check if the counts are equal
nrow(uniquea);
nrow(uniqueb);

## End stations had 481 stationIDs and start stations had 475 stations

## Checking if all the unique start stations are present in unique end station dataframe and vice versa

stationida=uniquea$'citi_clean$start.station.id'[uniquea$'citi_clean$start.station.id' %in% uniqueb$'citi_clean$end.station.id']

stationidb=uniqueb$'citi_clean$end.station.id'[!uniqueb$'citi_clean$end.station.id' %in% uniquea$'citi_clean$start.station.id']



#################################################################################


df = data.frame(unique(citi_clean$end.station.id))

## Using the tapply function to get the longitude, latitude and bike station name 
## from the unique end bike station id of the citibike cleansed data

long=tapply(citi_clean$end.station.longitude, citi_clean$end.station.id, FUN=min)
lat =tapply(citi_clean$end.station.latitude, citi_clean$end.station.id, FUN=min)
station_name=tapply(citi_clean$end.station.name,citi_clean$end.station.id,FUN=)

##Storing it in dataframe df1

df1= as.data.frame( cbind(long,lat,station_name));

##Creating a new dataframe df2 with one column

df2=data.frame()[,1];

## Storing the unique station ID in the frame
df2$citi_clean$end.station.id= data.frame(unique(citi_clean$end.station.id));

##Storing the latitude, longitude and station name 

df2$citilat=df1$lat
df2$citilong=df1$long
df2$citistationname=df1$station_name

citibikedf <- df2

##Writing the unique citibike information into a CSV file 
## with columns bike_id, station_name, Longitude, Latitude

write.csv(Citibikedf,file="C:/Users/Karpagam/Desktop/Citibikeinfo.csv")

#Creating a sample to get the closest bike station for the first 10 taxi records 

sample2 = taxi_clean[1:10,]



#####TWO-DIMENSIONAL CLUSTERING by joining taxi rides and bike rides data
#####Finding the closest bike station by calculating the distance between every bike station and the taxi ride pickup point
#####Storing the bike station which is at the least distance from the pickup point

for (i in 1:nrow(sample2)){
  
  sample2$minimum_distance_from_start[i]<- 1000
  
  
  
  for (j in 1:nrow(citibikedf)){
    
    temp1=c(earthDist(sample2$taxi.pickup_longitude[i],sample2$taxi.pickup_latitude[i],citibikedf$end.station.longitude[j],citibikedf$end.station.latitude[j]));
    
    
    if(temp1< sample2$minimum_distance_from_start[i]){
      
      sample2$minimum_distance_from_start[i]=temp1
      
      
      sample2$closest_station_from_start[i] = citibikedf$end.station.id[j]
      sample2$closest_station_name[i] = citibikedf$end.station.name[j]
      print(citibikedf$end.station.name[j])
    }
    
    
  }
  
  
}

##Storing the closest bike station from the dropoff point


for (i in 1:nrow(sample2)){
  
  
  sample2$minimum_distance_from_end[i] <- 1000
  
  
  for (j in 1:nrow(citibikedf)){
    
    temp=(earthDist(sample2$dropoff_longitude[i],sample2$dropoff_latitude[i],citibikedf$citilong[j],citibikedf$citilat[j]));
    
    
    if(temp< sample2$minimum_distance_from_end[i])
    {
      sample2$minimum_distance_from_end[i]=temp
      
      
      sample2$closest_station_end[i] = citibikedf$unique.citi_clean.end.station.id[j]
      
    }
    
  }
  
  
}

## The above code consumed a lot of time and hence decided to implement k- dimensional tree approach

## Ran the above code for a random sample
randomsample=taxi_clean[sample(nrow(taxi_clean), 100000,replace=FALSE), ]

##Ran the code for one day data

nrow(taxi_clean$tpep_dropoff_datetime[taxi_clean$tpep_dropoff_datetime <- "2016/05/01 12:17:31 AM"])

## Since the code took forever to run 100000 records, we decided to drop the above 2-D approach


######################################################################################################################


##Other Data Manipulation commands

#Renaming columns

library(plyr)

rename(sample2,c("taxi.total_amount"="taxi_amount" ))


names(sample2)[sample2$amount]<-paste("fare_amount");

colnames(sample2)[,"amount"] <- "fare_amount"

##########################################


sqldf("select closest_station,avg(minimum_distance),avg(fare_amount) from sample2  group by closest_station order by minimum_distance")


##Tried to plot the values of bike stations and found that bike stations are present only in
## Lower Manhattan and brooklyn area 

plot(df1$unique.citibike.start.station.latitude,df1$unique.citibike.start.station.longitude);

maxlat=max(df1$unique.citibike.start.station.latitude);
maxlong=max(df1$unique.citibike.start.station.longitude);
minlat=min(df1$unique.citibike.start.station.latitude);
minlong=min(df1$unique.citibike.start.station.longitude);



##############Change datetime format into month, date, year and time#################
taxi_clean1$tpep_pickup_datetime = as.POSIXlt(strptime(taxi_clean1$tpep_pickup_datetime,format="%Y/%m/%d %I:%M:%S %p"))
taxi_clean1$tpep_dropoff_datetime =as.POSIXlt(strptime(taxi_clean1$tpep_dropoff_datetime,format="%Y/%m/%d %I:%M:%S %p"))

taxi_clean1$tpep_pickup_date = as.Date(taxi_clean1$tpep_pickup_datetime,format="%Y/%m/%d")
taxi_clean1$tpep_drop_date = as.Date(taxi_clean1$tpep_dropoff_datetime,format="%Y/%m/%d")


sample$dropoff_time = format(sample$tpep_dropoff_datetime,format="%H:%M:%S")
sample$dropup_time = format(taxi$tpep_dropoff_datetime,format="%H:%M:%S")

write.csv(taxi_clean1,"C:/Users/Karpagam/Desktop/tripdata_cleansed/taxi_clean1.csv");


a=data.frame(unique(taxi_clean1$tpep_pickup_date));
a$no.ofrows<- (a$unique.taxi_clean1.tpep_pickup_date. %in% taxi_clean1$tpep_pickup_date)
  
library(data.table);

DT <- data.table(taxi_clean1);

DT[, (taxi_clean1$tpep_pickup_date),by=taxi_clean1$tpep_pickup_date]



taxi$pickup_day=weekdays(as.Date(taxi$tpep_pickup_date,'%d-%m-%Y'))
head(taxi)

taxi$hour = format(taxi$tpep_pickup_datetime,format="%H")




