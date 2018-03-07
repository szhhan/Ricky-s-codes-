# question 1

# function: loads the Bay Area bike share trip data from a CSV and convert to RDS
#the function that read the exvel and transfrom each column to the appropriate type
# save the rds file out 
trip.to.RDS = function(csv.path, rds.path){
  
  trip = read.csv(csv.path)
  
  trip$trip_id = as.character(trip$trip_id)
  trip$duration_sec = as.numeric(trip$duration_sec)
  trip$start_date = as.POSIXct(trip$start_date, tz="UTC")
  trip$start_station_name = as.character(trip$start_station_name)
  trip$start_station_id = as.character(trip$start_station_id)
  trip$end_date = as.POSIXct(trip$end_date, tz="UTC")
  trip$end_station_name = as.character(trip$end_station_name)
  trip$end_station_id = as.character(trip$end_station_id)
  trip$bike_number = as.numeric(trip$bike_number)
  trip$zip_code = as.character(trip$zip_code)
  trip$subscriber_type = as.factor(trip$subscriber_type)
  
  saveRDS(trip, file = rds.path)
  
}

#use the function to read bay area trip data and put the output at my Desktop in the rds form
trip.to.RDS("~/Desktop/sf_bikeshare_trips.csv", "~/Desktop/trip.rds")

# function: loads the Bay Area bike share station data from a CSV and convert to RDS
# the same as we did for the trip data
station.to.RDS = function(csv.path, rds.path){
  
  station = read.csv(csv.path)
  
  station$station_id = as.character(station$station_id)
  station$name = as.character(station$name)
  station$latitude = as.numeric(station$latitude)
  station$longitude = as.numeric(station$longitude)
  station$dockcount = as.numeric(station$dockcount)
  station$landmark = as.character(station$landmark)
  station$installation_date = as.Date(station$installation_date, "%Y-%m-%d")
  
  saveRDS(station, file = rds.path)
  
}

#use this function to load the station data and put it at the Desktop
station.to.RDS("~/Desktop/sf_bike_share_stations.csv", "~/Desktop/station.rds")

# question 2

# read all trips and stations for Bay Area
setwd("/Users/ricky/Desktop")
tripdata1 = readRDS("trip.rds")
stationdata = readRDS("station.rds")

# subset the station for San Francisco
sf_station2 = subset(stationdata, landmark == "San Francisco")
# make sure there is no duplicated stations
duplicated(sf_station2$name)
unique(sf_station2$name)
sf_station = sf_station2[!duplicated(sf_station2$name),]

# subset the trip start from San Francisco
sfname = sf_station$name
sftrip = tripdata1[tripdata1$start_station_name %in% sfname,]
table(sftrip$start_station_id)
sfstarttrip = as.numeric(table(sftrip$start_station_id))
#make the size of each station, useful for later to control the size of each station point
sf_stationfinal = data.frame(sf_station, size = 160*sfstarttrip/sum(sfstarttrip))

#install.packages("devtools")
library(devtools)
#install_github("dkahle/ggmap")
library(ggmap)
#install.packages("ggrepel")
library(ggrepel)

#get the SF map from the internet 
sf_map = get_map(location = 'San Francsico', zoom=14, maptype="terrain")
#locate the center of the map, or the interval of the graph we want 
sf_center = sapply(sf_stationfinal[c("longitude","latitude")],function(x) mean(range(x)))
# make the map
sf_map = get_map(sf_center, zoom=14)

ggmap(sf_map, ylab = "Latitude", xlab = "Longitude") + 
  geom_point(aes(x=longitude, y=latitude, color = station_id), data = sf_stationfinal, size = sf_stationfinal$size, alpha = 0.9) +
  geom_label_repel(data = sf_stationfinal, aes(x = longitude, y = latitude, label = name), size = 2) +
  labs(title="The map of the San Francisco")

# question 3

#make a function to read the 5 csv together 
# change the column name of 4th and 5th csv in order to avoid error
#change the column class to the approriate classes. Especially the time. 
trip2.to.RDS = function(csv.path, rds.path){
  
  filenames = c("2016_q3_la_metro_trips.csv", "2016_q4_la_metro_trips.csv", "2017_q1_la_metro_trips.csv",
                "2017_q2_la_metro_trips.csv", "2017_q3_la_metro_trips.csv")
  allcsv.path = paste(csv.path,"/", filenames, sep='')
  lalist = lapply(allcsv.path, read.csv)
  names(lalist[[4]]) = names(lalist[[1]])
  names(lalist[[5]]) = names(lalist[[1]])
  trip2 = do.call("rbind", lalist)
  
  trip2$trip_id = as.character(trip2$trip_id)
  trip2$duration = as.numeric(trip2$trip_id)
  trip2$start_time = strptime(trip2$start_time, "%m/%d/%Y %H:%M", tz="UTC")
  trip2$end_time = strptime(tripdata2$end_time, "%m/%d/%Y %H:%M", tz="UTC")
  trip2$start_station_id = as.character(trip2$start_station_id)
  trip2$start_lat = as.numeric(trip2$start_lat)
  trip2$start_lon = as.numeric(trip2$start_lon)
  trip2$end_station_id = as.character(trip2$end_station_id)
  trip2$end_lat = as.numeric(trip2$end_lat)
  trip2$end_lon = as.numeric(trip2$end_lon)
  trip2$bike_id = as.character(trip2$bike_id)
  trip2$plan_duration = as.numeric(trip2$plan_duration)
  trip2$trip_route_category = as.character(trip2$trip_route_category)
  trip2$passholder_type = as.factor(trip2$passholder_type)
  
  saveRDS(trip2, file = rds.path)
  
}

# read the csv as we did the same in the sf trip. 
trip2.to.RDS("~/Desktop", "~/Desktop/filename.rds")
# read the data 
tripdata2 = readRDS("filename.rds")

# function: loads the LA bike share station data from a CSV and convert to RDS

# the same as i did for the sf station 
station2.to.RDS = function(csv.path, rds.path){
  
  station2 = read.csv(csv.path)
  station2$Station_ID = as.character(station2$Station_ID)
  station2$Station_Name = as.character(station2$Station_Name)
  station2$Go_live_date = as.character(station2$Go_live_date)
  station2$Go_live_date[1] = "07/07/2016"
  station2$Go_live_date = as.Date(station2$Go_live_date, "%m/%d/%Y")
  station2$Region = as.factor(station2$Region)
  station2$Status = as.factor(station2$Status)
  
  saveRDS(station2, file = rds.path)
  
}

# read the data, as the same above. 
station2.to.RDS("~/Desktop/metro-bike-share-stations-2017-10-20.csv", "~/Desktop/station2.rds")
stationdata2 = readRDS("station2.rds")

#Question 4:
#subset the station for downtown LA
la_station2 = subset(stationdata2, Region == "DTLA")
# make sure there is no duplicated stations 
la_station = la_station2[!duplicated(la_station2$Station_Name),]
# subset the downtown la trips
laid = la_station$Station_ID
latrip = tripdata2[tripdata2$start_station_id %in% laid,]

# merge the location point to the station data from the trip data
locations = latrip[c("start_station_id","start_lon","start_lat")]
names(locations)=c("Station_ID","longtitude","latitude")
locations = locations[!duplicated(locations),]
station2final = merge(stationdata2,locations,by="Station_ID")

#recheck the stations again, be sure they are all in downtown La and there is no duplicate 
la_station2 = subset(station2final, Region == "DTLA")
duplicated(la_station2$Station_Name)
unique(la_station2$Station_Name)
la_station = la_station2[!duplicated(la_station2$Station_Name),]


# subset the trip start from LA
laid = la_station$Station_ID
latrip = tripdata2[tripdata2$start_station_id %in% laid,]
# investigate the stations correlated to the trips
table(latrip$start_station_id)
sort(unique(latrip$start_station_id))
sort(unique(la_station$Station_ID))
lastarttrip = as.numeric(table(latrip$start_station_id))
la_stationfix=la_station[-127]
# construct the size for the usage later (control the size of each station point)
la_stationfinal = data.frame(la_stationfix, size = 200*lastarttrip/sum(lastarttrip))

#install.packages("devtools")
library(devtools)
#install_github("dkahle/ggmap")
library(ggmap)

# get the la downtown map from the internet
la_map = get_map(location = 'DTLA', zoom=14, maptype="satellite")
# set the range as before
la_center = sapply(la_stationfinal[c("longtitude","latitude")],function(x) mean(range(x)))
# make the map
la_map = get_map(location = 'DTLA', zoom=14)
ggmap(la_map, ylab = "Latitude", xlab = "Longtitude", legend = "right") + 
  geom_point(aes(x=longtitude, y=latitude, color = Station_ID), data = la_stationfinal, size = la_stationfinal$size, alpha = 0.8) +
  geom_label_repel(data = la_stationfinal, aes(x = longtitude, y = latitude, label = Station_Name), size = 2) +
  labs(title="the map of Los Angels")
 

#5
#install.packages("geosphere")
#install.packages("lubridate")
library(geosphere)
library(lubridate)
#make the function to cut the time into 4 pieces
hourcut = function(x) {
  hr = hour(x)
  hours_cut = cut(hr,breaks = c(-Inf,6,10,15,19,Inf),right=FALSE,
                  labels = c("Fix","Morning","Noon","EVE","Night"))
  levels(hours_cut)[[1]]="Night"
  hours_cut
}

#hourcut the time for bay area data and la data 
tripdata1$hour_time = hourcut(tripdata1$start_date)
tripdata2$hour_time = hourcut(tripdata2$start_time)

#checking no duplicates 
stationfinal = stationdata[!duplicated(stationdata),]

#merge the start and end longitude to the trip data
# use for calculating the distance of each trip 
sfstartlocation = stationfinal[c("station_id","latitude","longitude")]
names(sfstartlocation)=c("start_station_id","startlatitude","startlongitude")
baystartlocation = sfstartlocation[!duplicated(sfstartlocation$start_station_id),]
baytripfinal = merge(tripdata1,baystartlocation,by="start_station_id")


bayendlocation = stationfinal[c("station_id","latitude","longitude")]
names(bayendlocation)=c("end_station_id","endlatitude","endlongitude")
bayendlocation = bayendlocation[!duplicated(bayendlocation$end_station_id),]
baytripfinal2 = merge(baytripfinal,bayendlocation,by="end_station_id")


#install.packages("data.table")
library(data.table)

#make a function to calculate the distance for every row
setDT(baytripfinal2)[ , dist_km := distGeo(matrix(c(startlongitude, startlatitude), ncol = 2), 
                                 matrix(c(endlongitude, endlatitude), ncol = 2))] 

#sftripfinal3$distance = distGeo(sftripfinal3[,c(15,14)],sftripfinal3[,c(17,16)])
#find the frequency plot
barplot(table(baytripfinal2$hour_time),main="frequency in different time periods(bay)",xlab="Time period",ylab="number of trips",ylim=c(0,350000))
#exclude outliers and the roundtrip data
baytripfinal2out = baytripfinal2[baytripfinal2$dist_km<4000]
baytripout3 = baytripfinal2out[baytripfinal2out$dist_km>0]
# make the distance plot 
boxplot(dist_km~hour_time,data=baytripout3,xlab="time period",ylab="distance(m)",main="bay area distance in different time periods")
#calculate the duration of each trip
baytripfinal2$dura = baytripfinal2$end_date - baytripfinal2$start_date
baytripfinal2$dura = as.numeric(baytripfinal2$dura)
baytripfinal2$dura = baytripfinal2$dura * 60
# make the duration plot 
boxplot(dura~hour_time,data=baytripfinal2,ylim=c(0,2700),ylab="duration(seconds)",xlab="time periods",main="Bay area duration in different time periods")

# checking the subsets for 4 different time periods
baymorning = baytripfinal2[baytripfinal2$hour_time=="Morning",]
baynoon = baytripfinal2[baytripfinal2$hour_time=="Noon",]
bayevening = baytripfinal2[baytripfinal2$hour_time=="EVE",]
baynight = baytripfinal2[baytripfinal2$hour_time=="Night",]


#for la: 
# do the same as i did before for sf
# just the similar code, same idea 
setDT(tripdata2)[ , dist_km := distGeo(matrix(c(start_lon, start_lat), ncol = 2), 
                                          matrix(c(end_lon, end_lat), ncol = 2))] 

barplot(table(tripdata2$hour_time),main="frequency in different time periods(la)",xlab="Time period",ylab="number of trips",ylim=c(0,300000))
latripanayl = tripdata2[tripdata2$dist_km<4000]
latripanayl2 = latripanayl[latripanayl$dist_km>0]
boxplot(dist_km~hour_time,data=latripanayl2,ylab="distance(m)",xlab="time periods",main="la data for distance in different time periods")


tripdata2$dura = tripdata2$end_time - tripdata2$start_time
tripdata2$dura = as.numeric(tripdata2$dura)
tripdata2$dura = tripdata2$dura * 60
boxplot(dura~hour_time,data=tripdata2,ylim=c(0,2700),ylab="duration(seconds)",xlab="time periods",main="la data for duration in different time periods")



#6
#checking no duplicates 
stationfinal5 = stationdata[!duplicated(stationdata$station_id),]
#merge the landmark to the trip data (use to subset out SF data from the whole bay data)
stationarea = stationfinal5[c("station_id","landmark")]
names(stationarea)=c("start_station_id","landmark")
baytripfinal3 = merge(baytripfinal2,stationarea,by="start_station_id")
# subset the SF data
sftripfinal3 = subset(baytripfinal3, landmark == "San Francisco")
# eliminate the round trip data, which is insignificant 
sftripfinal4 = sftripfinal3[sftripfinal3$dist_km>0]


#stationarea2 = sf_station[c("station_id","landmark")]
#names(stationarea2)=c("start_station_id","landmark")
#baytripfinal4 = merge(baytripfinal2,stationarea2,by="start_station_id")

# calculate the bearing for each trip
sftripfinal4 = 
setDT(sftripfinal4)[ , angle := bearing(matrix(c(startlongitude, startlatitude), ncol = 2), 
                                          matrix(c(endlongitude, endlatitude), ncol = 2))]
#subset to different time periods
morning6 = sftripfinal4[sftripfinal4$hour_time=="Morning"]
noon6 = sftripfinal4[sftripfinal4$hour_time=="Noon"]
eve6 = sftripfinal4[sftripfinal4$hour_time=="EVE"]
night6 = sftripfinal4[sftripfinal4$hour_time=="Night"]

# make the plot for 4 different time periods
g1= ggplot(morning6,aes(angle))+geom_histogram()+coord_polar(start=3.05) + labs(title="Bearing in the morning",y="frequency")
g2=ggplot(noon6,aes(angle))+geom_histogram()+coord_polar(start=3.05) + labs(title="Bearing in the noon",y="frequency")
g3=ggplot(eve6,aes(angle))+geom_histogram()+coord_polar(start=3.05) + labs(title="Bearing in the evening",y="frequency")
g4=ggplot(night6,aes(angle))+geom_histogram()+coord_polar(start=3.05) + labs(title="Bearing in the night",y="frequency")

# put these 4 plots together into a single one 
source("http://peterhaschke.com/Code/multiplot.R")
multiplot(g1,g2,g3,g4,cols=2)






