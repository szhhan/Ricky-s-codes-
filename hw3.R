# question 1

# function: loads the Bay Area bike share trip data from a CSV and convert to RDS

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

trip.to.RDS("~/Desktop/sf_bikeshare_trips.csv", "~/Desktop/trip.rds")

# function: loads the Bay Area bike share station data from a CSV and convert to RDS

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

station.to.RDS("~/Desktop/sf_bike_share_stations.csv", "~/Desktop/station.rds")

# question 2

# read all trips and stations for Bay Area
setwd("/Users/ricky/Desktop")
tripdata1 = readRDS("trip.rds")
stationdata = readRDS("station.rds")

# subset the station for San Francisco
sf.station = subset(ba.station, landmark == "San Francisco")
sf.station = sf.station[!duplicated(sf.station$name),]

# subset the trip start from San Francisco
sf.station.names = sf.station$name
sf.trip = ba.trip[ba.trip$start_station_name %in% sf.station.names,]
sf.start.trips = as.numeric(table(sf.trip$start_station_id))
sf.station = data.frame(sf.station, ptsize = 150*sf.start.trips/sum(sf.start.trips))

#install_github("dkahle/ggmap")

library(ggmap)

# sf_map = get_map(location = 'San Francsico', zoom=13, maptype="satellite")
sf_map = get_map(location = 'San Francsico', zoom=13)
ggmap(sf_map, extent = "device", ylab = "Latitude", xlab = "Longitude", legend = "right") + 
  geom_point(aes(x=longitude, y=latitude, color = station_id), data = sf.station, size = sf.station$ptsize, alpha = 0.5) +
  geom_text(data = sf.station, aes(x = longitude, y = latitude, label = name), size = 1.5, vjust = 0, hjust = 0)

# question 3

la.trip.to.RDS = function(csv.path, rds.path){
  
  filenames = c("2016_q3_la_metro_trips.csv", "2016_q4_la_metro_trips.csv", "2017_q1_la_metro_trips.csv",
                "2017_q2_la_metro_trips.csv", "2017_q3_la_metro_trips.csv")
  all.csv.path = paste(csv.path,"/", filenames, sep='')
  la.list = lapply(all.csv.path, read.csv)
  names(la.list[[4]]) = names(la.list[[1]])
  names(la.list[[5]]) = names(la.list[[1]])
  la.trip = do.call("rbind", la.list)
  
  la.trip$trip_id = as.character(la.trip$trip_id)
  la.trip$duration = as.numeric(la.trip$trip_id)
  la.trip$start_time = strptime(la.trip$start_time, "%m/%d/%Y %H:%M", tz="UTC")
  la.trip$start_station_id = as.character(la.trip$start_station_id)
  la.trip$start_lat = as.numeric(la.trip$start_lat)
  la.trip$start_lon = as.numeric(la.trip$start_lon)
  la.trip$end_station_id = as.character(la.trip$end_station_id)
  la.trip$end_lat = as.numeric(la.trip$end_lat)
  la.trip$end_lon = as.numeric(la.trip$end_lon)
  la.trip$bike_id = as.character(la.trip$bike_id)
  la.trip$plan_duration = as.numeric(la.trip$plan_duration)
  la.trip$trip_route_category = as.character(la.trip$trip_route_category)
  la.trip$passholder_type = as.factor(la.trip$passholder_type)
  
  saveRDS(la.trip, file = rds.path)
  
}

la.trip.to.RDS("./bikes", "./la.trip.rds")

# function: loads the LA bike share station data from a CSV and convert to RDS

la.station.to.RDS = function(csv.path, rds.path){
  
  la.station = read.csv(csv.path)
  la.station$Station_ID = as.character(la.station$Station_ID)
  la.station$Station_Name = as.character(la.station$Station_Name)
  la.station$Go_live_date = as.character(la.station$Go_live_date)
  la.station$Go_live_date[1] = "07/07/2016"
  la.station$Go_live_date = as.Date(la.station$Go_live_date, "%m/%d/%Y")
  la.station$Region = as.factor(la.station$Region)
  la.station$Status = as.factor(la.station$Status)
  
  saveRDS(la.station, file = rds.path)
  
}

la.station.to.RDS("./bikes/metro-bike-share-stations-2017-10-20.csv", "./la.station.rds")

