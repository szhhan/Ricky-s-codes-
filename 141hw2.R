#1
#read the data into r:
library(readr)
airfare <- read_csv("~/Desktop/airfare.csv")
View(airfare)
library(readxl)
cpi_1996_2017 <- read_excel("~/Desktop/cpi_1996_2017.xlsx")
View(cpi_1996_2017)
#change the data types of columns:
airfare$city_id1=as.factor(airfare$city_id1)
airfare$city_id2=as.factor(airfare$city_id2)
airfare$airport_id1=as.factor(airfare$airport_id1)
airfare$airport_id2=as.factor(airfare$airport_id2)
airfare$year=as.factor(airfare$year)
airfare$quarter=as.factor(airfare$quarter)

#seperate data into two tables: 1a and 6:
data1=airfare[airfare$table=="1a",]
data2=airfare[airfare$table=="6",]

#2
#to look at the general scale:
summary(data1$year)
summary(data2$year)
#to check whether there have a time with no data:
table(data1$year,data1$quarter)
table(data2$year,data2$quarter)
#find missing value pattern for columns:
summary(sapply(data1,is.na),na.rm=TRUE)
summary(sapply(data2,is.na),na.rm=TRUE)
#find missing value pattern for time:
missingvalue=data1[rowSums(is.na(data1))>0,]
table(missingvalue$year,missingvalue$quarter)
#I use >4 because originally airport1, airport2, airportid1 and airport2 these 4 variables are NA for all data in table 6:
missingvalue2=data2[rowSums(is.na(data2))>4,]
table(missingvalue2$year,missingvalue2$quarter)


#3
#to find all cities into a vector that contains all the city names appearing in the data2
citi1=c(data2$city1[data2$year==2017],data2$city2[data2$year==2017])
#to find the time each city appearing in that vector in order to find the maximum and minimum:
sort(table(citi1),decreasing=TRUE)
sort(table(citi1),decreasing=FALSE)
# do the same for 2007 and 1997:
cities2007=c(data2$city1[data2$year==2007&data2$quarter==1],data2$city2[data2$year==2007&data2$quarter==1])
sort(table(cities2007),decreasing=TRUE)
sort(table(cities2007),decreasing=FALSE)
cities1997=c(data2$city1[data2$year==1997&data2$quarter==1],data2$city2[data2$year==1997&data2$quarter==1])
sort(table(cities1997),decreasing=TRUE)
sort(table(cities1997),decreasing=FALSE)

#increasing
#construct a new dataset that bind the information of cities together in 1997:
data1997=cbind(data2$city1[data2$year==1997],data2$city2[data2$year==1997],data2$quarter[data2$year==1997])
#to make sure it is in the form of dataframe:
data1997=as.data.frame(data1997)
#to give it the column names:
colnames(data1997)= c("city1","city2","quarter")
#since 2017's data only contain one quarter, we consider the data in 1997 and 2007 too as only contain 1 quarter:
data1997q1=data1997[data1997$quarter==1,]
#do the same thing for 2007 and 2017:
data2007=cbind(data2$city1[data2$year==2007],data2$city2[data2$year==2007],data2$quarter[data2$year==2007])
data2007=as.data.frame(data2007)
colnames(data2007)= c("city1","city2","quarter")
data2007q1=data2007[data2007$quarter==1,]
data2017=cbind(data2$city1[data2$year==2017],data2$city2[data2$year==2017],data2$quarter[data2$year==2017])
data2017=as.data.frame(data2017)
colnames(data2017)= c("city1","city2","quarter")

# to find the common cities between 2007 and 2017(the airports appear in both data2007 and data2017)
citi2=c(data2$city1[data2$year==2007 & data2$quarter==1],data2$city2[data2$year==2007 & data2$quarter==1])
common = intersect(citi1,citi2)
citi1
# to consider the airports in 2007 that appear in both 2007 and 2017:
realciti2=citi2[citi2 %in% common]
dim(table(realciti2))
# to see the number of time these airports appear:
t2=table(realciti2)
# do the same thing for 2017:
realciti1=citi1[citi1 %in% common]
t1=table(realciti1)
t1dim(t1)
# subtract two tables, the largest number is increasing the most:
sort(t1-t2,decreasing=TRUE)
sort(t1-t2,decreasing=FALSE)
#but there are some airports that may appear in 2017 but not in 2007:
unique(citi1)
unique(common)
#find these cities and check their appearing times in 2017 data
#I check all of them, but the code is ugly if I put all of them here
#So I only put one that will affect my conclusion here:
citi11=as.data.frame(citi1)
city111 = citi11[citi11$citi1=="Punta Gorda, FL",]
length(city111)

#then do the same step checking the 1997 to 2017:
citi3=c(data2$city1[data2$year==1997 & data2$quarter==1],data2$city2[data2$year==1997 & data2$quarter==1])
common2=intersect(citi1,citi3)
realciti3=citi3[citi3 %in% common2]
dim(table(realciti3))
t3=table(realciti3)
realciti11=citi1[citi1 %in% common2]
t11=table(realciti11)
dim(t11)
sort(t11-t3,decreasing=TRUE)
sort(t11-t3,decreasing=FALSE)
city112 = citi11[citi11$citi1=="Sanford, FL",]
length(city112)


#4
#construct a variable that paste the year and quarter together
#use to do the tapply later:
data1_time=paste(data1$year, data1$quarter, sep="_")
#make sure it is a factor:
data1_time=factor(data1_time)
# do the tapply to see the total of passengers for each quarter:
data1_passenger=tapply(data1$passengers,data1_time,sum)
# since the data is given per day, so we need to change it to per quarter:
data1_passenger2=data1_passenger*365/4
#or:
tapply(data1$passengers,list(data1$year,data1$quarter),sum)

#I want to draw the graph, but the data in 2016-2 is missing in table 1a, so i consider it as zero:
# I create a zero and put 2016_2 to match it:
data1_passengercorrect=c(data1_passenger2,0)
names(data1_passengercorrect)=c(names(data1_passenger2), "2016_2")
#order it so 2016-2 is not at the very ending of the data column:
data1_passengercorrect2 = data1_passengercorrect[order(names(data1_passengercorrect))]
# draw the overall time series graph for it:
data1_passengerfinal=ts(data1_passengercorrect2,frequency=4,start=c(1996,1))
ts.plot(data1_passengerfinal,ylab="population",main="total passengers over time(table1a)")

# Now i want to see the trend in different quarters:
# get out the data from the first quarter:
data1_quarter1=data1[data1$quarter=="1",]
# use the tapply, the same method as above:
data1_passengerq1=tapply(data1_quarter1$passengers,data1_quarter1$year,sum)
data1_passenger_q1_2=data1_passengerq1*365/4
# prepare for the graph for the first graph:
data1_passenger_q1final=ts(data1_passenger_q1_2,frequency=1,start=c(1996,1))

#do the same thing for quarter 2,3 and 4:
data1_quarter2=data1[data1$quarter=="2",]
data1_passengerq2=tapply(data1_quarter2$passengers,data1_quarter2$year,sum)
data1_passenger_q2_2=data1_passengerq2*365/4
data1_passenger_q2final=ts(data1_passenger_q2_2,frequency=1,start=c(1996,1))

data1_quarter3=data1[data1$quarter=="3",]
data1_passengerq3=tapply(data1_quarter3$passengers,data1_quarter3$year,sum)
data1_passenger_q3_2=data1_passengerq3*365/4
data1_passenger_q3final=ts(data1_passenger_q3_2,frequency=1,start=c(1996,1))

data1_quarter4=data1[data1$quarter=="4",]
data1_passengerq4=tapply(data1_quarter4$passengers,data1_quarter4$year,sum)
data1_passenger_q4_2=data1_passengerq4*365/4
data1_passenger_q4final=ts(data1_passenger_q4_2,frequency=1,start=c(1996,1))

# draw the overall graph containing the trends for 4 differnet quarters:
ts.plot(data1_passenger_q1final,data1_passenger_q2final,data1_passenger_q3final,data1_passenger_q4final, gpars = list(col = c("black", "red","green","blue")),ylab="population",main="Total passengers in different quarters(table1a)")
legend('topleft',c("quarter1","quarter2","quarter3","quarter4"),lty=1,col=c("black", "red","green","blue"),cex=0.55,pt.cex=1)


#Then do the same thing for table6, exactly the same way as above:
data2time=paste(data2$year, data2$quarter, sep="_")
data2time=factor(data2time)
data2passenger=tapply(data2$passengers,data2time,sum)
data2passenger2=data2passenger*365/4
data2passengerfinal=ts(data2passenger2,frequency=4,start=c(1996,1))
ts.plot(data2passengerfinal,ylab="population",main="total passengers over time(table6)")

data2_quarter1=data2[data2$quarter=="1",]
data2_passengerq1=tapply(data2_quarter1$passengers,data2_quarter1$year,sum)
data2_passenger_q1_2=data2_passengerq1*365/4
data2_passenger_q1final=ts(data2_passenger_q1_2,frequency=1,start=c(1996,1))

data2_quarter2=data2[data2$quarter=="2",]
data2_passengerq2=tapply(data2_quarter2$passengers,data2_quarter2$year,sum)
data2_passenger_q2_2=data2_passengerq2*365/4
data2_passenger_q2final=ts(data2_passenger_q2_2,frequency=1,start=c(1996,1))

data2_quarter3=data2[data2$quarter=="3",]
data2_passengerq3=tapply(data2_quarter3$passengers,data2_quarter3$year,sum)
data2_passenger_q3_2=data2_passengerq3*365/4
data2_passenger_q3final=ts(data2_passenger_q3_2,frequency=1,start=c(1996,1))

data2_quarter4=data2[data2$quarter=="4",]
data2_passengerq4=tapply(data2_quarter4$passengers,data2_quarter4$year,sum)
data2_passenger_q4_2=data2_passengerq4*365/4
data2_passenger_q4final=ts(data2_passenger_q4_2,frequency=1,start=c(1996,1))

ts.plot(data2_passenger_q1final,data2_passenger_q2final,data2_passenger_q3final,data2_passenger_q4final, gpars = list(col = c("black", "red","green","blue")),ylab="population",main="Total passengers in different quarters(table6)")
legend('topleft',c("quarter1","quarter2","quarter3","quarter4"),lty=1,col=c("black", "red","green","blue"),cex=0.55,pt.cex=1)



#5
#get the cip2 into a dataframe from an excel:
cpi2=cpi_1996_2017
cpi2=as.data.frame(cpi2)
# only consider the useful data:
cpi=cpi2[12:33,]
cpifinal=sapply(cpi,as.numeric)

#derive the average fare for each quarter(ie.Jan+Feb+Mar/3)
cpiq1=apply(cpifinal[,2:4], 1, mean,na.rm=TRUE)
cpiq2=apply(cpifinal[,5:7], 1, mean,na.rm=TRUE)
cpiq3=apply(cpifinal[,8:10], 1, mean,na.rm=TRUE)
cpiq4=apply(cpifinal[,11:13], 1, mean,na.rm=TRUE)

#transform into a good format and delete 86 to 88(since it contains NA)
cpiquarter=c(t(cbind(cpiq1,cpiq2,cpiq3,cpiq4)))
cpi_index=cpiquarter[-c(86:88)]

#match current cpi to each column and use the formula to match real fare by using the equation given:
cpi_input=sapply(as.character(data2time),
                    function(x) match(x,names(data2passenger)))
currentcpi=cpi_index[cpi_input]
real17_fare=data2$fare*cpi_index[length(cpi_index)]/currentcpi
#combine the original data set and the real fare. 
newdata2=cbind(data2,real17_fare)

#6
#calculate the real fare for each quarter:
quarter_fare=tapply(real17_fare,data2time,mean)
## create the time series graph for real fare:
quarter_fare_ts=ts(quarter_fare,frequency = 4, start=c(1996,1))
ts.plot(quarter_fare_ts,ylab="average fare(dollar)",main="Average fare across time(table6)")

#draw the plot for different quarters:
#get out the data from table6 for the first quarter:
newdata2_quarter1=newdata2[newdata2$quarter=="1",]
#tapply it getting out average for different years
data2_fareq1=tapply(newdata2_quarter1$real17_fare,newdata2_quarter1$year,mean)
# making the time series graph:
data2_fare_q1final=ts(data2_fareq1,frequency=1,start=c(1996,1))

#then do the same thing for quarter2,3 and 4:
newdata2_quarter2=newdata2[newdata2$quarter=="2",]
data2_fareq2=tapply(newdata2_quarter2$real17_fare,newdata2_quarter2$year,mean)
data2_fare_q2final=ts(data2_fareq2,frequency=1,start=c(1996,1))

newdata2_quarter3=newdata2[newdata2$quarter=="3",]
data2_fareq3=tapply(newdata2_quarter3$real17_fare,newdata2_quarter3$year,mean)
data2_fare_q3final=ts(data2_fareq3,frequency=1,start=c(1996,1))

newdata2_quarter4=newdata2[newdata2$quarter=="4",]
data2_fareq4=tapply(newdata2_quarter4$real17_fare,newdata2_quarter4$year,mean)
data2_fare_q4final=ts(data2_fareq4,frequency=1,start=c(1996,1))


#create the total time series graph for 4 quarters, adding the legend to explain:
ts.plot(data2_fare_q1final,data2_fare_q2final,data2_fare_q3final,data2_fare_q4final, gpars = list(col = c("black", "red","green","blue")),ylab="average fare(dollar)",main="average fare in different quarters(table6)")
legend('top',c("quarter1","quarter2","quarter3","quarter4"),lty=1,col=c("black", "red","green","blue"),cex=0.55,pt.cex=1)




#7

# data1
#get out the data of 2015:
data1_2015=data1[data1$year=="2015",]
#plot the plot with all the data considering miles and fare:
plot(x=data1_2015$miles,y=data1_2015$fare,main="fare vs. distance in 2015",xlab="distance(miles)",ylab="fare(dollar)")
# construct the model:
fit1 = lm(fare~miles, data=data1_2015)
#look at the model and coefficients:
summary(fit1)

# residuals plot
par(mfrow=c(2,2))
plot(fit1)
par(mfrow=c(1,1))

# data2
#do the same thing for table 6:
data2_2015=data2[data2$year=="2015",]
plot(x=data2_2015$miles,y=data2_2015$fare,main="fare vs. distance in 2015(table6)",xlab="distance(miles)",ylab="fare(dollar)")
fit3 = lm(fare~miles, data=data2_2015)
summary(fit3)


# residuals plot
par(mfrow=c(2,2))
plot(fit3)
par(mfrow=c(1,1))


# q8
# create the model regress fare on passengers and distance:
fit5 = lm(fare~passengers+miles, data=data1_2015)
# look at the summary and the coefficients:
summary(fit5)
par(mfrow=c(2,2))
# residual plot:
plot(fit5)
par(mfrow=c(1,1))
#do the same thing on table 6:
fit6 = lm(fare~passengers+miles, data=data2_2015)
summary(fit6)
par(mfrow=c(2,2))
plot(fit6)
par(mfrow=c(1,1))

# q9
# create the dataset with all lg_fare < average fare:
d2 = as.data.frame(data2)
d2_2015 = d2[d2$year==2015,]
d2_belowaverage = d2_2015[d2_2015$lg_fare < d2_2015$fare,]
# create the opposite using to compare
d2_aboveaverage = d2_2015[d2_2015$lg_fare > d2_2015$fare,]
# see the table of companies:
table(d2_belowaverage$lg_carrier)
table(d2_aboveaverage$lg_carrier)
# compare the passengers:
quantile(d2_belowaverage$passengers,na.rm=TRUE,prob=seq(0,1,length=11))
quantile(d2_aboveaverage$passengers,na.rm=TRUE,prob=seq(0,1,length=11))
mean(d2_belowaverage$passengers,na.rm=TRUE)
mean(d2_aboveaverage$passengers,na.rm=TRUE)
median(d2_belowaverage$passengers,na.rm=TRUE)
median(d2_aboveaverage$passengers,na.rm=TRUE)


#10
# use the same method in question 5 to match the real fare for table 1a:
data1time=paste(data1$year, data1$quarter, sep="_")
data1time=factor(data1time)
cpi_input2=sapply(as.character(data1time),
                 function(x) match(x,names(data2passenger)))
currentcpi2=cpi_index[cpi_input2]
real17_fare2=data1$fare*cpi_index[length(cpi_index)]/currentcpi2
newdata1=cbind(data1,real17_fare2)

d1 = as.data.frame(newdata1)
d1_airports = d1[(d1$airport1 %in% c("SMF", "OAK", "SFO", "SJC")) | (d1$airport2 %in% c("SMF", "OAK", "SFO", "SJC")),]

# fare comparison
tapply(d1_airports$real17_fare2, factor(d1_airports$airport1=="SMF"|d1_airports$airport2=="SMF"), mean)
tapply(d1_airports$real17_fare2, factor(d1_airports$airport1=="OAK"|d1_airports$airport2=="OAK"), mean)
tapply(d1_airports$real17_fare2, factor(d1_airports$airport1=="SFO"|d1_airports$airport2=="SFO"), mean)
tapply(d1_airports$real17_fare2, factor(d1_airports$airport1=="SJC"|d1_airports$airport2=="SJC"), mean)


#or:
#select out the data sets with only containing these specific airports:
d1_smf=d1[d1$airport1 =="SMF" | d1$airport2=="SMF",]
d1_OAK=d1[d1$airport1 =="OAK" | d1$airport2=="OAK",]
d1_SFO=d1[d1$airport1 =="SFO" | d1$airport2=="SFO",]
d1_SJC=d1[d1$airport1 =="SJC" | d1$airport2=="SJC",]
#find the average of them:
mean(d1_smf$real17_fare2)
mean(d1_OAK$real17_fare2)
mean(d1_SFO$real17_fare2)
mean(d1_SJC$real17_fare2)
#look at the box plot for more detail:
boxplot(d1_smf$real17_fare2,d1_OAK$real17_fare2,d1_SFO$real17_fare2,d1_SJC$real17_fare2,names=c("SMF","OAK","SFO","SJC"),ylim=c(0,1500))

#seperate
#look at the fare when only considering airport1 or 2:
d1_airports1 = d1[(d1$airport1 %in% c("SMF", "OAK", "SFO", "SJC")),]
tapply(d1_airports1$real17_fare2, factor(d1_airports1$airport1),mean)

d1_airports2 = d1[(d1$airport2 %in% c("SMF", "OAK", "SFO", "SJC")),]
tapply(d1_airports2$real17_fare2, factor(d1_airports2$airport2),mean)

# longest distance
#look at the maximum distance:
sort(d1_smf$miles,decreasing=TRUE)
sort(d1_OAK$miles,decreasing=TRUE)
sort(d1_SFO$miles,decreasing=TRUE)
sort(d1_SJC$miles,decreasing=TRUE)


#i set the long distance flying index to miles>2500
# and check their dimensions to get the number of long time flyings:
d1_OAKlong=d1_OAK[d1_OAK$miles>2500,]
dim(d1_OAKlong)
d1_SFOlong=d1_SFO[d1_SFO$miles>2500,]
dim(d1_SFOlong)
d1_SJClong=d1_SJC[d1_SJC$miles>2500,]
dim(d1_SJClong)
d1_smflong=d1_smf[d1_smf$miles>2500,]
dim(d1_smflong)

#by year fare
# the same method i have done for question4, tapply the real fare across quarters:
datasmftime=paste(d1_smf$year, d1_smf$quarter, sep="_")
datasmftime=factor(datasmftime)
smffare= tapply(d1_smf$real17_fare2,datasmftime,mean)
smffare_correct=c(smffare,0)
names(smffare_correct)=c(names(smffare), "2016_2")
smffare_correct = smffare_correct[order(names(smffare_correct))]
smffare_ts=ts(smffare_correct,frequency = 4, start=c(1996,1))

# do the same for 4 different airports:
dataOAKtime=paste(d1_OAK$year, d1_OAK$quarter, sep="_")
dataOAKtime=factor(dataOAKtime)
OAKfare= tapply(d1_OAK$real17_fare2,dataOAKtime,mean)
OAKfare_correct=c(OAKfare,0)
names(OAKfare_correct)=c(names(OAKfare), "2016_2")
OAKfare_correct = OAKfare_correct[order(names(OAKfare_correct))]
OAKfare_ts=ts(OAKfare_correct,frequency = 4, start=c(1996,1))

dataSFOtime=paste(d1_SFO$year, d1_SFO$quarter, sep="_")
dataSFOtime=factor(dataSFOtime)
SFOfare= tapply(d1_SFO$real17_fare2,dataSFOtime,mean)
SFOfare_correct=c(SFOfare,0)
names(SFOfare_correct)=c(names(SFOfare), "2016_2")
SFOfare_correct = SFOfare_correct[order(names(SFOfare_correct))]
SFOfare_ts=ts(SFOfare_correct,frequency = 4, start=c(1996,1))

dataSJCtime=paste(d1_SJC$year, d1_SJC$quarter, sep="_")
dataSJCtime=factor(dataSJCtime)
SJCfare= tapply(d1_SJC$real17_fare2,dataSJCtime,mean)
SJCfare_correct=c(SJCfare,0)
names(SJCfare_correct)=c(names(SJCfare), "2016_2")
SJCfare_correct = SJCfare_correct[order(names(SJCfare_correct))]
SJCfare_ts=ts(SJCfare_correct,frequency = 4, start=c(1996,1))

# plot them together and add the legend:
ts.plot(smffare_ts, OAKfare_ts,SFOfare_ts,SJCfare_ts, gpars = list(col = c("black", "red","green","blue")),main="average fare for 4 different airports across years",ylab="average fare(dollar)")
legend('bottomleft',c("SMF","OAK","SFO","SJC"),lty=1,col=c("black", "red","green","blue"),cex=0.55,pt.cex=1)

#by year miles
#the same method as above, only change the fare to length:
datasmflongtime=paste(d1_smflong$year, d1_smflong$quarter, sep="_")
datasmflongtime=factor(datasmflongtime)
smfmile= tapply(d1_smflong$miles,datasmflongtime,length)
smfmile_correct=c(smfmile,0)
names(smfmile_correct)=c(names(smfmile), "2016_2")
smfmile_correct = smfmile_correct[order(names(smfmile_correct))]
smf_ts2=ts(smfmile_correct,frequency = 4, start=c(1996,1))

dataOAKlongtime=paste(d1_OAKlong$year, d1_OAKlong$quarter, sep="_")
dataOAKlongtime=factor(dataOAKlongtime)
OAKmile= tapply(d1_OAKlong$miles,dataOAKlongtime,length)
OAKmile_correct=c(OAKmile,0)
names(OAKmile_correct)=c(names(OAKmile), "2016_2")
OAKmile_correct = OAKmile_correct[order(names(OAKmile_correct))]
OAK_ts2=ts(OAKmile_correct,frequency = 4, start=c(1996,1))

dataSFOlongtime=paste(d1_SFOlong$year, d1_SFOlong$quarter, sep="_")
dataSFOlongtime=factor(dataSFOlongtime)
SFOmile= tapply(d1_SFOlong$miles,dataSFOlongtime,length)
SFOmile_correct=c(SFOmile,0)
names(SFOmile_correct)=c(names(SFOmile), "2016_2")
SFOmile_correct = SFOmile_correct[order(names(SFOmile_correct))]
SFO_ts2=ts(SFOmile_correct,frequency = 4, start=c(1996,1))

dataSJClongtime=paste(d1_SJClong$year, d1_SJClong$quarter, sep="_")
dataSJClongtime=factor(dataSJClongtime)
SJCmile= tapply(d1_SJClong$miles,dataSJClongtime,length)
SJCmile_correct=c(SJCmile,0)
names(SJCmile_correct)=c(names(SJCmile), "2016_2")
SJCmile_correct = SJCmile_correct[order(names(SJCmile_correct))]
SJC_ts2=ts(SJCmile_correct,frequency = 4, start=c(1996,1))

ts.plot(smf_ts2,OAK_ts2,SFO_ts2,SJC_ts2, gpars = list(col = c("black", "red","green","blue")),main="long distance flying comparison for 4 airports across years",ylab="number of long distance flying")
legend('bottom',c("smf","OAK","SFO","SJC"),lty=1,col=c("black", "red","green","blue"),cex=0.55,pt.cex=1)






