collegeData <- readRDS("/Users/ricky/Downloads/college_scorecard_2013.rds")

#1
#to see the number of observations:
dim(collegeData)
#there are 3312 observations:(by this code)
length(collegeData$name)
#there are data with universities having multiple campus.
#we only want to calculate the unique universities.
#which is 2418,not 3312. by this code:
length(levels(as.factor(collegeData$ope_id)))
#if we want to see excatly which universities has multiple campus:
vec3<-table(collegeData$ope_id)
vec3[which(vec3>1)]   


#2
#to see numbers of features:
ncol(collegeData)
#to discriminate the number of categorical and discrete data:
table(sapply(collegeData,class))
#following codes are not useful to hw1, it's for my future use:
#usage is to find the number of different classes by not using table.
isfac <- sapply(collegeData,is.factor)
sum(isfac,na.rm=TRUE)
is.log <- sapply(collegeData,is.logical)
sum(is.log,na.rm=TRUE)
is.char <- sapply(collegeData,is.character)
sum(is.char,na.rm=TRUE)


#3
#to find the number of na's:
isna <- sapply(collegeData,is.na)
sum(isna,na.rm=TRUE)
#to find which column has the most na's:
which.max(colSums(isna))
#try to find the pattern:
summary(isna)
colSums(is.na(collegeData))
colnames(collegeData)[colSums(is.na(collegeData)) > 1800]


#4
#to find there's more public schools or private schools:
table(collegeData$ownership)
#to find out the number of each highest degrees in public and private:
privateschools<- table(collegeData$highest_degree[collegeData$ownership!="Public"])
publicschool = table(collegeData$highest_degree[collegeData$ownership=="Public"])
#to make these numbers into proportions: 
prop.table(privateschools)
prop.table(publicschool)
#Begin to make the mosaic graph to illustrate the phenomenon:
#first to put the above data(numbers) together:
highestdegreegraph <- cbind(public = publicschool,private = privateschools)
#set the rownames:
newrowname = c("O","C","As","BA","MS")
dimnames(highestdegreegraph) = list(highestDegree = newrowname, schoolType = c("public", "private"))
#substitute the rownames:
rownames(highestdegreegraph)=newrowname
rownames(highestdegreegraph)
#make the mosaic graph:
install.package("vcd")
library("vcd")
mosaic(highestdegreegraph,shade = TRUE, main="proportions of highest degree in public and private school")


#5
#find the mean,median,quantile of the undergrad population
#number
mean(collegeData$undergrad_pop,na.rm=TRUE)
median(collegeData$undergrad_pop,na.rm=TRUE)
quantile(collegeData$undergrad_pop,probs=seq(0,1,length=11),na.rm=TRUE)
#plot the density plot of undergrad population
plot(density(collegeData$undergrad_pop,na.rm=TRUE),lwd=4,main="undergraduate population density distribution graph",xlab="undergrad population(for different schools)")
#add the line for the mean,make it blue
abline(v = mean(collegeData$undergrad_pop,na.rm=TRUE),col = "royalblue",lwd=2)
#add the line for median, make it green
abline(v = median(collegeData$undergrad_pop,na.rm=TRUE),col = "green",lwd=2)
#add the line for quantiles, make them red
abline(v = quantile(collegeData$undergrad_pop,probs=seq(0.1,1,length=10),na.rm=TRUE),col = "red",lty=8)
#insert legends to explian the lines 
#also,illustrate numbers into the graph.
legend('top',c("mean=3599.502","median=1295","decile(from0%to100%):\n  0,  153,  319.2,  536,  847.6\n,  1295,  1811.8,  2674.5,  4550.8,  9629.8,  166816"),lty=c(1,1,8),col=c('royalblue',  'green',' red'),cex=0.55,pt.cex=1)


#6
#to select the data from 5 most populous state from the big data:
popstate2 <- which(collegeData$state %in% c("CA","FL","TX","NY","IL"))
popstate22 <- collegeData[popstate2,]

#To graph 5 seperate tuition density graphs. Check the ylim:
ca = which(collegeData$state == "CA")
CA <- collegeData[ca,]
plot(density(CA$tuition,na.rm=TRUE))
fl = which(collegeData$state == "FL")
FL <- collegeData[fl,]
plot(density(FL$tuition,na.rm=TRUE))
tx = which(collegeData$state == "TX")
TX <- collegeData[tx,]
plot(density(TX$tuition,na.rm=TRUE))
ny = which(collegeData$state == "NY")
NY <- collegeData[ny,]
plot(density(NY$tuition,na.rm=TRUE))
il = which(collegeData$state == "IL")
IL <- collegeData[il,]
plot(density(IL$tuition,na.rm=TRUE))

#then plot these graphs into one single density plot:
plot(density(FL$tuition,na.rm=TRUE),main="tuition density plot for 5 most populous states",xlab="tuition(dollar)")
lines(density(CA$tuition,na.rm=TRUE),col='red')
lines(density(TX$tuition,na.rm=TRUE),col='blue')
lines(density(NY$tuition,na.rm=TRUE),col='green')
lines(density(IL$tuition,na.rm=TRUE),col='purple')
legend('topright',c("CA","FL","TX","NY","IL"),lty=1,col=c('red',  'black','blue','green','purple'))
#to make it more clear, insert the abline of means:
abline(v=mean(FL$tuition,na.rm=TRUE))
abline(v=mean(CA$tuition,na.rm=TRUE),col='red')
abline(v=mean(TX$tuition,na.rm=TRUE),col='blue')
abline(v=mean(NY$tuition,na.rm=TRUE),col='green')
abline(v=mean(IL$tuition,na.rm=TRUE),col='purple')
#to make boxplots of tuition for these 5 states:
top5 <- collegeData[collegeData$state %in% c("CA","FL","NY","TX","IL"),]
tuitiondata = top5[c("state","tuition")]
tmp=as.character(tuitiondata$state)
tuitiondata$state2=tmp
boxplot(data=tuitiondata,tuition~state2,xlab="states",ylab="tuition(dollar)",main="boxplots for tuitions in 5 most populous states")

#for nonresident tuition:
#then plot graphs for non-resid tuition of 5 states into one single density plot:
plot(density(FL$tuition_nonresident,na.rm=TRUE),main="nonresident tuition density plot for 5 most populous states",xlab="tuition(dollar)")
lines(density(CA$tuition_nonresident,na.rm=TRUE),col='red')
lines(density(TX$tuition_nonresident,na.rm=TRUE),col='blue')
lines(density(NY$tuition_nonresident,na.rm=TRUE),col='green')
lines(density(IL$tuition_nonresident,na.rm=TRUE),col='purple')
legend('topright',c("CA","FL","TX","NY","IL"),lty=1,col=c('red',  'black','blue','green','purple'))
#to make it more clear, insert the abline of means:
abline(v=mean(FL$tuition_nonresident,na.rm=TRUE))
abline(v=mean(CA$tuition_nonresident,na.rm=TRUE),col='red')
abline(v=mean(TX$tuition_nonresident,na.rm=TRUE),col='blue')
abline(v=mean(NY$tuition_nonresident,na.rm=TRUE),col='green')
abline(v=mean(IL$tuition_nonresident,na.rm=TRUE),col='purple')
#then make the boxplot:
top5 <- collegeData[collegeData$state %in% c("CA","FL","NY","TX","IL"),]
nonrestuitiondata = top5[c("state","tuition_nonresident")]
tmp2=as.character(nonrestuitiondata$state)
nonrestuitiondata$state3=tmp2
boxplot(data=nonrestuitiondata,tuition_nonresident~state3,xlab="states",ylab="tuition(dollar)",main="boxplots for nonresident tuitions in 5 most populous states")

#7
#use the scatter smooth function do draw the plot of 
#average 10yrs salary and their spend per student:
scatter.smooth(x=collegeData$spend_per_student,y=collegeData$avg_10yr_salary,col='red',main="average10year salaries vs. spend per student",xlab="spend per student(dollar)",ylab="average 10 year salaries(dollar)")
#then conducted the fitted line for it:
abline(lm(collegeData$avg_10yr_salary~collegeData$spend_per_student),col='blue')
#do the same for the median 10 year salaries:
scatter.smooth(x=collegeData$spend_per_student,y=collegeData$med_10yr_salary,col='red',main="median10year salaries vs. spend per student",xlab="spend per student(dollar)",ylab="median 10 year salaries(dollar)")
abline(lm(collegeData$med_10yr_salary~collegeData$spend_per_student),col='blue')
#derive the slope of the fitted line
lm(collegeData$med_10yr_salary~collegeData$spend_per_student)
lm(collegeData$avg_10yr_salary~collegeData$spend_per_student)

#start to examine the trait in 3 different types of schools
#put 3 plots together:
par(mfrow=c(1,3))
#make the scatter plot for public school students:
pub= which(collegeData$ownership=="Public")
public <- collegeData[pub,]
scatter.smooth(public$spend_per_student,public$avg_10yr_salary,main="average graph",xlim=c(0,200000),ylim=c(0,1000000),col="red",xlab="spend per student in public school(dollar)",ylab="average 10 yr salaries for public students(dollar)")
abline(lm(public$avg_10yr_salary~public$spend_per_student))
lm(public$avg_10yr_salary~public$spend_per_student)
#make the scatter plot for forprofit school students:
forpro= which(collegeData$ownership=="For Profit")
forprofit <- collegeData[forpro,]
scatter.smooth(forprofit$spend_per_student,forprofit$avg_10yr_salary,main="average graph",xlim=c(0,200000),ylim=c(0,1000000),col="red",xlab="spend per student in forprofit school(dollar)",ylab="average 10 yr salaries for forprofit school students(dollar)")
abline(lm(forprofit$avg_10yr_salary~forprofit$spend_per_student))
lm(forprofit$avg_10yr_salary~forprofit$spend_per_student)
#make the plot for nonprofit school students:
nonpro= which(collegeData$ownership=="Nonprofit")
nonprofit <- collegeData[nonpro,]
scatter.smooth(nonprofit$spend_per_student,nonprofit$avg_10yr_salary,main="average graph",xlim=c(0,200000),ylim=c(0,1000000),col="red",xlab="spend per student in nonprofit school(dollar)",ylab="average 10 yr salaries for nonprofit school students")
abline(lm(nonprofit$avg_10yr_salary~nonprofit$spend_per_student))
lm(nonprofit$avg_10yr_salary~nonprofit$spend_per_student)
par(mfrow=c(1,1))

#again do the samething as above
#change the average 10 yr salaries to median 10 yr salaries:
par(mfrow=c(1,3))
#make the scatter plot for public school students:
pub= which(collegeData$ownership=="Public")
public <- collegeData[pub,]
scatter.smooth(public$spend_per_student,public$med_10yr_salary,main="median graph",xlim=c(0,200000),ylim=c(0,1000000),col="red",xlab="spend per student in public school(dollar)",ylab="median 10 yr salaries for public students(dollar)")
abline(lm(public$med_10yr_salary~public$spend_per_student))
lm(public$med_10yr_salary~public$spend_per_student)
#make the scatter plot for forprofit school students:
forpro= which(collegeData$ownership=="For Profit")
forprofit <- collegeData[forpro,]
scatter.smooth(forprofit$spend_per_student,forprofit$med_10yr_salary,main="median graph",xlim=c(0,200000),ylim=c(0,1000000),col="red",xlab="spend per student in forprofit school(dollar)",ylab="median 10 yr salaries for forprofit school students(dollar)")
abline(lm(forprofit$med_10yr_salary~forprofit$spend_per_student))
lm(forprofit$med_10yr_salary~forprofit$spend_per_student)
#make the plot for nonprofit school students:
nonpro= which(collegeData$ownership=="Nonprofit")
nonprofit <- collegeData[nonpro,]
scatter.smooth(nonprofit$spend_per_student,nonprofit$med_10yr_salary,main="median graph",xlim=c(0,200000),ylim=c(0,1000000),col="red",xlab="spend per student in nonprofit school(dollar)",ylab="median 10 yr salaries for nonprofit school students")
abline(lm(nonprofit$med_10yr_salary~nonprofit$spend_per_student))
lm(nonprofit$med_10yr_salary~nonprofit$spend_per_student)
par(mfrow=c(1,1))

#8
#find the maximum of average 10 year salaries divided cost
#highest result will be the school which has the highest return rate:
ration=collegeData$avg_10yr_salary/collegeData$cost
which.max(ration)
collegeData$name[1418]
which.max(ration[-1418])
collegeData$name[3069]
which.max(ration[-c(3069,1418)])
collegeData$name[2442]
#United States Merchant Marine Academy Kings Point NY

#9
#select the race column data out:
race = collegeData[c("race_white","race_black","race_hispanic","race_asian","race_native","race_pacific","race_other")]
#use the apply function to calculate sd of each university:
diversity<-apply(race,1,sd,na.rm=TRUE)
race_new=collegeData[c("name","race_white","race_black","race_hispanic","race_asian","race_native","race_pacific","race_other")]
race_new$diversity = diversity
#to see which school has the lowest university:
head(order(race_new$diversity))
#to clean dirty data(0,0,0,0,0,0,0data):
dirtydata=which(diversity==0)
dirtydata
#get the result:
cleandata = race_new[-c(93,182,1000,2571,2965),]
head(order(cleandata$diversity))

#10
#First,comparing the average sat.
#find the id of uc davis:
which(collegeData$name=="University of California-Davis")
#see the average sat score of ucdavis:
collegeData$avg_sat[123]
#plot the average sat distribution graph without data of davis:
plot(density(collegeData$avg_sat[-123],na.rm=TRUE),main="universities' average sat distribution",xlab="sat scores(points)")
#add the davis average sat score into the graph:
abline(v=1192,col="red")
#add the average line and the median line:
abline(v = mean(collegeData$avg_sat[-123],na.rm=TRUE),col = "royalblue",lwd=2)
abline(v = median(collegeData$avg_sat[-123],na.rm=TRUE),col = "green",lwd=2)
#to see the number of average and median, also quantile:
mean(collegeData$avg_sat[-123],na.rm=TRUE)
median(collegeData$avg_sat[-123],na.rm=TRUE)
quantile(collegeData$avg_sat[-123],probs=seq(0,1,length=11),na.rm=TRUE)
quantile(collegeData$avg_sat[-123],probs=seq(0.8,0.9,length=11),na.rm=TRUE)
# ADD the legend:
legend('topright',c("UC Davis","average","median"),lty=1,col=c('red',  'royalblue','green'))

gradpop4 = collegeData$grad_pop
plot(density(gradpop4[-123],na.rm=TRUE),main="graduate population distribution graph",xlab="graduate popuation(people)")
gradpop4[123]
mean(gradpop4[-123],na.rm=TRUE)
median(gradpop4[-123],na.rm=TRUE)
quantile(gradpop4[-123],probs=seq(0,1,length=11),na.rm=TRUE)
quantile(gradpop4[-123],probs=seq(0.9,1,length=11),na.rm=TRUE)
aa=which(gradpop4>6774)
length(aa)
abline(v = mean(gradpop4[-123],na.rm=TRUE),col = "royalblue",lwd=2)
abline(v = median(gradpop4[-123],na.rm=TRUE),col = "green",lwd=2)
abline(v=6774,col="red")
legend('topright',c("UC Davis","average","median"),lty=1,col=c('red',  'royalblue','green'))

tnon1 = collegeData$tuition_nonresident
tnon1[123]
tnon=tnon1[-123]
plot(density(tnon,na.rm=TRUE),main="nonresidential tuition distribution",xlab="nonresidential tuition(dollar)")
mean(tnon,na.rm=TRUE)
median(tnon,na.rm=TRUE)
quantile(tnon,probs=seq(0,1,length=11),na.rm=TRUE)
quantile(tnon,probs=seq(0.9,1,length=11),na.rm=TRUE)
bb=which(tnon>36773)
length(bb)
abline(v = mean(tnon,na.rm=TRUE),col = "royalblue",lwd=2)
abline(v = median(tnon,na.rm=TRUE),col = "green",lwd=2)
abline(v=36773,col="red")
legend('topright',c("UC Davis","average","median"),lty=1,col=c('red',  'royalblue','green'))



