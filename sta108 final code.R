mydata = read.table("diabetesfull.txt", header=T) 
index.na = apply(is.na(mydata), 1, any) 
newdata = mydata[index.na==FALSE,] 
any(is.na(newdata))  
dim(newdata)

hist(newdata$chol,main = 'chol', xlab = 'chol')
hist(newdata$stab.glu,main = 'stab.glu', xlab = 'stab.glu')
hist(newdata$hdl,main = 'hdl', xlab = 'hdl')
hist(newdata$ratio,main = 'ratio', xlab = 'ratio')
hist(newdata$glyhb,main = 'glyhb', xlab = 'glyhb')
hist(newdata$age,main = 'age', xlab = 'age')
hist(newdata$height,main = 'height', xlab = 'height')
hist(newdata$weight,main = 'weight', xlab = 'weight')
hist(newdata$bp.1s,main = 'bp.1s', xlab = 'bp.1s')
hist(newdata$bp.1d,main = 'bp.1d', xlab = 'bp.1d')
hist(newdata$waist,main = 'waist', xlab = 'waist')
hist(newdata$hip,main = 'hip', xlab = 'hip')
hist(newdata$ratio,main = 'time.ppn', xlab = 'time.ppn')

lbls = c('large','medium','small')
pct = round(100*table(newdata$frame)/366)
lab = paste(lbls,pct)
lab = paste(lab,'%',sep='')
pie(table(newdata$frame),main='frame',labels=lab)

lbls = c('male','female')
pct = round(100*table(newdata$gender)/366)
lab = paste(lbls,pct)
lab = paste(lab,'%',sep='')
pie(table(newdata$gender),main='gender',labels=lab)

lbls = c('Buckingham','Louisa')
pct = round(100*table(newdata$location)/366)
lab = paste(lbls,pct)
lab = paste(lab,'%',sep='')
pie(table(newdata$location),main='location',labels=lab)

pairs(~chol+stab.glu+hdl+ratio+glyhb+age+height+weight+bp.1s+bp.1d+waist+hip+time.ppn,data=newdata)
cor(cbind(newdata$chol,newdata$stab.glu,newdata$hdl,newdata$ratio,newdata$glyhb,newdata$age,newdata$height,newdata$weight,newdata$bp.1s,newdata$bp.1d,newdata$waist,newdata$hip,newdata$time.ppn),cbind(newdata$chol,newdata$stab.glu,newdata$hdl,newdata$ratio,newdata$glyhb,newdata$age,newdata$height,newdata$weight,newdata$bp.1s,newdata$bp.1d,newdata$waist,newdata$hip,newdata$time.ppn))

X=cbind(rep(1,366),newdata$chol,newdata$stab.glu,newdata$hdl,newdata$ratio,newdata$location,newdata$age,newdata$gender,newdata$height,newdata$weight,newdata$frame,newdata$bp.1s,newdata$bp.1d,newdata$waist,newdata$hip,newdata$time.ppn)
Y=cbind(newdata$glyhb)
fit = lm(Y ~ X)
plot(fit)

install.packages("MASS")
library(MASS)
boxcox(fit)
#so we need to transform to inverse of y

Y2 = 1/Y
X2 = cbind(rep(1,366),newdata$chol,newdata$stab.glu,newdata$hdl,newdata$ratio,newdata$location,newdata$age,newdata$gender,newdata$height,newdata$weight,newdata$frame,newdata$bp.1s,newdata$bp.1d,newdata$waist,newdata$hip,newdata$time.ppn)
fit2 = lm(Y2 ~ X2)
plot(fit2)
boxcox(fit2)

set.seed(10)
N=nrow(newdata)
index=sample(1: N, size=N/2, replace=FALSE)
data.t=newdata[index,]
data.v=newdata[-index,]

fit3 = lm(1/glyhb~., data=data.t)
SSE3 = sum((fit3$residuals)^2)
SSE3
MSE3 = SSE3 / (183-17)
MSE3

library(leaps)
best = regsubsets(1/glyhb~., data=data.t, nbest=1, nvmax=16)
summary(best)
Rp =summary(best)$rsq 
p = rowSums(summary(best)$which)
plot(p,Rp)
Rap =summary(best)$adjr2
p = rowSums(summary(best)$which)
plot(p,Rap)
Rap
Rp
Cp =summary(best)$cp
p = rowSums(summary(best)$which)
plot(p,Cp)
Cp

SSE = summary(best)$rss
n = dim(data.t)[1]
p = rowSums(summary(best)$which)
AIC = n*log(SSE)-n*log(n)+2*p
BIC = n*log(SSE)-n*log(n)+p*log(n)
SSE
AIC
BIC

fit4 = lm(1/glyhb~.^2, data=data.t)
fit4
names(fit4)
length(fit4$coefficients)
MSE4 = SSE4/(183-136)
MSE4

#10
modelnull = lm(1/glyhb ~ 1,data=data.t)
step(modelnull,scope=list(upper=fit4,lower=modelnull),direction="forward")
fs1=lm(1/glyhb ~ stab.glu + age + waist + ratio + stab.glu:ratio+age:ratio, data = data.t)

#11
step(fit3,scope=list(lower=modelnull,upper=fit4),direction="forward")
fs2 = lm(1/glyhb ~ chol + stab.glu + hdl + ratio + location + 
age + gender + height + weight + frame + bp.1s + bp.1d + 
waist + hip + time.ppn + stab.glu:gender + hdl:ratio + age:bp.1d +  age:gender + weight:bp.1s + hip:time.ppn + age:hip + gender:height + 
stab.glu:bp.1s + location:bp.1d + location:frame + hdl:weight + 
location:age + ratio:waist + stab.glu:time.ppn + chol:time.ppn + 
stab.glu:waist + stab.glu:location + frame:bp.1d + weight:hip + 
age:frame + ratio:location + bp.1s:time.ppn + waist:time.ppn + 
bp.1s:waist + frame:time.ppn, data = data.t)

#12
 SSEfs1 = sum((fs1$residuals)^2)
 n = dim(data.t)[1]
 pfs1 = length(fs1$coefficients)
 AICfs1 = n*log(SSEfs1)-n*log(n)+2*pfs1
 BICfs1 = n*log(SSEfs1)-n*log(n)+pfs1*log(n)
 AICfs1
BICfs1
SSEfs2 = sum((fs2$residuals)^2)
pfs2=length(fs2$coefficients)
AICfs2 = n*log(SSEfs2)-n*log(n)+2*pfs2
BICfs2 = n*log(SSEfs2)-n*log(n)+pfs2*log(n)
AICfs2
BICfs2

