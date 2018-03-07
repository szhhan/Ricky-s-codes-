#9.1
opinion=data.frame(expand.grid(G=factor(c("Male","Female")),I=factor(c("S","O")),
                               H=factor(c("S","O"))),count=c(76,114,6,11,160,181,25,48))
model.GH.GI=glm(count~G+H+I+G:H+G:I,family=poisson,data=opinion)
model.GH.HI=glm(count~G+H+I+G:H+H:I,family=poisson,data=opinion)
model.GI.HI=glm(count~G+H+I+G:I+H:I,family=poisson,data=opinion)
model.GH.GI.HI=glm(count~G+H+I+G:H+G:I+H:I,family=poisson,data=opinion)
summary(model.GH.GI)
summary(model.GH.HI)
summary(model.HI.GI)
summary(model.GH.GI.HI)



#9.2 

personality=data.frame(expand.grid(JP=c("J","P"),SN=c("S","N"),EI=c("E","I"),
                                   TF=c("T","F")),count=c(77,42,23,18,140,52,13,35,106,79,31,80,138,106,31,79))
homo.fit=glm(count~.^2,family=poisson,data=personality)
summary(homo.fit)


# 4.3 
snoreScores1 = c(0,2,4,6)
snoreScores2 = c(0,1,2,3)
snoreScores3 = c(1,2,3,4)
yes = c(24,35,21,30)
no = c(1355,603,192,224)
prop.yes = yes/(yes+no)

snoreDF1 = data.frame(snore = snoreScores1, yes, no)
snoreDF2 = data.frame(snore = snoreScores2, yes, no)
snoreDF3 = data.frame(snore = snoreScores3, yes, no)

# snoreScores1
snoringGLM1 = glm(cbind(yes, no)~snore,family = binomial(link = "identity"),data = snoreDF1 )
g1 = summary(snoringGLM1)
g1

# snoreScores2
snoringGLM2 = glm(cbind(yes, no)~snore,family = binomial(link="identity"),data = snoreDF2 )
g2 = summary(snoringGLM2)
g2

# snoreScores3
snoringGLM3 = glm(cbind(yes, no)~snore,family = binomial(link="identity"),data = snoreDF3 )
g3 = summary(snoringGLM3)
g3


#4.4
data=read.csv("/Users/ricky/Desktop/crabdata.csv")
mydata$weight=mydata$weight/1000
fit2=lm(y~weight/1000, data=mydata)
summary(fit2)

fit3=glm(y~weight, data=mydata, family=binomial(link=logit))
summary(fit3)




