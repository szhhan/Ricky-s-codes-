
#9.3
personality=data.frame(expand.grid(JP=c("J","P"),SN=c("S","N"),EI=c("E","I"),
                                   TF=c("T","F")),count=c(77,42,23,18,140,52,13,35,106,79,31,80,138,106,31,79))
model.simple = glm(count~JP+SN+EI+TF,family=poisson,data=personality)
summary(model.simple)
model.intercept=glm(count~JP+SN+EI+TF+JP:SN+JP:EI+JP:TF+SN:EI+SN:TF+EI:TF,family=poisson,data=personality)
summary(model.intercept)
model.whole = glm(count~JP+SN+EI+TF+JP:SN+JP:EI+JP:TF+SN:EI+SN:TF+EI:TF+JP:SN:EI+JP:SN:TF+JP:EI:TF+SN:EI:TF,family=poisson,data=personality)
summary(model.whole)

#9.7 
safety=data.frame(expand.grid(S=factor(c("Seat belt","None")),E=factor(c("Yes","No")),
                               H=factor(c("Nonfatal","Fatal"))),count=c(1105,4624,411111,157342,14,497,483,1008))
model.simple9.7=glm(count~S+E+H,family=poisson,data=safety)
model.SE.SH=glm(count~S+E+H+S:E+S:H,family=poisson,data=safety)
model.SE.EH=glm(count~S+E+H+S:E+E:H,family=poisson,data=safety)
model.SH.EH=glm(count~S+E+H+S:H+E:H,family=poisson,data=safety)
model.SE.SH.EH=glm(count~S+E+H+S:E+S:H+E:H,family=poisson,data=safety)
model.SE.H=glm(count~S+E+H+S:E,family=poisson,data=safety)
model.SH.E=glm(count~S+E+H+S:H,family=poisson,data=safety)
model.EH.S=glm(count~S+E+H+E:H,family=poisson,data=safety)
model.all.9.7 = glm(count~S*E*H,family=poisson,data=safety)
summary(model.SE.SH) #1769.4
summary(model.SE.EH) #1233.6
summary(model.SH.EH) # 7223
summary(model.SE.SH.EH) #93.853
summary(model.simple9.7) #11529
summary(model.SE.H) # 3654.7
summary(model.SH.E) #9644.1
summary(model.EH.S) #9108.3
anova(model.SE.SH.EH,model.all.9.7,test='LRT')
nonfatal= c(1105,411111,4624,157342)
fatal = c(14,483,497,1008)
ejected = c(1,0,1,0)
safety = c(1,1,0,0)
fit = glm(cbind(nonfatal,fatal)~ejected+safety,family=binomial)
summary(fit)
sum(abs(nonfatal/(fatal+nonfatal)-fitted(fit)))/2


#9.13
Religion=data.frame(expand.grid(P=factor(c("1","2","3")),S=factor(c("1","2")),
                               R=factor(c("1","2")),B=factor(c("1","2"))),count=c(99,73,51,8,20,6,73,87,51,24,50,33,15,20,19,4,13,12,25,37,36,22,60,88))

fitinitial = glm(count~P+S+R+B,family=poisson,data=Religion)
fit=glm(count~P+S+R+B+P:B+S:P+R:B+B:S+P:R+R:S,family=poisson,data=Religion)
fit2=glm(count~.,family=poisson,data=Religion)
summary(fit2)
fitnull = glm(count~1,family=poisson,data=Religion)
summary(fitnull)
fitup=update(fitinitial,.~.^3, data=Religion)

step(fit2,scope=list(lower=fitnull,upper=fitup),direction="both")
step(fit,scope=list(lower=fitnull,upper=fitup),direction="both")
step(fitinitial,scope=list(lower=fitnull,upper=fitup),direction="both")


model1= glm(count~P+S+R+B+P:B+S:P+R:B+B:S+P:R+R:S,family=poisson,data=Religion)
model2 = glm(count~P+S+R+B+P:B+S:P+R:B+B:S+R:S,family=poisson,data=Religion)
summary(model1)
summary(model2)
model.all.9.13 = glm(count~P*S*R*B,family=poisson,data=Religion)
anova(model1,model.all.9.13,test='LRT')
anova(model2,model.all.9.13,test='LRT')



