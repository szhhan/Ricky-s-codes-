collegeData <- readRDS("/Users/ricky/Downloads/college_scorecard_2013.rds")
CA = collegeData[collegeData$state=='CA',]
NY = collegeData[collegeData$state=="NY",]
CAprivate = CA[CA$ownership!='Public',]
CApublic = CA[CA$ownership=='Public',]
NYpublic = NY[NY$ownership=='Public',]
NYprivate = NY[NY$ownership!='Public',]
CApublic_highspent = CApublic[CApublic$spend_per_student>=10000,]
CAprivate_highspent = CAprivate[CAprivate$spend_per_student>=10000,]
NYprivate_highspent = NYprivate[NYprivate$spend_per_student>=10000,]
NYpublic_highspent = NYpublic[NYpublic$spend_per_student>=10000,]

data = expand.grid(state=c("California","New York"),
                  ownership=c("Public","Private"),
                  spent_level=c("High","Low"))
data
count=c(11,23,88,98,24,21,244-88,192-98)
tab = data.frame(data,counts=count)
tab

fit = glm(counts~state+ownership+spent_level,data = tab, family=poisson(link=log))

summary(fit)

fit.all = glm(counts~state+ownership+spent_level+state:ownership+state:spent_level+ownership:spent_level+state:ownership:spent_level, data=tab, family=poisson(link=log))

summary(fit.all)

fit.three = glm(counts~state+ownership+spent_level+state:ownership+state:spent_level+ownership:spent_level,data=tab,family=poisson(link=log))

summary(fit.three)

fit.state_o.state.s = glm(counts~state+ownership+spent_level+state:ownership+state:spent_level,data=tab,family=poisson(link=log))

summary(fit.state_o.state.s)

fit.state_o.ownership_l = glm(counts~state+ownership+spent_level+state:ownership+ownership:spent_level,data=tab,family=poisson(link=log))

summary(fit.state_o.ownership_l)

fit.state_l.ownership_l = glm(counts~state+ownership+spent_level+state:spent_level+ownership:spent_level,data=tab,family=poisson(link=log))

summary(fit.state_l.ownership_l)

fit.state.ownership = glm(counts~state+ownership+spent_level+state:ownership,data=tab,family=poisson(link=log))

summary(fit.state.ownership)

fit.state.spent_level = glm(counts~state+ownership+spent_level+state:spent_level,data=tab,family=poisson(link=log))

summary(fit.state.spent_level)


fit.ownership.spent_level = glm(counts~state+ownership+spent_level+ownership:spent_level,data=tab,family=poisson(link=log))

summary(fit.ownership.spent_level)


tab = data.frame(tab,fit$fitted.values,fit.all$fitted.values,fit.three$fitted.values,fit.state_o.state.s$fitted.values,fit.state_o.ownership_l$fitted.values,fit.state_l.ownership_l$fitted.values,fit.state.ownership$fitted.values,fit.state.spent_level$fitted.values,fit.ownership.spent_level$fitted.values)


sum((tab$counts-tab$fit.fitted.values)^2/tab$fit.fitted.values)
sum((tab$counts-tab$fit.all.fitted.values)^2/tab$fit.all.fitted.values)
sum((tab$counts-tab$fit.three.fitted.values)^2/tab$fit.three.fitted.values)
sum((tab$counts-tab$fit.state_o.state.s.fitted.values)^2/tab$fit.state_o.state.s.fitted.values)
sum((tab$counts-tab$fit.state_o.ownership_l.fitted.values)^2/tab$fit.state_o.ownership_l.fitted.values)
sum((tab$counts-tab$fit.state_l.ownership_l.fitted.values)^2/tab$fit.state_l.ownership_l.fitted.values)
sum((tab$counts-tab$fit.state.ownership.fitted.values)^2/tab$fit.state.ownership.fitted.values)
sum((tab$counts-tab$fit.state.spent_level.fitted.values)^2/tab$fit.state.spent_level.fitted.values)
sum((tab$counts-tab$fit.ownership.spent_level.fitted.values)^2/tab$fit.ownership.spent_level.fitted.values)

anova(fit,fit.all,test="LRT")
anova(fit.all,fit.all,test="LRT")
anova(fit.three,fit.all,test="LRT")
anova(fit.state_o.state.s,fit.all,test="LRT")
anova(fit.state_o.ownership_l,fit.all,test="LRT")
anova(fit.state_l.ownership_l,fit.all,test="LRT")
anova(fit.state.ownership,fit.all,test="LRT")
anova(fit.state.spent_level,fit.all,test="LRT")
anova(fit.ownership.spent_level,fit.all,test="LRT")
