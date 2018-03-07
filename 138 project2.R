throat=data.frame(duration=c(45,15,40,83,90,25,35,65,95,35,75,45,50,75,30,25,20,60,70,30,60,61,65,15,20,45,15,25,15,30,40,15,135,20,40),
                comp=c(0,0,1,1,1,1,1,1,1,1,1,1,0,1,0,1,0,1,1,1,1,0,1,0,0,1,0,1,0,1,1,0,1,0,0))
throat


fit=glm(comp~duration, data=throat, family=binomial(link=logit))

summary(fit)
fitted(fit)

prob.fitted=fitted(fit)
plot(prob.fitted~throat$duration,xlab="duration time(minutes)")

#if x = 55
exp(coef(fit)[1]+coef(fit)[2]*55)/(1+exp(coef(fit)[1]+coef(fit)[2]*55))
predict(fit, newdata = data.frame(duration=c(55,60,65,70,75)), type='response')
confint(fit)
AIC(fit)
BIC(fit)
AIC(fit, k=0)
anova(fit, test='LRT')
anova(fit, test='Rao')

exp(fit$coef[2])
exp(fit$coef[2]-1.96*summary(fit)$coef[2,2])
exp(fit$coef[2]+1.96*summary(fit)$coef[2,2])

#install.packages("binomTools", repos="http://R-Forge.R-project.org")
library(binomTools)
resid=Residuals(fit, type='standard.pearson')
# plot against X
plot(resid~throat$duration,xlab="duration")
plot(resid~fitted(fit),xlab="prob")


new.x=-coef(fit)[1]/coef(fit)[2]
fitted2 = data.frame(cbind(fit$y,fit$fitted.values))
ones = fitted2[fitted2[,1]==1,]
zeros = fitted2[fitted2[,1]==0,]
concordance = function(model){
  fitted = data.frame(cbind(model$y,model$fitted.values))
  colnames(fitted) = c('real','estimate')
  ones = fitted[fitted[,1]==1,]
  zeros = fitted[fitted[,1]==0,]
  
  pairs_tested = 0
  conc = 0
  disc = 0
  ties = 0
  
  for(i in 1:nrow(ones))
  {
    for(j in 1:nrow(zeros))
    {
      pairs_tested = pairs_tested+1
      if(ones[i,2] > zeros[j,2]) {conc = conc+1}
      else if(ones[i,2]==zeros[j,2]){ties = ties+1}
      else {disc = disc+1}
    }
  }
  concordance = conc/pairs_tested
  discordance = disc/pairs_tested
  ties_perc = ties/pairs_tested
  return(list("Concordance"=concordance,
              "Discordance"=discordance,
              "Tied"=ties_perc,
              "Pairs"=pairs_tested))
}

concordance(fit)
throat$fitted=fit$fitted.values

install.packages('ResourceSelection')
library(ResourceSelection)
hoslem.test(x = throat$comp, 
            y=throat$fitted, g=10)
summary(fit)
confint(fit)

exp(confint(fit))
resid=resid(fit, type='pearson')/sqrt(1 - hatvalues(fit))
plot(resid~throat$duration
     )


