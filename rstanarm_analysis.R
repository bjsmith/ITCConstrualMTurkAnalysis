#rstanarm.R set up to run the stan model
#This file analyses it.
load("../../../data/ITCConstrual_mTurkExp/stanmodel.RData")
summary(model.stan.ALL)
summary(model.stan.ALL)[1:5,]
#OK, fine, but we never controlled for hypotheticality here. It seems like that is important..
#we also should have tried this separately for "best subjects" and all subjects.
#I am pleased that our Bayesian model gives us this result! I don't think we could get it from the hierarchical linear model.
#For Choice01, 0==SS, 1=LL
mean_val<-summary(model.stan.ALL)[1:5,"mean"]

table(model.stan.ALL$data$Choice01,model.stan.ALL$data$salienceCondition)
#In the AmountSalient condition, the log odds of choosing LargerLater is 0.11; the odds of choosing LL over SS is 1.12 to 1.
#(this is counter-intuitive! it goes against the raw odds figures. but because the result isn't significant, perhaps we can ignore that)
#So in the AmountSalient condition, the likelihood of a LL over SS choice is not significantly different from zero.
#In the DelaySalient condition, the log odds of a LL over SS choice...is probably significantly lower than zero, but we don't have a measure for that;
#what we can say is that the odds of choosing 




#in the DelaySalient condition the log odds of choosing SmallerSooner is 
mean_val[1]+ (mean_val)[5]
#; the odds of choosing LL over SS is 
exp(mean_val[1]+mean_val[5])
# is about 0.8 to 1

#and the odds of choosing LL over SS in the DelaySalient condition are 74% (95% CI [73% to 81%]) compared to the Amount Salient condition.
summary(model.stan.ALL)[5,c(1,4:8)]
exp(summary(model.stan.ALL)[5,c(1,4:8)])

#and we can talk about the odds within the DelaySalient condition alone by adding the DelaySalient value to the intercept:
summary(model.stan.ALL)[1,c(1,4:8)]+summary(model.stan.ALL)[5,c(1,4:8)]
#this does not appear to be significant. That's actually what we want.

model.stan.ALL$data[salienceCondition=="DelaySalient",.N,by=.(LLamount,SSamount)]
summary(model.stan.ALL$data[salienceCondition=="DelaySalient",.(LLdelay,SSdelay)])

model.stan.ALL$data[salienceCondition=="AmountSalient",.N,by=.(LLdelay,SSdelay)]
summary(model.stan.ALL$data[salienceCondition=="AmountSalient",.(LLamount,SSamount)])



load(file="../../../data/ITCConstrual_mTurkExp/stanmodel_rr_bestsubj.RData")
summary(model.stan.rr.bestsubj)[1:6,]
exp(summary(model.stan.rr.bestsubj)[6,])
exp(summary(model.stan.rr.bestsubj)[5,])
summary(model.stan.rr.bestsubj)[1:6,1]/probit(invlogit(summary(model.stan.rr.bestsubj)[1:6,1]))
library(LaplacesDemon)

probit(invlogit(summary(model.stan.rr.bestsubj)[1:6,1]))
probit(invlogit(summary(model.stan.rr.bestsubj)[1:6,c(1,4,8)]))



