#So let's say there's a within-subject effect on Y by X1 that only affects condition C1
test.data<-data.frame(
  "SubID"=rep(1:100,times=1,each=40),
  "X1"=c(rnorm(50*40,0,1),rnorm(50*40,0,1)),
  "Condition"=c(rep(0,50,each=40),rep(1,50,each=40))
)
test.data$Y<-
  (rnorm(100*40,0,1)+test.data$X1*test.data$Condition)>0

library(lme4)
test.m2<-
  glmer(Y~X1*Condition+(1|SubID),
  test.data,
  family = binomial, control = glmerControl(optimizer = "bobyqa"))

summary(test.m2)

test.m3<-
  glmer(Y~X1*Condition+(X1|SubID),
        test.data,
        family = binomial, control = glmerControl(optimizer = "bobyqa"))

summary(test.m3)

test.m4<-
  glmer(Y~X1*Condition+(Condition|SubID),
        test.data,
        family = binomial, control = glmerControl(optimizer = "bobyqa"))

summary(test.m4)

test.m5<-
  glmer(Y~X1*Condition+(X1*Condition|SubID),
        test.data,
        family = binomial, control = glmerControl(optimizer = "bobyqa"))

summary(test.m5)
table(test.data$Condition,test.data$SubID)

#OK cool, but we're actually looking for *within* subject variance. if ONLY within-subject variance exists, and if we control for those slopes
#then will we lose the fixed effect within subjects?
#also, we're actually testing prediction of an ordinal variable from a binomial one, so let's mimic that here.
test.data.ws<-data.frame(
  "SubID"=rep(1:100,times=1,each=40),
  "X1"=c(rnorm(50*40,0,1),rnorm(50*40,0,1)),
  #"Condition"=c(rep(0,50,each=40),rep(1,50,each=40)),
  "WithinSubCondition"=rep(0:1,times=100,each=20)
)
test.data.ws$X1_B<-test.data.ws$X1>0
test.data.ws$Y<-
  (rnorm(100*40,0,1)+test.data.ws$X1*test.data.ws$WithinSubCondition)>0
test.data.ws$Y_C<-round(test.data.ws$Y,0)

testws.m1<- glmer(Y_C~X1_B:WithinSubCondition+(1|SubID),
                  test.data.ws,
                  family = binomial, control = glmerControl(optimizer = "bobyqa"))

summary(testws.m1)

testws.m2<- glmer(Y_C~X1_B:WithinSubCondition+(X1_B:WithinSubCondition|SubID),
                  test.data.ws,
                  family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(testws.m2)

testws.m3<- glmer(Y_C~X1_B*WithinSubCondition+(X1_B*WithinSubCondition|SubID),
                  test.data.ws,
                  family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(testws.m3)

#no, controlling for random effects shouldn't lose us our effect here.