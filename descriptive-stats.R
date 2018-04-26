source("load_and_preprocess_compiled_data20180330.R")

#data.itc[is.na(EstimatedSubjKIndifferenceAllTrials)]

data.itc<-data.itc[EstimatedSubjKIndifferenceAllTrials!=Inf & !is.nan(EstimatedSubjKIndifferenceAllTrials) & !is.na(EstimatedSubjKIndifferenceAllTrials)]
which(is.na(data.itc$EstimatedSubjKIndifferenceAllTrials))
data.itc$LogKIndiff<-log(data.itc$EstimatedSubjKIndifferenceBySalienceCondition)
data.itc<-data.itc[(LogKIndiff> -10000) & (LogKIndiff< 10000)]


# We want to test whether our titration process actually works.

#we can do that by looking to see, first of all, whether startingK predicts Choice
#we should do this in a way that controls for everything that should be controlled, so no t-test!

library(lme4)
library(ggplot2)
data.itc$logStartingK<-log(data.itc$startingK)

summary(glmer(Choice01~logStartingK+(1|SubjectUniqueID),data.itc,family = binomial))
#Across subjects and trials, there's a negative relationship between trial K and the Choice:
ggplot(data.itc,aes(x=Choice,y=logStartingK,group=Choice))+geom_boxplot()+geom_jitter(width=0.3,height=0,alpha=0.05)+
  labs(title="Across subjects and trials\nSmallerSooner is associated with higher starting K values")
#Smaller Sooner trials are associated with higher log starting K values.
#This might be because subjects with higher k indifference points titrate to higher k values


summary(glmer(Choice01~logStartingK+salienceCondition+(1|SubjectUniqueID),data.itc,family = binomial))

summary(glmer(Choice01~logStartingK+salienceCondition+(1+salienceCondition|SubjectUniqueID),data.itc,family = binomial))
#DelaySalient seems to be related to SmallerSooner trials
#this holds when we control for subject effects, which means that the fixed effect is interpretable:
#We've captured subject effects and can say that DelaySalient tends to predict SmallerSooner choices.


model.logstartingK<-glmer(Choice01~
                            scale(logStartingK)+
                            salienceCondition+
                            scale(TrialId)+
                            (1+scale(logStartingK)+salienceCondition|SubjectUniqueID)
                          ,data.itc,family = binomial
              , control = glmerControl(optimizer = "Nelder_Mead"))
summary(model.logstartingK)

ranef(model.logstartingK)
#coef(model.logstartingK)$SubjectUniqueID[,3]
logStartingKWithinSubjectCoefficient <- coef(model.logstartingK)$SubjectUniqueID[,2]

hist(logStartingKWithinSubjectCoefficient)


#Once we control for a subject-level intercepts and slope, then we find that across subjects,
#a higher trial K predicts a LargerLater Choice.
#When startingK is higher, we create a more dramatic difference in LargerLater and SmallerSooner choices.
#LargerLater becomes more enticing than SmallerSooner
#and subjects respond to that by being more likely to pick LargerLater.
#So the titration 'works' in the sense of motivating subjects in a particular direction.

#This would make sense if higher startingK values mean that we make LargerLater trials more enticing and thus choice is predicted.

#In our AmountSalient trials, we dynamically adjust the smaller sooner amount to reach an indifference point
#In our DelaySalient trials, we dyanmically adjust the larger later amount to reach an indifferencde point.
#So if this is correct, we'd expect SSAmount to mediate/moderate(?) the relationship between logStartingK and Choice01

summary(model.logstartingK)
logStartingKWithinSubjectCoefficient <- coef(model.logstartingK)$SubjectUniqueID[,2]

hist(logStartingKWithinSubjectCoefficient)
t.test(logStartingKWithinSubjectCoefficient)

summary(coef(model.logstartingK)$SubjectUniqueID[,2]) #the mean of the subject effect is very close to the group effect.
fixef(model.logstartingK)
#let's run the model separately for each subject to see that it's the same kind of effect.
by.subject.scale.logstartingK.param<-NULL
for (s in unique(data.itc$SubjectUniqueID)){#s<-unique(data.itc$SubjectUniqueID)[1]
  if(length(unique(data.itc[SubjectUniqueID==s,salienceCondition]))>1){
    subjectmodel<-glm(Choice01~
                        scale(logStartingK)+
                        salienceCondition+
                        scale(TrialId),data.itc[SubjectUniqueID==s,],
                      family=binomial(link="logit"))
  }else{
    subjectmodel<-glm(Choice01~scale(logStartingK)+scale(TrialId),data.itc[SubjectUniqueID==s,],
                      family=binomial(link="logit"))
  }
    
  by.s.logstartingK.param<-data.frame(t(summary(subjectmodel)$coefficients["scale(logStartingK)",]))
  by.s.logstartingK.param[["SubjectUniqueID"]]<-s
  if(is.null(by.subject.scale.logstartingK.param)){
    by.subject.scale.logstartingK.param<-by.s.logstartingK.param
  }else{
    by.subject.scale.logstartingK.param<-rbind(by.subject.scale.logstartingK.param,by.s.logstartingK.param)
  }
  cat(".")
}
cat("\n")
View(by.subject.scale.logstartingK.param)
cor.test(scale(by.subject.scale.logstartingK.param$Estimate),scale(coef(model.logstartingK)$SubjectUniqueID[,2]))
summary(by.subject.scale.logstartingK.param$Estimate)
# doing models on their own, the effect is slightly higher, but either way, when we look at individual subjects, we see that 
# *within subjects*
# 
plot(scale(by.subject.scale.logstartingK.param$Estimate),scale(coef(model.logstartingK)$SubjectUniqueID[,2]),xlim=c(-0.3,0.5))
hist(by.subject.scale.logstartingK.param$Estimate,breaks=500,xlim=c(-10,10))
t.test(by.subject.scale.logstartingK.param$Estimate)

