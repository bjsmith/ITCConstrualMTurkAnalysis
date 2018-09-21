
library(ggplot2)
library(knitr)
library(tidyr)
library(lme4)
source("load_and_preprocess_compiled_data20180330.R")
source("getWellPerformingSubjs.R")

data.itc<-data.itc[EstimatedSubjKIndifferenceAllTrials!=Inf & !is.nan(EstimatedSubjKIndifferenceAllTrials) & !is.na(EstimatedSubjKIndifferenceAllTrials)]

#filter out subjects at extremes
data.itc$LogKIndiff<-log(data.itc$EstimatedSubjKIndifferenceBySalienceCondition)
data.itc<-data.itc[(LogKIndiff> -10000) & (LogKIndiff< 10000)]


exclusion_stats<-data.itc[,.(
  LeftProp=sum(ChoiceKey==1,na.rm = TRUE)/sum(!is.na(ChoiceKey)),
  RightProp=sum(ChoiceKey==2,na.rm = TRUE)/sum(!is.na(ChoiceKey)),
  LeftRightChoices=sum(ChoiceKey==1 | ChoiceKey==2,na.rm=TRUE),
  ConstrualChoice1Prop=sum(ChoiceConstrual==1,na.rm = TRUE)/sum(!is.na(Answer)),
  ConstrualChoice2Prop=sum(ChoiceConstrual==2,na.rm = TRUE)/sum(!is.na(Answer)),
  #LogK=EstimatedSubjKIndifference,
  finalK=endingK[.N]),by=.(SubjectUniqueID,ConditionCode)]

kvalStats<-data.itc[,.(LogKValBySalienceCondition=log(EstimatedSubjKIndifferenceBySalienceCondition[1]),finalK=log(endingK[.N])),by=.(SubjectUniqueID,ConditionCode,salienceCondition)]
too_selective_subjs<- exclusion_stats%>% 
  .[LeftProp<.167 | LeftProp>(1-.167) | finalK<=0.0001,SubjectUniqueID]
data.itc<-data.itc[!(SubjectUniqueID %in% too_selective_subjs)]

data.itc[,.(NAChoices=sum(is.na(ChoiceKey))),by=.(SubjectUniqueID)] %>% .[order(NAChoices)]
#table(data.itc[SubjectUniqueID=="M730ARVGMonApr092018184028GMT0200CEST37168831",.(Choice,ChoiceKey,SSonLorR)])

data.itc$logStartingK<-log(data.itc$startingK)

subjs.to.include<-getWellPerformingSubjs(data.itc)
data.itc.bestsubj<-data.itc[SubjectUniqueID %in% subjs.to.include]

library(rstanarm)
library(parallel)

##########
## TO TRY:
## - across all subjects
## - using exclusion criteria I worked out with John
## - specific conditions I think might have an effect
## - with or without various confounders

## - All vs. some
## - Using hypotheticality as a covariate

model.stan.rr.bestsubj.concrete_to_abstract<-stan_glmer(Choice01~
                             scale(TrialIdItcSC)+
                             scale(logStartingK)+
                            AbstractionLevel + AbstractionLevel:scale(logStartingK)+
                             salienceCondition+
                             RewardReality+
                             (1+scale(TrialIdItcSC)+
                                #salienceCondition+
                                #scale(LogKIndiff)+
                                scale(logStartingK)
                              |SubjectUniqueID),
                           prior = student_t(df=5,autoscale = TRUE),control=list(adapt_delta=0.99),
                           data = data.itc.bestsubj[Group=="BlockDesign_concrete_first"],
                           family = binomial(link = "logit"),
                           chains = 6, cores = parallel::detectCores(), seed = 42)
summary(model.stan.rr.bestsubj.concrete_to_abstract)

save( model.stan.rr.bestsubj.concrete_to_abstract,file="../../../data/ITCConstrual_mTurkExp/stanmodel-interact.RData")


