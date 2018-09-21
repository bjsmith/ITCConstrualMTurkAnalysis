#parallel to the first Bayesian model with RewardReality I did.
even_folder<-"/Users/benjaminsmith/Google Drive/neural-construal-level/data/ITCConstrual_mTurkExp/even_presentation/"
length(unique(data.itc.bestsubj$SubjectUniqueID))
library(lme4)
linearmodel.bestsubj<-
  glmer(Choice01~
          AbstractionLevel+scale(TrialIdItcSC)+
          scale(logStartingK)+
          salienceCondition+
          RewardReality+
          (1+scale(TrialIdItcSC)+
             #salienceCondition+
             #scale(LogKIndiff)+
             scale(logStartingK)
           |SubjectUniqueID),
        data.itc.bestsubj, family=binomial)
summary(linearmodel.bestsubj)
save(linearmodel.bestsubj,file=paste0(even_folder,"lm.bestsubj.Rdata"))

#Parallel to the interaction Bayesian model
linearmodel.rr.interact.bestsubj<-
  glmer(Choice01~
          scale(TrialIdItcSC)+
          scale(logStartingK)+
          AbstractionLevel + AbstractionLevel:scale(logStartingK)+
          salienceCondition + salienceCondition:scale(logStartingK)+
          RewardReality + RewardReality:scale(logStartingK)+
          (1+scale(TrialIdItcSC)+
             #salienceCondition+
             #scale(LogKIndiff)+
             scale(logStartingK)
           |SubjectUniqueID),
        data.itc.bestsubj, family=binomial)
save(linearmodel.rr.interact.bestsubj,file=paste0(even_folder,"lm.bestsubj.interact.Rdata"))
summary(linearmodel.rr.interact.bestsubj)


#Best guess of making all the controls I should do.
model.stan.rr.interact.bestsubj<-
  glmer(Choice01~
          scale(TrialIdItcSC)+
          scale(logStartingK)+
          AbstractionLevel + AbstractionLevel:scale(logStartingK)+
          salienceCondition + salienceCondition:scale(logStartingK)+
          RewardReality + RewardReality:scale(logStartingK)+
          (1+scale(TrialIdItcSC)+
             #salienceCondition+
             #scale(LogKIndiff)+
             scale(logStartingK)
           |SubjectUniqueID),
        data.itc.bestsubj, family=binomial)

summary(model.stan.rr.interact.bestsubj)
names(model.stan.rr.interact.bestsubj)

#focus on a subset of the subjects for concrete-abstract analysis.
model.stan.abstractionlevel.bestsubj_concretefirst<-
  glmer(
    Choice01~
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
  data = data.itc.bestsubj[Group=="BlockDesign_concrete_first"], family=binomial)
summary(model.stan.abstractionlevel.bestsubj_concretefirst)
##OK, all the above except for the concrete_first model work.
#so we could either go to the Bayesian analysis, or we could try Nelder_Mead instead...

model.stan.abstractionlevel.bestsubj_concretefirst_nm<-
  glmer(
    Choice01~
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
    data = data.itc.bestsubj[Group=="BlockDesign_concrete_first"], family=binomial, 
    control = glmerControl(optimizer = "Nelder_Mead"))
summary(model.stan.abstractionlevel.bestsubj_concretefirst_nm)
#OK. This doesn't work either, so we really should be going to the Bayesian analysis
# to see if we can shed any more light on this question.
