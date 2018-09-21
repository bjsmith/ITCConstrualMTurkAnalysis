
getWellPerformingSubjs <- function(data.itc){
  ## How about split by salienceCondition
  #Doing this individually because I want to get standard errors.
  by.subject.scale.logstartingK.scSplit.param<-NULL
  for (s in unique(data.itc$SubjectUniqueID)){#s<-unique(data.itc$SubjectUniqueID)[1]
    for (sc in unique(data.itc[SubjectUniqueID==s,salienceCondition])){#sc<-unique(data.itc[SubjectUniqueID==s,salienceCondition])[1]
      subjectmodel<-glm(Choice01~scale(logStartingK)+scale(TrialId),
                        data.itc[SubjectUniqueID==s & salienceCondition==sc],
                        family=binomial(link="logit"))
      
      by.s.logstartingK.param<-data.frame(t(summary(subjectmodel)$coefficients["scale(logStartingK)",]))
      by.s.logstartingK.param[["SubjectUniqueID"]]<-s
      by.s.logstartingK.param[["salienceCondition"]]<-sc
      if(is.null(by.subject.scale.logstartingK.scSplit.param)){
        by.subject.scale.logstartingK.scSplit.param<-by.s.logstartingK.param
      }else{
        by.subject.scale.logstartingK.scSplit.param<-rbind(by.subject.scale.logstartingK.scSplit.param,by.s.logstartingK.param)
      }
      
    }
    
    cat(".")
  }
  
  
  ggplot(by.subject.scale.logstartingK.scSplit.param,aes(x=Estimate))+
    facet_wrap(~salienceCondition,ncol = 2,nrow=1)+
    geom_histogram(binwidth=0.5)+
    labs(x="log K beta coefficient",y="# of subjects")+geom_vline(xintercept = 0)+coord_cartesian(xlim=c(-5,10))
  
  #keep subjects who EITHER have amountSalient or DelaySalient with 1 SE above zero.
  by.subject.scale.logstartingK.scSplit.param$EstM1SE<-by.subject.scale.logstartingK.scSplit.param$Estimate-by.subject.scale.logstartingK.scSplit.param$Std..Error
  by.subject.scale.logstartingK.scSplit.param$EstM1_5SE<-by.subject.scale.logstartingK.scSplit.param$Estimate-1.5*by.subject.scale.logstartingK.scSplit.param$Std..Error
  by.subject.scale.logstartingK.scSplit.param$EstM2SE<-by.subject.scale.logstartingK.scSplit.param$Estimate-2*by.subject.scale.logstartingK.scSplit.param$Std..Error
  across.sC<-spread(data.table(by.subject.scale.logstartingK.scSplit.param)[,.(salienceCondition,EstM1_5SE,SubjectUniqueID)],key = salienceCondition,value = EstM1_5SE)
  
  subjs.to.keep<- across.sC[AmountSalient>0 | DelaySalient>0,SubjectUniqueID]
  return(subjs.to.keep)
}
