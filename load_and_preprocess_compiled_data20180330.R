library(psych)
library(data.table)
library(dplyr)
#from the JS:
# var CHOICE_SS="SS";//CHOICE_SS=1;
# var CHOICE_LL="LL";//CHOICE_LL=2;
# var SALIENCE_CONDITION_AMOUNT_SALIENT=0;
# var SALIENCE_CONDITION_DELAY_SALIENT=1;
load("system_settings.RData")
#save(data.directory,file="system_settings.RData")
#save("~/Google Drive/neural-construal-level/data/ITCConstrual_mTurkExp/",file="system_settings.RData")
#data.directory<-"/expdata/bensmith/neural-construal/data/ITCConstrual_mTurkExp/";save(data.directory,file="system_settings.RData")



preprocess_version<-"20170412g"
preprocess_filename<-paste0(data.directory,"preprocess_cache_",preprocess_version,".RData")
if(file.exists(preprocess_filename)){
  load(preprocess_filename)
}else{
  #construalfolder<-"/Users/eustacehsu/Documents/Monterossolab/sugar/construal_ITC/"
  construalfolder<-data.directory
  data.main.raw<-read.csv(paste(construalfolder,"all_data_anon_2018-03-30.csv",sep=""),header=TRUE,stringsAsFactors = FALSE)
  data.main<-data.main.raw
  data.main$logk<-log(data.main$startingK)
  data.main$Choice01<-ifelse(data.main$Choice=="SS",0,ifelse(data.main$Choice=="LL",1,NA))
  
  data.main$SubjectUniqueID<-as.factor(data.main$DataSource)
  
  #create a unique ID, compact as possible, without information loss, from the data source.
  data.main$SubjectUniqueID<-
    unlist(lapply(data.main$DataSource,function(x){
      gsub(".csv","",
           gsub("\\.","",
                gsub("GMT\\+0","Gp",
                     gsub("GMT\\-0","Gm",
                          gsub("_","",
                               gsub("00_Mountain_Standard_Time","MST",
                                    gsub("00_Mountain_Daylight_Time","MDT",
                                         gsub("00_Pacific_Daylight_Time","PDT",
                                              gsub("00_Hawaiian_Standard_Time","HawaiianST",
                                                   gsub("00_Eastern_Daylight_Time","EDT",
                                                        gsub("00_Central_Daylight_Time","CDT",x)))))))))))}))
  
  #which data can we merge with? We can use question text - not ideal but 
  construal_stim_table<-read.csv("../../SmithAdaptConstrualITC/spunt_dataset/stim.csv",stringsAsFactors = FALSE)
  construal_data_table<-read.csv("../../SmithAdaptConstrualITC/spunt_dataset/data.csv",stringsAsFactors = FALSE)
  
  construal.dt<-as.data.table(cbind(construal_stim_table,construal_data_table))
  
  
  #data cleaning
  data.main$Choice[data.main$Choice==""]=NA
  
  #constural levels
  data.main$Construal_QuestionText<-trimws(data.main$Construal_QuestionText)
  data.main$Construal_AnswerText<-trimws(data.main$Construal_AnswerText)
  data.main$Construal_QuestionText[trimws(data.main$Construal_QuestionText)==""]<-NA
  data.main$Construal_AnswerText[data.main$Construal_AnswerText==""]<-NA
  
  #should mark out practice trials.
  data.main$IsPracticeTrial<-FALSE
  data.main$IsPracticeTrial[data.main$Construal_QuestionText=="How to buy food?" | data.main$Construal_QuestionText=="Why buy food?"]<-TRUE
  
  #construal match
  data.main$cue_text<-gsub("How to ","",gsub("Why ","",gsub("\\?","",trimws(data.main$Construal_QuestionText))))
  data.main$answer_raw_text<-tolower(gsub("\\.","",trimws(data.main$Construal_AnswerText)))
  
  data.main.dt<-data.table(data.main)
  data.main.dt<-data.main.dt[order(RewardReality,Group,TaskArrangement,SalienceCondition..firstRewardScheduleItem.,SubjectUniqueID,choiceUp)]
  
  #new with construal data.
  data.main_w_c<-data.table(merge(data.main,construal.dt,by=c("cue_text","answer_raw_text"),all.x = TRUE,suffixes = c("","_ConstrualData")))
  
  data.main_w_c<-data.main_w_c[order(RewardReality,Group,TaskArrangement,SalienceCondition..firstRewardScheduleItem.,SubjectUniqueID,choiceUp)]
  
  
  #so now we should conveniently have a construal level marked out for each trial, as either a construal_condition_block or constural_condition
  data.main_w_c$ConstrualConditionAllocated<-data.main_w_c$Construal_Condition
  data.main_w_c[is.na(Construal_Condition),ConstrualConditionAllocated:=Construal_Condition_Block]
  
  #need to set an ordinal level that we can use in regression
  data.main_w_c$AbstractionLevel<-as.double(NA)
  data.main_w_c[ConstrualConditionAllocated==4,AbstractionLevel:=1.0]
  data.main_w_c[ConstrualConditionAllocated==5,AbstractionLevel:=2.0]
  data.main_w_c[ConstrualConditionAllocated==2,AbstractionLevel:=3.0]
  data.main_w_c[ConstrualConditionAllocated==3,AbstractionLevel:=4.0]
  
  #can we get a better way to measure K?
  source("estimate_indifference_point.R")
  data.main_w_c$EstimatedSubjKIndifferenceAllTrials<-NA
  data.main_w_c$EstimatedSubjKIndifferenceAmountSalient<-NA
  data.main_w_c$EstimatedSubjKIndifferenceDelaySalient<-NA
  data.main_w_c$EstimatedSubjKIndifferenceBySalienceCondition<-NA
  for (s in unique(data.main_w_c$SubjectUniqueID)){#s<-unique(data.main_w_c$SubjectUniqueID)[1]#s<-"GSS2U1EFFriApr062018121300Gm400EDT173446014"
    reltrials<-!is.na(data.main_w_c$Choice01) & data.main_w_c$SubjectUniqueID==s
    data.main_w_c$EstimatedSubjKIndifferenceAllTrials[reltrials]<-
      estimate_indifference_point(
        Choice01 = data.main_w_c$Choice01[reltrials],
        assumedK = data.main_w_c$startingK[reltrials]
      )
    # var SALIENCE_CONDITION_AMOUNT_SALIENT=0;
    # var SALIENCE_CONDITION_DELAY_SALIENT=1;
    
    
    
    #amountSalient
    reltrialsAmountSalient<-reltrials & data.main_w_c$salienceCondition==0
    data.main_w_c$EstimatedSubjKIndifferenceAmountSalient[reltrialsAmountSalient]<-
      estimate_indifference_point(
        Choice01 = data.main_w_c$Choice01[reltrialsAmountSalient],
        assumedK = data.main_w_c$startingK[reltrialsAmountSalient]
      )
    
    #delaySalient
    reltrialsDelaySalient<-reltrials & data.main_w_c$salienceCondition==1
    data.main_w_c$EstimatedSubjKIndifferenceDelaySalient[reltrialsDelaySalient]<-
      estimate_indifference_point(
        Choice01 = data.main_w_c$Choice01[reltrialsDelaySalient],
        assumedK = data.main_w_c$startingK[reltrialsDelaySalient]
      )
    
    data.main_w_c$EstimatedSubjKIndifferenceBySalienceCondition[reltrialsAmountSalient]<-
      data.main_w_c$EstimatedSubjKIndifferenceAmountSalient[reltrialsAmountSalient]
                                                           
    data.main_w_c$EstimatedSubjKIndifferenceBySalienceCondition[reltrialsDelaySalient]<-
      data.main_w_c$EstimatedSubjKIndifferenceDelaySalient[reltrialsDelaySalient]
    
    
  }
  
  #additional tidying
  
  warning("We've artifically excluded a few values that had NA choice or abstraction level and haven't worked out why. There's only maybe 42-130 values that are unaccounted for, but should work out what's behind this.")
  #data.to.analyze<-data.main_w_c[RewardReality!="IncorrectlyToldReal" & IsPracticeTrial==FALSE & !is.na(Choice) & !is.na(AbstractionLevel)]
  data.to.analyze<-data.main_w_c[RewardReality!="IncorrectlyToldReal" & IsPracticeTrial==FALSE]
  data.itc<-data.to.analyze[!is.na(salienceCondition)]
  #data.itc$Choice<-factor(data.itc$Choice,ordered=TRUE,levels=c("randomized_counterbalanced","BlockDesign_abstract_first","BlockDesign_concrete_first"))
  
  data.itc$Choice<-factor(trimws(data.itc$Choice))
  data.itc$SSonLorR<-factor(trimws(data.itc$SSonLorR))
  data.itc$TaskArrangement<-factor(data.itc$TaskArrangement,ordered = TRUE, levels = c("TasksInterleaved","TasksBlocked"))
  data.itc$TaskArrangement<-factor(data.itc$TaskArrangement,ordered = TRUE, levels = c("TasksInterleaved","TasksBlocked"))
  data.itc$Group<-as.factor(data.itc$Group)
  data.itc$Group<-as.factor(data.itc$Group)
  data.itc$salienceCondition<-as.factor(ifelse(data.itc$salienceCondition==1,"DelaySalient","AmountSalient"))
  data.itc$SubjectUniqueID<-as.character(data.itc$SubjectUniqueID)
  data.itc[, TrialIdItc:=1:.N,by=SubjectUniqueID]#add a trial ID that counts out ITC trials in particular.
  data.itc[, TrialIdItcSC:=1:.N,by=.(SubjectUniqueID,salienceCondition)]#and one that counts ITC trials for a particular salienceCondition
  
  #for readability, I want to separate these out.
  data.itc$TaskArrangementBlocked<-data.itc$TaskArrangement=="TasksBlocked"
  data.itc$BlockDesignAbstractFirst<-data.itc$Group=="BlockDesign_abstract_first"
  data.itc$BlockDesignConcreteFirst<-data.itc$Group=="BlockDesign_concrete_first"
  data.itc$BlockDesignRandomized<-data.itc$Group=="randomized_counterbalanced"
  data.itc$ChoiceIsLL<-data.itc$Choice=="LL"
  
  data.itc$ButtonPress<-vector("character",length=dim(data.itc)[1])
  data.itc[Choice=="SS" & SSonLorR=="L",ButtonPress:="Left"]
  data.itc[Choice=="SS" & SSonLorR=="R",ButtonPress:="Right"]
  data.itc[Choice=="LL" & SSonLorR=="R",ButtonPress:="Left"]
  data.itc[Choice=="LL" & SSonLorR=="L",ButtonPress:="Right"]
  
  
  
  
  save(data.main_w_c,data.to.analyze,data.itc,file = preprocess_filename)
}


