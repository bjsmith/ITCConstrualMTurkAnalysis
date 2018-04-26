library(ggplot2)
source("load_and_preprocess_compiled_data20180330.R")


performance_by_subj<-
  data.main_w_c[,.(ConstrualCorrectAccept=sum(ChoiceConstrual==Answer & ChoiceConstrual==1,na.rm = TRUE),
                   ConstrualCorrectReject=sum(ChoiceConstrual==Answer & ChoiceConstrual==2,na.rm = TRUE),
                   ConstrualIncorrectAccept=sum(ChoiceConstrual==1 & Answer==2,na.rm = TRUE),
                   ConstrualIncorrectReject=sum(ChoiceConstrual==2 & Answer==1,na.rm = TRUE),
                   ConstrualMismatchTrialProportionDetected=sum(Answer==2 & ChoiceConstrual==2,na.rm = TRUE)/sum(Answer==2,na.rm = TRUE),
                   ConstrualMatchTrialProportionDetected=sum(Answer==1 & ChoiceConstrual==1,na.rm = TRUE)/sum(Answer==1,na.rm = TRUE),
                   ConstrualNAChoiceSum=sum(is.na(ChoiceConstrual)),
                   ConstrualNAChoiceProportion=sum(is.na(ChoiceConstrual))/.N,
                   ITCChoiceSS=sum(Choice=="SS"),
                   ITCChoiceLL=sum(Choice=="LL")
  ),by=.(SubjectUniqueID,
         ConditionCode,
         task_version,
         RewardReality,
         SalienceCondition..firstRewardScheduleItem.,
         Group,
         TaskArrangement
  )]

performance_by_subj[,.(SubjectMeanSSChoices=mean(ITCChoiceSS),
                       SubjectMeanLLChoices=mean(ITCChoiceLL),
                       Subjects=.N
                       ),by=.(RewardReality,
                    SalienceCondition..firstRewardScheduleItem.,
                    Group,
                    TaskArrangement)]