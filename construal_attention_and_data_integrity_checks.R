library(ggplot2)
source("load_and_preprocess_compiled_data20180330.R")

#we really need to validate that they pay attention to the abstraction condition.
table(data.main_w_c$ChoiceConstrual)
#this is good.

table(data.main_w_c$ChoiceConstrual,data.main_w_c$Answer)
#generally good. But let's get performance by subject.
table(data.main_w_c$Choice)

length(unique(data.main_w_c$SubjectUniqueID))

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
                        Condition,
                        SalienceCondition..firstRewardScheduleItem.,
                        Group,
                        TaskArrangement
                        )]
performance_by_subj[,.(ProportionOfSubjectsWithManyMissingConstrualChoices=sum(ConstrualNAChoiceProportion>0.6)/.N),by=task_version]

#The square root of the product of the proportion of correctly detected matches and the proportion of correctly detected mismatches
performance_by_subj$CompositePerformance<-sqrt(performance_by_subj$ConstrualMismatchTrialProportionDetected*performance_by_subj$ConstrualMatchTrialProportionDetected)
summary(performance_by_subj$CompositePerformance)
ggplot(performance_by_subj[ConstrualNAChoiceProportion<0.6],aes(ConstrualMismatchTrialProportionDetected,ConstrualMatchTrialProportionDetected,color=task_version))+
  geom_point(alpha=0.5)+geom_jitter(width = 0.02,height=0.02)+
  geom_hline(aes(yintercept=mean(ConstrualMatchTrialProportionDetected)),linetype="dashed")+
  geom_vline(aes(xintercept=mean(ConstrualMismatchTrialProportionDetected)),linetype="dashed")+
  ggtitle("Performance for correctly recognizing match and mismatch construal trials by subject",subtitle = "By Task Version, EXCLUDES subjects with many missing construal choices")

ggplot(performance_by_subj[task_version=="html20180329165253"],aes(ConstrualMismatchTrialProportionDetected,ConstrualMatchTrialProportionDetected,color=task_version))+
  geom_point(alpha=0.5)+geom_jitter(width = 0.02,height=0.02)+
  geom_hline(aes(yintercept=mean(ConstrualMatchTrialProportionDetected)),linetype="dashed")+
  geom_vline(aes(xintercept=mean(ConstrualMismatchTrialProportionDetected)),linetype="dashed")+
  ggtitle("Performance for correctly recognizing match and mismatch construal trials by subject",subtitle = "html20180329165253 task version")


table(performance_by_subj$ConstrualMismatchTrialProportionDetected)
table(performance_by_subj$ConstrualMatchTrialProportionDetected)

sum(performance_by_subj$ConstrualMismatchTrialProportionDetected>0.25)/length(performance_by_subj$ConstrualMismatchTrialProportionDetected)

View(performance_by_subj[,.(ConstrualMatchTrialProportionDetected<0.05,SubjectUniqueID)])

#case study: this subject had no good construal data.
subj_no_construal<-data.main_w_c[SubjectUniqueID %in% performance_by_subj[ConstrualMatchTrialProportionDetected<0.05,SubjectUniqueID][[1]]]
View(subj_no_construal)
subj_no_construal$Construal_AnswerText
table(subj_no_construal$ChoiceConstrual)


View(performance_by_subj[ConstrualMatchTrialProportionDetected<0.05,])
View(performance_by_subj[,.(SubjectUniqueID,task_version)])

table(performance_by_subj$ConditionCode)



###
exclusion_stats<-data.main_w_c[,.(
  LeftProp=sum(ChoiceKey==1,na.rm = TRUE)/sum(!is.na(ChoiceKey)),
  RightProp=sum(ChoiceKey==2,na.rm = TRUE)/sum(!is.na(ChoiceKey)),
  ConstrualChoice1Prop=sum(ChoiceConstrual==1,na.rm = TRUE)/sum(!is.na(Answer)),
  ConstrualChoice2Prop=sum(ChoiceConstrual==2,na.rm = TRUE)/sum(!is.na(Answer)),
  finalK=endingK[.N]),by=.(SubjectUniqueID,ConditionCode)]
exclusion_stats[ConstrualChoice1Prop==0,.(SubjectUniqueID,ConditionCode)] %>%.[,.N,ConditionCode]
exclusion_stats[ConstrualChoice1Prop==0,.(SubjectUniqueID,ConditionCode)]
View(data.main_w_c[SubjectUniqueID=="GSS2U1EFThuMar152018161209Gm4EDT1732392407"])