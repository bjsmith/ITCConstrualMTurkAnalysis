library(ggplot2)
source("load_and_preprocess_compiled_data20180330.R")
#alright, now, we need to get this data in a shape where every row contains a choice with its preceeding construal trial.
ChoicesByCondition=data.main_w_c[
  !IsPracticeTrial & 
    !(task_version %in% c("html20180315014343")), #exclude this task version because we just forgot to record construal choices for this version! so it's irrelevant.
  .(#ChoiceSS=sum(Choice=="SS"),
    #ChoiceLL=sum(Choice=="SS"),
    #ChoiceNA=sum(Choice=="SS"),
    #ProportionWithPrecedingConstrualChoice=sum(!is.na(ChoiceConstrual))/.N,
    ProportionWithPrecedingConstrualQuestionText=sum(!is.na(Construal_QuestionText))/.N,
    ProportionWithRunID=sum(!is.na(runid))/.N
  )
  ,by=.(#SalienceCondition..firstRewardScheduleItem.,
    Group,
    TaskArrangement,
    task_version
  )] %>% .[order(ProportionWithPrecedingConstrualQuestionText)]
ChoicesByCondition
#right, so for the concrete_first or abstract_first tasks blocked, how do we mark out construal conditions? let's take one subject...
mysubj<-data.main_w_c[Group=="BlockDesign_concrete_first",.N,by=SubjectUniqueID]$SubjectUniqueID[[1]]
table(data.main_w_c$Construal_Condition_Block)

table(data.main_w_c$ConstrualConditionAllocated)
#3 is most abstract, followed by 2, 5, and then 4. We can't fit 1 or 6 into the strict hierarchy
#therefore


#what are the abstraction levels - have we got them ordered well?
View(data.main_w_c[,.N,.(Construal_Condition,Construal_Condition_Block,Construal_QuestionText,Construal_AnswerText) ])