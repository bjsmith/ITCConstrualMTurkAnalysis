library(data.table)
library(dplyr)
source("load_and_preprocess_compiled_data20180330.R")

table(is.na(data.main$choiceUp))
options(scipen=10)

View(data.main_w_c[order(choiceUp)] %>% .[,.(finalK=endingK[.N],meanK=mean(endingK,na.rm=TRUE)),by=.(SubjectUniqueID,ConditionCode,Condition,SalienceCondition..firstRewardScheduleItem.,Group,TaskArrangement,task_version)])

data.main_w_c[order(choiceUp)] %>% 
  .[,.(finalK=endingK[.N],meanK=mean(endingK,na.rm=TRUE)),by=.(SubjectUniqueID,ConditionCode,Condition,SalienceCondition..firstRewardScheduleItem.,Group,TaskArrangement,task_version)] %>%
  .[finalK>5000,SubjectUniqueID]

data.main_w_c[order(choiceUp)] %>% .[SubjectUniqueID=="RZYPUKWOWedApr042018142825Gm4EDT1071521413"]
View(data.main_w_c[order(choiceUp)] %>% .[SubjectUniqueID=="GSS2U1EFWedApr042018163909Gm400EDT508913725"])
data.main_w_c[order(choiceUp)] %>% .[endingK>30,]
