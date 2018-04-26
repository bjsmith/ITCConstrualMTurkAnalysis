library(ggplot2)
library(data.table)
source("load_and_preprocess_compiled_data20180330.R")

data.main_w_c[,.(SubjectsInGroup=length(unique(SubjectUniqueID))),by=.(ConditionCode,RewardReality,Condition.y)]

data.main_w_c[RewardReality=="IncorrectlyToldReal",.(SubjectsInGroup=length(unique(SubjectUniqueID))),by=.(ConditionCode,task_version,Condition.y)]

data.main_w_c[,.(SubjectsInGroup=length(unique(SubjectUniqueID))),by=.(ConditionCode,task_version,Condition.y)]
data.main_w_c[,.(SubjectsInGroup=length(unique(SubjectUniqueID))),by=.(task_version,Condition.y)]

data.main_w_c[task_version=="html20180321233637",.(SubjectsInGroup=length(unique(SubjectUniqueID))),by=.(ConditionCode,RewardReality)]
data.main_w_c[RewardReality!="IncorrectlyToldReal",.(SubjectsInGroup=length(unique(SubjectUniqueID))),by=.(ConditionCode,Condition.y)]
View(data.main_w_c[RewardReality!="IncorrectlyToldReal" & is.na(Condition.y)])

#counts of subjects we actualy have.
data.main_w_c[,.(SubjectsInGroup=length(unique(SubjectUniqueID))),by=.(ConditionCode,RewardReality,Condition.y)]

data.main_w_c[RewardReality!="IncorrectlyToldReal",.(SubjectsInGroup=length(unique(SubjectUniqueID))),by=.(ConditionCode,RewardReality)]
