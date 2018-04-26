library(plyr)
library(data.table)
library(tidyr)
data.directory<-"~/Google Drive/neural-construal-level/data/ITCConstrual_mTurkExp/"
metaloop_dir<-"~/Google Drive/neural-construal-level/code/SmithAdaptConstrualITCmTurk/html-gen/"
task_version="html20180315014343"
raw.filedir<-paste0(data.directory,"raw_files/",task_version,"/")
condition_info<-read.csv("~/Google Drive/neural-construal-level/code/SmithAdaptConstrualITCmTurk/condition_info.csv")

names(condition_info)[names(condition_info)=="Condition"]<-"RewardReality"

raw.filenames<-list.files(raw.filedir)

source("get_psychopy_datafile.R")
all_miniblocks<-NULL
all_trials<-NULL
for (rfn in raw.filenames){#rfn<-raw.filenames[2]#rfn<-"YGFIWQQF__Thu_Mar_15_2018_112759_GMT-0800_AKDT_107.77.205.115.csv"
  print(rfn)
  pruned_csv<-data.table(get_psychopy_datafile(rfn=rfn,raw.filedir=raw.filedir,task_version=task_version))
  #now, I think we need to take tasks that include two choices on a row and split them into separate rows.
  if ("SSamount1" %in% colnames(pruned_csv) | "LLamount1" %in% colnames(pruned_csv)){
    print(paste0("merging for condition ",pruned_csv$ConditionCode[1]))
    doubleTrial=TRUE
    #we need to split.
  
    row_trial1_colnames<-colnames(pruned_csv)[endsWith(colnames(pruned_csv),"1")]
    row_trial2_colnames<-colnames(pruned_csv)[endsWith(colnames(pruned_csv),"2")]
    basecolnames1<-sapply(row_trial1_colnames,function(x){substring(x,1,nchar(x)-1)})
    basecolnames2<-sapply(row_trial2_colnames,function(x){substring(x,1,nchar(x)-1)})
    #remove the values that we don't want
    other_colnames<-colnames(pruned_csv)[
      !(colnames(pruned_csv) %in% union(union(row_trial1_colnames,row_trial2_colnames),basecolnames1))]
    #with this many columns, I'm not sure how to use gather, so I'll do it 'manually'...
    row_trial1<-pruned_csv[,c(other_colnames,row_trial1_colnames),with=FALSE]
    row_trial2<-pruned_csv[,c(other_colnames,row_trial2_colnames),with=FALSE]
    #View(row_trial1)
    row_trial1<-rename(row_trial1,basecolnames1)
    row_trial2<-rename(row_trial2,basecolnames2)
    list_by_trial<-rbind(row_trial1,row_trial2) %>% .[order(choiceUp)]
  }else{
    print(paste0("not merging for condition ",pruned_csv$ConditionCode[1]))
    list_by_trial<-pruned_csv %>% .[order(choiceUp)]
  }
  
  if(is.null(all_trials)){
    all_miniblocks<-pruned_csv
    all_trials<-list_by_trial
  }else{
    all_miniblocks<-rbind(all_miniblocks,pruned_csv,fill=TRUE)
    all_trials<-rbind(all_trials,list_by_trial,fill=TRUE)
  }
  print(table(all_trials$ConditionCode))
}
all_trials<-merge(data.table(data.frame(all_trials)),condition_info,by="ConditionCode")
all_trials$KChange<-all_trials$endingK-all_trials$startingK
all_trials$KChange<-all_trials$endingK-all_trials$startingK
all_trials$salienceCondition<-as.factor(all_trials$salienceCondition)
all_trials$SubjectCondition<-paste0(all_trials$ConditionCode,all_trials$Subject)
library(ggplot2)
all_trials$Choice<-trimws(all_trials$Choice)
summary_measures<-all_trials[,.(SCMean=mean(endingK),SCSD=sd(endingK),SCN=.N),by=SubjectCondition]
#we have some absurdly high k-values going on here - it's weird and I wonder what happened?
all_trials2<-merge(data.table(data.frame(all_trials)),summary_measures) %>% .[SCMean<10 & SCSD<10 & SCN>10]


ggplot(all_trials2,aes(x=choiceUp,y=KChange,group=SubjectCondition,color=SubjectCondition,shape=salienceCondition))+geom_point()+geom_line()+
  coord_cartesian(ylim=c(-1,1))


ggplot(all_trials2,aes(x=choiceUp,y=log(endingK),group=SubjectCondition,color=SubjectCondition,shape=salienceCondition))+geom_point()+geom_line()+
  coord_cartesian(ylim=c(-9,0))

ggplot(all_trials2,aes(x=salienceCondition,y=KChange))+geom_boxplot()+coord_cartesian(ylim=c(-1,1))

ggplot(all_trials2,aes(x=salienceCondition,y=KChange))+geom_violin(adjust=10,trim=TRUE)+geom_jitter()+coord_cartesian(ylim=c(-.1,.1))

all_trials2[,.(Count=.N),by=.(Choice,salienceCondition,SubjectCondition)] %>% spread(Choice,Count) %>% 
  .[,.(SSProportion=SS/(LL+SS)),by=.(salienceCondition,SubjectCondition)] %>% spread(salienceCondition,SSProportion)


t.test(KChange~salienceCondition,all_trials2)
t.test(endingK~salienceCondition,all_trials2)

print(format(all_trials[,.(M=mean(endingK),SD=sd(endingK),N=.N),by=SubjectCondition],scientific=FALSE))

#clean up by removing subjects with very few trials and subjects with absurdly high means or SDs.


#Do we have a difference in trials chosen?
#yes we do.
all_trials_with_choices<-all_trials[trimws(Choice)!="" & !is.na(salienceCondition)]
#chisq.test(all_trials_with_choices$salienceCondition,all_trials_with_choices$Choice)
#the chi-sq isn't really valid because this isn't across-subject data.
table(all_trials_with_choices$salienceCondition,all_trials_with_choices$Choice)
table(all_trials_with_choices$Choice,all_trials_with_choices$Condition)
# var SALIENCE_CONDITION_AMOUNT_SALIENT=0; IE DELAY FIXED
# var SALIENCE_CONDITION_DELAY_SALIENT=1; AMOUNT FIXED
all_trials$ConditionCode

table(all_trials$RewardReality,all_trials$Choice)
