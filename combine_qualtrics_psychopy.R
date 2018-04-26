library(data.table)
data.directory<-"~/Google Drive/neural-construal-level/data/ITCConstrual_mTurkExp/"
qual1<-"raw_files/qualtrics_march15/ITCConstrual_March+20,+2018_11.55_data.csv"
qual2<-"raw_files/qualtrics_march15/ITCConstrual-attempt2_March+20,+2018_11.54_data.csv"
qual3<-"raw_files/qualtrics_march15/ITCConstrualattempt2-Responses in Progress_data.csv"
qual1.fullpath<-paste0(data.directory,qual1)
qual2.fullpath<-paste0(data.directory,qual2)
qual3.fullpath<-paste0(data.directory,qual3)

dataset.1.raw<-read.csv(qual1.fullpath)
dataset.2.raw<-read.csv(qual2.fullpath)
dataset.3.raw<-read.csv(qual3.fullpath)
View(dataset.3.raw)
dataset.march15<-rbind(data.table(dataset.1.raw),data.table(dataset.2.raw),data.table(dataset.3.raw),fill=TRUE)
#now merge with the psychopy data using the ConditionCode and the subject ID.
colnames(dataset.march15)
source("get_psychopy_rewards_exp20180315.R")

data_qualtrics_psychopy<-merge(subjlist,dataset.march15,by.x=c("ConditionCode","SubID"),by.y=c("ConditionCode","ConditionQuota"))
View(data_qualtrics_psychopy[,c("ConditionCode","SubID","workerId","RewardDelay","RewardAmount","FinalK")])

