
#html20180329165253


data.directory<-"~/Google Drive/neural-construal-level/data/ITCConstrual_mTurkExp/"
task_version="html20180329165253"
localdata_subdir="exp20180330/"
#source("download_datafiles.R")
#download the datafiles
#download_datafiles(task_version,localdata_subdir=localdata_subdir)
#combine the files.
source("compile_psychopy_data.R")
at<-compile_psychopy_data(task_version,localdata_subdir,verbose=TRUE)

data_qualtrics_psychopy_all<-merge(at,qualtrics.data,by.x=c("qualtricsresponseid","ConditionCode"),by.y=c("ResponseId","ConditionCode"),all.x=TRUE)

data_qualtrics_psychopy_all$ButtonPress<-vector("character",length=dim(data_qualtrics_psychopy_all)[1])
data_qualtrics_psychopy_all[trimws(Choice)=="SS" & trimws(SSonLorR)=="L",ButtonPress:="Left"]
data_qualtrics_psychopy_all[trimws(Choice)=="SS" & trimws(SSonLorR)=="R",ButtonPress:="Right"]
data_qualtrics_psychopy_all[trimws(Choice)=="LL" & trimws(SSonLorR)=="R",ButtonPress:="Left"]
data_qualtrics_psychopy_all[trimws(Choice)=="LL" & trimws(SSonLorR)=="L",ButtonPress:="Right"]
data_qualtrics_psychopy_all[which(data_qualtrics_psychopy_all$DataSource=="L62QJAAN__Mon_Apr_09_2018_230954_GMT-0700_PDT_104.35.229.11.csv"),ButtonPress]