source("compile_psychopy_data.R")
source("get_qualtrics_data.R")
condition_info<-read.csv("~/Google Drive/neural-construal-level/code/SmithAdaptConstrualITCmTurk/condition_info.csv",stringsAsFactors = FALSE)
warning("this script assumes data was already downloaded from the webserver.")
########################################################################
#20180329
data.directory<-"~/Google Drive/neural-construal-level/data/ITCConstrual_mTurkExp/"
task_version="html20180328224216"
localdata_subdir="exp20180329/"
#source("download_datafiles.R")
#download_datafiles(task_version,localdata_subdir=localdata_subdir)
at<-compile_psychopy_data(task_version,localdata_subdir)
qualtrics.data<-get_qualtrics_data(paste0(data.directory,"raw_files/exp20180329/ITCConstrual-attempt5_March+29,+2018_17.11.csv"))# read.csv()
at$qualtricssid<-as.character(at$qualtricssid)
data_qualtrics_psychopy_all<-merge(at,qualtrics.data,by.x=c("ConditionCode","qualtricssid","IpAddressPsychoPy"),by.y=c("ConditionCode","ConditionQuota","IPAddress"),
                                   all.x = TRUE)

data_qualtrics_psychopy_all$task_version<-task_version
data_full_20180329<-merge(data_qualtrics_psychopy_all,condition_info,
                          by="ConditionCode")
########################################################################
#20180327
data.directory<-"~/Google Drive/neural-construal-level/data/ITCConstrual_mTurkExp/"
task_version="html20180321233637"
localdata_subdir="exp20180327/"


#download the datafiles
#download_datafiles(task_version,localdata_subdir=localdata_subdir)
#combine the files.

at<-compile_psychopy_data(task_version,localdata_subdir)
#now we have the psychopy data. Time to get the qualtrics data.
qualtrics.data<-get_qualtrics_data(paste0(data.directory,"raw_files/exp20180327/ITCConstrual-attempt4_March+29,+2018_11.43_data.csv"))# read.csv()


#we might also want to get the psychopy rewards table. this is a slightly distinct process.
metaloop_dir<-"~/Google Drive/neural-construal-level/code/SmithAdaptConstrualITCmTurk/html-gen/"
raw.psychopy.filedir<-paste0(data.directory,"raw_files/",localdata_subdir,task_version,"/")

data_qualtrics_psychopy_all<-merge(at,qualtrics.data,by.x=c("ConditionCode","qualtricssid","IpAddressPsychoPy"),by.y=c("ConditionCode","ConditionQuota","IPAddress"),
                                   all.x = TRUE)

data_qualtrics_psychopy_all$task_version<-task_version
data_full_20180327<-merge(data_qualtrics_psychopy_all,condition_info,
                                      by="ConditionCode")

########################################################################
#20180315
data.directory<-"~/Google Drive/neural-construal-level/data/ITCConstrual_mTurkExp/"
task_version="html20180315014343"
localdata_subdir=""
at<-compile_psychopy_data(task_version,localdata_subdir)
qualtrics.data<-rbind(
  get_qualtrics_data(paste0(data.directory,"raw_files/qualtrics_march15/ITCConstrual-attempt2_March+20,+2018_11.54_data.csv")),# read.csv()
  get_qualtrics_data(paste0(data.directory,"raw_files/qualtrics_march15/ITCConstrual_March+20,+2018_11.55_data.csv")),fill=TRUE)
get_qualtrics_data(paste0(data.directory,"raw_files/qualtrics_march15/ITCConstrualattempt2-Responses in Progress_data.csv"))
metaloop_dir<-"~/Google Drive/neural-construal-level/code/SmithAdaptConstrualITCmTurk/html-gen/"
raw.psychopy.filedir<-paste0(data.directory,"raw_files/",localdata_subdir,task_version,"/")
condition_info<-read.csv("~/Google Drive/neural-construal-level/code/SmithAdaptConstrualITCmTurk/condition_info.csv",stringsAsFactors = FALSE)

data_qualtrics_psychopy_all<-merge(at,qualtrics.data,by.x=c("ConditionCode","qualtricssid","IpAddressPsychoPy"),by.y=c("ConditionCode","ConditionQuota","IPAddress"),
                                   all.x = TRUE)
data_qualtrics_psychopy_all$task_version<-task_version
data_full_20180315<-merge(data_qualtrics_psychopy_all,condition_info,
                                      by="ConditionCode")



data_all<-rbind(data_full_20180315,data_full_20180327,data_full_20180329, fill=TRUE)
colnames(data_full_20180315)
data_all_anon<-data_all
data_all_anon$workerId<-NULL

write.csv(data_all_anon,paste0(data.directory,"all_data_anon_2018-03-29.csv"))

hist(data_all_anon$SSamount)
hist(data_all_anon$LLamount)
table(data_all_anon$Choice)
table(data_all_anon$Condition)
table(data_all_anon$Condition.x)
table(data_all_anon$Condition.y)
table(data_all_anon$Group.x,data_all_anon$Group.y)
length(unique(data_all_anon$DataSource))
#find out how much money was promised to people for HYPOTHETICAL tasks.

