data.directory<-"~/Google Drive/neural-construal-level/data/ITCConstrual_mTurkExp/"
task_version="html20180315014343"
localdata_subdir=""
source("download_datafiles.R")
#download the datafiles
#download_datafiles(task_version,localdata_subdir=localdata_subdir)
#combine the files.
source("compile_psychopy_data.R")
at<-compile_psychopy_data(task_version,localdata_subdir)
#now we have the psychopy data. Time to get the qualtrics data.
source("get_qualtrics_data.R")
library(data.table)

qualtrics.data<-rbind(
  get_qualtrics_data(paste0(data.directory,"raw_files/qualtrics_march15/ITCConstrual-attempt2_March+20,+2018_11.54_data.csv")),# read.csv()
  get_qualtrics_data(paste0(data.directory,"raw_files/qualtrics_march15/ITCConstrual_March+20,+2018_11.55_data.csv")),fill=TRUE)
get_qualtrics_data(paste0(data.directory,"raw_files/qualtrics_march15/ITCConstrualattempt2-Responses in Progress_data.csv"))
#turkprime.data<-read.csv(paste0(data.directory,"raw_files/exp20180321/turkprime-ITC Construal test 1-attempt4 [Categorization game task for experiment(_ 15 minutes)] (79973).csv"),,stringsAsFactors = FALSE)


#we might also want to get the psychopy rewards table. this is a slightly distinct process.
metaloop_dir<-"~/Google Drive/neural-construal-level/code/SmithAdaptConstrualITCmTurk/html-gen/"
raw.psychopy.filedir<-paste0(data.directory,"raw_files/",localdata_subdir,task_version,"/")
condition_info<-read.csv("~/Google Drive/neural-construal-level/code/SmithAdaptConstrualITCmTurk/condition_info.csv",stringsAsFactors = FALSE)
debugSource("get_psychopy_rewards.R")
subjlist<-get_psychopy_rewards(data.directory,metaloop_dir,task_version,raw.psychopy.filedir,condition_info)
subjlist$ConditionCode<-as.character(subjlist$ConditionCode)
subjlist$IpAddress<-as.character(subjlist$IpAddress)
qualtrics.data$ConditionCode
qualtrics.data$ConditionQuota
subjlist_wips<-merge(subjlist,
                     qualtrics.data[,.(ConditionQuota,ConditionCode,IPAddress,workerId)],
                     by.x=c("SubID","ConditionCode","IpAddress"),by.y=c("ConditionQuota","ConditionCode","IPAddress"),
                     all.x = TRUE)

data_qualtrics_psychopy_all<-merge(at,qualtrics.data,by.x=c("ConditionCode","Subject","IpAddressPsychoPy"),by.y=c("ConditionCode","ConditionQuota","IPAddress"),
                                   all.x = TRUE)
unmatched_psychopy_responses_summary<-data_qualtrics_psychopy_all[is.na(data_qualtrics_psychopy_all$ResponseId),.N,
                                                                  by=.(DataSource,ConditionCode,Subject,IpAddressPsychoPy)]

matched_psychopy_responses_summary<-data_qualtrics_psychopy_all[!is.na(data_qualtrics_psychopy_all$ResponseId),.N,
                                                                by=.(DataSource,ConditionCode,Subject,IpAddressPsychoPy)]

print(paste0("Warning: ",dim(unmatched_psychopy_responses_summary)[1], " psychopy responses were unmatched by a qualtrics entry. These may have been test runs, but check."))
