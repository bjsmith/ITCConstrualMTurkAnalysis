data.directory<-"~/Google Drive/neural-construal-level/data/ITCConstrual_mTurkExp/"
task_version="html20180328224216"
localdata_subdir="exp20180329/"
source("compile_psychopy_data.R")
source("get_qualtrics_data.R")
#source("download_datafiles.R")
#download_datafiles(task_version,localdata_subdir=localdata_subdir)
at<-compile_psychopy_data(task_version,localdata_subdir)
qualtrics.data<-get_qualtrics_data(paste0(data.directory,"raw_files/exp20180329/ITCConstrual-attempt5_March+29,+2018_17.11.csv"))# read.csv()
at$qualtricssid<-as.character(at$qualtricssid)
data_qualtrics_psychopy_all<-merge(at,qualtrics.data,by.x=c("ConditionCode","qualtricssid","IpAddressPsychoPy"),by.y=c("ConditionCode","ConditionQuota","IPAddress"),
                                   all.x = TRUE)

data_qualtrics_psychopy_all$task_version<-task_version
turkprime.data.1<-cbind(read.csv(paste0(data.directory,"raw_files/exp20180329/ITC Construal test 1-attempt8 [Categorization game task for experiment(_ 15 minutes)] (80910).csv"), stringsAsFactors = FALSE),TurkPrimeRun="ITC Construal test 1-attempt8")
turkprime.data<-rbind(turkprime.data.1)

#this set includes all the data collected in the Qualtrics set 4.
#It includes TurkPrime responses ITCConstrual-attempt{4-7}
#and is all html20180321233637

#we might also want to get the psychopy rewards table. this is a slightly distinct process.
metaloop_dir<-"~/Google Drive/neural-construal-level/code/SmithAdaptConstrualITCmTurk/html-gen/"
raw.psychopy.filedir<-paste0(data.directory,"raw_files/",localdata_subdir,task_version,"/")
condition_info<-read.csv("~/Google Drive/neural-construal-level/code/SmithAdaptConstrualITCmTurk/condition_info.csv",stringsAsFactors = FALSE)

source("get_psychopy_rewards.R")
subjlist<-get_psychopy_rewards(data.directory,metaloop_dir,task_version,raw.psychopy.filedir,condition_info)

subjlist_wips<-data.table(merge(subjlist,
                                qualtrics.data[,.(ConditionQuota,ConditionCode,IPAddress,workerId)],
                                by.x=c("SubID","ConditionCode","IpAddress"),by.y=c("ConditionQuota","ConditionCode","IPAddress"),all.x=TRUE))
subjlist_wips_tp<-merge(turkprime.data,qualtrics.data,by.x="AmazonIdentifier","workerId")
#RECORD OF SUBJECTS PAID OUT ADDITIONAL AMOUNTS DUE TO THE HYPOTHETICAL-REAL MIXUP
subjlist_wips_tp<-data.table(merge(subjlist_wips,turkprime.data,by.x="workerId",by.y="AmazonIdentifier"))

source("generate_bonus_table.R")
generate_bonus_table(
  subjlist_wips_tp,
  as.POSIXct(bonus_date),
  paste0(data.directory,"raw_files/exp20180329/")
)

#now the unmatched subjects who are recorded in my TurkPrime data
unmatched.subjects<-turkprime.data[!(turkprime.data$AmazonIdentifier %in% subjlist_wips_tp$workerId),]
#no bonus for these folks. These completion codes are suspicious.