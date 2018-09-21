#this set includes all the data collected in the Qualtrics set 4.
#It includes TurkPrime responses ITCConstrual-attempt{4-7}
#and is all html20180321233637


data.directory<-"~/Google Drive/neural-construal-level/data/ITCConstrual_mTurkExp/"
task_version="html20180321233637"
localdata_subdir="exp20180327/"
#source("download_datafiles.R")
#download the datafiles
#download_datafiles(task_version,localdata_subdir=localdata_subdir)
#combine the files.
source("compile_psychopy_data.R")
at<-compile_psychopy_data(task_version,localdata_subdir)
#now we have the psychopy data. Time to get the qualtrics data.
source("get_qualtrics_data.R")
qualtrics.data<-get_qualtrics_data(paste0(data.directory,"raw_files/exp20180327/ITCConstrual-attempt4_March+29,+2018_11.43_data.csv"))# read.csv()
turkprime.data.1<-cbind(read.csv(paste0(data.directory,"raw_files/exp20180327/ITC Construal test 1-attempt4 [Categorization game task for experiment(_ 15 minutes)] (79973).csv"), stringsAsFactors = FALSE),TurkPrimeRun="ITC Construal test 1-attempt4")
turkprime.data.2<-cbind(read.csv(paste0(data.directory,"raw_files/exp20180327/ITC Construal test 1-attempt5 [Categorization game task for experiment(_ 15 minutes)] (80150).csv"), stringsAsFactors = FALSE),TurkPrimeRun="ITC Construal test 1-attempt5")
turkprime.data.3<-cbind(read.csv(paste0(data.directory,"raw_files/exp20180327/ITC Construal test 1-attempt6 [Categorization game task for experiment(_ 15 minutes)] (80517).csv"), stringsAsFactors = FALSE),TurkPrimeRun="ITC Construal test 1-attempt6")
turkprime.data.4<-cbind(read.csv(paste0(data.directory,"raw_files/exp20180327/ITC Construal test 1-attempt7 [Categorization game task for experiment(_ 15 minutes)] (80745).csv"), stringsAsFactors = FALSE),TurkPrimeRun="ITC Construal test 1-attempt7")
turkprime.data<-rbind(turkprime.data.1,turkprime.data.2,turkprime.data.3,turkprime.data.4)


#we might also want to get the psychopy rewards table. this is a slightly distinct process.
metaloop_dir<-"~/Google Drive/neural-construal-level/code/SmithAdaptConstrualITCmTurk/html-gen/"
raw.psychopy.filedir<-paste0(data.directory,"raw_files/",localdata_subdir,task_version,"/")
condition_info<-read.csv("~/Google Drive/neural-construal-level/code/SmithAdaptConstrualITCmTurk/condition_info.csv",stringsAsFactors = FALSE)


source("get_psychopy_rewards_5.R")
subjlist<-get_psychopy_rewards_5(data.directory,metaloop_dir,task_version,raw.psychopy.filedir,condition_info)

subjlist_wips<-data.table(merge(subjlist,
                                qualtrics.data[,.(ConditionQuota,ConditionCode,IPAddress,workerId)],
                                by.x=c("SubID","ConditionCode","IpAddress"),by.y=c("ConditionQuota","ConditionCode","IPAddress"),all.x=TRUE))

#subjlist_wips_tp<-merge(turkprime.data,qualtrics.data,by.x="AmazonIdentifier",by.y="workerId")
subjlist_wips_tp<-data.table(merge(subjlist_wips,turkprime.data,by.x="workerId",by.y="AmazonIdentifier"))
source("generate_bonus_table.R")
generate_bonus_table(
  data.table(subjlist_wips_tp),
  as.POSIXct(bonus_date),
  paste0(data.directory,"raw_files/exp20180327/"),
  startdate = bonus_start_date
)
