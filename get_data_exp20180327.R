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
  as.POSIXct("2018-04-07"),
  paste0(data.directory,"raw_files/exp20180327/")
)


#RECORD OF SUBJECTS PAID OUT ADDITIONAL AMOUNTS DUE TO THE HYPOTHETICAL-REAL MIXUP
#subjlist_wips_tp<-data.table(merge(subjlist_wips,turkprime.data,by.x="workerId",by.y="AmazonIdentifier"))
subjlist_wips_tp[workerId=="A3DSR11QOMALGX"]
subjlist_wips_tp[workerId=="A2PGJXWUMD0M2I"]



#REWARDS IF WE GAVE THEM ALL TEH ACTUAL HYPOTHETICAL AMOUNTS.
#ALSO MATCHES THE BUG IN EARLIER VERSIONS OF THE JAVASCRIPT
source("get_psychopy_rewards_3.R")
subjlist_3<-get_psychopy_rewards_3(data.directory,metaloop_dir,task_version,raw.psychopy.filedir,condition_info)

subjlist_wips_3<-merge(subjlist_3,
                     qualtrics.data[,.(ConditionQuota,ConditionCode,IPAddress,workerId)],
                     by.x=c("SubID","ConditionCode","IpAddress"),by.y=c("ConditionQuota","ConditionCode","IPAddress"),all.x=TRUE)

subjlist_wips_tp_3<-data.table(merge(subjlist_wips_3,turkprime.data,by.x="workerId",by.y="AmazonIdentifier"))

subjlist_wips_tp_3[workerId=="A2PGJXWUMD0M2I"]
View(subjlist_wips_3)
sum(subjlist_wips_3$RewardAmount)
sum(unlist(lapply(subjlist_wips_2$RewardAmount,function(x){min(x,5)})))


View(subjlist_wips_tp)

source("generate_bonus_table.R")
generate_bonus_table(
  subjlist_wips_tp_3,
  as.POSIXct("2018-04-02"),
  paste0(data.directory,"raw_files/exp20180327/")
  )

#now the unmatched subjects who are recorded in my TurkPrime data
unmatched.subjects<-turkprime.data[!(turkprime.data$AmazonIdentifier %in% subjlist_wips_tp$workerId),]
unmatched.subjects$workerId<-unmatched.subjects$AmazonIdentifier
unmatched.subjects$RewardAmount<-0.6
unmatched.subjects$RewardDate<-as.POSIXct("2018-03-29")
generate_bonus_table(
    data.table(unmatched.subjects),
    as.POSIXct("2018-04-02"),
    paste0(data.directory,"raw_files/exp20180327/unmatched_")
  )

#how many Qualtrics entries can't be properly matched to turkPrime?
turkprime_qualtrics<-merge(turkprime.data,qualtrics.data,by.x="AmazonIdentifier","workerId")
View(turkprime.data[!(turkprime.data$AmazonIdentifier %in% turkprime_qualtrics$AmazonIdentifier),])

############
data_qualtrics_psychopy_all<-merge(at,qualtrics.data,by.x=c("ConditionCode","Subject","IpAddressPsychoPy"),by.y=c("ConditionCode","ConditionQuota","IPAddress"),
                                   all.x = TRUE)

data_qualtrics_psychopy<-data_qualtrics_psychopy_all[!is.na(data_qualtrics_psychopy_all$ResponseId)]

ggplot(data_qualtrics_psychopy,
       aes(x=Trial,y=log(endingK),group=interaction(ResponseId,runid),color=salienceCondition))+geom_line()+
  coord_cartesian(ylim=c(-10,10))