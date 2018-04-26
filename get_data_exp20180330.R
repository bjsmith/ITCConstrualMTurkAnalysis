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
length(unique(at$DataSource))
#now we have the psychopy data. Time to get the qualtrics data.
source("get_qualtrics_data.R")
qualtrics.data<-get_qualtrics_data(paste0(data.directory,"raw_files/exp20180330/ITCConstrual-attempt6_April+11,+2018_23.06.csv"))# read.csv()
turkprime.data.1<-cbind(read.csv(paste0(data.directory,"raw_files/exp20180330/ITC Construal test 1-attempt9 [Categorization game task for experiment(_ 15 minutes)] (81100).csv"), stringsAsFactors = FALSE),TurkPrimeRun="ITC Construal test 1-attempt9")
turkprime.data.2<-cbind(read.csv(paste0(data.directory,"raw_files/exp20180330/ITC Construal test 1-attempt10 [Categorization game task for experiment(_ 15 minutes)] (81229).csv"), stringsAsFactors = FALSE),TurkPrimeRun="ITC Construal test 1-attempt10")
turkprime.data.3<-cbind(read.csv(paste0(data.directory,"raw_files/exp20180330/ITC Construal test 1-attempt11 [Categorization game task for experiment(_ 15 minutes)] (82025).csv"), stringsAsFactors = FALSE),TurkPrimeRun="ITC Construal test 1-attempt11")
turkprime.data.4<-cbind(read.csv(paste0(data.directory,"raw_files/exp20180330/ITC Construal test 1-attempt12 [Categorization game task for experiment(_ 15 minutes)] (82221).csv"), stringsAsFactors = FALSE),TurkPrimeRun="ITC Construal test 1-attempt12")
turkprime.data.5<-cbind(read.csv(paste0(data.directory,"raw_files/exp20180330/ITC Construal test 1-hypothetical-catchup [Categorization game task for experiment(_ 15 minutes)] (82542).csv"), stringsAsFactors = FALSE),TurkPrimeRun="ITC Construal test 1-hypothetical-catchup")

turkprime.data<-rbind(turkprime.data.1,turkprime.data.2,turkprime.data.3,turkprime.data.4,turkprime.data.5)
#View(turkprime.data)
metaloop_dir<-"~/Google Drive/neural-construal-level/code/SmithAdaptConstrualITCmTurk/html-gen/"
raw.psychopy.filedir<-paste0(data.directory,"raw_files/",localdata_subdir,task_version,"/")
condition_info<-read.csv("~/Google Drive/neural-construal-level/code/SmithAdaptConstrualITCmTurk/condition_info.csv",stringsAsFactors = FALSE)


########################################################################
############  Payment stuff
#we might also want to get the psychopy rewards table. this is a slightly distinct process.

source("get_psychopy_rewards_5.R")
subjlist<-get_psychopy_rewards_5(data.directory,metaloop_dir,task_version,raw.psychopy.filedir,condition_info)


subjlist_wips<-data.table(merge(subjlist,
                     qualtrics.data[,.(ConditionQuota,ConditionCode,IPAddress,workerId,ResponseId,ResponseIDToPass)],
                     by.x="QualtricsResponseID",by.y="ResponseId",all=TRUE))
#RECORD OF SUBJECTS PAID OUT ADDITIONAL AMOUNTS DUE TO THE HYPOTHETICAL-REAL MIXUP
subjlist_wips_tp<-data.table(merge(subjlist_wips,turkprime.data,by.x="workerId",by.y="AmazonIdentifier",all=TRUE))

#only subjects who got a turkprime ID and a qualtrics response ID.
subjlist_wips_tp_tpqonly<-subjlist_wips_tp[!is.na(workerId) & !(trimws(workerId)=="") & !is.na(QualtricsResponseID)]

source("generate_bonus_table.R")
generate_bonus_table(
  subjlist_wips_tp_tpqonly,
  as.POSIXct(bonus_date),
  paste0(data.directory,"raw_files/exp20180330/")
  )

#if subjects didn't even get a qualtrics response ID, we can't give them a bonus.

#I do want to know how many subjects 
#(a) signed up for turkprime but didn't get a qualtrics response ID;
only_tp_subjects<-subjlist_wips_tp[!is.na(workerId) & !(trimws(workerId)=="") & is.na(QualtricsResponseID)]
print(paste0("N of subjects who signed up on TurkPrime but didn't generate a qualtrics responseID:",dim(only_tp_subjects)[1]))
#should NOT pay these subjects the bonus or the base payment.
#(b) got a qualtrics response ID but didn't create a corresponding record in the psychopy data.
#should NOT pay these subjects the bonus or the base payment.
only_tp_and_q_subjects<-subjlist_wips_tp[!is.na(workerId) & !(trimws(workerId)=="") & !is.na(QualtricsResponseID) & is.na(FileName)]
print(paste0("N of subjects who signed up on TurkPrime AND Qualtrics but didn't generate a filename record that recorded their responses:",dim(only_tp_and_q_subjects)[1]))


#what about orphaned psychopy records
orphaned_psychopy_records<-subjlist_wips_tp[is.na(ResponseIDToPass)  & !is.na(FileName)]
print(paste0("N of Psychopy records for whom we couldn't identify a qualtrics response:",dim(orphaned_psychopy_records)[1]))

#records on psychopy and qualtrics but not mTurk
orphaned_psychopyqualtrics_records<-subjlist_wips_tp[!is.na(QualtricsResponseID) & !is.na(FileName) &(is.na(AssignmentId) | (trimws(AssignmentId)==""))]
print(paste0("N of Psychopy/qualtrics records for whom we couldn't identify a TurkPrime response:",dim(orphaned_psychopyqualtrics_records)[1]))

########################################################################
############  Data Processing
data_qualtrics_psychopy_all<-merge(at,qualtrics.data,by.x="qualtricsresponseid",by.y="ResponseId",all.x=TRUE)

data_qualtrics_psychopy<-data_qualtrics_psychopy_all[!is.na(data_qualtrics_psychopy_all$workerId) & !is.na(data_qualtrics_psychopy_all$qualtricsresponseid)]
data_qualtrics_psychopy_excluded<-data_qualtrics_psychopy_all[is.na(data_qualtrics_psychopy_all$workerId) | is.na(data_qualtrics_psychopy_all$qualtricsresponseid)]
table(data_qualtrics_psychopy_excluded$qualtricsresponseid)
# table(data_qualtrics_psychopy$qualtricsresponseid)
# ggplot(data_qualtrics_psychopy,
#        aes(x=Trial,y=log(endingK),group=interaction(qualtricsresponseid,runid),color=salienceCondition))+geom_line()+
#   coord_cartesian(ylim=c(-10,10))