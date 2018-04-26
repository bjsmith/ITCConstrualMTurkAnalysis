source("compile_psychopy_data.R")
source("get_qualtrics_data.R")
condition_info<-read.csv("~/Google Drive/neural-construal-level/code/SmithAdaptConstrualITCmTurk/condition_info.csv",stringsAsFactors = FALSE)
warning("this script assumes data was already downloaded from the webserver.")

########################################################################
#20180330
data.directory<-"~/Google Drive/neural-construal-level/data/ITCConstrual_mTurkExp/"
task_version="html20180329165253"
localdata_subdir="exp20180330/"
at<-compile_psychopy_data(task_version,localdata_subdir)
length(unique(at$DataSource))
qualtrics.data<-get_qualtrics_data(paste0(data.directory,"raw_files/exp20180330/ITCConstrual-attempt6_April+11,+2018_15.38.csv"))# read.csv()
data_qualtrics_psychopy_all<-merge(at,qualtrics.data,by.x=c("qualtricsresponseid","ConditionCode"),by.y=c("ResponseId","ConditionCode"),all.x=TRUE)
#this is quite important, and cuts out all records for which we don't have a qualtrics response ID and a worker ID
#that would remove all entries from subjects whose IDs were misplaced.
#I'm not sure if that's really a choice we want to make???
data_qualtrics_psychopy<-data_qualtrics_psychopy_all[!is.na(data_qualtrics_psychopy_all$workerId) & !is.na(data_qualtrics_psychopy_all$qualtricsresponseid)]
data_qualtrics_psychopy.noworkerID<-data_qualtrics_psychopy_all[is.na(data_qualtrics_psychopy_all$workerId)]
data_qualtrics_psychopy.noQualtricsResponseIdInPsychoPy<-data_qualtrics_psychopy_all[trimws(data_qualtrics_psychopy_all$qualtricsresponseid)==""]

data_qualtrics_psychopy$task_version<-task_version
data_full_20180330<-data_qualtrics_psychopy
length(unique(data_full_20180330$DataSource))
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
length(unique(at$DataSource))
data_qualtrics_psychopy_all<-merge(at,qualtrics.data,by.x=c("ConditionCode","qualtricssid","IpAddressPsychoPy"),by.y=c("ConditionCode","ConditionQuota","IPAddress"),
                                   all.x = TRUE)

data_qualtrics_psychopy_all$task_version<-task_version
data_full_20180329<-data_qualtrics_psychopy_all
# data_full_20180329<-merge(data_qualtrics_psychopy_all,condition_info,
#                           by="ConditionCode")
length(unique(data_full_20180329$DataSource))
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

#In this dataset, our hypothetical labels were messed up for at least some subjects. We can't tell who, but it's likely to be all of the hypothetical subjects.
#this will make it difficult to include these subjects, at least when looking at the hypotheticality
data_qualtrics_psychopy_all[RewardReality=="Hypothetical",RewardReality:="IncorrectlyToldReal"]



data_full_20180327<-data_qualtrics_psychopy_all


########################################################################
#20180315
data.directory<-"~/Google Drive/neural-construal-level/data/ITCConstrual_mTurkExp/"
task_version="html20180315014343"
localdata_subdir=""
at<-compile_psychopy_data(task_version,localdata_subdir)
qualtrics.data.1<-get_qualtrics_data(paste0(data.directory,"raw_files/qualtrics_march15/ITCConstrual_March+20,+2018_11.55_data.csv"))
qualtrics.data.2<-get_qualtrics_data(paste0(data.directory,"raw_files/qualtrics_march15/ITCConstrual-attempt2_April+6,+2018_15.27_data.csv"))# read.csv()

#key reconciliation
setnames(qualtrics.data.1,old=c("Q21",
                                  "Q25",
                                  "Q12",
                                  "Q14",
                                  "Q14_6_TEXT",
                                  "Q16",
                                  "Q16_3_TEXT",
                                  "Q18",
                                  "Q22",
                                  "Q24",
                                  "Q26",
                                  "Q28",
                                  "Q30"),
         new=c("Code",
               "Q21ErrorRepeat",
               "QHis",
               "QRace",
               "QRace_6_TEXT",
               "QGender",
               "QGender_3_TEXT",
               "QAge",
               "QPolSoc",
               "QPolEco",
               "QEdu",
               "QHHI",
               "QHHS"))
qualtrics.data<-rbind(qualtrics.data.1,qualtrics.data.2,fill=TRUE)

#get_qualtrics_data(paste0(data.directory,"raw_files/qualtrics_march15/ITCConstrualattempt2-Responses in Progress_data.csv"))

metaloop_dir<-"~/Google Drive/neural-construal-level/code/SmithAdaptConstrualITCmTurk/html-gen/"
raw.psychopy.filedir<-paste0(data.directory,"raw_files/",localdata_subdir,task_version,"/")
condition_info<-read.csv("~/Google Drive/neural-construal-level/code/SmithAdaptConstrualITCmTurk/condition_info.csv",stringsAsFactors = FALSE)

data_qualtrics_psychopy_all<-merge(at,qualtrics.data,by.x=c("ConditionCode","qualtricssid","IpAddressPsychoPy"),by.y=c("ConditionCode","ConditionQuota","IPAddress"),
                                   all.x = TRUE)
data_qualtrics_psychopy_all$task_version<-task_version
#there's some bad columns from qualtrics 
data_full_20180315<-data_qualtrics_psychopy_all
# data_full_20180315<-merge(data_qualtrics_psychopy_all,condition_info,
#                                       by="ConditionCode")

########################################################################
#Harmonization. Let's take a look at all the columns that differ between these versions.
#use the most recent version and harmonize back from there.
setdiff(colnames(data_full_20180330),colnames(data_full_20180329))
setdiff(colnames(data_full_20180329),colnames(data_full_20180330))


setdiff(colnames(data_full_20180330),colnames(data_full_20180327))
setdiff(colnames(data_full_20180327),colnames(data_full_20180330))

setdiff(colnames(data_full_20180330),colnames(data_full_20180315))
setdiff(colnames(data_full_20180315),colnames(data_full_20180330))


data_all<-rbind(data_full_20180315,data_full_20180327,data_full_20180329,data_full_20180330, fill=TRUE)
data_all_anon<-data_all
data_all_anon$workerId<-NULL

write.csv(data_all_anon,paste0(data.directory,"all_data_anon_2018-03-30.csv"))

