stop("Deprecated in favor of get_data_exp20180315.R which generates subjlist_wips with the reward payment information.")
library(plyr)
data.directory<-"~/Google Drive/neural-construal-level/data/ITCConstrual_mTurkExp/"
metaloop_dir<-"~/Google Drive/neural-construal-level/code/SmithAdaptConstrualITCmTurk/html-gen/"
task_version="html20180315014343"
raw.filedir<-paste0(data.directory,"raw_files/",task_version,"/")
condition_info<-read.csv("~/Google Drive/neural-construal-level/code/SmithAdaptConstrualITCmTurk/condition_info.csv")
#for 

raw.filenames<-list.files(raw.filedir)

source("get_psychopy_datafile.R")
#for this loop, we're going to loop through the CSVs and actually create a list of subjects and when to pay them.
subjlist<-NULL
for (rfn in raw.filenames){#rfn<-raw.filenames[1]#rfn<-"YGFIWQQF__Thu_Mar_15_2018_112759_GMT-0800_AKDT_107.77.205.115.csv"
  print(rfn)
  pruned_csv<-get_psychopy_datafile(raw.filedir,rfn,task_version)
  #get the subject ID
  subid<-pruned_csv$Subject[!is.na(pruned_csv$Subject)][3]#first two rows are practice, don't take them seriously.
  conditionCode<-pruned_csv$ConditionCode[1]
  
  #View(condition_info)
  #print(pruned_csv[,c("Choice1","Choice2" , "Choice")])
  
  #
  doubleTrial=length(trimws(pruned_csv$RewardTrialRanked2)!="")>length(trimws(pruned_csv$RewardTrialRanked)!="")
  #the condition
  #how to get the reward condition?
  if(condition_info$Condition[condition_info$ConditionCode==conditionCode]=="Hypothetical"){
    rewardAmount=0.30
    rewardDelay=0
    if(doubleTrial){
      finalK<-pruned_csv$endingK2
    }else{
      finalK<-pruned_csv$endingK
    }
  }else{#real
    
    if(doubleTrial){
      choosecol<-pruned_csv$RewardTrialRanked2
      responseCol<-pruned_csv$Choice2
      SSdelayCol=pruned_csv$SSdelay2
      LLdelayCol=pruned_csv$LLdelay2
      SSamountCol=pruned_csv$SSamount2
      LLamountCol=pruned_csv$LLamount2
      finalK<-pruned_csv$endingK2
    }else{
      choosecol<-pruned_csv$RewardTrialRanked
      responseCol<-pruned_csv$Choice
      
      SSdelayCol=pruned_csv$SSdelay
      LLdelayCol=pruned_csv$LLdelay
      SSamountCol=pruned_csv$SSamount
      LLamountCol=pruned_csv$LLamount
      finalK<-pruned_csv$endingK
    }
    #has to be the rewarded run and there has to have been a response
    includedInDrawer=pruned_csv$runid==pruned_csv$RewardRun & trimws(responseCol)!=""
    rewardedRow=choosecol==min(choosecol[includedInDrawer],na.rm = TRUE) & includedInDrawer
    pruned_csv$RewardRow=rewardedRow
    if (responseCol[pruned_csv$RewardRow]=="SS"){
      rewardDelay=SSdelayCol[rewardedRow]
      rewardAmount=SSamountCol[rewardedRow]
    }else{
      rewardDelay=LLdelayCol[rewardedRow]
      rewardAmount=LLamountCol[rewardedRow]
    }
  }
  
  subjListEntry<-data.frame("SubID"=subid,"ConditionCode"=conditionCode,
                            "RewardDelay"=rewardDelay,"RewardDay"=as.POSIXct("2018-03-15")+rewardDelay*24*60*60,
                            "RewardAmount"=rewardAmount,
                            "FinalK"=formatC(finalK[length(finalK)],digits=3))
  if(is.null(subjlist)){
    subjlist<-subjListEntry
  }else{
    subjlist<-rbind(subjlist,subjListEntry)
  }
}




