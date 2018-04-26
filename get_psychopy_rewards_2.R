library(plyr)

source("get_psychopy_datafile.R")

get_psychopy_rewards_2 <- function(data.directory,metaloop_dir,task_version,raw.filedir,condition_info){#raw.filedir<-raw.psychopy.filedir
  
  #for this loop, we're going to loop through the CSVs and actually create a list of subjects and when to pay them.
  subjlist<-NULL
  raw.filenames<-list.files(raw.filedir)
  
  for (rfn in raw.filenames){#rfn<-raw.filenames[1]#rfn<-"YGFIWQQF__Thu_Mar_15_2018_112759_GMT-0800_AKDT_107.77.205.115.csv"
    print(rfn)
    pruned_csv<-get_psychopy_datafile(raw.filedir = raw.filedir,rfn=rfn,task_version)
    #get the subject ID
    subid<-pruned_csv$qualtricssid[!is.na(pruned_csv$qualtricssid)][3]#first two rows are practice, don't take them seriously.
    if(is.na(subid)){
      #try qualtricssid1
      subid<-pruned_csv$qualtricssid1[!is.na(pruned_csv$qualtricssid1)][1]#this might exist if the first try didn't work well.
    }
    conditionCode<-pruned_csv$ConditionCode[1]
    IpAddress<-pruned_csv$IpAddressPsychoPy[1]
    #View(condition_info)
    #print(pruned_csv[,c("Choice1","Choice2" , "Choice")])
    
    #
    doubleTrial=length(trimws(pruned_csv$RewardTrialRanked2)!="")>length(trimws(pruned_csv$RewardTrialRanked)!="")
    #the condition
    #how to get the reward condition?
    print(task_version)
    print(condition_info$Condition[condition_info$ConditionCode==conditionCode]=="Hypothetical" & 
            task_version!="html20180321233637")
    if(condition_info$Condition[condition_info$ConditionCode==conditionCode]=="Hypothetical" & 
       task_version!="html20180321233637" # I screwed up with this version and promised a whole lot of "hypothetical" people real money!
       ){
      rewardAmount=0.30
      rewardDelay=0
      if(doubleTrial){
        finalK<-pruned_csv$endingK2
      }else{
        finalK<-pruned_csv$endingK
      }
      subjChoice=NA
    }else{#real
      
      if(doubleTrial){
        choosecol<-c(pruned_csv$RewardTrialRanked1,pruned_csv$RewardTrialRanked2)
        responseCol<-trimws(c(pruned_csv$Choice1,pruned_csv$Choice2))
        SSdelayCol=c(pruned_csv$SSdelay1,pruned_csv$SSdelay2)
        LLdelayCol=c(pruned_csv$LLdelay1,pruned_csv$LLdelay2)
        SSamountCol=c(pruned_csv$SSamount1,pruned_csv$SSamount2)
        LLamountCol=c(pruned_csv$LLamount1,pruned_csv$LLamount2)
        finalK<-c(pruned_csv$endingK1,pruned_csv$endingK2)
        includedInDrawer=rep(pruned_csv$runid==pruned_csv$RewardRun,2) & trimws(responseCol)!="" & rep(pruned_csv$PracticeRun==FALSE,2)
      }else{
        choosecol<-pruned_csv$RewardTrialRanked
        responseCol<-trimws(pruned_csv$Choice)
        SSdelayCol=pruned_csv$SSdelay
        LLdelayCol=pruned_csv$LLdelay
        SSamountCol=pruned_csv$SSamount
        LLamountCol=pruned_csv$LLamount
        finalK<-pruned_csv$endingK
        includedInDrawer=pruned_csv$runid==pruned_csv$RewardRun & trimws(responseCol)!="" & pruned_csv$PracticeRun==FALSE
      }
      #has to be the rewarded run and there has to have been a response
      #length(choosecol)
      #length(min(choosecol[includedInDrawer],na.rm = TRUE))
      #length(includedInDrawer)
      rewardedRow=choosecol==min(choosecol[includedInDrawer],na.rm = TRUE) & includedInDrawer
      if(doubleTrial){
        pruned_csv$RewardRow1=rewardedRow[1:(length(rewardedRow)/2)]
        pruned_csv$RewardRow2=rewardedRow[(length(rewardedRow)/2+1):length(rewardedRow)]
      }else{
        pruned_csv$RewardRow<-rewardedRow
      }
      
      if (responseCol[rewardedRow]=="SS"){
        rewardDelay=SSdelayCol[rewardedRow]
        rewardAmount=SSamountCol[rewardedRow]
        subjChoice=responseCol[rewardedRow]
      }else{
        rewardDelay=LLdelayCol[rewardedRow]
        rewardAmount=LLamountCol[rewardedRow]
        subjChoice=responseCol[rewardedRow]
      }
    }
    
    subjListEntry<-data.frame("SubID"=subid,"ConditionCode"=conditionCode,
                              "RewardDelay"=rewardDelay,
                              "RewardDate"=as.POSIXlt(substr(pruned_csv$date[1],6,16),format="%b %d %Y")+rewardDelay*24*60*60,
                                "RewardAmount"=rewardAmount,
                              "Choice"=subjChoice,
                              "FinalK"=formatC(finalK[length(finalK)],digits=3),
                              "FileName"=rfn,
                              "IpAddress"=IpAddress)
    if(is.null(subjlist)){
      subjlist<-subjListEntry
    }else{
      subjlist<-rbind(subjlist,subjListEntry)
    }
  }
  
  return(subjlist)
}
  
