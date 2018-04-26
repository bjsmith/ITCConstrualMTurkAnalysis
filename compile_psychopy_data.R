library(plyr)
library(data.table)
library(tidyr)
source("get_psychopy_datafile.R")
#files_subdir<-"exp20180521/"
#files_subdir=localdata_subdir;verbose=TRUE
compile_psychopy_data<-function(task_version,files_subdir="",
                                condition_info_file="~/Google Drive/neural-construal-level/code/SmithAdaptConstrualITCmTurk/condition_info.csv",
                                verbose=FALSE){
  data.directory<-"~/Google Drive/neural-construal-level/data/ITCConstrual_mTurkExp/"
  
  raw.filedir<-paste0(data.directory,"raw_files/",files_subdir,task_version,"/")
  condition_info<-read.csv(condition_info_file,stringsAsFactors = FALSE)
  
  names(condition_info)[names(condition_info)=="Condition"]<-"RewardReality"
  
  raw.filenames<-list.files(raw.filedir)
  
  if(verbose) print(paste0("N of raw psychopy files: ",length(raw.filenames)))
  
  all_miniblocks<-NULL
  all_trials<-NULL
  for (rfn in raw.filenames){#rfn<-raw.filenames[2]#rfn<-"YGFIWQQF__Thu_Mar_15_2018_112759_GMT-0800_AKDT_107.77.205.115.csv"
    #rfn<-"QOUE41IK__Fri_Apr_06_2018_214740_GMT-0700_Pacific_Daylight_Time_112.133.237.31.csv"
    pruned_csv_df<-get_psychopy_datafile(raw.filedir = raw.filedir,rfn=rfn,task_version)
    
    if(dim(pruned_csv_df)[1]==0){
      print(paste0("No rows for ",rfn))
      next
    }
    pruned_csv<-data.table(pruned_csv_df)
    #now, I think we need to take tasks that include two choices on a row and split them into separate rows.
    if ("SSamount1" %in% colnames(pruned_csv) | "LLamount1" %in% colnames(pruned_csv)){
      
      #print(paste0("merging for condition ",pruned_csv$ConditionCode[1]))
      doubleTrial=TRUE
      #we need to split.
      
      row_trial1_colnames<-colnames(pruned_csv)[endsWith(colnames(pruned_csv),"1")]
      row_trial2_colnames<-colnames(pruned_csv)[endsWith(colnames(pruned_csv),"2")]
      basecolnames1<-unlist(sapply(row_trial1_colnames,function(x){substring(x,1,nchar(x)-1)}))
      basecolnames2<-unlist(sapply(row_trial2_colnames,function(x){substring(x,1,nchar(x)-1)}))
      #remove the values that we don't want
      other_colnames<-colnames(pruned_csv)[
        !(colnames(pruned_csv) %in% union(union(row_trial1_colnames,row_trial2_colnames),basecolnames1))]
      #with this many columns, I'm not sure how to use gather, so I'll do it 'manually'...
      row_trial1<-data.frame(pruned_csv[,c(other_colnames,row_trial1_colnames),with=FALSE])
      row_trial2<-data.frame(pruned_csv[,c(other_colnames,row_trial2_colnames),with=FALSE])
      
      names(row_trial1)[names(row_trial1) %in% names(basecolnames1)]<-basecolnames1
      names(row_trial2)[names(row_trial2) %in% names(basecolnames2)]<-basecolnames2
      #row_trial1<-data.frame(rename(row_trial1,basecolnames1))
      #row_trial2<-data.frame(rename(row_trial2,basecolnames2))
      list_by_trial<-rbind(row_trial1,row_trial2) %>% data.table %>% .[order(choiceUp)]
    }else{
      #print(paste0("not merging for condition ",pruned_csv$ConditionCode[1]))
      list_by_trial<-pruned_csv %>% data.table %>% .[order(choiceUp)]
    }
    
    #for (suid in unique(data.main_w_c[(Group %in% c("BlockDesign_concrete_first","BlockDesign_abstract_first") & TaskArrangement=="TasksBlocked"),SubjectUniqueID])){
    if(dim(list_by_trial)[1]==162){
      if(verbose)print("blocked run detected; augmenting runID")

      expected_na_runid_pattern<-c(rep(FALSE,2),rep(c(rep(TRUE,20),rep(FALSE,20)),4))
      pattern_meets_expected_pattern<-all(is.na(list_by_trial$runid)==expected_na_runid_pattern)
      if(!pattern_meets_expected_pattern) stop("Unexpected formation of construal and itc trials.")
      #if it does meet the expected pattern,
      list_by_trial$Construal_Condition_Block<-NA
      for (run in 0:3){
        list_by_trial$runid[2+1:20+run*40]<-list_by_trial$runid[20+2+1:20+run*40]
        #for block trials only, we mark out that there's a construal condition.
        
        list_by_trial$Construal_Condition_Block[2+1:40+run*40]<-mean(list_by_trial$Construal_Condition[2+1:20+run*40])
      }
    }
    
    if(is.null(all_trials)){
      all_miniblocks<-pruned_csv
      all_trials<-list_by_trial
    }else{
      all_miniblocks<-rbind(all_miniblocks,pruned_csv,fill=TRUE)
      all_trials<-rbind(all_trials,list_by_trial,fill=TRUE)
      if(verbose)print(paste0(dim(all_trials)[1]," trials for ",length(unique(all_trials$DataSource))," subjects."))
    }
    # print("---")
    # print(dim(list_by_trial))
    # print(dim(all_trials))
    
  }
  
  all_trials<-data.table(merge(data.frame(all_trials),condition_info,by="ConditionCode",all.x=TRUE))
  
  all_trials$KChange<-all_trials$endingK-all_trials$startingK
  all_trials$KChange<-all_trials$endingK-all_trials$startingK
  all_trials$salienceCondition<-as.factor(all_trials$salienceCondition)
  
  all_trials$SubjectCondition<-paste0(all_trials$ConditionCode,all_trials$qualtricssid)
  
  all_trials$Choice<-trimws(all_trials$Choice)
  summary_measures<-all_trials[,.(SCMean=mean(endingK,na.rm = TRUE),SCSD=sd(endingK,na.rm = TRUE),SCN=.N),by=DataSource]
  
  #clean up by removing subjects with very few trials and subjects with absurdly high means or SDs.
  all_trials<-merge(all_trials,summary_measures,by="DataSource",all.x=TRUE)
  all_begun_trials<-all_trials %>% .[SCN>10] # %>% .[SCMean<10 & SCSD<10 & SCN>10]
  short_trials<-all_trials%>% .[SCN<=10] # %>% .[SCMean<10 & SCSD<10 & SCN>10]
  if(dim(short_trials)[1]>0) warning("Excluded ", dim(short_trials)[1], " subjects because we recorded less than 10 trials for them.")
  all_begun_trials[,.(Count=.N),by=.(Choice,salienceCondition,SubjectCondition)] %>% spread(Choice,Count) %>% 
    .[,.(SSProportion=SS/(LL+SS)),by=.(salienceCondition,SubjectCondition)] %>% spread(salienceCondition,SSProportion)
  
  #get a trialID that starts from teh beginning of each run and from the beginning of the session.
  all_begun_trials[, TrialId:=1:.N,by=DataSource]
  all_begun_trials[, RunTrialId:=1:.N,by=.(DataSource,runid)]
  
  #Do we have a difference in trials chosen?
  #yes we do.
  #all_trials_with_choices<-all_trials[trimws(Choice)!="" & !is.na(salienceCondition)]
  #the chi-sq isn't really valid because this isn't across-subject data.

  # var SALIENCE_CONDITION_AMOUNT_SALIENT=0; IE DELAY FIXED
  # var SALIENCE_CONDITION_DELAY_SALIENT=1; AMOUNT FIXED
  #some tidying
  all_begun_trials$qualtricsresponseid<-trimws(all_begun_trials$qualtricsresponseid)
  
  
  
  
  return(all_begun_trials)
}
