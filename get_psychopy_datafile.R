#function to get the CSV
get_psychopy_datafile<-function(raw.filedir,rfn,task_version){
  metaloop_dir<-"~/Google Drive/neural-construal-level/code/SmithAdaptConstrualITCmTurk/html-gen/"
  cols_to_drop<-c()
  cols_rename_from=c()
  cols_rename_to=c()
  raw_csv<-read.csv(paste0(raw.filedir,rfn),stringsAsFactors = FALSE)
  
  ipaddress_startpos<-gregexpr("_",rfn,ignore.case=TRUE)[[1]] %>% .[length(.)]+1
  ipaddress_endpos<-gregexpr("\\.csv",rfn,ignore.case=TRUE)[[1]][1]-1
  
  ip_address<-substr(rfn,ipaddress_startpos,ipaddress_endpos)
  raw_csv$DataSource<-rfn
  raw_csv$IpAddressPsychoPy<-ip_address
  
  #we have to do a lot of cleaning to clean up this messy psychopy format.
  #loop through columns
  for (coli in colnames(raw_csv)){#c<-"Trial1.1" #coli<-"qualtricssid.1"
    #first of all, where there are more htan one column with the same name, find out which has data in it
    if (endsWith(coli,".1")){
      #print(coli)
      #get the remaining columns, which will be ascending from 1.
      col_basename<-substring(coli,1,nchar(coli)-2)
      col_duplicate_i=0
      while(TRUE){
        col_duplicate_i_test=col_duplicate_i+1
        col_duplicate_i_name=paste0(col_basename,".",col_duplicate_i_test)
        if(col_duplicate_i_name %in% colnames(raw_csv)){
          #print(col_duplicate_i_test)
          col_duplicate_i=col_duplicate_i_test
        }else{
          break
        }
      }
      #now we know what the max is.
      cols_in_set=paste0(col_basename,c("",paste0(".",1:col_duplicate_i)))
      #print(raw_csv[cols_in_set])
      #if more than one of those has data
      empty.cols<-cols_in_set[apply(raw_csv[cols_in_set],2,function(x){all(is.na(x) | x=="")})]
      
      non.empty.cols<-cols_in_set[!(cols_in_set %in% empty.cols)]
      #(if all are empty, remove all and go to next)
      if(length(non.empty.cols)==0){
        #not ideal to remove all and go to next but I think it'll be OK.
        cols_to_drop<-c(cols_to_drop,empty.cols)
        next
      }
      #AND if that data is not identical
      for (cname in non.empty.cols[2:length(non.empty.cols)]){#cname=non.empty.cols[2]
        #skip the first col
        na.rows<-is.na(raw_csv[non.empty.cols[1]]) | is.na(raw_csv[cname])
        cols.are.identical<-all(raw_csv[!na.rows,non.empty.cols[1]]==raw_csv[!na.rows,cname])
        if(!cols.are.identical){
          #, throw an error.
          stop(paste0("For basecol ",col_basename,", there were more than one non-empty column with conflicting data."))
        }
        cols_to_drop<-c(cols_to_drop,cname)
      }
      #and then rename the remaining column to a name without the supplementary index
      cols_rename_from<-c(cols_rename_from,coli)
      cols_rename_to<-c(cols_rename_to,col_basename)
    }
  }
  
  
  pruned.csv<-raw_csv[!(colnames(raw_csv)%in% cols_to_drop)]
  
  for (cn in 1:length(cols_rename_from)){#cn=1
    names(pruned.csv)[names(pruned.csv)==cols_rename_from[cn]]<-cols_rename_to[cn]
  }
  
  #now merge columns that have the exact same names; we won't have picked them up, yet.
  #iterate through all the columns that are doubled-up.
  #for each pair, test their contents and remove the empty one. If only one is empty, generate an error.
  # doubled_up_columns<-names(which(table(colnames(pruned.csv))>1))
  # for (dc in doubled_up_columns){#dc<-doubled_up_columns[[1]]
  #   which(names(pruned.csv)==dc)
  # }
  
  #add the conditioncode  
  
  pruned.csv$ConditionCode<-substring(rfn,1,8)
  #so first priority is to find out when this person should get paid!
  pruned.csv$RewardTrialRanked1
  pruned.csv$RewardTrialRanked2
  #we have to load the metaloop file for the associated condition.
  #this is denoted by the task_version, conditioncode, and subject ID.
  
  metaloop_file<-paste0(metaloop_dir,task_version,"/",pruned.csv$ConditionCode[1],"/resources/design_csv_files/metaloop_subclass",pruned.csv$Subject[1],".csv")
  
  metaloop_csv<-read.csv(metaloop_file,stringsAsFactors = FALSE)
  #if metaloop includes a practice round, then mark the practiceround
  pruned.csv$PracticeRun=FALSE
  if(metaloop_csv$LoopFile[[1]]=="practice.csv"){
    #find the last practice row
    rid=1
    firstRunId<-pruned.csv$runid[rid]
    pruned.csv$PracticeRun[rid]=TRUE
    #print(firstRunId)
    #print(pruned.csv$runid)
    while(firstRunId==pruned.csv$runid[rid+1] & !is.na(pruned.csv$runid[rid+1])){
      rid=rid+1
      pruned.csv$PracticeRun[rid]=TRUE
    }
    #print(rid)
  }
  runtextpos<-regexpr("\\.csv",as.character(metaloop_csv$LoopFile[metaloop_csv$RewardRun==1]))[1]
  pruned.csv$RewardRun<-
    as.integer(substr(as.character(metaloop_csv$LoopFile[metaloop_csv$RewardRun==1]),runtextpos-1,runtextpos-1))
  
  #print("returning")
  return(pruned.csv)
}
