generate_bonus_table<-function(data,date,filedir,startdate="2018-01-01"){
  #date<-as.POSIXct("2018-04-02")
  #data<-subjlist_wips_tp
  #filedir<-"/Users/benjaminsmith/Google Drive/neural-construal-level/data/ITCConstrual_mTurkExp/raw_files/exp20180527/"
  
  for (tpr in as.character(unique(data$TurkPrimeRun))){#tpr<-"ITC Construal test 1-attempt6"
    payment_list<-data[TurkPrimeRun==tpr & RewardDate<date & RewardDate>as.POSIXct(startdate),
                     .("workerId"=workerId, "TotalBonus"=RewardAmount)]
    write.csv(payment_list,paste0(
      filedir,"mTurkbonus",
      tpr,as.character(date),".csv"),row.names = FALSE
      )
  }
}