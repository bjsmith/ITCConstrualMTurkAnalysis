library(RCurl)
data.directory<-"~/GDrive/neural-construal-level/data/ITCConstrual_mTurkExp/"
task_version="html20180315014343"
task_conditioncodes=c("4NY6RAY0",
                      "M730ARVG",
                      "L62QJAAN",
                      "147HE9PW",
                      "QOUE41IK",
                      "GSS2U1EF",
                      "RZYPUKWO",
                      "BB4N3XSL",
                      "SSGV62B4",
                      "YGFIWQQF",
                      "9TSONA2Y",
                      "PUZ190C2")
#credentials
credentials_path=paste0(data.directory,"ftpcredentials.Rdata")
if (file.exists(credentials_path)){
  load(credentials_path)
}else{
  server_username=readline("Enter the FTP server username")
  server_password=readline("Enter the FTP server password")
  userpwd=paste0(server_username,":",server_password)
  save(userpwd,file=p)
  
}
#see https://stackoverflow.com/questions/39705379/r-downloading-multiple-file-from-ftp-using-rcurl for a possible remedy to get rid of the delay.
delay=10
url="ftp://files.000webhost.com/"
#iterate through conditions
print("Due to bandwidth limitations on 000webhost, this will take a while...")
for (tcc in task_conditioncodes){#tcc<-task_conditioncodes[[1]] #tcc<-"9TSONA2Y"
  #dir to download
  dir_to_sweep=paste0(url,"public_html/",task_version,"/",tcc,"/data/")
  folder_contents_list_all<-strsplit(getURL(dir_to_sweep, userpwd = userpwd,
         #ftp.use.epsv = FALSE,
         dirlistonly = TRUE),"\n")[[1]]
  Sys.sleep(delay)
  
  folder_contents_list_csvs<-folder_contents_list_all[grep(".csv",folder_contents_list_all)]
  
  for (datafile in folder_contents_list_csvs){#datafile<-folder_contents_list_csvs[[1]]
    full_datafile_path=paste0(dir_to_sweep,datafile)
    print(paste0("downloading ",datafile))
    csv.raw.contents<-getURL(full_datafile_path, userpwd = userpwd)
    
    write.dir<-paste0(data.directory,"raw_files/",task_version,"/")
    Sys.sleep(delay)
    if(!dir.exists(write.dir))dir.create(write.dir)
    write(csv.raw.contents,file=paste0(write.dir,
                                       tcc,
                                       datafile))
  }
}
