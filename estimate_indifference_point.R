estimate_indifference_point <- function(Choice01,assumedK){
  #so this takes info from a trial where we tried to estimate k values and then
  #used these k values to estimate an indifference point.
  #We can then build a probit model to predict the choices people make from the log k
  #this relationship is ultimately arbitrary 
  #and is not related to the theoretical exponential relationship between k and choices
  #but now that we set that up, we can reverse the direction and calculate the k value that would
  #predict Choice==0 and Choice==1 equally.
  # hashtag bothsideswin
  # if((sort(unique(Choice01))!=c(0,1))){
  #   print(paste(sort(unique(Choice01)),c(0,1)))
  # }
  #stopifnot(sort(unique(Choice01))==c(0,1))
  ptmodel<-glm(Choice~logk,data.frame("Choice"=Choice01,"logk"=log(assumedK)),family=binomial(link="probit"))	
  
  #ok let's run separate models for salience condition

  #indifference k estimate
  indiff.k<-exp(-(coef(ptmodel)[1])/coef(ptmodel)[2])
  names(indiff.k)<-names(0)
  #coef(ptmodel)[1]+log(indiff.k)*coef(ptmodel)[2]
  cat(".")
  return(indiff.k)
}