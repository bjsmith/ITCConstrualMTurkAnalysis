library(data.table)
library(dplyr)
source("load_and_preprocess_compiled_data20180330.R")

#let's look at decisions of each participant
pdf(paste(construalfolder,"individual plots.pdf"))
par(mfrow=c(2,1))
data_summary<-NULL
for (pt in sort(unique(data.main_w_c$SubjectUniqueID))){
  #pt<-data.main_w_c$SubjectUniqueID[[1]]
  #setup
	ptdata<-subset(data.main_w_c,SubjectUniqueID==pt)
	minK<-min(ptdata$logk,na.rm = TRUE)
	maxK<-max(ptdata$logk,na.rm = TRUE)
	
	#run models
	#add (logistic) regression fits, can do it separately by conditions
	ptmodel<-glm(Choice01~logk,ptdata,family=binomial(link="probit"))	
	coef(ptmodel)
	#ok let's run separate models for salience condition
	ptmodelamt<-glm(Choice01~logk,subset(ptdata,salienceCondition==0),family=binomial(link="probit"))
	ptmodeldel<-glm(Choice01~logk,subset(ptdata,salienceCondition==1),family=binomial(link="probit"))
	
	#indifference k estimate
	indiff.k<-exp(-(coef(ptmodel)[1])/coef(ptmodel)[2])
	coef(ptmodel)[1]+log(indiff.k)*coef(ptmodel)[2]
	summary(ptmodel)
	#plot
	with(ptdata,plot(logk,Choice01,xlim=c(minK,maxK),col=Construal_Condition,main=pt))
	curve(pnorm(coef(ptmodel)[1]+coef(ptmodel)[2]*x),add=TRUE)
	text(minK+1,0.1,formatC(coef(ptmodel)[2],digits=2))
	ptlogindiffk<-(coef(ptmodel)[1]/coef(ptmodel)[2])*-1 #use this formula to get indifference-k, can be subsetted for any condition
	curve(pnorm(coef(ptmodelamt)[1]+coef(ptmodelamt)[2]*x),col="#009900",add=TRUE)
	text(minK+1,0.2,formatC(coef(ptmodelamt)[2],digits=2),col = "#009900")
	curve(pnorm(coef(ptmodeldel)[1]+coef(ptmodel)[2]*x),col="red",add=TRUE)
	text(minK+1,0.3,formatC(coef(ptmodel)[2],digits=2),col = "red")
	
	#record results
	data_summary_pt<-data.frame(
	  "SubjectUniqueID"=pt,
	  "ptmodel_Intercept"=coef(ptmodel)[1],
    "ptmodel_LogK"=coef(ptmodel)[2],
    "ptmodelamt_Intercept"=coef(ptmodelamt)[1],
    "ptmodelamt_LogK"=coef(ptmodelamt)[2],
    "ptmodeldel_Intercept"=coef(ptmodeldel)[1],
    "ptmodeldel_LogK"=coef(ptmodeldel)[2]
	                            )
	if(is.null(data_summary))data_summary<-data_summary_pt
	else data_summary<-rbind(data_summary,data_summary_pt)

}
dev.off()
data_summary_dt<-data.table(data_summary)
#include participants where logks are above zero.

#best set of participants so far?
mTurkptindex<-c(1:9,11,15:16,18:19,22,30,34,38,39:41,43:44,47,50,55:57,59:61,64:65,67,70)
length(mTurkptindex)
#this hopefully gets a list of ids for good pts
#mTurkpts<-unique(construalITC_mTurk$DataSource)[mTurkptindex]
#arbitrary cut-off which aims to have a similar threshold to what Eustace set.
mTurkpts<-data_summary_dt[ptmodel_LogK>0.05& ptmodelamt_LogK>0.05& ptmodeldel_LogK>0.05,SubjectUniqueID]
library(lme4)

#hierarchical model with random intercept and fixed slope by participant
model1.1<-glmer(Choice01~(1|SubjectUniqueID)+logk,data=subset(construalITC_mTurk,SubjectUniqueID%in%mTurkpts),family=binomial(link="probit"))
summary(model1.1)
#hierarchical model with random intercept and slope by participant
model1.2<-glmer(Choice01~(1+logk|SubjectUniqueID),data=subset(construalITC_mTurk,SubjectUniqueID%in%mTurkpts),family=binomial(link="probit"))
summary(model1.2)
model2<-glmer(Choice01~(1+logk|SubjectUniqueID)+salienceCondition,data=subset(construalITC_mTurk,SubjectUniqueID%in%mTurkpts),family=binomial(link="probit"))
table(construalITC_mTurk$Choice01,construalITC_mTurk$salienceCondition)
summary(model2)
model2ransalience<-glmer(Choice01~(1+logk+salienceCondition|SubjectUniqueID),data=subset(construalITC_mTurk,SubjectUniqueID%in%mTurkpts),family=binomial(link="probit"))
summary(model2ransalience)


#what about construal? how many construal items are correct vs. incorrect?
#will need to cross-reference with the construal data.
construalITC_mTurk_dt<-data.table(construalITC_mTurk)

construalITC_mTurk_dt[,mean(endingK),by=salienceCondition]
construalITC_mTurk_dt$
hist(construalITC_mTurk_dt$LLdelay,breaks=1000000,xlim = c(0,100))
sort(construalITC_mTurk_dt[LLdelay>100,LLdelay])
median(construalITC_mTurk_dt$LLdelay,na.rm=TRUE)
mean(construalITC_mTurk_dt$LLdelay,na.rm=TRUE)
table(construalITC_mTurk_dt$salienceCondition)
table(is.na(construalITC_mTurk_dt$salienceCondition))
table(is.na(construalITC_mTurk_dt$Condition))

construalITC_mTurk_dt[!is.na(salienceCondition) &!is.na(Condition) ]%>% .[,.(KValue_med=mean(endingK),
                         SSamount_med=median(SSamount),LLamount_med=median(LLamount),
                         SSdelay_med=median(SSdelay),LLdelay_med=median(LLdelay)
                         ),by=.(salienceCondition,Condition)]

dim(construalITC_mTurk_dt)

table(construalITC_mTurk_dt$salienceCondition)
construalITC_mTurk_dt[SubjectUniqueID==]