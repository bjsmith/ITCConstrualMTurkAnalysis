discountF<-function (v,k,d){
  return(v/(1+k*d))
  
}

d<-seq(1,100,1)
kvals<-c(rep(0.1,100),rep(0.01,100),rep(0.001,100))
discountTable<-data.frame("delay"=rep(d,3),"KVal"=kvals,DiscountedRate=discountF(10,kvals,rep(d,3)))
  

ggplot(discountTable,aes(x=delay,y=DiscountedRate,group=KVal,color=as.factor(KVal)))+geom_line(linetype = 2)+
  labs(x="Delay (days)",y="Discounted Value",title="Discounted value of $10 after a delay\nat different discount rates",color="Discount rate k")

discountF2<-function (v_f,k,d){
  return(v_f*(1+k*d))
  
}

discountTable2<-data.frame("delay"=rep(d,3),"KVal"=kvals,DiscountedRate=discountF2(10,kvals,rep(d,3)))

ggplot(discountTable2,aes(x=delay,y=DiscountedRate,group=KVal,color=as.factor(KVal)))+geom_line(linetype = 2)+
  labs(x="Delay (days)",y="Discounted Value",title="Discounted value of $10 after a delay\nat different discount rates",color="Discount rate k")
