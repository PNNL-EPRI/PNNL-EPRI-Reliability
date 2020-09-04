dt<-5/60
dfRacksCast3<-dfRacksCast2%>%
  group_by(Rack)%>%
  arrange(Time)%>%
  mutate(CumDis=(cumsum(Power*(Power<0))-Power*(Power<0)/2-first(Power*(Power<0))/2)*dt,
         CumAh=(cumsum(Current)-Current/2-first(Current)/2)*dt)%>%
  group_by(Index2)%>%
  filter(max(RackNo)==20,
         n()>=12,
         max(abs(diff(SOC)))<=0.1,
         max(SOC)>0.5,
         min(SOC)<0.5,
         State!=0)%>%
  arrange(Time)%>%
  #filter(State!=0)%>%
  mutate(Weight=(max(SOC)-min(SOC))/n(),
         CumAh50=approx(SOC,CumAh,0.5)$y,
         Hours=(Time-first(Time))/60/60,
         kWhChg=(cumsum(Power*(Power>0))-Power*(Power>0)/2-first(Power*(Power>0))/2)*dt,
         kWhDis=(cumsum(Power*(Power<0))-Power*(Power<0)/2-first(Power*(Power<0))/2)*dt,
         kWhChgT=(cumsum(Temp*Power*(Power>0))-Temp*Power*(Power>0)/2-first(Temp*Power*(Power>0))/2)*dt,
         kWhDisT=(cumsum(Power*Temp*(Power<0))-Power*Temp*(Power<0)/2-first(Power*Temp*(Power<0))/2)*dt,
         AhChg=(cumsum(Current*(Current>0))-Current*(Current>0)/2-first(Current*(Current>0))/2)*dt,
         AhDis=(cumsum(Current*(Current<0))-Current*(Current<0)/2-first(Current*(Current<0))/2)*dt,
         AhChgT=(cumsum(Temp*Current*(Current>0))-Temp*Current*(Current>0)/2-first(Temp*Current*(Current>0))/2)*dt,
         AhDisT=(cumsum(Current*Temp*(Current<0))-Current*Temp*(Current<0)/2-first(Current*Temp*(Current<0))/2)*dt,
         TempdT=(cumsum(Temp)-Temp/2-first(Temp/2))*dt,
         SOCdt=(cumsum(SOC)-SOC/2-first(SOC/2))*dt,
         kWhChgSOC=(cumsum(SOC*Power*(Power>0))-SOC*Power*(Power>0)/2-first(SOC*Power*(Power>0))/2)*dt,
         kWhDisSOC=(cumsum(SOC*Power*(Power<0))-SOC*Power*(Power<0)/2-first(SOC*Power*(Power<0))/2)*dt,
         P2=(cumsum(Power^2)-(Power^2)/2-first(Power^2)/2)*dt,
         delSOC=SOC-first(SOC))


dfRacksCast3%>%
  ungroup%>%
  lm(data=.,
     delSOC~(kWhChg+kWhDis+Hours)*(CumDis)-CumDis+P2+0+TempdT+kWhChgT+kWhDisT,
     weights = Weight)->LinearModel

Residuals<-NULL

for(i in dfRacksCast3%>%
    filter(Time>="2019-01-01")%>%
    .$Index%>%unique%>%sort){
  print(i)
  training<-dfRacksCast3%>%
    ungroup%>%
    filter(Index<i)%>%
    mutate(SqrtCumDis=sqrt(-CumDis))
  
  test<-dfRacksCast3%>%
    ungroup%>%
    filter(Index>=i)%>%
    mutate(SqrtCumDis=sqrt(-CumDis))
  
  modeldegrad<-training%>%
    ungroup%>%
    lm(data=.,
       delSOC~(kWhChg+kWhDis+Hours)*(SqrtCumDis)-SqrtCumDis+P2+0+TempdT+kWhChgT+kWhDisT,
       weights = Weight)
  
  modeldegrad2<-training%>%
    ungroup%>%
    lm(data=.,
       delSOC~(kWhChg+kWhDis+Hours)*(CumAh50)-CumAh50+P2+0+TempdT+kWhChgT+kWhDisT,
       weights = Weight)
  
  modelconstant<-training%>%
    ungroup%>%
    lm(data=.,
       delSOC~(kWhChg+kWhDis+Hours)+P2+0+TempdT+kWhChgT+kWhDisT,
       weights = Weight)
  
  test%>%
    ungroup%>%
    select(Time,
           Weight,
           delSOC,
           Index)%>%
    mutate(preddelSOCConst=predict(modelconstant,test),
           preddelSOCDegrad=predict(modeldegrad,test),
           preddelSOCDegrad2=predict(modeldegrad2,test),
           PredictedIndex=i)%>%
    summarise(Time=first(Time),
              MeanErrorConst=sum((preddelSOCConst-delSOC)*Weight)/sum(Weight),
              RMSEErrorConst=sqrt(sum((preddelSOCConst-delSOC)^2*Weight)/sum(Weight)),
              MeanErrorDegrad=sum((preddelSOCDegrad-delSOC)*Weight)/sum(Weight),
              RMSEErrorDegrad=sqrt(sum((preddelSOCDegrad-delSOC)^2*Weight)/sum(Weight)),
              MeanErrorDegrad2=sum((preddelSOCDegrad2-delSOC)*Weight)/sum(Weight),
              RMSEErrorDegrad2=sqrt(sum((preddelSOCDegrad2-delSOC)^2*Weight)/sum(Weight)),
              Index=i
    )%>%
    bind_rows(Residuals)->Residuals
  
}

Residuals%>%
  select(-Index)%>%
  melt(id.vars=c("Time","Weight"))%>%
  ggplot(aes(x=Time,y=value,colour=variable,size=Weight,wts=Weight))+geom_point()+ylim(c(-0.1,0.1))+
  stat_smooth(method="lm")+
  scale_color_brewer(palette = "Set1")+
  theme_bw()

Residuals%>%
  ungroup%>%
  arrange(Time)%>%
  filter(Time>="2018-01-01")%>%
  mutate(CumRMSEConst=sqrt(cumsum((preddelSOCConst-delSOC)^2*Weight)/cumsum(Weight)),
         CumRMSEDegrad=sqrt(cumsum((preddelSOCDegrad-delSOC)^2*Weight)/cumsum(Weight)),
         CumRMSEDegrad2=sqrt(cumsum((preddelSOCDegrad2-delSOC)^2*Weight)/cumsum(Weight)))%>%
  select(CumRMSEConst,CumRMSEDegrad,CumRMSEDegrad2,Time)%>%
  melt(id.var="Time")%>%
  ggplot(aes(x=Time,y=value,colour=variable))+geom_line()+scale_color_brewer(palette = "Set1")+theme_bw()

Residuals%>%
  melt(id.vars=c("Time","Index"))%>%
  filter(Time>="2019-01-01")%>%
  mutate(Type=ifelse(grepl("Mean",variable),"Mean","RMSE"),
         Type2=gsub(".*Error","",variable))%>%
  ggplot(aes(x=Time,y=value,colour=Type2))+geom_line(size=1.2)+theme_bw()+facet_wrap(.~Type,scales="free")+scale_colour_brewer(palette="Set1")

dfRacksCast3%>%
  group_by(Index2)%>%
  filter(max(SOC)>0.75,
         min(SOC)<0.25)%>%
  summarise(CumAh50=approx(SOC,CumAh,0.5)$y,
            CumAh75=approx(SOC,CumAh,0.75)$y,
            CumAh25=approx(SOC,CumAh,0.25)$y,
            Time=first(Time))%>%
  ungroup%>%
  arrange(Time)%>%
  mutate(CumAh50=CumAh50-first(CumAh50),
         CumAh25=CumAh25-first(CumAh25),
         CumAh75=CumAh75-first(CumAh75))%>%
  select(-Index2)%>%
  melt(id.var="Time")%>%
  ggplot(aes(x=Time,y=value,colour=variable))+geom_point()+theme_bw()+scale_colour_brewer(palette="Set1")


dfRacksCast3%>%
  group_by(Index2)%>%
  filter(max(SOC)>0.5,
         min(SOC)<0.5)%>%
  summarise(CumAh50=approx(SOC,CumAh,0.5)$y,
            CumDis=first(CumDis),
            Time=first(Time))%>%
  ungroup%>%
  arrange(Time)%>%
  mutate(CumAh50=CumAh50-first(CumAh50))%>%
  ggplot(aes(x=-CumDis,y=CumAh50))+geom_point()+theme_bw()+scale_colour_brewer(palette="Set1")



Residuals<-NULL


scenarios<-expand.grid(Formula=c("delSOC~(kWhChg+kWhDis+Hours)+P2+0+TempdT+kWhChgT+kWhDisT",
                                 "delSOC~(kWhChg+kWhDis+Hours)*(SqrtCumDis)-SqrtCumDis+P2+0+TempdT+kWhChgT+kWhDisT",
                       "delSOC~(kWhChg+kWhDis+Hours)*(CumDis)-CumDis+P2+0+TempdT+kWhChgT+kWhDisT"),
                       Weight=c(0,1/(0.5)),
                       stringsAsFactors = FALSE)

scenarios<-bind_rows(scenarios,
                     data.frame(Formula="delSOC~(kWhChg+kWhDis+Hours)*(Time)-Time+P2+0+TempdT+kWhChgT+kWhDisT",
                                Weight=1/0.5,
                                stringsAsFactors = FALSE))

for(j in 7){
print(j)
for(i in dfRacksCast3%>%
    filter(Time>="2019-01-01")%>%
    .$Index%>%unique%>%sort){
  print(i)
  
  Wt<-scenarios$Weight[j]
  Form<-scenarios$Formula[j]%>%as.formula
  
  training<-dfRacksCast3%>%
    ungroup%>%
    filter(Index<i)%>%
    mutate(SqrtCumDis=sqrt(-CumDis),
           ExpWt=exp(as.numeric(Time-first(Time))*Wt/60/60/24/365),
           Weight=Weight*ExpWt)
  
  test<-dfRacksCast3%>%
    ungroup%>%
    filter(Index>=i)%>%
    mutate(SqrtCumDis=sqrt(-CumDis))
  
  model<-training%>%
    ungroup%>%
    lm(data=.,
       Form,
       weights = Weight)
  
  test%>%
    ungroup%>%
    select(Time,
           Weight,
           delSOC,
           Index)%>%
    mutate(preddelSOC=predict(model,test),
           PredictedIndex=i)%>%
    summarise(Time=first(Time),
              MeanErrorConst=sum((preddelSOC-delSOC)*Weight)/sum(Weight),
              RMSEErrorConst=sqrt(sum((preddelSOC-delSOC)^2*Weight)/sum(Weight)),
              Index=i
    )%>%
    mutate(Scenario=j)%>%
    bind_rows(Residuals)->Residuals
  
}
}


Residuals%>%
  melt(id.vars=c("Time","Index","Scenario"))%>%
  filter(Time>="2018-01-01",
         Time<="2020-04-10",
         Scenario>=4)%>%
  mutate(Type=ifelse(grepl("Mean",variable),"Mean","RMSE"),
         Type2=gsub(".*Error","",variable))%>%
  merge(data.frame(Scenario=4:7,Formula=c("Constant","sqrt(Edis)","Edis","t")))%>%
  ggplot(aes(x=Time,y=value,colour=factor(Formula)))+geom_line(size=1.2)+theme_bw()+facet_wrap(.~Type,scales="free")+scale_colour_brewer(palette="Set1")
