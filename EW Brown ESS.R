library(tidyverse)
library(feather)
library(lubridate)

Channels<- "C:\\Users\\craw038\\OneDrive - PNNL\\Documents2\\EPRI\\Data\\EPRI Channels.feather"%>%read_feather()

datafiles<-list.files("C:\\Users\\craw038\\OneDrive - PNNL\\Documents2\\EPRI\\Data",full.names = TRUE,pattern="*.feather")

datafiles[[4]]%>%read_feather->df

df%>%
  transmute(Time=TimeUTC%>%fast_strptime(format="%Y-%m-%d %H:%M:%S",lt = FALSE),
            Racks=ch146256_State.LGChem.RackCount_Avg,
            SOC=`ch146264_StateOfCharge_Avg_%`/100,
            Power=ch146276_DC.Power_Avg_kW)->df2

Channels%>%
  select(-ChannelAggregation,
        -SystemNodeID,
        -DataSourceID,
        -DataSourceFolderName,
        -DataSourceImportingEnabled,
        -LogFileID,
        -LogFilePrefix,
        -LogFileIsUsed,
        -LogFileSamplingPeriod,
        -LogFileUnit,
        -ChannelImportingEnabled)%>%
  write.csv(row.names = FALSE,"DIAMONDChannels.csv")

df2%>%
  na.omit%>%
  mutate(State=abs(abs(Power)>50)*sign(Power),
         StChg=c(0,diff(State)),
         Index=cumsum(StChg!=0),
         CumDis=cumsum(Power*(Power<0))*5/60)->df3
dt<-5/60
df3%>%
  group_by(Index)%>%
  filter(max(Racks)==20,
         n()>=12,
         max(abs(diff(SOC)))<=0.1,
         State!=0)%>%
  #filter(State!=0)%>%
  mutate(Weight=(max(SOC)-min(SOC))/n(),
         Hours=(Time-first(Time))/60/60,
         kWhChg=(cumsum(Power*(Power>0))-Power*(Power>0)/2-first(Power*(Power>0))/2)*dt,
         kWhDis=(cumsum(Power*(Power<0))-Power*(Power<0)/2-first(Power*(Power<0))/2)*dt,
         delSOC=SOC-first(SOC))->df4

df4%>%
  ungroup%>%
  lm(data=.,
     delSOC~kWhChg+kWhDis+Hours+0,
     weights = Weight)->LinearModel

LinearModel%>%summary

df4%>%
  ungroup%>%
  mutate(Prediction=LinearModel$fitted.values,
         Residuals=LinearModel$residuals)->df5

df5%>%
  group_by(Index)%>%
  mutate(Index2=paste(Index,
                      first(Time%>%as.Date()),
                       mean(Power)%>%round,
                      "kW"))%>%
  mutate(Prediction2=Prediction+first(SOC))%>%
  ggplot(aes(x=Hours,y=SOC*100))+geom_point()+facet_wrap(.~Index2,scales="free")+
  geom_line(aes(y=Prediction2*100),colour="blue")+ylab("SOC (%)")+xlab("Time (h)")+theme_bw()

df5%>%
  group_by(Index)%>%
  summarise(Time=median(Time),
            meanResid=mean(Residuals),
            Weight=sum(Weight),
            CumDis=-mean(CumDis),
            State=ifelse(median(State)>0,"Charge","Discharge"))%>%
  ggplot(aes(x=CumDis/1000,y=meanResid*100,weight=Weight,colour=factor(State)))+
  geom_point()+stat_smooth(method="lm")+
  theme_bw()+
  scale_color_brewer(palette = "Set1")+
  ylab("Mean Residual (%)")+
  xlab("Cumulative Discharge Energy (MWh)")


RackChannels<-Channels%>%
  filter(SiteID==17020,
         SystemNodeType=="Battery Rack",
         ChannelCatalog %in% c("DC.Voltage","StateOfCharge","DC.Current","Temp.Battery.Module"),
         is.na(Descriptor) | Descriptor=="Sum of Cells" | Descriptor=="Avg of Modules")%>%View
  select(ChannelID,
         SystemNode,
         ChannelCatalog)

RackChannelsString<-paste0(RackChannels$ChannelID,collapse="|")%>%paste0("|Time|RackNo")

dfRacks<-df%>%
  mutate(Time=TimeUTC%>%fast_strptime(format="%Y-%m-%d %H:%M:%S",lt = FALSE),
         #RackNo=ch146256_State.LGChem.RackCount_Avg
         RackNo=20
         )%>%
  select(matches(RackChannelsString))%>%
  select(-TimeUTC,
         -TimeLocal_US_ET)%>%
  na.omit

dfRacksMelted<-dfRacks%>%
  melt(id.vars=c("Time","RackNo"))%>%
  arrange(Time,variable)

dfRacksMelted2<-dfRacksMelted%>%
  group_by(Time,variable)%>%
  summarise_all(first)%>%
  group_by(Time)%>%
  mutate(Rack=ceiling(seq_along(Time)/4),
         variable=rep(c("Voltage","SOC","Temp","Current"),20))

dfRacksCast<-dfRacksMelted2%>%
  dcast(Time+RackNo+Rack~variable,value.var = "value")


dfRacksCast2<-dfRacksCast%>%
  na.omit%>%
  mutate(Power=Voltage*Current,
         SOC=SOC/100)%>%
  group_by(Rack)%>%
  mutate(State=abs(abs(Power)>50)*sign(Power),
         StChg=c(0,diff(State)),
         Index=cumsum(StChg!=0),
         Index2=paste0(Rack,"-",Index),
         CumDis=cumsum(Power*(Power<0))*5/60)

dt<-5/60
dfRacksCast3<-dfRacksCast2%>%
  group_by(Index2)%>%
  filter(max(RackNo)==20,
         n()>=12,
         max(abs(diff(SOC)))<=0.1,
         State!=0)%>%
  #filter(State!=0)%>%
  mutate(Weight=(max(SOC)-min(SOC))/n(),
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
     delSOC~kWhChg+kWhDis+Hours+P2+0+TempdT+kWhChgT+kWhDisT,
     weights = Weight)->LinearModel

LinearModel%>%summary

dfRacksCast3%>%
  ungroup%>%
  mutate(Prediction=LinearModel$fitted.values,
         Residuals=LinearModel$residuals)->dfRacksCast4


dfRacksCast4%>%
  group_by(Index2)%>%
  summarise(Time=median(Time),
            meanResid=mean(Residuals),
            Weight=sum(Weight),
            CumDis=-mean(CumDis),
            State=ifelse(median(State)>0,"Charge","Discharge"),
            Temp=mean(Temp))%>%
  ggplot(aes(x=Time,y=meanResid*100,weight=Weight,colour=factor(State)))+
  geom_point(aes(size=Weight))+stat_smooth(method="lm")+
  facet_wrap(.~State)+
  theme_bw()+
  scale_color_brewer(palette = "Set1",name="")+
  ylab("Mean Residual (%)")+
  xlab("Cumulative Discharge Energy (MWh)")

dfRacksCast4%>%
  group_by(Index2,Rack)%>%
  summarise(Time=median(Time),
            meanResid=mean(Residuals),
            Weight=sum(Weight),
            CumDis=-mean(CumDis),
            State=ifelse(median(State)>0,"Charge","Discharge"),
            Temp=mean(Temp))%>%
  filter(State=="Discharge")%>%
  ggplot(aes(x=CumDis,y=meanResid*100,weight=Weight,group=factor(Rack)))+
  #geom_point(aes(size=Weight))+
  stat_smooth(method="lm")+
  #facet_wrap(.~Rack)+
  theme_bw()+
  scale_color_brewer(palette = "Set1",name="")+
  ylab("Mean Residual (%)")+
  xlab("Cumulative Discharge Energy (MWh)")

dfRacksCast5<-NULL
for(i in 1:20){
  print(i)
  dfRacksCast3%>%
    ungroup%>%
    filter(Rack==i)%>%
    lm(data=.,
       delSOC~kWhChg+kWhDis+Hours+P2+0+TempdT+kWhChgT+kWhDisT+SOCdt+kWhChgSOC+kWhDisSOC,
       weights = Weight)->LinearModel
  
  LinearModel%>%summary
  
  dfRacksCast5<-dfRacksCast3%>%
    ungroup%>%
    filter(Rack==i)%>%
    mutate(Prediction=LinearModel$fitted.values,
           Residuals=LinearModel$residuals)%>%
    bind_rows(dfRacksCast5)
}


dfRacksCast5%>%
  group_by(Index2,Rack)%>%
  summarise(Time=median(Time),
            meanResid=mean(Residuals),
            Weight=sum(Weight),
            CumDis=-mean(CumDis),
            State=ifelse(median(State)>0,"Charge","Discharge"),
            Temp=mean(Temp),
            meanSOC=mean(SOC),
            Variation=sd(Power))%>%
  #filter(Time>"2018-01-01")%>%
  ggplot(aes(x=(CumDis/1000000),y=meanResid*100,weight=Weight,colour=factor(State)))+
  geom_point(aes(size=Weight))+stat_smooth(method="lm")+
  #facet_grid(State~Rack)+
  facet_wrap(.~State)+
  theme_bw()+
  scale_color_brewer(palette = "Set1",name="")+
  ylab("Mean Residual (%)")+
  xlab("Cumulative Discharge Energy (MWh)")


dfRacksCast5%>%
  group_by(Index2,Rack)%>%
  summarise(Time=median(Time),
            meanResid=mean(Residuals),
            Weight=sum(Weight),
            CumDis=-mean(CumDis),
            State=ifelse(median(State)>0,"Charge","Discharge"),
            Temp=mean(Temp),
            meanSOC=mean(SOC),
            Variation=sd(Power))%>%
  #filter(Time>"2018-01-01")%>%
  ggplot(aes(x=(CumDis/1000000),y=meanResid*100,weight=Weight,group=Rack))+
  #geom_point(aes(size=Weight))+
  stat_smooth(method="lm")+
  #facet_grid(State~Rack)+
  facet_wrap(.~State)+
  #facet_wrap(.~Rack)+
  theme_bw()+
  scale_color_brewer(palette = "Set1",name="")+
  ylab("Mean Residual (%)")+
  xlab("Cumulative Discharge Energy (MWh)")
