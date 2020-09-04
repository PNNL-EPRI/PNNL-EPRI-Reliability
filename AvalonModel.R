library(glmnet)
library(dplyr)
library(glmnetUtils)
library(ggplot2)
library(reshape2)
library(lubridate)

Channels<- "C:\\Users\\craw038\\OneDrive - PNNL\\Documents2\\EPRI\\Data\\EPRIChannels.feather"%>%read_feather()

datafiles<-list.files("C:\\Users\\craw038\\OneDrive - PNNL\\Documents2\\EPRI\\Data",full.names = TRUE,pattern="*.feather")

datafiles[[1]]%>%read_feather->df


RackChannels<-Channels%>%
  filter(SiteID==18514,
         SystemNodeType=="Battery Module",
         ChannelCatalog %in% c("DC.Power","StateOfCharge","DC.Current","Temp.Electrolyte"),
         is.na(Descriptor) | Descriptor=="Sum" | Descriptor=="Average")

RackChannelsString<-paste0(RackChannels$ChannelID,collapse="|")%>%paste0("|Time")


df%>%
  mutate(Time=TimeUTC%>%fast_strptime(format="%Y-%m-%d %H:%M:%S",lt = FALSE),
         PF=ch202234_AC.Power.Q_Avg_kVAR/ch202238_AC.Power.S_Avg_kVA)%>%
  select(matches(RackChannelsString),PF)%>%
  na.omit%>%
  select(-TimeLocal_US_CT,-TimeUTC)->df2


df2%>%select(-PF)%>%melt(id.vars=c("Time"))%>%
  filter(!grepl("State.Cum",variable))%>%
  mutate(ChannelID=parse_number(variable%>%as.character))%>%
  merge(RackChannels%>%select(ChannelID,SystemNode))%>%
  mutate(Type=ifelse(grepl("Power",variable),"Power","SOC"),
         Type=ifelse(grepl("Temp",variable),"Temp",Type))%>%
  select(Time,Type,SystemNode,value)%>%
  dcast(Time+SystemNode~Type,value.var="value")%>%
  mutate(SystemNode=SystemNode%>%as.character)->df3

df3%>%
  merge(df2%>%select(Time,PF))%>%
  #filter(Time<="2019-11-15")%>%
  group_by(SystemNode)%>%
  filter(SOC>1,SOC<99,
         PF>=-0.99)%>%
  mutate(State=sign(Power),
         SOCJump=c(0,diff(SOC)),
         diffState=c(0,diff(State)),
         dt=c(0,Time%>%as.numeric%>%diff),
         Index=cumsum(diffState!=0 | dt>median(dt) | abs(SOCJump)>5),
         Index2=paste0(Index,"-",SystemNode))%>%
  group_by(Index2)%>%
  mutate(SOC=SOC/100,
        delSOC=last(SOC)-first(SOC),
         Elapsed=Time%>%as.numeric-first(Time%>%as.numeric),
         Elapsed=Elapsed/60/60,
         dt=c(0,diff(Elapsed)),
         dSOC=SOC-first(SOC),
         PChg=cumsum(Power*(Power<0)*dt),
        PDis=cumsum(Power*(Power>0)*dt),
        PChgSOC=cumsum(Power*(Power<0)*dt*SOC),
        PDisSOC=cumsum(Power*(Power>0)*dt*SOC),
        PChgTemp=cumsum(Power*(Power<0)*dt*Temp),
        PDisTemp=cumsum(Power*(Power>0)*dt*Temp),
        SOCTerm=cumsum(SOC*dt),
        TempTerm=cumsum(Temp*dt),
        P2=cumsum(Power^2*dt))%>%
  filter(abs(delSOC)>0.1,
         State!=0)->df4

df4<-df4%>%filter(Time>="2019-12-01",
                  !(SystemNode%in% c("Battery 7","Battery 8","Battery 9"))
                  )
  ggplot(aes(x=Elapsed,y=SOC))+facet_wrap(.~Index2,scales="free")+geom_line()+theme_bw()
  
df4%>%
  group_by(Index2)%>%
  summarise(SOCJump=SOC%>%diff%>%abs%>%max,
            delSOC=mean(delSOC),
            AvgPower=mean(Power),
            Points=n())%>%View
    
df4%>%
  lm(data=.,dSOC~Elapsed+0+PChg+PDis+PChgSOC+PDisSOC+PChgTemp+PDisTemp+SOCTerm+TempTerm+P2)%>%summary

Indexes<-df4%>%group_by(Index2)%>%summarise(StartTime=min(Time),Count=n())%>%filter(Count>=6)%>%mutate(Order=seq_along(Index2))

Terms<-c("P2","PChgTemp+PDisTemp","PChgSOC+PDisSOC")%>%.[-2]%>%.[-1]

Predictions<-NULL
for(j in 0:length(Terms)){
for(i in 10:nrow(Indexes)){
  print(i)
  formula<-ifelse(j==0,
                  paste0("dSOC~Elapsed+0+PChg+PDis+SOCTerm+TempTerm+",paste0(Terms,collapse="+"),collapse="+"),
                  paste0("dSOC~Elapsed+0+PChg+PDis+SOCTerm+TempTerm",ifelse(j==1 & length(Terms)==1,"","+"),paste0(Terms[-j],collapse="+"),collapse="+"))%>%as.formula
  train<-df4%>%
    filter(Index2 %in% (Indexes%>%filter(Order<i)%>%.$Index2))
  
  test<-df4%>%
    filter(Index2 %in% (Indexes%>%filter(Order==i)%>%.$Index2))
  
  Model<-train%>%ungroup%>%
    select(PChg:P2,dSOC,Elapsed)%>%
    lm(data=.,formula)
  
  Predictions<-test%>%
    mutate(Prediction=predict(Model,test),
           Term=j)%>%
    bind_rows(Predictions)
}
}

Predictions%>%
  mutate(Error=Prediction-dSOC)%>%
  filter(Term==0)%>%
  #filter(SystemNode!="Battery 4")%>%
  group_by(Index2,Term)%>%
  summarise(SystemNode=first(SystemNode),
            StartTime=first(Time),
            MSE=Error%>%.^2%>%mean,
            delSOC=mean(delSOC)%>%abs)%>%
  group_by(SystemNode,Term)%>%
  mutate(CumRMSE=sqrt(cumsum(MSE*delSOC)/cumsum(delSOC)))%>%ggplot(aes(x=StartTime,y=CumRMSE,colour=SystemNode))+geom_line(size=1.2)+theme_bw()+
  scale_color_brewer(palette = "Set1")+scale_y_log10()+ylab("Cum. RMS Error")+xlab("Date")
  
  
  Predictions%>%
    mutate(Error=Prediction-dSOC)%>%
    filter(Term==0)%>%
    #filter(SystemNode!="Battery 4")%>%
    group_by(Index2,Term)%>%
    summarise(SystemNode=first(SystemNode),
              StartTime=first(Time),
              MSE=Error%>%.^2%>%mean,
              ME=Error%>%mean,
              delSOC=mean(delSOC)%>%abs)%>%
    group_by(SystemNode,Term)%>%
  mutate(CumRMSE=sqrt(cumsum(MSE*delSOC)/cumsum(delSOC)),
         CumME=cumsum(ME*delSOC)/cumsum(delSOC))%>%
    ggplot(aes(x=StartTime,y=CumME,colour=SystemNode))+geom_line(size=1.2)+theme_bw()+
    scale_color_brewer(palette = "Set1")+ylab("Cum. Mean Error")+xlab("Date")


Predictions%>%
  group_by(Term,Index2)%>%
  #filter(SystemNode!="Battery 4")%>%
  mutate(Error=Prediction-dSOC,
         delSOC=c(0,diff(SOC)))%>%
  group_by(Term)%>%
 #group_by(Term,Index2)%>%
 # filter(Term==0)%>%
  summarise(RMSE=((abs(delSOC)*(Error%>%.^2))%>%sum)/(delSOC%>%abs%>%sum),
            StartTime=first(Time))%>%View


Indexes<-df4%>%group_by(Index2)%>%summarise(StartTime=min(Time),Count=n())%>%filter(Count>=6)%>%mutate(Order=seq_along(Index2))

RMSEAvalon<-function(x){
  Indexes<-df4%>%group_by(Index2)%>%summarise(StartTime=min(Time),Count=n())%>%filter(Count>=6)%>%mutate(Order=seq_along(Index2))
  
  glmalpha<-(tanh(x[1])+1)/2
  glmlambda<-exp(x[2])
  print(c(glmalpha,glmlambda))
  
  Predictions<-NULL
  for(i in 10:nrow(Indexes)){
   # print(i)
    
    formula<-"dSOC~Elapsed+0+PChg+PDis+SOCTerm+TempTerm+P2+PChgSOC+PDisSOC+PChgTemp+PDisTemp"%>%as.formula

    train<-df4%>%
      filter(Index2 %in% (Indexes%>%filter(Order<i)%>%.$Index2))
    
    test<-df4%>%
      filter(Index2 %in% (Indexes%>%filter(Order==i)%>%.$Index2))
    
    Model<-train%>%ungroup%>%
      select(PChg:P2,dSOC,Elapsed)%>%
      glmnet(data=.,
             formula,
             alpha=glmalpha,
             lambda=glmlambda)
    
    Predictions<-test%>%
      mutate(Prediction=predict(Model,test))%>%
      bind_rows(Predictions)
  }
  Predictions%>%mutate(Error=Prediction-dSOC)%>%.$Error%>%.^2%>%na.omit%>%mean%>%sqrt%T>%print%>%return
}

z<-optim(c(1.9 ,-7.2),RMSEAvalon)

glmalpha<-0.8598268369 #(tanh(z$par[1])+1)/2
glmlambda<-0.0008591152#exp(z$par[2])
print(c(glmalpha,glmlambda))

Predictions2<-NULL
for(i in 10:nrow(Indexes)){
   print(i)
  
  formula<-"dSOC~Elapsed+0+PChg+PDis+SOCTerm+TempTerm+P2+PChgSOC+PDisSOC+PChgTemp+PDisTemp"%>%as.formula
  
  train<-df4%>%
    filter(Index2 %in% (Indexes%>%filter(Order<i)%>%.$Index2))
  
  test<-df4%>%
    filter(Index2 %in% (Indexes%>%filter(Order==i)%>%.$Index2))
  
  Model<-train%>%ungroup%>%
    select(PChg:P2,dSOC,Elapsed)%>%
    glmnet(data=.,
           formula,
           alpha=glmalpha,
           lambda=glmlambda)
  
  Predictions2<-test%>%
    mutate(Prediction=predict(Model,test))%>%
    bind_rows(Predictions2)
}

Predictions2%>%mutate(Type="ElasticNet")%>%
  bind_rows(Predictions%>%filter(Term==0)%>%mutate(Type="MLR"))%>%
  mutate(Error=Prediction-dSOC)%>%
  #filter(SystemNode!="Battery 4")%>%
  group_by(Index2,Type)%>%
  summarise(SystemNode=first(SystemNode),
            StartTime=first(Time),
            MSE=Error%>%.^2%>%mean,
            ME=Error%>%mean,
            delSOC=mean(delSOC)%>%abs)%>%
  group_by(SystemNode,Type)%>%
  mutate(CumRMSE=sqrt(cumsum(MSE*delSOC)/cumsum(delSOC)),
         CumME=cumsum(ME*delSOC)/cumsum(delSOC))%>%
  ggplot(aes(x=StartTime,y=CumRMSE,colour=SystemNode))+geom_line(size=1.2)+theme_bw()+facet_wrap(.~Type)+
  scale_color_brewer(palette = "Set1")+
  #scale_y_log10()+
  ylab("Cum. RMS Error")+
  xlab("Date")
