library(tidyverse)
library(feather)
library(lubridate)
library(reshape2)

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
  rename(Module=SystemNode)%>%
  filter(Time>="2019-12-01",
            !(Module%in% c("Battery 7","Battery 8","Battery 9")))%>%
  group_by(Time)%>%
  mutate(ModuleNo=n())%>%
  ungroup->AvalonData

AvalonData%>%write_feather("DataPreProcess\\Avalon.feather")