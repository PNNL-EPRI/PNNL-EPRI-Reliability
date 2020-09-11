library(tidyverse)
library(feather)
library(lubridate)
library(reshape2)

Channels<- "C:\\Users\\craw038\\OneDrive - PNNL\\Documents2\\EPRI\\Data\\EPRIChannels.feather"%>%read_feather()

datafiles<-list.files("C:\\Users\\craw038\\OneDrive - PNNL\\Documents2\\EPRI\\Data",full.names = TRUE,pattern="*.feather")


datafiles[[3]]%>%read_feather->df


RackChannels<-Channels%>%
  group_by(ChannelID)%>%
  filter(SiteID==18517,
         SystemNodeType=="Battery Rack",
         ChannelCatalog %in% c("DC.Power","StateOfCharge","DC.Current","DC.Voltage","Temp.Battery.Module"),
         !grepl("Limit",Descriptor),
         !(ChannelCatalog=="DC.Voltage" & !grepl("Average",Descriptor)),
         !(ChannelCatalog=="Temp.Battery.Module" & !grepl("Average",Descriptor)))


RackChannelsString<-paste0(RackChannels$ChannelID,collapse="|")%>%paste0("|Time")


dfRacks<-df%>%
  mutate(Time=TimeUTC%>%fast_strptime(format="%Y-%m-%d %H:%M:%S",lt = FALSE),
         #RackNo=ch146256_State.LGChem.RackCount_Avg
  )%>%
  select(matches(RackChannelsString))%>%
  select(-TimeUTC,
         -TimeLocal_US_ET)%>%
  na.omit


dfRacksMelted<-dfRacks%>%
  melt(id.vars=c("Time"))%>%
  arrange(Time,variable)

dfRacksMelted2<-dfRacksMelted%>%
  group_by(Time,variable)%>%
  summarise_all(first)%>%
  group_by(Time)%>%
  mutate(Rack=ceiling(seq_along(Time)/5),
         variable=rep(c("SOC","Current","Power","Voltage","Temp"),12))

dfRacksCast<-dfRacksMelted2%>%
  dcast(Time+RackNo+Rack~variable,value.var = "value")