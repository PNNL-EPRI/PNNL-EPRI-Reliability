library(tidyverse)
library(feather)
library(lubridate)
library(reshape2)

Channels<- "Data\\EPRI Channels.feather"%>%read_feather()

datafiles<-list.files("Data\\",full.names = TRUE,pattern="*.feather")

datafiles[[6]]%>%read_feather->df



RackChannels<-Channels%>%
  filter(SiteID==17020,
         SystemNodeType=="Battery Rack",
         ChannelCatalog %in% c("DC.Voltage","StateOfCharge","DC.Current","Temp.Battery.Module"),
         is.na(Descriptor) | Descriptor=="Sum of Cells" | Descriptor=="Avg of Modules")%>%
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

dfRacksCast%>%
  rename(ModuleNo=RackNo,
         Module=Rack)%>%
  mutate(Power=Current*Voltage)->EWBrownData

EWBrownData%>%write_feather("DataPreProcess\\EWBrown.feather")
