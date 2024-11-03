library(tidyverse)
library(ggplot2)
library(ggcharts)
library(dplyr)
#Access the data locally 

data<-read.csv("user_behavior_dataset.csv")

str(data)

data2<-data|>
  mutate(User.Behavior.Class=as.factor(User.Behavior.Class))
 

#One way and Two way contingency Table 
table(data2$Gender)
table(data2$Operating.System)


data2|>
  drop_na(Gender,Operating.System)|>
  group_by(Gender,Operating.System)|>
  summarize(count=n())|>
  pivot_wider(names_from = Operating.System,values_from = count)


data2|>
  drop_na(Gender,User.Behavior.Class)|>
  group_by(Gender,User.Behavior.Class)|>
  summarize(count=n())|>
pivot_wider(names_from = Gender,values_from = count)

data2|>
  group_by(Gender)|>
  summarize(mean_age=mean(Age,na.rm=TRUE),med_age=median(Age,na.rm=TRUE),min_age=min(Age,na.rm=TRUE),Max_age=max(Age,na.rm = TRUE))

data2|>
  group_by(Gender)|>
  summarize(mean_apps=mean(Number.of.Apps.Installed,na.rm=TRUE),sd_apps=sd(Number.of.Apps.Installed,na.rm=TRUE),med_apps=median(Number.of.Apps.Installed,na.rm=TRUE),iqr_apps=IQR(Number.of.Apps.Installed,na.rm=TRUE),min_apps=min(Number.of.Apps.Installed,na.rm=TRUE),Max_apps=max(Number.of.Apps.Installed,na.rm = TRUE))

data2|>
  group_by(Gender)|>
  summarize(mean_scrntime=mean(Screen.On.Time..hours.day.,na.rm=TRUE),sd_scrntime=sd(Screen.On.Time..hours.day.,na.rm=TRUE),med_scrntime=median(Screen.On.Time..hours.day.,na.rm=TRUE),iqr_scrntime=IQR(Screen.On.Time..hours.day.,na.rm=TRUE),min_scrntime=min(Screen.On.Time..hours.day.,na.rm=TRUE),Max_scrntime=max(Screen.On.Time..hours.day.,na.rm = TRUE))

#Plot1

ggplot(data=data2|>drop_na(Gender,Operating.System), aes(x=Gender, fill=data2$Operating.System))+
  geom_bar()+
  labs(x="Gender")+
  scale_fill_discrete("Operating_System")+
  coord_flip()


#Plot2

ggplot(data=data2|>drop_na(Age,Gender),aes(x=Age))+
  geom_density(alpha=0.05,color="black",aes(fill=Gender))+
  scale_fill_manual(values=c("Female"="purple","Male"="green"))

#Plot3

ggplot(data=data2,aes(x=Screen.On.Time..hours.day.,y=Data.Usage..MB.day.,color=User.Behavior.Class))+
  geom_point(shape=17,size=2)


#Plot4



#Plot5

ggcharts_set_theme("theme_nightblue")
bar_chart(data=data2,x=Device.Model,facet=Gender)


