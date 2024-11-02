library(tidyverse)
library(ggplot2)
#Access the data locally 

data<-read.csv("user_behavior_dataset.csv")

str(data)

data2<-data|>
  mutate(User.Behavior.Class=as.factor(User.Behavior.Class))
 

#One way contingency Table 
table(data2$Gender)
table(data2$Operating.System)
table(data2$gender,data2$Operating.System)

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



