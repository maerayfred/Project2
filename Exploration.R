str(data2)




#One way and Two way contingency Table 
OneWayG<-table(data2$Gender)
print(OneWayG)

OneWayOS<-table(data2$Operating.System)
print(OneWayOS)

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

ggplot(data=data2|>drop_na(Gender,Operating.System), aes(x=Gender, fill=Operating.System))+
  geom_bar()+
  labs(title="Relationship Between Gender and Operating Systems",
       x="Gender")+
  scale_fill_discrete("Operating_System")+
  coord_flip()


#Plot2

ggplot(data=data2|>drop_na(Age,Gender),aes(x=Age))+
  geom_density(alpha=0.05,color="black",aes(fill=Gender))+
  
  labs(title = "Density Plot of Age by Gender"
       
  )+
  scale_fill_manual(values=c("Female"="purple","Male"="green"))

#Plot3

ggplot(data=data2,aes(x=Screen.On.Time..hours.day.,y=Data.Usage..MB.day.,color=User.Behavior.Class))+
  labs(title = "Relationship Between Screen Time and Data Usage Grouped by Behavior"
       
  )+
  geom_point(shape=17,size=2)


#Plot4
g <- ggplot(data2 |> drop_na(Gender, Age) )
g + geom_boxplot(aes(x = Gender, y = Age, fill = Gender))+
  labs(title = "Relationship Between Gender and Age"
       
  )


#Plot5

ggcharts_set_theme("theme_nightblue")
bar_chart(data=data2,x=Device.Model,facet=Gender)


#Plot6
ggplot(data=data2, aes(x=Number.of.Apps.Installed, y=App.Usage.Time..min.day., fill=Gender)) + 
  geom_area() +
  scale_fill_ipsum() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_comma() +
  labs(title="Relationship Between Number of Apps and App Usage by Gender",
       x="Number of Apps Installed",
       y="App Usage (min/day)"
  ) +
  
  theme(legend.position="bottom")