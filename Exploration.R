library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(ggcharts)
library(dplyr)
library(DT)
library(hrbrthemes)
library(gcookbook)

#This data set of of 700 simulated behavior patterns from mobile device users was very intriguing to me. I've always wondered if age or gender played a role in phone usage and this data set allowed me to explore that as well as other factors such as number of apps on a phone or a users screen time. I'll walk you through some of the relationships I was interested in exploring. 



#I knew there were 700 observations and all included non missing data but I was curious about the gender divide first so I created a frequency table and determined there were slightly more males than females.
table(data2$Gender)

#I was also interested in knowing what the break down was for Android versus iOS users. I was shocked to learn that the majority of users would be Android users. Even though this is simulated data I'm going to assume the data was international, not just based off the US. 
table(data2$Operating.System)


#I was curious if there was a relationship between gender and the operating system. In looking at the break down I can see that the number of men versus women who chose an Android is basically the difference in number of men and women. It also helped to visualize this relationship with a bar chart.

data2|>
  drop_na(Gender,Operating.System)|>
  group_by(Gender,Operating.System)|>
  summarize(count=n())|>
  pivot_wider(names_from = Operating.System,values_from = count)

ggplot(data=data2|>drop_na(Gender,Operating.System), aes(x=Gender, fill=Operating.System))+
  geom_bar()+
  labs(title="Relationship Between Gender and Operating Systems",
       x="Gender")+
  scale_fill_discrete("Operating_System")+
  coord_flip()

#If there wasn't a difference in operations systems broken down by gender, I was curious if there was a device preference between gender. The Xiaomi Mi 11 was the most popular for women but the least popular for men. Similarly the Samsung Galazy S21 was the most popular for men and the least popular for women. 


ggcharts_set_theme("theme_nightblue")
bar_chart(data=data2,x=Device.Model,facet=Gender)+
  labs(title = "Model of Phones",
       x="Device Model",
    
  )


#On the website it defines the behavior class as patterns based on usage, but it doesn't indicate the direction of the scale so I wanted to first see if there was anything interesting in the patterns based on gender. I noticed pretty similar numbers except for the highest number of women was behavior class 3 and the highest behavior class for men was 2. I decided to explore the behavior class relationship a little further with a scatter plot. 

data2|>
  drop_na(Gender,User.Behavior.Class)|>
  group_by(Gender,User.Behavior.Class)|>
  summarize(count=n())|>
  pivot_wider(names_from = Gender,values_from = count)

#In this scatter plot I wanted to confirm that the behavioral class was gradual with 1 being the lowest and 5 being the highest. I assumed that screen time has to play a role in behavior class and typically is you had more screen time you would have higher data usage. It also confirms that the behavior class of 5 will be assigned to simulated observations with higher cell phone usage. 

ggplot(data=data2,aes(x=Screen.On.Time..hours.day.,y=Data.Usage..MB.day.,color=User.Behavior.Class))+
  labs(title = "Relationship Between Screen Time and Data Usage Grouped by Behavior",
       x="Screen Time (hours/day)",
       y="Data Usage (MB/day)"
       
  )+
  geom_point(shape=17,size=2)

#Next I was curious if there was a relationship between the age and gender of these participants. As a whole the age distribution by gender is similar with slightly more younger Females than males and slight older Males than Females. 

data2|>
  group_by(Gender)|>
  summarize(mean_age=mean(Age,na.rm=TRUE),med_age=median(Age,na.rm=TRUE),min_age=min(Age,na.rm=TRUE),Max_age=max(Age,na.rm = TRUE))

#Density Plot of the Age distribution by Gender
ggplot(data=data2|>drop_na(Age,Gender),aes(x=Age))+
  geom_density(alpha=0.05,color="black",aes(fill=Gender))+
  
  labs(title = "Density Plot of Age by Gender"
       
  )+
  scale_fill_manual(values=c("Female"="purple","Male"="green"))

#Box Plot of the Age distribution by Gender
g <- ggplot(data2 |> drop_na(Gender, Age) )
g + geom_boxplot(aes(x = Gender, y = Age, fill = Gender))+
  labs(title = "Relationship Between Gender and Age"
       
  )

#Finally I was interested in the relationship between the number of apps installed on the phone and screen time and if there was a difference between the genders. The summaries of how many apps are installed separated by gender are pretty similar. 

data2|>
  group_by(Gender)|>
  summarize(mean_apps=mean(Number.of.Apps.Installed,na.rm=TRUE),sd_apps=sd(Number.of.Apps.Installed,na.rm=TRUE),med_apps=median(Number.of.Apps.Installed,na.rm=TRUE),iqr_apps=IQR(Number.of.Apps.Installed,na.rm=TRUE),min_apps=min(Number.of.Apps.Installed,na.rm=TRUE),Max_apps=max(Number.of.Apps.Installed,na.rm = TRUE))

#There was a distinct difference on screen time (hours/day) between men and women when similar numbers of apps were installed on their phone. 

ggplot(data=data2, aes(x=Number.of.Apps.Installed, y=Screen.On.Time..hours.day., fill=Gender)) + 
  geom_area() +
  scale_fill_ipsum() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_comma() +
  labs(title="Relationship Between Number of Apps and App Usage by Gender",
       x="Number of Apps Installed",
       y="Screen Time (hours/day)"
  ) +
  
  theme(legend.position="bottom")
