library(tidyverse)
library(ggplot2)
library(ggcharts)
library(dplyr)
library(shiny)
library(bslib)
library(DT)
library(hrbrthemes)
library(gcookbook)

library(rsconnect)


data<-read.csv("user_behavior_dataset.csv")



data2<-data|>
  mutate(User.Behavior.Class=as.factor(User.Behavior.Class))



numeric_vars <- c("App Usage" = "App.Usage.Time..min.day.",
                  "Screen Time" = "Screen.On.Time..hours.day.",
                  "Battery Drain" = "Battery.Drain..mAh.day.",
                  "Number of Apps" = "Number.of.Apps.Installed",
                  "Data Usage" = "Data.Usage..MB.day.",
                  "Age" = "Age")

cat_vars<-c("Gender"="Gender",
            "Behavior Class"="User.Behavior.Class",
            "Operating System"="Operating.System",
            "Device Model"="Device.Model")

cat_vars2<-cat_vars



