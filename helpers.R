library(tidyverse)
library(ggplot2)
library(ggcharts)
library(dplyr)
library(shiny)
library(bslib)
library(DT)




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



observeEvent(c(input$cat1, input$cat2), {
  cat1 <- input$cat1
  cat2<- input$cat2
  choices <- numeric_vars
  if (cat1 == cat2){
    choices <- choices[-which(choices == cat1)]
    updateSelectizeInput(session,
                         "cat2",
                         choices = choices)
  }
}
)