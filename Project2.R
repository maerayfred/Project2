source("helpers.R")
#Access shiny#Access the data locally 

data<-read.csv("user_behavior_dataset.csv")

str(data)

data2<-data|>
  mutate(User.Behavior.Class=as.factor(User.Behavior.Class))

str(data2)




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

ggplot(data=data2|>drop_na(Gender,Operating.System), aes(x=Gender, fill=Operating.System))+
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


ui <- fluidPage(
  titlePanel("Mobile Device Data Exploration"),
  
  sidebarLayout(
    sidebarPanel(
      h2("Subset Data"),
                  selectInput(
                    "cat1",
                    label = "Categorical Variable",
                    choices=cat_vars
                    
                    
                  ),
                  selectInput(
                    "cat2",
                    label = "Categorical Variable",
                    choices=cat_vars
                    
                    
                  ),
                  uiOutput("cat1levels"),
                  uiOutput("cat2levels"),
                  
                  selectInput(
                    "num1",
                    label = "Numeric Variable",
                    choices=numeric_vars
                    ,
                    selected="Age"
                    
                  ),
                  selectInput(
                    "num2",
                    label = "Numeric Variable",
                    choices=numeric_vars[-6]
                    ,
                    selected="Screen.On.Time..hours.day."
                    
                  ) ,
                  
                  uiOutput("num1range"),
                  uiOutput("num2range"),
                  
                  
                  actionButton("show_data","Show Data Table",value=FALSE)
                  
                  
                  
                  
                  
  ),
  mainPanel(
    card(
      card_header("About Tab"),
      "This is where all the information is going to go about the data and the website where it will be found"
    ),
    card(
      card_header("Data Download"),
      dataTableOutput(outputId = "mobiledata")
    ),
    card(
      card_header("Categorical Variables to Summarize"),
      selectInput(
        "cat11",
        label = "Categorical Variable",
        choices=cat_vars[-1])),
      card(
        card_header("Bar Chart"),
        plotOutput(outputId = "barchart") , 
        plotOutput(outputId = "twobar")
    ),
    card(
      card_header("Numerical Variable to Summarize"),
      plotOutput(outputId = "scatterplot"),
      plotOutput(outputId = "density")
    )
      )
  
 
  )
  
  
)

# Define server logic ----
server <- function(input, output,session) {
  
  output$cat1levels <- renderUI({
    req(input$cat1)  
    selectInput("cat1levels", "Please Choose a Filter for First Categorical Variable", 
                choices = unique(data[[input$cat1]]), 
                selected = unique(data[[input$cat1]]), 
                multiple = FALSE)
  })
  
  output$cat2levels <- renderUI({
    req(input$cat2)  
    selectInput("cat2levels", "Please Choose a Filter for Second Categorical Variable", 
                choices = unique(data[[input$cat2]]), 
                selected = unique(data[[input$cat2]]), 
                multiple = FALSE)
  })
  
  output$num1range <- renderUI({
    req(input$num1)  
    rangeval <- range(data2[[input$num1]], na.rm = TRUE)
    sliderInput("num1range", 
                label = paste("Select range for", input$numvar1),
                min = rangeval[1], max = rangeval[2], 
                value = rangeval)
  })
  
  output$num2range <- renderUI({
    req(input$num2)  
    rangeval <- range(data2[[input$num2]], na.rm = TRUE)
    sliderInput("num2range", 
                label = paste("Select range for", input$numvar2),
                min = rangeval[1], max = rangeval[2], 
                value = rangeval)
  })
  
  filteredData <- reactive({
    data2 %>%
      select(input$cat1,input$cat2,input$num1,input$num2) %>%
      filter(
        !!sym(input$cat1) %in% input$cat1levels,   
        !!sym(input$cat2) %in% input$cat2levels,   

      )
  })
  
  output$mobiledata<-renderDataTable({
    if(input$show_data){
      DT::datatable(data=filteredData()
                    ,
                    options=list(pageLenth=15),
                    rownames=FALSE)
    }
  }
  
  )
  
  filteredData2 <- reactive({
    data2 %>%
      select(Gender,input$cat11) %>%
      drop_na(Gender,input$cat11)
     
  })

 
  
  output$barchart<-renderPlot({
    ggplot(data=data2, aes(x=Gender, fill=Operating.System))+
      geom_bar()+
      labs(x="Gender")+
      scale_fill_discrete()+
      coord_flip()
  })
  
  output$scatterplot<-renderPlot({
    ggplot(data=data2,aes(x=Screen.On.Time..hours.day.,y=Data.Usage..MB.day.,color=User.Behavior.Class))+
      geom_point(shape=17,size=2)
  })
  
  output$twobar<-renderPlot({
  ggcharts_set_theme("theme_nightblue")
  bar_chart(data=data2,x=Device.Model,facet=Gender)
  })
  
  output$density<-renderPlot({
    ggplot(data=data2|>drop_na(Age,Gender),aes(x=Age))+
      geom_density(alpha=0.05,color="black",aes(fill=Gender))+
      scale_fill_manual(values=c("Female"="purple","Male"="green"))
    
  })
  
  
  
}

# Run the app ----
shinyApp(ui= ui, server= server)


