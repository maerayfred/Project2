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
g <- ggplot(data2 |> drop_na(Gender, Age) )
g + geom_boxplot(aes(x = Gender, y = Age, fill = Gender))


#Plot5

ggcharts_set_theme("theme_nightblue")
bar_chart(data=data2,x=Device.Model,facet=Gender)


#Plot6
ggplot(data=data2, aes(x=Number.of.Apps.Installed, y=App.Usage.Time..min.day., fill=Gender)) + 
  geom_area() +
  scale_fill_ipsum() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_comma() +
  labs(title="Title",
       
       ) +

  theme(legend.position="bottom")


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
                    choices=cat_vars[-1]
                    
                    
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
                  
                  
                  actionButton("show_data","Show Data Table",value=FALSE),
                  
      card(
        HTML("Select filetype and variables, then hit 'Download data'."),
        downloadButton("download_data", "Download data")
      )
              
                  
                  
                  
                  
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
      card_header("Please Choose the Categorical Variables You would Like to Summarize on The Left Panel.")),
     
 
    
      card(
        card_header("Visual Summaries"),
        plotOutput(outputId = "barchart") , 
        plotOutput(outputId = "twobar"),
        plotOutput(outputId = "scatterplot"),
        plotOutput(outputId = "density"),
        plotOutput(outputId = "box"),
        plotOutput(outputId = "mountain"),
    ),
    
    card(
      card_header("Numeric Variables to Summarize"),
      selectInput(
        "num11",
        label = "Numeric Variable",
        choices=numeric_vars))
    
 
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
        !!sym(input$num1) >= input$num1range[1],    
        !!sym(input$num1) <= input$num1range[2],    
        !!sym(input$num2) >= input$num2range[1],    
        !!sym(input$num2) <= input$num2range[2]
      
    
        
        
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
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("mobiledata.csv")
    },
    content = function(file) {
    {
        write_csv(filteredData(), file)
      }
      
    }
  )

  
  filtered2<-reactive({
    second<-input$cat11
    data2%>%
     select(Gender,second)
    
  
    
  })
  
  
  output$barchart<-renderPlot({
   
    ggplot(data=data2, aes_string(x=cat_vars[1], fill=input$cat1))+
      geom_bar()+
      labs(x="Gender")+
      scale_fill_discrete()+
      coord_flip()
  })
  
  output$scatterplot<-renderPlot({
   
    
    ggplot(data=data2,aes_string(x=input$num1,y=input$num2,color=input$cat1))+
      geom_point(shape=17,size=2)
  })
  
  output$twobar<-renderPlot({
 
  ggcharts_set_theme("theme_nightblue")
  bar_chart(data=data2,x=Gender,facet=Operating.System)
  })
  
  output$density <- renderPlot({

    
    ggplot(data2, aes_string(x = input$num1, fill = input$cat1)) +
      geom_density(alpha = 0.5) +
      labs(x = input$num1, 
           title = paste("Density Plot of", input$num1, "by", input$cat1),
           fill = input$cat1) +
      theme_minimal()
  })

  
  output$box<-renderPlot({
    g <- ggplot(data2  )
    g + geom_boxplot(aes_string(x = input$cat1, y = input$num1, fill = input$cat1))
    
  })
  
  output$mountain<-renderPlot({
    ggplot(data=data2, aes(x=Number.of.Apps.Installed, y=App.Usage.Time..min.day., fill=Gender)) + 
      geom_area() +
      scale_fill_ipsum() +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_comma() +
      labs(title="Title",
           
      ) +
      
      theme(legend.position="bottom")
    
  })
  
  
}

# Run the app ----
shinyApp(ui= ui, server= server)


