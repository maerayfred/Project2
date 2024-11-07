

source("helpers.R")

ui <- fluidPage(
  titlePanel("Mobile Device Data Exploration"),
  #Adding an input to choose from categorical variables
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
      #Adding in order to choose the levels from the categorical variables
                  uiOutput("cat1levels"),
                  uiOutput("cat2levels"),
      #Adding in order to choose  from  Numeric  variables            
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
      #Adding in order to choose the ranges from the numeric variables             
                  uiOutput("num1range"),
                  uiOutput("num2range"),
                  
        #Adding button to allow the data to display once subset          
                  actionButton("show_data","Show Data Table",value=FALSE),
       #Allowing the user to download the data in the server section           
      card(
        HTML("Select filetype and variables, then hit 'Download data'."),
        downloadButton("download_data", "Download data")
      )
              
                  
                  
                  
                  
  ),
  
  mainPanel(
    card(
      h2("About Tab"),
      "Welcome to the Mobile Device Data Explorer app! This tool allows you to explore simulated mobile device data and analyze user behavior patterns. The dataset contains 700 complete observations across 11 variables, including age, gender, data usage, and operating system, among others. For more detailed information about the dataset and its variables, feel free to visit the following link:
    

  https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset/data
      
 On the left panel, you’ll find options to select both categorical and numerical variables from the dataset. You can choose to filter the data based on specific factors, levels, or numeric ranges. Once you’ve narrowed down the data to your preferred subset, you can easily download it as a CSV file for further analysis.

In addition, the app provides opportunities to explore the data both visually and numerically. Simply toggle between the available categorical and numerical variables in the sidebar to adjust your analysis and gain insights from the data. Happy Exploring!"
    ),

   #Adding a card to display the data 
    card(
      card_header("Data Download"),
      dataTableOutput(outputId = "mobiledata")
    ),
    
    #Grouping All the Graphs together
      card(
        h2("Visual Summaries"),
        plotOutput(outputId = "barchart") , 
        plotOutput(outputId = "twobar"),
        plotOutput(outputId = "scatterplot"),
        plotOutput(outputId = "density"),
        plotOutput(outputId = "box"),
        plotOutput(outputId = "mountain"),
    ),
    
    card(
      h2("Numeric Summaries"),
      textOutput(outputId = "one")
      )
    
 
      )
  
 
  )
  
  
)

# Define server logic ----
server <- function(input, output,session) {
  #Allowing the user to subset the data for the different levels of categorical variable 1
  output$cat1levels <- renderUI({
    req(input$cat1)  
    selectInput("cat1levels", "Please Choose a Filter for First Categorical Variable", 
                choices = unique(data[[input$cat1]]), 
                selected = unique(data[[input$cat1]]), 
                multiple = FALSE)
  })
  #Allowing the user to subset the data for the different levels of categorical variable 2
  output$cat2levels <- renderUI({
    req(input$cat2)  
    selectInput("cat2levels", "Please Choose a Filter for Second Categorical Variable", 
                choices = unique(data[[input$cat2]]), 
                selected = unique(data[[input$cat2]]), 
                multiple = FALSE)
  })
  
  #Allowing the user to subset the data for the range of numeric variable 1
  output$num1range <- renderUI({
    req(input$num1)  
    rangeval <- range(data2[[input$num1]], na.rm = TRUE)
    sliderInput("num1range", 
                label = paste("Select range for", input$numvar1),
                min = rangeval[1], max = rangeval[2], 
                value = rangeval)
  })
  #Allowing the user to subset the data for the range of numeric variable 2
  output$num2range <- renderUI({
    req(input$num2)  
    rangeval <- range(data2[[input$num2]], na.rm = TRUE)
    sliderInput("num2range", 
                label = paste("Select range for", input$numvar2),
                min = rangeval[1], max = rangeval[2], 
                value = rangeval)
  })
  #Filtering the data that will be displayed and downloaded based on subsets above
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
 #Rendering the subsetted data 
  output$mobiledata<-renderDataTable({
    if(input$show_data){
      DT::datatable(data=filteredData()
                    ,
                    options=list(pageLenth=15),
                    rownames=FALSE)
    }
  }
  
  )
  #Downloading the subsetted data
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

  
  #Bar Chart
  
  output$barchart<-renderPlot({
   
    ggplot(data=data2, aes_string(x=input$cat1, fill=input$cat2))+
      geom_bar()+
      labs( title = paste("Relationship Between",input$cat1,"and",input$cat),
        x="Gender")+
      scale_fill_discrete()+
      coord_flip()
  })
  
  #Scatterplot
  output$scatterplot<-renderPlot({
   
    
    ggplot(data=data2,aes_string(x=input$num1,y=input$num2,color=input$cat1))+
      labs(title = paste("Relationship Between",input$num1,"and",input$num2,"Grouped by",input$cat1)
        
      )+
      geom_point(shape=17,size=2)
  })
  
  #
  output$twobar<-renderPlot({
 
  ggcharts_set_theme("theme_nightblue")
  bar_chart(data=data2,x=Gender,facet=Device.Model)
  })
  
  #Density Plot
  output$density <- renderPlot({

    
    ggplot(data2, aes_string(x = input$num1, fill = input$cat1)) +
      geom_density(alpha = 0.5) +
      labs(x = input$num1, 
           title = paste("Density Plot of", input$num1, "by", input$cat1),
           fill = input$cat1) +
      theme_minimal()
  })

  #Box Plot
  output$box<-renderPlot({
    g <- ggplot(data2  )
    g + geom_boxplot(aes_string(x = input$cat1, y = input$num1, fill = input$cat1))+
      labs(title=paste("Relationship Between",input$cat1,"and",input$num1)
        
      )
    
  })
  
  #Typogrpahy Graphy
  output$mountain<-renderPlot({
    ggplot(data=data2, aes(x=Number.of.Apps.Installed, y=App.Usage.Time..min.day., fill=Gender)) + 
      geom_area() +
      scale_fill_ipsum() +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_comma() +
      labs(title="Relationship Between Number of Apps and App usage by Gender",
           x="Number of Apps Installed",
           y="App Usage (min/day)"
           
      ) +
      
      theme(legend.position="bottom")
    
  })
  
 
  
  
}

# Run the app ----
shinyApp(ui= ui, server= server)


