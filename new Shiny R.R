library(ggplot2)
library(dplyr)
library(ggmap)
library(stringr)
library(tidyr)
library(shiny)

crime=read.csv("crime.csv")
la_map = get_map(location = "Los Angeles", zoom = 10)
crime$VICTIM.DESCENT = factor(ifelse(crime$VICTIM.DESCENT == "B", "Black",
                                     ifelse(crime$VICTIM.DESCENT == "H", "Hispanic", 
                                            ifelse(crime$VICTIM.DESCENT=="W","White",
                                                   ifelse(crime$VICTIM.DESCENT=="A","Asian","Others")))))

crime$VICTIM.DESCENT=ordered(crime$VICTIM.DESCENT, levels=c("Asian","White","Black","Hispanic","Others"))



ui <- fluidPage(
  tabsetPanel(
#    titlePanel("hhhh"),
    tabPanel("Crime Type",
             
             sidebarPanel(
               helpText("Crime Types in Los Angeles"),
               checkboxGroupInput(inputId = "type",
                                  label = "Choose a crime type to display",
                                  choices = list ("AGGRAVATED ASSAULT", "SIMPLE ASSAULT", "ROBBERY","THEFT","RAPE","OTHERS"), 
                                  selected = "AGGRAVATED ASSAULT")),
             mainPanel(
               verticalLayout(
                  h2("Crime Type in LA", align = "center"),
                  plotOutput(outputId = "crime_type"),
              splitLayout(
                  plotOutput(outputId = "type_month"),
                  plotOutput(outputId = "type_hour")),
              splitLayout(
                  plotOutput(outputId = "age_type"),
                  plotOutput(outputId = "gender_type"),
                  plotOutput(outputId = "ethnicity_type"))
               ))
             ),
  
  tabPanel("Crime Premise",
           
           sidebarPanel(
             helpText("Crime Premise in Los Angeles"),
             checkboxGroupInput(inputId = "premise",
                                label = "Choose a crime premise to display",
                                choices = list ("STREET", "SIDEWALK", "PARKING LOT","PARK/PLAYGROUND","DWELLING","OTHERS"), 
                                selected = "STREET")),
           mainPanel(
             verticalLayout(
               h2("Crime Premise in LA", align = "center"),
               plotOutput(outputId = "crime_premise"),
               splitLayout(
                 plotOutput(outputId = "premise_month"),
                 plotOutput(outputId = "premise_hour")),
               splitLayout(
                 plotOutput(outputId = "age_premise"),
                 plotOutput(outputId = "gender_premise"),
                 plotOutput(outputId = "ethnicity_premise"))
             ))
  ),
  
  tabPanel("Crime Weapon",
           
           sidebarPanel(
             helpText("Weapon used in Crime"),
             checkboxGroupInput(inputId = "weapon",
                                label = "Choose a kind of weapon to display",
                                choices = list ("STRONG-ARM", "KNIFE", "STICK","GUN","PIPE","OTHERS"), 
                                selected = "STRONG-ARM")),
           mainPanel(
             verticalLayout(
               h2("Weapon used in Crime", align = "center"),
               plotOutput(outputId = "crime_weapon"),
               splitLayout(
                 plotOutput(outputId = "weapon_month"),
                 plotOutput(outputId = "weapon_hour")),
               splitLayout(
                 plotOutput(outputId = "age_weapon"),
                 plotOutput(outputId = "gender_weapon"),
                 plotOutput(outputId = "ethnicity_weapon"))
             ))
          
  )
  
  )
)

server=function(input,output) {
  
  output$crime_type = renderPlot({
    
    crime_type_shiny = reactive({
      crime %>%
        filter(crime_type %in% input$type)})
    
    ggmap(la_map) +
      geom_point(data=crime_type_shiny(),
                 aes(x=LONGITUDE,
                     y=LATITUDE,color=crime_type))+
      theme(legend.position = "none")+
      theme_void()
    
  })
  output$type_month = renderPlot ({
    crime_type_shiny = reactive({
      crime_type_shiny = crime %>%
        filter(crime_type %in% input$type)})
    
    ggplot(crime_type_shiny(),aes(as.factor(newdate)))+
      geom_histogram(stat="count",fill="#009E73")+
      geom_line(stat="count",group=1,adjust=5,color="#D55E00")+
      ggtitle("Month Distribution of Crime Occurred")+
      xlab("Month")+
      theme_bw()
    
  })
  
  output$type_hour = renderPlot ({
    crime_type_shiny = reactive({
      crime_type_shiny = crime %>%
        filter(crime_type %in% input$type)})
    
    ggplot(crime_type_shiny(),aes(as.factor(H)))+
      geom_histogram(stat="count",fill="#009E73")+
      geom_line(stat="count",group=1,adjust=5,color="#D55E00")+
      ggtitle("Hour Distribution of Crime Occurred")+
      xlab("Hour")+
      theme_bw()
    
  })
  
  output$age_type = renderPlot ({
    crime_type_shiny = reactive({
      crime_type_shiny = crime %>%
        filter(crime_type %in% input$type)})
    
    ggplot(crime_type_shiny(),aes(x=VICTIM.AGE))+
      geom_histogram(aes(y=..density..), fill="#009E73",binwidth=3)+
      geom_density(aes(y=..density..),color="#D55E00")+
      ggtitle("Age Distribution of the Victims")+
      xlab("Age of Victim")+
      ylab("")+
      theme_bw()
    
  })
  
  output$gender_type = renderPlot ({
    crime_type_shiny = reactive({
      crime_type_shiny = crime %>%
        filter(crime_type %in% input$type)})
    
    ggplot(crime_type_shiny(),aes(x=VICTIM.SEX,fill=VICTIM.SEX))+
      geom_histogram(stat="count")+
      xlab("Gender")+
      ggtitle("Gender of the Victims")+
      theme_bw()+
      theme(legend.position="none")
    
  })
  
  output$ethnicity_type = renderPlot ({
    crime_type_shiny = reactive({
      crime_type_shiny = crime %>%
        filter(crime_type %in% input$type)})
    
    
    ggplot(crime_type_shiny(),aes(x=VICTIM.DESCENT))+
      geom_histogram(stat="count",fill="#009E73")+
      xlab("Ethnicity of the Victims")+
      ggtitle("Ethnicity Distribution of the Victims")+
      theme_bw()
    
  })
  
  output$crime_premise = renderPlot ({
    crime_premise_shiny = reactive({
      crime_premise_shiny = crime %>%
        filter(crime_premise %in% input$premise)})
    
    ggmap(la_map) +
      geom_point(data=crime_premise_shiny(),
                 aes(x=LONGITUDE,
                     y=LATITUDE,color=crime_premise))+
      theme(legend.position = "none")
    
  })
  
  output$premise_month = renderPlot ({
    crime_premise_shiny = reactive({
      crime_premise_shiny = crime %>%
        filter(crime_premise %in% input$premise)})
    
    ggplot(crime_premise_shiny(),aes(as.factor(newdate)))+
      geom_histogram(stat="count",fill="#009E73")+
      geom_line(stat="count",group=1,adjust=5,color="#D55E00")+
      ggtitle("Month Distribution of Crime Occurred")+
      xlab("Month")+
      theme_bw()
    
  })
  
  output$premise_hour = renderPlot ({
    crime_premise_shiny = reactive({
      crime_premise_shiny = crime %>%
        filter(crime_premise %in% input$premise)})
    
    ggplot(crime_premise_shiny(),aes(as.factor(H)))+
      geom_histogram(stat="count",fill="#009E73")+
      geom_line(stat="count",group=1,adjust=5,color="#D55E00")+
      ggtitle("Hour Distribution of Crime Occurred")+
      xlab("Hour")+
      theme_bw()
    
  })
  
  output$age_premise = renderPlot ({
    crime_premise_shiny = reactive({
      crime_premise_shiny = crime %>%
        filter(crime_premise %in% input$premise)})
    
    ggplot(crime_premise_shiny(),aes(x=VICTIM.AGE))+
      geom_histogram(aes(y=..density..), fill="#009E73",binwidth=3)+
      geom_density(aes(y=..density..),color="#D55E00")+
      ggtitle("Age Distribution of the Victims")+
      xlab("Age of Victim")+
      ylab("")+
      theme_bw()
    
  })
  
  output$gender_premise = renderPlot ({
    crime_premise_shiny = reactive({
      crime_premise_shiny = crime %>%
        filter(crime_premise %in% input$premise)})
    
    ggplot(crime_premise_shiny(),aes(x=VICTIM.SEX,fill=VICTIM.SEX))+
      geom_histogram(stat="count")+
      xlab("Gender")+
      ggtitle("Gender of the Victims")+
      theme_bw()+
      theme(legend.position="none")
      
    
  })
  
  output$ethnicity_premise = renderPlot ({
    crime_premise_shiny = reactive({
      crime_premise_shiny = crime %>%
        filter(crime_premise %in% input$premise)})
    
    
    ggplot(crime_premise_shiny(),aes(x=VICTIM.DESCENT))+
      geom_histogram(stat="count",fill="#009E73")+
      xlab("Ethnicity of the Victims")+
      ggtitle("Ethnicity Distribution of the Victims")+
      theme(legend.position="none")+
      theme_bw()
    
  })
  
  output$crime_weapon = renderPlot ({
    crime_weapon_shiny = reactive({
      crime_weapon_shiny = crime %>%
        filter(crime_weapon %in% input$weapon)})
    
    ggmap(la_map) +
      geom_point(data=crime_weapon_shiny(),
                 aes(x=LONGITUDE,
                     y=LATITUDE,color=crime_weapon))+
      theme(legend.position = "none")
    
  })
  
  output$weapon_month = renderPlot ({
    crime_weapon_shiny = reactive({
      crime_weapon_shiny = crime %>%
        filter(crime_weapon %in% input$weapon)})
    
    ggplot(crime_weapon_shiny(),aes(as.factor(newdate)))+
      geom_histogram(stat="count",fill="#009E73")+
      geom_line(stat="count",group=1,adjust=5,color="#D55E00")+
      ggtitle("Month Distribution of Crime Occurred")+
      xlab("Month")+
      theme_bw()
    
  })
  
  output$weapon_hour = renderPlot ({
    crime_weapon_shiny = reactive({
      crime_weapon_shiny = crime %>%
        filter(crime_weapon %in% input$weapon)})
    
    ggplot(crime_weapon_shiny(),aes(as.factor(H)))+
      geom_histogram(stat="count",fill="#009E73")+
      geom_line(stat="count",group=1,adjust=5,color="#D55E00")+
      ggtitle("Hour Distribution of Crime Occurred")+
      xlab("Hour")+
      theme_bw()
    
  })
  
  output$age_weapon = renderPlot ({
    crime_weapon_shiny = reactive({
      crime_weapon_shiny = crime %>%
        filter(crime_weapon %in% input$weapon)})
    
    ggplot(crime_weapon_shiny(),aes(x=VICTIM.AGE))+
      geom_histogram(aes(y=..density..), fill="#009E73",binwidth=3)+
      geom_density(aes(y=..density..),color="#D55E00")+
      ggtitle("Age Distribution of the Victims")+
      xlab("Age of Victim")+
      ylab("")+
      theme_bw()
    
  })
  
  output$gender_weapon = renderPlot ({
    crime_weapon_shiny = reactive({
      crime_weapon_shiny = crime %>%
        filter(crime_weapon %in% input$weapon)})
    
    ggplot(crime_weapon_shiny(),aes(x=VICTIM.SEX,fill=VICTIM.SEX))+
      geom_histogram(stat="count")+
      xlab("Gender")+
      ggtitle("Gender of the Victims")+
      theme_bw()+
      theme(legend.position="none")
    
  })
  
  output$ethnicity_weapon = renderPlot ({
    crime_weapon_shiny = reactive({
      crime_weapon_shiny = crime %>%
        filter(crime_weapon %in% input$weapon)})
    
    
    ggplot(crime_weapon_shiny(),aes(x=VICTIM.DESCENT))+
      geom_histogram(stat="count",fill="#009E73")+
      xlab("Ethnicity of the Victims")+
      ggtitle("Ethnicity Distribution of the Victims")+
      theme_bw()
    
  })
  
}


shinyApp(server = server, ui=ui)
