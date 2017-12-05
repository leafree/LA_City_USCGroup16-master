library(ggplot2)
library(dplyr)
library(ggmap)
library(stringr)
library(tidyr)
library(shiny)

data_311call = read.csv("xudong_311_prepare.csv")
data_311call_grading = read.csv("xudong_311_prepare_grading.csv")
la_map = get_map(location = "Los Angelos", zoom = 11)

# order to plots
data_311call$CREATEDMONTH = 
  ordered(data_311call$CREATEDMONTH, level = c("Jan", "Feb", "Mar", "Apr", "May", 
                                              "Jun", "Jul", "Aug", "Sep", "Oct"))
data_311call$SERVICEMONTH = 
  ordered(data_311call$SERVICEMONTH, level = c("Jan", "Feb", "Mar", "Apr", "May", 
                                              "Jun", "Jul", "Aug", "Sep", "Oct"))
data_311call$RESPONSE_LEVEL = 
  ordered(data_311call$RESPONSE_LEVEL, level = c("1-4 days", "5-7 days", 
                                                "8-14 days", "14 days above"))
data_311call$SERVICE_WDAY = 
  ordered(data_311call$SERVICE_WDAY, level = c("Sun", "Mon", "Tues", "Wed", "Thurs",
                                              "Fri", "Sat"))
data_311call$SPA = as.factor(data_311call$SPA)

# order the plots
data_311call_grading$SERVICEMONTH = ordered(data_311call_grading$SERVICEMONTH,
                                    level = c("Jan", "Feb", "Mar", "Apr", "May", 
                                              "Jun", "Jul", "Aug", "Sep", "Oct"))
data_311call_grading$SPA = as.factor(data_311call_grading$SPA)

############ui
############ui
ui <- fluidPage(tabsetPanel(
  
    tabPanel("311Call Service Positivity(by month)", 
#        titlePanel("LA City Homeless Service Evaluation Tools   ", 
#                   windowTitle = "311 Call Response Positivity"),
        
        sidebarPanel(
            helpText("Tools to evaluate the response level over Month and SPA"),  
          
            radioButtons(inputId = "res_level",
                           label = "Choose a response level to display",
                           choices = list ("1-4 days", "5-7 days", "8-14 days", "14 days above"), 
                           selected = "1-4 days"),
            verbatimTextOutput(outputId = "res_level_note"),
            h5("Below is the bar chart to describe the response level distribution over months"),  
            
            plotOutput(outputId = "map1_bar_side")),
        
        mainPanel(
          plotOutput(outputId = "map1"),
          plotOutput(outputId = "map1_bar"))
),
      tabPanel("311Call_Service_Positivity(by weekday)", 
#         titlePanel("LA City Homeless Service Evaluation Tools   ", 
#                    windowTitle = "311 Call Response Positivity"),
         sidebarPanel(
           helpText("Tools to evaluate the response level over Weekdays and SPAs"),  
           radioButtons(inputId = "res_level2",
                        label = "Choose a response level to display",
                        choices = list ("1-4 days", "5-7 days", "8-14 days", "14 days above"), 
                        selected = "1-4 days"),
#           verbatimTextOutput(outputId = "res_level_note"),
           h5("Below is the bar chart to describe the response level distribution over months"),  
           plotOutput(outputId = "map2_bar_side")),
      
         mainPanel(
           # textOutput(outputId = "selected_var")
           plotOutput(outputId = "map2"),
           plotOutput(outputId = "map2_bar")
         )),

      tabPanel("311Call Service Capability(by month)",
           sidebarPanel(
             helpText("Tools to evaluate the problem solving ability over Month and SPA"),
             radioButtons(inputId = "status",
                          label = "Choose a call status to display",
                          choices = list ("Closed", "Pending", "Cancelled"), 
                          selected = "Closed")),
           mainPanel(
             plotOutput(outputId = "map0"),
             plotOutput(outputId = "map0_bar")
           )),

      tabPanel("Self-designed matrics",
            sidebarPanel(
              helpText("Select the wight of each variable and get a bar chart of each SPA's monthly service grades"),
              h4("Wight By Request Status"),
              sliderInput(inputId = "Closed", label = "#Closed requests", min = 0, max = 1, value = .9),
              sliderInput(inputId = "Pending", label = "#Pending requests", min = 0, max = 1, value = .1),
              h4("Weight By Response Level"),
              sliderInput(inputId = "Fast", label = "#Fast response", min = 0, max = 1, value = .5),
              sliderInput(inputId = "Medium", label = "#Medium requests", min = 0, max = 1,value = .3),
              sliderInput(inputId = "Slow", label = "#Slow requests", min = 0, max = 1, value = .1),
              sliderInput(inputId = "VerySlow", label = "#Very Slow requests", min = 0, max = 1, value = .1)
            ),
            mainPanel(
              plotOutput(outputId = "gradingBar"
            ))
)
))


###########server
server <- function(input, output) {
  
  output$map0 <- renderPlot({
    
    data_311call_selected = reactive({
      data_311call_selected = data_311call %>%
        filter(STATUS %in% input$status)})
    
    ggmap(la_map) +
      stat_density2d(data = data_311call_selected(), 
                     aes(x = LONGITUDE, y = LATITUDE,
                         fill = CREATEDMONTH,
                         alpha = ..level..),
                     geom = "polygon") +
      facet_wrap(~CREATEDMONTH)+
      theme(legend.position = "none")+
      labs(x = "", y = "", 
           title = "Requests Created By Month")+
      theme_void()
    
  })
  
    output$map0_bar <- renderPlot({
      
      data_311call_selected = reactive({
        data_311call_selected = data_311call %>%
          filter(STATUS %in% input$status)})
      
      ggplot(data_311call_selected(), aes(x = CREATEDMONTH, fill = SPA)) +
        geom_bar(position = "dodge")
    })
    
  # ggmap
  output$map1 <- renderPlot({
    
    # modularized reactions: caches its value to reduce computation
    # can be called by other code *_in renderPlot_*, with data()
    data_311call_selected = reactive({
      data_311call_selected = data_311call %>%
        filter(RESPONSE_LEVEL %in% input$res_level)})
  
    ggmap(la_map) +
      stat_density2d(data = data_311call_selected(), 
                     aes(x = LONGITUDE, y = LATITUDE,
                         fill = RESPONSE_LEVEL,
                         alpha = ..level..),
                     geom = "polygon") +
      theme_void()+
      labs(x = "", y = "", 
           title = "Service delivered by month") +
      theme(legend.position = "none") +
      facet_wrap(~SERVICEMONTH)
    })
  # bar chart over quarter and SPA
  output$map1_bar <- renderPlot({
      
      data_311call_selected = reactive({
        data_311call_selected = data_311call %>%
          filter(RESPONSE_LEVEL %in% input$res_level)        
      })
      
      ggplot(data = data_311call_selected(), #[data_311call_selected()$SPA %in% c(4,5,6),], 
             aes(x = SPA, fill = SERVICEQUARTER)) +
        geom_bar(position = "dodge")+
        theme_bw()+
        labs(x = "Month", y = "Service Counts", 
             title = "Service delivered by weekday")+
        theme(legend.position = c(.9,.85))
    })
    
    # bar chart over month and res_level (isolate)
    output$map1_bar_side <- renderPlot({
      
      ggplot(data = data_311call, #[data_311call_selected()$SPA %in% c(4,5,6),], 
             aes(x = SERVICEMONTH, fill = RESPONSE_LEVEL)) +
        geom_bar(position = "dodge")+
        theme_bw()+
        labs(x = "Month", y = "Service Counts", 
             title = "Response Level by Month")+
        theme(legend.position="bottom", legend.title = element_blank())
    })
    
    output$map2 <- renderPlot({
      
        data_311call_selected2 = reactive({
          data_311call_selected2 = data_311call %>%
            filter(RESPONSE_LEVEL %in% input$res_level2)        
        })
      
        ggmap(la_map) +
          stat_density2d(data = data_311call_selected2(), 
                         aes(x = LONGITUDE, y = LATITUDE,
                             fill = RESPONSE_LEVEL,
                             alpha = ..level..),
                         geom = "polygon") +
          theme_void()+
          labs(x = "", y = "", 
               title = "Service delivered by weekday") +
          theme(legend.position = "none")  +
        facet_wrap(~SERVICE_WDAY)
    })
    
    output$map2_bar <- renderPlot({
      
      data_311call_selected2 = reactive({
        data_311call_selected2 = data_311call %>%
          filter(RESPONSE_LEVEL %in% input$res_level2)        
      })
      
      ggplot(data = data_311call_selected2(), #[data_311call_selected()$SPA %in% c(4,5,6),], 
             aes(x = SPA, fill = SERVICE_WDAY)) +
        geom_bar(position = "dodge")+
        theme_bw()+
        labs(x = "Weekday", y = "Service Counts", 
             title = "Service delivered by weekday")+
        theme(legend.position = c(.9,.75))
    })
    
    # bar chart over month and res_level (isolate)
    output$map2_bar_side <- renderPlot({
      
      ggplot(data = data_311call, #[data_311call_selected()$SPA %in% c(4,5,6),], 
             aes(x = SERVICE_WDAY, fill = RESPONSE_LEVEL)) +
        geom_bar(position = "dodge")+
        theme_bw()+
        labs(x = "Week Day", y = "Service Counts", 
             title = "Response Level by Weekday")+
        theme(legend.position="bottom", legend.title = element_blank())
    })
    
    output$res_level_note <- renderText({
      paste("Note:", "Fast (1-4 days)", "Medium (5-7 days)",
            "Slow (8-14 days)", "Very Slow (14 days above)",sep="\n")
    })
    
    output$gradingBar <- renderPlot({
      
      Closed <- data_311call_grading$STATUS == "Closed"
      
      data_311call_grading[Closed,]$grading = 
        data_311call_grading[Closed,]$Fast * input$Fast * input$Closed + 
        data_311call_grading[Closed,]$Medium * input$Medium * input$Closed  +
        data_311call_grading[Closed,]$Slow * input$Slow * input$Closed  +
        data_311call_grading[Closed,]$VerySlow * input$VerySlow * input$Closed
      
      data_311call_grading[!Closed,]$grading = 
        data_311call_grading[!Closed,]$Fast * input$Fast * input$Pending + 
        data_311call_grading[!Closed,]$Medium * input$Medium * input$Pending  +
        data_311call_grading[!Closed,]$Slow * input$Slow * input$Pending  +
        data_311call_grading[!Closed,]$VerySlow * input$VerySlow * input$Pending
        
      data_311call_grading = data_311call_grading %>%
        group_by(SPA, SERVICEMONTH,totpeople)%>%
        summarise(tot_grading_index = round(sum(grading)/sum(totpeople)*100,3))
      
      ggplot(data_311call_grading, aes(x = SERVICEMONTH, y = tot_grading_index, 
                                       fill = SPA)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_label(aes(label = tot_grading_index, fill = SPA)) +
        labs(title = "311 Call Service Quality Grading Tool",
             subtitle = "Index = Closed%(Fast% + Medium% + Slow% + VerySlow%)\n            
             Penging%(Fast% + Medium% + Slow% + VerySlow%)",
             y = "Grading Indext") +
        theme(plot.subtitle = element_text(size = 12,colour="darkgrey",hjust = 1)) 
    })
    
}

# run
shinyApp(ui = ui, server =server)