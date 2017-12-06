library(ggplot2)
library(dplyr)
library(ggmap)
library(stringr)
library(tidyr)
library(shiny)
library(DT)


data_311call = read.csv("xudong_311_prepare.csv")
data_311call_grading = read.csv("xudong_311_prepare_grading.csv")
la_map = get_map(location = "Los Angeles", zoom = 10)

# order the plots
data_311call$CREATEDMONTH = ordered(data_311call$CREATEDMONTH, 
                                    level = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))
data_311call$SERVICEMONTH = ordered(data_311call$SERVICEMONTH, 
                                    level = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))
data_311call$RESPONSE_LEVEL = ordered(data_311call$RESPONSE_LEVEL, 
                                      level = c("1-4 days", "5-7 days", "8-14 days", "14 days above"))
data_311call$SERVICE_WDAY = ordered(data_311call$SERVICE_WDAY, 
                                    level = c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"))
data_311call$SPA = as.factor(data_311call$SPA)
# order the plots
data_311call_grading$SERVICEMONTH = ordered(data_311call_grading$SERVICEMONTH,
                                    level = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))
data_311call_grading$SPA = as.factor(data_311call_grading$SPA)
Closed <- data_311call_grading$STATUS == "Closed"

############ui
############ui
ui <- fluidPage(tabsetPanel(
    tabPanel("311Call Positivity(month)", 
        titlePanel("LA City Homeless Service Evaluation Tools",  # title panel
                   windowTitle = "311 Call Response Positivity"),
        sidebarPanel(
            helpText("Tools to evaluate the response level over Month Quarter and SPA"),  
          
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
      tabPanel("311Call Positivity(weekday)", 
            sidebarPanel(
            helpText("Tools to evaluate the response level over Weekdays and SPA"),  
            radioButtons(inputId = "res_level2",
                        label = "Choose a response level to display",
                        choices = list ("1-4 days", "5-7 days", "8-14 days", "14 days above"), 
                        selected = "1-4 days"),
            h5("Below is the bar chart to describe the response level distribution over weekday"),  
            plotOutput(outputId = "map2_bar_side")),
            mainPanel(
            # textOutput(outputId = "selected_var")
            plotOutput(outputId = "map2"),
            plotOutput(outputId = "map2_bar"))
            ),
      tabPanel("311Call Capability(month)",
            sidebarPanel(
            helpText("Tools to evaluate the problem solving ability over Month and SPA"),
            radioButtons(inputId = "status",
                        label = "Choose a call status to display",
                        choices = list ("Closed", "Pending", "Cancelled"),  
                        selected = "Closed"),
            h5("Below is the bar chart to describe different requests' status distribution over weekday"),  
            plotOutput(outputId = "map3_bar_side")),
            mainPanel(
            plotOutput(outputId = "map3"),
            plotOutput(outputId = "map3_bar"))
            ),
      tabPanel("311Call Grading(SPA)",
            sidebarPanel(
              helpText("Select the wight of each variable and get a bar chart of each SPA's monthly service grades"),
              h5("--Weight By Request Status"),
              sliderInput(inputId = "Closed", label = "#Closed requests", min = 0, max = 1, value = .9),
              sliderInput(inputId = "Pending", label = "#Pending requests", min = 0, max = 1, value = .1),
              h5("--Weight By Response Level"),
              sliderInput(inputId = "Fast", label = "#Fast response", min = 0, max = 1, value = .5),
              sliderInput(inputId = "Medium", label = "#Medium requests", min = 0, max = 1,value = .3),
              sliderInput(inputId = "Slow", label = "#Slow requests", min = 0, max = 1, value = .1),
              sliderInput(inputId = "VerySlow", label = "#Very Slow requests", min = 0, max = 1, value = .1)
            ),
            mainPanel(
              plotOutput(outputId = "gradingBar"), 
              dataTableOutput(outputId = "gradingTable"))
            )
))


###########server
###########server
###########server
server <- function(input, output) {
    #1 reactive res_level  #1 reactive res_level
    data_311call_selected_res_level = reactive({
        data_311call %>% filter(RESPONSE_LEVEL %in% input$res_level)})
    #1 map
    output$map1 <- renderPlot({
        ggmap(la_map) +
            stat_density2d(data = data_311call_selected_res_level(), 
                         aes(x = LONGITUDE, y = LATITUDE, fill = RESPONSE_LEVEL,
                             alpha = ..level..),
                         geom = "polygon") +
            theme_void()+
            labs(x = "", y = "", 
                 title = paste("Response Level (",input$res_level, ") Distribution", collapse = ""))+
            theme(legend.position = "none") +
            theme(plot.title = element_text(size = 18, hjust = .5))+
            facet_wrap(~SERVICEMONTH)
        })
    #1 bar chart over quarter and SPA
    output$map1_bar <- renderPlot({
        ggplot(data = data_311call_selected_res_level(), 
             aes(x = SERVICEMONTH, fill = SPA)) +
        geom_bar(position = "dodge")+
        geom_label(stat = "count", aes(label = ..count.., y = ..count..))+
        theme_bw()+
        theme(plot.title = element_text(size = 18, hjust = .5))+
        labs(x = "Month", y = "Service Counts", 
             title = paste("Response Level  (",input$res_level, ")  Counts by Month & SPA", collapse = ""))+
        theme(legend.position = "bottom")
    })
    #1 side bar chart over month and res_level (isolate)
    output$map1_bar_side <- renderPlot({
      ggplot(data = data_311call, #[data_311call_selected()$SPA %in% c(4,5,6),], 
             aes(x = SERVICEMONTH, fill = RESPONSE_LEVEL)) +
        geom_bar(position = "dodge")+
#        geom_label(stat = "count", aes(label = ..count.., y = ..count..))+
        theme_bw()+
        labs(x = "Month", y = "Counts", 
             title = paste("Response Level Counts by month", collapse = ""))+
        theme(legend.position="bottom", legend.title = element_blank())
    })
    #1 note
    output$res_level_note <- renderText({
        paste("Notes of response level:", "Fast (1-4 days)", "Medium (5-7 days)",
              "Slow (8-14 days)", "Very Slow (14 days above)",sep="\n")
    })
    
    #2 reactive res_level
    data_311call_selected2_res_level = reactive({
        data_311call %>% filter(RESPONSE_LEVEL %in% input$res_level2)        
    })
    #2 map
    output$map2 <- renderPlot({
        ggmap(la_map) +
          stat_density2d(data = data_311call_selected2_res_level(), 
                         aes(x = LONGITUDE, y = LATITUDE,
                             fill = RESPONSE_LEVEL,
                             alpha = ..level..),
                         geom = "polygon") +
          theme_void()+
          theme(plot.title = element_text(size = 18, hjust = .5))+    
          labs(x = "", y = "", 
               title = paste("Response Level Counts by Weekday", collapse = ""))+
          theme(legend.position = "bottom")  +
        facet_wrap(~SERVICE_WDAY)
    })
    #2 map_bar
    output$map2_bar <- renderPlot({
        ggplot(data = data_311call_selected2_res_level(), 
             aes(x = SERVICE_WDAY, fill = SPA)) +
        geom_bar(position = "dodge")+
        geom_label(stat = "count", aes(label = ..count.., y = ..count..))+
        theme_bw()+
        theme(plot.title = element_text(size = 18, hjust = .5))+
        labs(x = "Weekday", y = "Service Counts", 
             title = paste("Response Level  (",input$res_level2, ")  Counts by Weekday & SPA", collapse = ""))+
        theme(legend.position = "bottom")
    })
    #2 bar_side: bar chart over month and res_level (isolate)
    output$map2_bar_side <- renderPlot({
      ggplot(data = data_311call, 
             aes(x = SERVICE_WDAY, fill = RESPONSE_LEVEL)) +
        geom_bar(position = "dodge")+
#        geom_label(stat = "count", aes(label = ..count.., y = ..count..))+
        theme_bw()+
        labs(x = "Week Day", y = "Service Counts", 
             title = paste("Response Level  (",input$res_level2, ")  Counts\n by weekday", collapse = ""))+
        theme(legend.position="bottom", legend.title = element_blank())
    })
    
    ##3 reactive_status  ##3 reactive_status
    data_311call_selected_status = reactive({
        data_311call %>% filter(STATUS %in% input$status)})
    #3 map
    output$map3 <- renderPlot({
        ggmap(la_map) +
            stat_density2d(data = data_311call_selected_status(), 
                           aes(x = LONGITUDE, y = LATITUDE,fill = CREATEDMONTH,
                               alpha = ..level..), geom = "polygon") +
            facet_wrap(~CREATEDMONTH)+
            theme(legend.position = "none")+
            theme(plot.title = element_text(size = 18, hjust = .5))+
            labs(x = "", y = "", 
                 title = paste(input$status," Requests Distribution", collapse = ""))+
            theme_void()
    })
    #3 map_bar
    output$map3_bar <- renderPlot({
        ggplot(data_311call_selected_status(), aes(x = CREATEDMONTH, fill = SPA)) +
            geom_bar(position = "dodge")+
            geom_label(stat = "count", aes(label = ..count.., y = ..count..))+
            theme_bw()+
            theme(plot.title = element_text(size = 18, hjust = .5),
                  legend.position = "bottom")+
            labs(y = "counts",
                 title = paste(input$status," Requests Counts by Month & SPA", collapse = ""))
    })
    #3 map_bar_side
    output$map3_bar_side <- renderPlot({
        ggplot(data_311call, aes(x = CREATEDMONTH, fill = STATUS)) +
            geom_bar(position = "dodge")+
            theme_bw()+
            theme(legend.position = "bottom")+
            labs(y = "counts",
                 title = paste(input$status," Requests Counts by Month", collapse = ""))
    })
    
    
    output$map2_bar_side <- renderPlot({
        ggplot(data = data_311call, 
               aes(x = SERVICE_WDAY, fill = RESPONSE_LEVEL)) +
            geom_bar(position = "dodge")+
            #        geom_label(stat = "count", aes(label = ..count.., y = ..count..))+
            theme_bw()+
            labs(x = "Week Day", y = "Service Counts", 
                 title = paste("Response Level  (",input$res_level2, ")  Counts\n by weekday", collapse = ""))+
            theme(legend.position="bottom", legend.title = element_blank())
    })
    
    
    #4 reactive_grading #4 reactive_grading
    data_311call_grading2 = reactive({
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
        
        data_311call_grading %>%
            group_by(SPA, SERVICEMONTH,totpeople)%>%
            summarise(tot_grading_index = round(sum(grading)/sum(totpeople)*100,3))
    })
    #4 grading bar
    output$gradingBar <- renderPlot({
      ggplot(data_311call_grading2(), aes(x = SERVICEMONTH, y = tot_grading_index, 
                                       fill = SPA)) +
        geom_bar(stat = "identity", position = "dodge") +
        #geom_label(stat = "count", aes(label = ..count.., y = ..count..))
        geom_label(aes(label = tot_grading_index, fill = SPA)) +
        theme_bw()+
        theme(plot.title = element_text(size = 18, hjust = .5),
              legend.position = "bottom")+
        labs(title = "311 Call Service Quality Grading Tool",
             subtitle = "Index = [Closed%(Fast% + Medium% + Slow% + VerySlow%)\n            
                   Penging%(Fast% + Medium% + Slow% + VerySlow%)]/Total homelss",
             y = "Grading Index") +
        theme(plot.subtitle = element_text(size = 12,colour="steelblue",hjust = .9)) 
    })
    #4 table
    output$gradingTable <- renderDataTable({
      DT::datatable(data_311call_grading2()%>%
                        select(-totpeople) %>%
                        arrange(desc(tot_grading_index)))
    })
}

# run
shinyApp(ui = ui, server =server)