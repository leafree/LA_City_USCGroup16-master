library(ggmap)
library(ggplot2)
library(dplyr)
library(shiny)
library(DT)

shinyServer(function(input,output,session){
  shelters_fun = read.csv("shelters_fun.csv")
  shelters_fun2= shelters_fun
  la_map = get_map("LosAngeles", zoom = 9 , scale = 2)
  
  shelters_fun3 = reactive({
    if(is.null(input$show_funs)){
      shelters_fun2
    }
    else{
      shelters_fun3 = shelters_fun2 %>% 
        select(1:7,input$show_funs)
      shelters_fun3[shelters_fun3 == 0] =NA
      na.omit(shelters_fun3)
    }
  })
  output$plot1 = renderPlot(
    {
      
      ggmap(la_map) +
        geom_point(data = shelters_fun3(),
                   aes(x=shelters.LONGITUDE,
                       y=shelters.LATITUDE,
                       color = shelters.CITY),
                   size =3)+
        xlab("")+
        ylab("")+
        ggtitle("Shelters Distribution")+
        theme(legend.position = "bottom")
    }
  )
  
  output$table1 = renderDataTable(
    {
      
      DT::datatable(shelters_fun3()[,c(2,5,6,7,8)])
    }
  ) 
  
  }) 
