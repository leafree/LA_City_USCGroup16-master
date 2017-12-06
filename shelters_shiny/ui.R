library(shiny)
shelters_fun = read.csv("shelters_fun.csv")
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Shelter distribution"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(      
    sidebarPanel("Function",
                 checkboxGroupInput("show_funs", "Function to show",
                                    names(shelters_fun)[9:27], 
                                    selected = NULL)),    
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("Shelters' Information", DT::dataTableOutput("table1")),
        tabPanel("plot", plotOutput(outputId = "plot1"))
      )
    )   
    ) 
))
