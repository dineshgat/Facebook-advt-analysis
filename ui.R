library(shiny)
library(shinydashboard)
library(DT)

convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}

shinyUI
(
  dashboardPage
  (
    dashboardHeader(title = "Facebook Ad Analysis"
                    
                    
    ),
    dashboardSidebar
    (
      sidebarMenu
      (
        menuItem("Dashboard Home",tabName = "dashboard",icon=icon("dashboard")),
        br(),
        menuItem("Data",tabName = "data",icon = icon("book")),
        br(),
        #menuItem("Plot",tabName = "plot",icon=icon("bar-chart"))
        convertMenuItem(menuItem("Predict",tabName = "predict",icon = icon("bar-chart-o"),selected=T,
                            
                                 menuSubItem("Plot1",tabName = "predict1"),
                                 menuSubItem("Plot2",tabName = "plot2"),
                                 menuSubItem("Plot3",tabName = "plot3") ),"Plots"),
        convertMenuItem(menuItem("Plots",tabName = "Plots",icon = icon("bar-chart-o"),selected=T,
                                 
                                 menuSubItem("Plot1",tabName = "plot1"),
                                 menuSubItem("Plot2",tabName = "plot2"),
                                 menuSubItem("Plot3",tabName = "plot3") ),"Plots")
        
      )
      
    ),
    
    
    dashboardBody
    (   
      tabItems
      (
        tabItem
        (
          tabName = "dashboard",
          fluidRow
          (
            
            
          )       
          
        ),
        tabItem
        (
          tabName = "data",
          h1("Row Data of Facebook Ads"),   
          fluidRow(
           
            box(
              title = "Data",
              DT::dataTableOutput("mytable"),
              width = 500
              
            )
          )
        ),
        tabItem
        (
          tabName = "plot",
          h1("Plots Of Facebook Ads")   
          
        ),
        tabItem
        (
          tabName = "predict1",
          h1("Predict Data"),
          fluidRow(
            textInput("in1","Lable", value = "", width = 100,
                      placeholder = "Enter Data : "),
            sliderInput("obs", "Number of observations", 0, 1000, 500),
            
            actionButton("go", "Go!"),
            textOutput("distPlot")
            
           )
          
        ),
        tabItem
        (
          tabName = "hdfcamc",
          h1("HDFCAMC"), 
          tabBox
          (
            title="January",
            
            tabPanel(title="Open Price",status = "primary",solidHeader = T,plotOutput("barplot")),
            tabPanel(title="Close Price",status = "primary",solidHeader = T,plotOutput("barplot1")),
            tabPanel(title="High Price",status = "primary",solidHeader = T,plotOutput("barplot2")),
            tabPanel(title="Low Price",status = "primary",solidHeader = T,plotOutput("barplot3")),
            tabPanel(title="Last Price",status = "primary",solidHeader = T,plotOutput("barplot4"))
          ),
          tabBox
          (
            title="February",
            tabPanel(title="Open Price",status = "primary",solidHeader = T,plotOutput("barplot5")),
            tabPanel(title="Close Price",status = "primary",solidHeader = T,plotOutput("barplot6")),
            tabPanel(title="High Price",status = "primary",solidHeader = T,plotOutput("barplot7")),
            tabPanel(title="Low Price",status = "primary",solidHeader = T,plotOutput("barplot8")),
            tabPanel(title="Last Price",status = "primary",solidHeader = T,plotOutput("barplot9"))
          )
          
          
        ),
        tabItem
        (
          tabName = "yesbank",
          h1("YESBANK"), 
          tabBox
          (
            title="January", 
            tabPanel(title="Open Price",status = "primary",solidHeader = T,plotOutput("barplot15")),
            tabPanel(title="Close Price",status = "primary",solidHeader = T,plotOutput("barplot16")),
            tabPanel(title="High Price",status = "primary",solidHeader = T,plotOutput("barplot17")),
            tabPanel(title="Low Price",status = "primary",solidHeader = T,plotOutput("barplot18")),
            tabPanel(title="Last Price",status = "primary",solidHeader = T,plotOutput("barplot19"))
          ),
          tabBox
          (
            title="February",
            tabPanel(title="Open Price",status = "primary",solidHeader = T,plotOutput("barplot20")),
            tabPanel(title="Close Price",status = "primary",solidHeader = T,plotOutput("barplot21")),
            tabPanel(title="High Price",status = "primary",solidHeader = T,plotOutput("barplot22")),
            tabPanel(title="Low Price",status = "primary",solidHeader = T,plotOutput("barplot23")),
            tabPanel(title="Last Price",status = "primary",solidHeader = T,plotOutput("barplot24"))
          )
          
          
        ),
        tabItem
        (
          tabName = "bse",
          h1("BSE"),
          tabBox
          (
            tabPanel(title="Open Price",status = "primary",solidHeader = T,plotOutput("barplot29")),
            tabPanel(title="Close Price",status = "primary",solidHeader = T,plotOutput("barplot30")),
            tabPanel(title="High Price",status = "primary",solidHeader = T,plotOutput("barplot31")),
            tabPanel(title="Low Price",status = "primary",solidHeader = T,plotOutput("barplot32"))
          )
          
        )
        
        
        
      )
      
    )
  )
)




  
  
  