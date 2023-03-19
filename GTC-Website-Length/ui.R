library(shiny)
library(shinydisconnect)
library(shinyjs)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(style="border-right:1px solid #dddddd;border-bottom:1px solid #999999;",
 
        useShinyjs(),
              
        disconnectMessage(
            text = "Current session has timed out. Please refresh the page.",
            refresh = "",
            background = "#646464E6",
            colour = "#FFFFFF",
            refreshColour = "#337AB7",
            overlayColour = "#999999",
            overlayOpacity = 0.6,
            width = "full",
            top = "center",
            size = 28,
            css = ""
        ),
        
        fluidRow(style="font-size:16px;margin:0;",
                 div(class="col-lg-3 col-md-3 col-sm-3", selectInput("GA_week", "GA (week)", choices = c("", c(22:30)), width = "100%")),
                 div(class="col-lg-3 col-md-3 col-sm-3", selectInput("GA_day", "GA (day)", choices = c("", c(0:6)), width = "100%")),
                 div(class="col-lg-3 col-md-3 col-sm-3", selectInput("sex", "Sex", choices = c("", "Female", "Male"), width = "100%")),
                 div(class="col-lg-2 col-md-2 col-sm-2", actionButton("plot", "READY-SET-GO", width = "100%", style="margin-top:27px;font-size:16px;background-color:#546E7A;color:#FFFFFF;border:0;"))
        ),
        fluidRow(style="font-size:12px;margin:0;",
                 div(class="col-lg-5 col-md-5 col-sm-5",
                     uiOutput("table")
                 ),
                 div(class="col-lg-7 col-md-7 col-sm-7",
                     plotOutput("length_plot", height = "400px")
                 )
        ),
        br()
        
        
    )
    
)
