library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinydisconnect)
library(shinyWidgets)
library(plotly)
library(DT)

shinyUI(
    dashboardPage(
        dashboardHeader(disable = TRUE),
        dashboardSidebar(disable = TRUE),
        dashboardBody(style="font-family:'Nunito';",
                      useShinyjs(),
                      tags$head(
                        tags$link(rel='stylesheet', type = 'text/css', href='style.css')
                      ),
                      tags$script('src="https://kit.fontawesome.com/a175aacf3d.js"'),
                      setSliderColor(rep("#546E7A",3), c(1,2,3)),
                      fluidRow(
                          div(class="col-lg-6 col-md-6 col-sm-6",
                              div(style="margin:15px;", class="greetings", uiOutput("greeting"))),
                          div(class="col-lg-6 col-md-6 col-sm-6",
                              div(align="right", style="margin:15px;",
                                  actionButton("restore_box", "RESTORE ALL PANELS", class = "gtc-button", icon = tags$i(class="fa-solid fa-diagram-predecessor")),
                                  actionButton("settings_button", "OPEN SETTINGS", icon = tags$i(class="fa-solid fa-gear"), class = "gtc-button")))
                          ),
                      uiOutput("settings"),
                      box(id = "input_box",
                          title = span("INPUT", class = "box-header-title"),
                          collapsible = TRUE,
                          closable = TRUE,
                          width = 12,
                          fluidRow(
                              div(class="col-lg-3 col-md-3 col-sm-3", style="height:440px;",
                                  tabsetPanel(id="main", type = "hidden",
                                              tabPanel("login",
                                                       div(style="font-size:11px;color:red;word-break:normal;", "Forgot password? Send us a message, and we will send you a link to reset the password."),
                                                       br(),
                                                       textInput("id", "USER ID"),
                                                       passwordInput("password", "PASSWORD"),
                                                       hr(),
                                                       div(align="right",
                                                           actionButton("signin", icon = tags$i(class="fa-solid fa-right-to-bracket"), "SIGN IN", class = "gtc-button")
                                                       ),
                                                       br(),
                                                       uiOutput("signin_msg")
                                              ),
                                              tabPanel("selector",
                                                       div(id = "id_select_div", style="padding:5px 5px 0 5px;border-radius:5px;background-color:#546E7A;color:#FFFFFF;", 
                                                           uiOutput("id_selector")
                                                       ),
                                                       div(id = "GA_sex_select_div", style="padding:5px 5px 0 5px;border-radius:5px;background-color:#7895a2;color:#FFFFFF;", 
                                                           selectInput("GA_week", "GA (week)", choices = c("", c(22:30)), width = "100%"),
                                                           selectInput("GA_day", "GA (day)", choices = c("", c(0:6)), width = "100%"),
                                                           selectInput("sex", "Sex", choices = c("", "Female", "Male"), width = "100%")
                                                       ),
                                                       hr(),
                                                       div(actionButton("plot", icon = tags$i(class="fa-solid fa-jet-fighter-up"), span("READY-SET-GO", tags$i(class="fa-solid fa-jet-fighter-up")), width = "100%", class = "gtc-button")),
                                                       br(),
                                                       uiOutput("msg")))
                              ),
                              div(class="col-lg-9 col-md-9 col-sm-9", style="height:440px;",
                                  fluidRow(
                                      div(DTOutput("meas_table"), style = "height:400px;padding-right:12px;font-size:11px;"),
                                      div(column(12, align="right", style="padding:8px 15px 0 0;",
                                                 actionButton("delete_record", icon = tags$i(class="fa-solid fa-trash-can"), "DELETE RECORD", class = "gtc-button"),
                                                 actionButton("save_data", icon = tags$i(class="fa-solid fa-server"), "SAVE DATA", class = "gtc-button"),
                                                 actionButton("calculate", icon = tags$i(class="fa-solid fa-chart-line"), "UPDATE TRAJECTORY", class = "gtc-button")
                                                 
                                      ))))
                          )
                      ),
                      
                      box(id = "weight_box",
                          title = span("WEIGHT", class = "box-header-title"),
                          collapsible = TRUE,
                          collapsed = TRUE,
                          closable = TRUE,
                          width = 12,
                          fluidRow(class="meas-box",
                                   div(class="col-lg-8 col-md-8 col-sm-8",
                                       plotlyOutput("weight_plot", height = "480px")
                                   ),
                                   div(class="col-lg-4 col-md-4 col-sm-4",
                                       div(class="meas-info-box",
                                           div(style="padding:20px;font-size:16px;",
                                               div(sliderInput("weight_range", HTML("Set PMA Range for Trajectory&nbsp;Percentile Calculation"), min = 22, max = 45, value = c(22,45))),
                                               div(HTML("<hr class='line'>")),
                                               div("Trajectory Percentile:", style="font-weight:600;"),
                                               uiOutput("weight_trajectory_percentile"),
                                               br(),
                                               div("Transition to WHO Charts at: ", style="font-weight:600;"),
                                               uiOutput("weight_WHO_transition"))
                                       )
                                   )
                          )
                          
                          
                      ),
                      
                      box(id = "length_box",
                          title = span("LENGTH", class = "box-header-title"),
                          collapsible = TRUE,
                          collapsed = TRUE,
                          closable = TRUE,
                          width = 12,
                          fluidRow(class="meas-box",
                                   div(class="col-lg-8 col-md-8 col-sm-8",
                                       plotlyOutput("length_plot", height = "480px")
                                   ),
                                   div(class="col-lg-4 col-md-4 col-sm-4",
                                       div(class="meas-info-box",
                                           div(style="padding:20px;font-size:16px;",
                                               div(sliderInput("length_range", HTML("Set PMA Range for Trajectory&nbsp;Percentile Calculation"), min = 22, max = 45, value = c(22,45))),
                                               div(HTML("<hr class='line'>")),
                                               div("Trajectory Percentile:", style="font-weight:600;"),
                                               uiOutput("length_trajectory_percentile"),
                                               br(),
                                               div("Transition to WHO Charts at: ", style="font-weight:600;"),
                                               uiOutput("length_WHO_transition"))
                                       )
                                   )
                          )
                          
                      ),
                      
                      box(id = "HC_box",
                          title = span("HEAD CIRCUMFERENCE", class = "box-header-title"),
                          collapsible = TRUE,
                          collapsed = TRUE,
                          closable = TRUE,
                          width = 12,
                          fluidRow(class="meas-box",
                                   div(class="col-lg-8 col-md-8 col-sm-8",
                                       plotlyOutput("HC_plot", height = "480px")
                                   ),
                                   div(class="col-lg-4 col-md-4 col-sm-4",
                                       div(class="meas-info-box",
                                           div(style="padding:20px;font-size:16px;",
                                               div(sliderInput("HC_range", HTML("Set PMA Range for Trajectory&nbsp;Percentile Calculation"), min = 22, max = 45, value = c(22,45))),
                                               div(HTML("<hr class='line'>")),
                                               div("Trajectory Percentile:", style="font-weight:600;"),
                                               uiOutput("HC_trajectory_percentile"),
                                               br(),
                                               div("Transition to WHO Charts at: ", style="font-weight:600;"),
                                               uiOutput("HC_WHO_transition"))
                                       )
                                   )
                          )
                          
                      ),
                      
                      box(id = "output_box",
                          title = span("OUTPUT", class = "box-header-title"),
                          collapsible = TRUE,
                          collapsed = TRUE,
                          closable = TRUE,
                          width = 12,
                          fluidRow(class="meas-box",
                                   accordion(id="output_accordion",
                                             accordionItem(title = span("Plot", class = "box-header-title"),
                                                           collapsed = FALSE,
                                                           fluidRow(
                                                               div(class="col-lg-4 col-md-4 col-sm-4",
                                                                   radioButtons("plot_output_size", "Paper Size",
                                                                                choices = c("Letter", "Legal", "A4", "B5"),
                                                                                selected = character(0)),
                                                                   radioButtons("plot_output_orientation", "Orientation",
                                                                                choices = c("Landscape", "Portrait"),
                                                                                selected = character(0))),
                                                                div(class="col-lg-8 col-md-8 col-sm-8",
                                                                    div("Layout", style="font-size:14px;font-weight:700;margin-bottom:5px;"),
                                                                    actionGroupButtons(c("plot_layout_1", "plot_layout_2", "plot_layout_3"),
                                                                                       labels = list(
                                                                                           tags$img(src="plate-layout.png", style="height:75px;"),
                                                                                           tags$img(src="vertical-layout.png", style="height:75px;"),
                                                                                           tags$img(src="horizontal-layout.png", style="height:75px;")
                                                                                       )),
                                                                    hr(),
                                                                    div(align="left", 
                                                                        uiOutput("download_plot_msg"),
                                                                        downloadButton("download_plot", "DOWNLOAD PLOT")))
                                                           )
                                                           ),
                                             accordionItem(title = span("Table", class = "box-header-title"),
                                                           collapsed = FALSE,
                                                           fluidRow(
                                                               div(class="col-lg-4 col-md-4 col-sm-4",
                                                                   radioButtons("table_output_format", "File Format",
                                                                                 choices = c("CSV", "MS Excel", "MS Word"),
                                                                                selected = character(0))),
                                                               div(class="col-lg-8 col-md-8 col-sm-8",
                                                                   selectInput("table_output_columns", "Select columns to download (arranged by selection order)",
                                                                               multiple = TRUE,
                                                                               choices = c("DOL", "PMA", "Week", "Day", "Weight", "Weight Z (%ile)", "Length", "Length Z (%ile)", "HC", "HC Z (%ile)"),
                                                                               selected = c("DOL", "PMA", "Week", "Day", "Weight", "Weight Z (%ile)", "Length", "Length Z (%ile)", "HC", "HC Z (%ile)")),
                                                                   div(align="left", 
                                                                       uiOutput("download_table_msg"),
                                                                       downloadButton("download_table", "DOWNLOAD TABLE")),
                                                                   br())
                                                           ))))
                          
                      )
                      
        )
        
    ))
