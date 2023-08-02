library(shiny)
library(duckdb)
library(DBI)
library(tidyverse)
library(DT)
library(shinyjs)
library(safer)
library(plotly)
library(patchwork)

# con <- dbConnect(duckdb::duckdb(), dbdir="~/Apps/GTC-Website-Apps/GTC-DB/db.duckdb", read_only=FALSE)
# dbWriteTable(con, "demographics_fullApp", demographic, overwrite=T)
# dbWriteTable(con, "meas_table_fullApp", meas_table, overwrite=T)
# dbDisconnect(con, shutdown = T)

shinyServer(function(input, output, session) {
    
    color_palette <- 
        tribble( 
            ~color1,   ~color2,   ~color3,   ~color4,   ~color5,   ~color6,   ~color7,   ~color8,   ~color9,
            "#F1C232", "#445760", "#546E7A", "#7895A2", "#AAAAAA", "#DDDDDD", "#F0F0F0", "#FFFFFF", "#CC0000",
            "#ff1b6b", "#61a2e2", "#3e196e", "#99a5d3", "#b4a6cb", "#d0a8c3", "#eca9bb", "#FFFFFF", "#6f7bf7",
            "#000814", "#ba2a4e", "#a5345f", "#903f6f", "#7a497f", "#e8b7e2", "#91a5c3", "#FFFFFF", "#f55a9b",
            "#b8475d", "#623a73", "#805174" ,"#9f6976" ,"#bd8078", "#dc9779", "#faae7b", "#FFFFFF", "#133a94",
            "#080808", "#e65763", "#b01041", "#9c3d45", "#773036", "#d4ac94", "#cb997e", "#FFFFFF", "#57276f",
            "#0091ad", "#b7094c", "#a01a58", "#723c70", "#5c4d7d", "#eef0f2", "#f4f5f6", "#FFFFFF", "#455e89",
            "#cf203e", "#f4f269", "#75bd6f", "#c1dd6b", "#a8d26d", "#1e7662", "#ffeda0", "#FFFFFF", "#cf203e", 
        ) %>% t(.) %>% as.data.frame() %>% rownames_to_column(var = "color_order")
    
    color_df <- data.frame(Theme = character(0))
    
    for (i in c(1:(ncol(color_palette)-1))) {
        color_df[i,"Theme"] <- paste0("<span style='font-size:14px;'>Option ",i,"&nbsp;&nbsp;&nbsp", paste0("<span><i class='fa-solid fa-square' style=color:", color_palette[[paste0("V",i)]], ";></i></span>", collapse = ""),"</span>")
    }
    
    rv <- reactiveValues(Chou_weight = data.frame(), 
                         dt = data.frame(),
                         proxy = data.frame(),
                         offset = NA,
                         percentile_min = NA,
                         saved_infant_id = NA,
                         hasDisplay = 0,
                         setting_html = "",
                         plot_width_x = as.numeric(NA),
                         plot_heigth = as.numeric(NA),
                         layout = "plate",
                         output_table_format = as.character(NA),
                         font = ""
    )
    
    plots <- reactiveValues(
        weight_plot = ggplot(),
        length_plot = ggplot(),
        HC_plot     = ggplot(),
        layout      = ggplot()
    )
    
    colors <- reactiveValues(
        color1 = color_palette[1, "V1"],
        color2 = color_palette[2, "V1"],
        color3 = color_palette[3, "V1"],
        color4 = color_palette[4, "V1"],
        color5 = color_palette[5, "V1"],
        color6 = color_palette[6, "V1"],
        color7 = color_palette[7, "V1"],
        color8 = color_palette[8, "V1"],
        color9 = color_palette[9, "V1"],
    )
    
    updateBox("output_box", "remove")
    
    restore <- function() {
        rv$dt <-
            data.frame(
                DOL = numeric(0),
                PMA = numeric(0),
                Weight = numeric(0),
                Weight_Z = numeric(0), 
                Length = numeric(0), 
                Length_Z = numeric(0), 
                HC = numeric(0), 
                HC_Z = numeric(0)
            ) %>%
            mutate(Week = PMA %/% 7, Day = PMA %% 7)   
        rv$proxy <- rv$dt
        #output$meas_table <- renderUI({})
        output$weight_plot <- renderPlotly({})
        output$length_plot <- renderPlotly({})
        output$HC_plot     <- renderPlotly({})
        updateBox("weight_box", action = "toggle")
        updateBox("length_box", action = "toggle")
        updateBox("HC_box", action = "toggle")
        updateSelectInput(session, "GA_week", selected = "")
        updateSelectInput(session, "GA_day", selected = "")
        updateSelectInput(session, "sex", selected = "")
        rv$hasDisplay = 0 
        
        
        output$weight_trajectory_percentile <- renderUI({})
        output$weight_WHO_transition <- renderUI({})
        output$length_trajectory_percentile <- renderUI({})
        output$length_WHO_transition <- renderUI({})
        output$HC_trajectory_percentile <- renderUI({})
        output$HC_WHO_transition <- renderUI({})
    }
    
    refresh_saved_infant <- function() {
        con <- dbConnect(duckdb::duckdb(), dbdir="~/Apps/GTC-Website-Apps/GTC-DB/db.duckdb", read_only=TRUE)
        saved_infant_id <- dbGetQuery(con, paste0("SELECT ID FROM \"demographics_fullApp\" WHERE USERID = '", input$id, "'"))
        saved_infant_id <- saved_infant_id$ID
        dbDisconnect(con, shutdown = TRUE)
        return(saved_infant_id)
    }
    
    change_color_palette <- function(selected_color_theme) {
        colors$color1 <- color_palette[1, paste0("V",selected_color_theme)]
        colors$color2 <- color_palette[2, paste0("V",selected_color_theme)]
        colors$color3 <- color_palette[3, paste0("V",selected_color_theme)]
        colors$color4 <- color_palette[4, paste0("V",selected_color_theme)]
        colors$color5 <- color_palette[5, paste0("V",selected_color_theme)]
        colors$color6 <- color_palette[6, paste0("V",selected_color_theme)]
        colors$color7 <- color_palette[7, paste0("V",selected_color_theme)]
        colors$color8 <- color_palette[8, paste0("V",selected_color_theme)]
        colors$color9 <- color_palette[9, paste0("V",selected_color_theme)]
    }
    
    change_color_theme <- function() {
        runjs(paste0("$('.skin-blue .main-sidebar').css('background-color','", colors$color3, "')"))
        runjs(paste0("$('.content-wrapper').css('background-color','", colors$color7, "')"))
        runjs(paste0("$('.greetings').css('color','", colors$color1, "')"))
        runjs(paste0("$('#id_select_div').css('background-color','", colors$color3, "')"))
        runjs(paste0("$('#GA_sex_select_div').css('background-color','", colors$color4, "')"))
        runjs(paste0("$('.line').css('background-color','", colors$color3, "')"))
        runjs(paste0("$('.meas-box').css('color','", colors$color3, "')"))
        runjs(paste0("$('.msg-box').css('color','", colors$color7, "')"))
        runjs(paste0("$('.box-header-title').css('color','", colors$color3, "')"))
        runjs(paste0("$('.meas-info-box').css('background-color','", colors$color7, "').css('box-shadow','3px 3px 1px ", colors$color5, "')"))
        runjs(paste0("$('.gtc-button').css('color','", colors$color3, "').css('background-color','", colors$color6, "')"))
        runjs(paste0("$('.gtc-button:hover').css('color','", colors$color8, "').css('background-color','", colors$color3, "')"))
        runjs(paste0("$('.gtc-button-rev').css('color','", colors$color8, "').css('background-color','", colors$color3, "')"))
        runjs(paste0("$('.gtc-button-rev:hover').css('color','", colors$color6, "').css('background-color','", colors$color8, "')"))
        runjs(paste0("$('.js-irs-0 .irs-to, .js-irs-0 .irs-from, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar').css('background-color','", colors$color3, "').css('border-color','", colors$color3, "')"))
        runjs(paste0("$('.js-irs-1 .irs-to, .js-irs-1 .irs-from, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar').css('background-color','", colors$color3, "').css('border-color','", colors$color3, "')"))
        runjs(paste0("$('.js-irs-2 .irs-to, .js-irs-2 .irs-from, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar').css('background-color','", colors$color3, "').css('border-color','", colors$color3, "')"))

    }
    
    change_font <- function(font) {
      
      #font family change
      if (!is.null(font) && font != "") {
        runjs(paste0("$('.content-wrapper .content, .box-header-title').css('font-family','", font, "')"))
      }
    }
    
    observeEvent(input$restore_box, {
        updateBox("input_box", action = "restore")
        updateBox("weight_box", action = "restore")
        updateBox("length_box", action = "restore")
        updateBox("HC_box", action = "restore")
        if (rv$hasDisplay == 1) {
            updateBox("output_box", action = "restore")
        }
    })
    
    observeEvent(input$settings_button, {
        if (rv$setting_html[1] == "") {
            updateActionButton(session, "settings_button", label = "CLOSE SETTINGS", icon = tags$i(class="fa-solid fa-circle-xmark"))
            #hide("restore_box")
            rv$setting_html = 
                box(class = "settings-box", 
                    title = span("SETTINGS", class = "box-header-title", style=paste0("color:",colors$color3,";font-family:",rv$font,";")),
                    width = 12,
                    collapsible = FALSE,
                    closable = FALSE,
                    div(align="right", 
                        actionButton("save_settings", "APPLY SETTINGS", class = "gtc-button", style=paste0("color:",colors$color3,";background-color:",colors$color6,";border:0;"))),
                    accordion(id = "settings_accordion",
                              accordionItem(title = span(class = "box-header-title", "Color Theme", style=paste0("color:",colors$color3,";font-family:",rv$font,";")), collapsed = FALSE,
                                            fluidRow(
                                                column(12,
                                                       selectizeInput("color_theme_selection", "Pick a color theme",
                                                                      choices = NULL))
                                            )),
                              accordionItem(title = span(class = "box-header-title", "System Font", style=paste0("color:",colors$color3,";font-family:",rv$font,";")), collapsed = FALSE,
                                            fluidRow(
                                                column(12,
                                                       selectInput("font_selection", "Select a font", 
                                                                   choices = c("Choose One..." = "", "Nunito", "Lexend", "Times New Roman", "Arial", "Archivo Narrow"),
                                                                   selected = "")
                                                )
                                            ))
                    )

                    )

            #updateBox("input_box", "remove")
            #updateBox("weight_box", "remove")
            #updateBox("length_box", "remove")
            #updateBox("HC_box", "remove")
            #updateBox("output_box", "remove")
            
        } else if (class(rv$setting_html) == "shiny.tag") {
            rv$setting_html = ""
            updateActionButton(session, "settings_button", label = "OPEN SETTINGS", icon = tags$i(class="fa-solid fa-gear"))
            #show("restore_box")
            #updateBox("input_box", "restore")
            #updateBox("weight_box", "restore")
            #updateBox("length_box", "restore")
            #updateBox("HC_box", "restore")
            if (rv$hasDisplay == 1) {
                updateBox("output_box", "restore")
            }
        }
        
        id <- read_csv("~/Apps/GTC-Website-Apps/GTC-Website-Registration/www/user.csv")
        selected_color_theme <- id$color_theme[id$id %in% input$id]
        font                 <- id$font[id$id %in% input$id]
        
        output$settings <- renderUI({rv$setting_html})
        
        updateSelectInput(
          session, "font_selection",
          selected = font
        )
        
        updateSelectizeInput(
            session, "color_theme_selection",
            selected = color_df$Theme[selected_color_theme],
            choices = color_df,
            options = list(
                render = I(
                    '{
        item: function(item, escape) {
          return "<div>" + item.value + "</div>";
          },
        option: function(item, escape) {
          return "<div>" + item.value + "</div>";
          }
        }'), server = FALSE
            )
        )

        
    })
    
    observeEvent(input$signin, {
        output$signin_msg <- renderUI({})
                
         #display name
            name <- read_csv("~/Apps/GTC-Website-Apps/GTC-Website-Registration/www/registration.csv") %>% 
                    filter(username %in% input$id) %>% 
                    select(firstName, lastName)
                full.name <- paste(name$firstName, name$lastName)
                output$greeting <- renderUI({
                    div(paste0("Hi! ", full.name))
                })
                
                #load color theme
                selected_color_theme <- id$color_theme[id$id %in% input$id]
                rv$font <- id$font[id$id %in% input$id]
                change_color_palette(selected_color_theme)
                change_color_theme()
                change_font(rv$font)
                
                #load saved infant ID
                saved_infant_id <- refresh_saved_infant()
                output$id_selector <- renderUI({
                    selectInput("id_dropdown", "Select an Infant", choices = c(list("Select an ID..." = list(""), "Exising ID" = as.list(saved_infant_id), "---" = list("CREATE A NEW ID...", "---"))))
                })
                
                updateTabsetPanel(session, "main", "selector")

    })
    
    observeEvent(input$id_dropdown, {
        saved_infant_id <- refresh_saved_infant()
        
        if (input$id_dropdown == "CREATE A NEW ID...") {
            showModal(
                modalDialog(
                    title = "CREATE A NEW ID",
                    textInput("new_id", "Enter an ID"),
                    uiOutput("new_id_msg"),
                    footer = list(actionButton("create_id", "CREATE"), modalButton("CANCEL"))
                )
            )
        } else if (input$id_dropdown %in% saved_infant_id) {
            if (rv$hasDisplay == 1) {
                restore()
            }
            
            con <- dbConnect(duckdb::duckdb(), dbdir="~/Apps/GTC-Website-Apps/GTC-DB/db.duckdb", read_only=FALSE)
            temp_demographics <- dbGetQuery(con, paste0("SELECT * FROM \"demographics_fullApp\" WHERE USERID = '", input$id, "'AND ID = '", input$id_dropdown, "';"))
            dbDisconnect(con, shutdown = TRUE)
            updateSelectInput(session, "GA_week", selected = temp_demographics$GA_week[1])
            updateSelectInput(session, "GA_day", selected = temp_demographics$GA_day[1])
            updateSelectInput(session, "sex", selected = temp_demographics$Sex[1])
            disable("GA_week")
            disable("GA_day")
            disable("sex")
        } else if (input$id_dropdown != "") {
            if (rv$hasDisplay == 1) {
                restore()
            }
            enable("GA_week")
            enable("GA_day")
            enable("sex")
            updateSelectInput(session, "GA_week", selected = "")
            updateSelectInput(session, "GA_day", selected = "")
            updateSelectInput(session, "sex", selected = "")
        }
    })
    
    observeEvent(input$create_id, {
        saved_infant_id <- refresh_saved_infant()
        if (input$new_id %in% saved_infant_id) {
            output$new_id_msg <- renderUI({
                span(style="color:red;", "ID already existed!")
            })
        } else {
            output$new_id_msg <- renderUI({})
            updateSelectInput(session, "id_dropdown", 
                              choices = c(list("Exising ID" = as.list("Select an ID..." = list(""), c(saved_infant_id, input$new_id)), "---" = list("CREATE A NEW ID...", "---"))),
                              selected = input$new_id)
            removeModal()
        }

    })
    
    observeEvent(input$save_data, {
        saved_infant_id <- refresh_saved_infant()
        
        if (input$id_dropdown %in% saved_infant_id) {
            con <- dbConnect(duckdb::duckdb(), dbdir="~/Apps/GTC-Website-Apps/GTC-DB/db.duckdb", read_only=FALSE)
            dbExecute(con, paste0("DELETE FROM \"meas_table_fullApp\" WHERE USERID = '", input$id, "'AND ID = '", input$id_dropdown, "';"))
            dbAppendTable(con, "meas_table_fullApp", rv$proxy %>% 
                              mutate(USERID = input$id, ID = input$id_dropdown) %>% 
                              select(USERID, ID, DOL, PMA, Weight, Length, HC))
            dbDisconnect(con, shutdown = TRUE)
        
        } 
        showModal(
            modalDialog(
                title = "SAVE DATA",
                footer = modalButton("CLOSE"),
                h5(span(tags$i(class="fa-regular fa-circle-check"), "Data table has been successfully saved to the database."), style=paste0("color:",colors$color3,";font-size:16px;"))
            )
        )
        
    })
    
    observeEvent(input$delete_record, {
        showModal(
            modalDialog(
                title = "DELETE RECORD",
                footer = list(actionButton("confirm_delete", "CONFIRM"), modalButton("CANCEL")),
                h5("Are you sure you want to delete all records pertaining to ID: ", strong(input$id_dropdown), "?", style="color:red;font-size:16px;")
            )
        )
    })
    
    observeEvent(input$confirm_delete, {
        removeModal()
        con <- dbConnect(duckdb::duckdb(), dbdir="~/Apps/GTC-Website-Apps/GTC-DB/db.duckdb", read_only=FALSE)
        dbExecute(con, paste0("DELETE FROM \"demographics_fullApp\" WHERE USERID = '", input$id, "'AND ID = '", input$id_dropdown, "';"))
        dbExecute(con, paste0("DELETE FROM \"meas_table_fullApp\" WHERE USERID = '", input$id, "'AND ID = '", input$id_dropdown, "';"))
        dbDisconnect(con, shutdown = TRUE)
        
        saved_infant_id <- refresh_saved_infant()
        updateSelectInput(session, "id_dropdown", 
                          choices = c(list("Select an ID..." = list(""), "Exising ID" = as.list(saved_infant_id), "---" = list("CREATE A NEW ID...", "---"))),
                          selected = "")
        if (rv$hasDisplay == 1) {
            restore()
            
        }
        
    })
    
    observeEvent(input$plot, {
        GAdays <- as.numeric(input$GA_week) * 7 + as.numeric(input$GA_day) #calculate GA in days
        GAn <- round(GAdays/7)
        rv$offset <- GAdays - (GAn * 7)
        
        output$msg <- renderUI({})
        
        if (is.na(GAn)) {
            output$msg <- renderUI({
                div(class = "msg-box", "Please select/create an ID or enter GA!")
            })
            #output$meas_table <- renderUI({})
        } else if (GAn<=22 | GAn>=31) {
            output$msg <- renderUI({
                div(class = "msg-box", "GA Out of Range!")
            })
            #output$meas_table <- renderUI({})
        } else if (GAn>22 & GAn<31) {
            
            if (input$sex == "") {
                output$msg <- renderUI({
                    div(class = "msg-box", "Please enter infant sex!")
                })
                #output$meas_table <- renderUI({})
                
            } else if (input$sex %in% c("Male", "Female")) {
                rv$hasDisplay = 1

                ### retrieve data
                con <- dbConnect(duckdb::duckdb(), dbdir="~/Apps/GTC-Website-Apps/GTC-DB/db.duckdb", read_only=TRUE)
                rv$Chou_weight <- dbGetQuery(con, paste0("SELECT * FROM GA",GAn,"_",req(input$sex),"_Weight")) %>% mutate(percentile = as.numeric(NA))
                rv$Chou_length <- dbGetQuery(con, paste0("SELECT * FROM GA",GAn,"_",req(input$sex),"_Length")) %>% mutate(percentile = as.numeric(NA))
                rv$Chou_HC     <- dbGetQuery(con, paste0("SELECT * FROM GA",GAn,"_",req(input$sex),"_HeadCircumference")) %>% mutate(percentile = as.numeric(NA))
                rv$WHO_weight  <- dbGetQuery(con, paste0("SELECT * FROM WHO_Weight_",req(input$sex),"_Agemos0"))
                rv$WHO_length  <- dbGetQuery(con, paste0("SELECT * FROM WHO_Length_",req(input$sex),"_Agemos0"))
                rv$WHO_HC      <- dbGetQuery(con, paste0("SELECT * FROM WHO_HC_",req(input$sex),"_Agemos0"))
                dbDisconnect(con, shutdown = TRUE)
                
                saved_infant_id <- refresh_saved_infant()
                if (input$id_dropdown %in% saved_infant_id) {
                    con <- dbConnect(duckdb::duckdb(), dbdir="~/Apps/GTC-Website-Apps/GTC-DB/db.duckdb", read_only=TRUE)
                    rv$dt   <- dbGetQuery(con, paste0("SELECT * FROM \"meas_table_fullApp\" WHERE USERID = '", input$id, "'AND ID = '", input$id_dropdown, "';")) %>% 
                        select(-USERID, -ID) %>% 
                        mutate(DOL = as.numeric(DOL),
                               PMA = as.numeric(PMA),
                               Weight = as.numeric(Weight),
                               Weight_Z = NA,
                               Length = as.numeric(Length),
                               Length_Z = NA,
                               HC = as.numeric(HC),
                               HC_Z = NA)  %>%
                        mutate(Week = PMA %/% 7, Day = PMA %% 7) %>%
                        arrange(DOL)
                    dbDisconnect(con, shutdown = TRUE)
                    rv$proxy <- rv$dt
                    
                } else {
                    rv$dt <-
                        data.frame(
                            DOL = c(0:(44*7-GAdays+rv$offset)),
                            PMA = c((GAdays):(44*7+rv$offset)),
                            Weight = NA,
                            Weight_Z = NA, 
                            Length = NA, 
                            Length_Z = NA, 
                            HC = NA, 
                            HC_Z = NA
                        ) %>%
                        mutate(Week = PMA %/% 7, Day = PMA %% 7)
                    demographic_temp <- data.frame(
                        USERID = input$id,
                        ID = input$id_dropdown,
                        GA_week = input$GA_week,
                        GA_day = input$GA_day,
                        Sex = input$sex
                    )
                    rv$proxy <- rv$dt
                    con <- dbConnect(duckdb::duckdb(), dbdir="~/Apps/GTC-Website-Apps/GTC-DB/db.duckdb", read_only=FALSE)
                    dbAppendTable(con, "demographics_fullApp", demographic_temp)
                    dbAppendTable(con, "meas_table_fullApp", rv$proxy %>% 
                                      mutate(USERID = input$id, ID = input$id_dropdown) %>% 
                                      select(USERID, ID, DOL, PMA, Weight, Length, HC))
                    dbDisconnect(con, shutdown = TRUE)
                }
                
                output$meas_table <- renderDT({
                    datatable(rv$dt %>% select(DOL, Week, Day, Weight, Weight_Z, Length, Length_Z, HC, HC_Z) %>% 
                                  rename(`Weight Z (%ile)` ="Weight_Z",
                                         `Length Z (%ile)` = "Length_Z",
                                         `HC Z (%ile)` = "HC_Z"), 
                              rownames = FALSE, 
                              selection = "none", 
                              editable = list(target = "cell", disable = list(columns = c(0,1,2,4,6,8))),
                              options = list(dom = "t", 
                                             paging = FALSE,
                                             scrollY = "360",
                                             scrollX = TRUE,
                                             columnDefs = list(list(className = 'dt-right', targets = "_all"),
                                                               list(width = "7%", targets = c(0,1,2,3,5,7)))))
                })
                
                
                output$weight_plot <- renderPlotly({
                    plots$weight_plot <-
                    ggplot(rv$Chou_weight, aes(x = ((Day + rv$offset)/7))) +
                        geom_line(aes(y = percentile_3_var * 1000), size = 0.75, color = colors$color3, linetype = "solid") +
                        geom_line(aes(y = percentile_10_var * 1000), size = 0.5, color = colors$color3, linetype = "dashed") +
                        geom_line(aes(y = percentile_25_var * 1000), size = 0.5, color = colors$color3, linetype = "dotted") +
                        geom_line(aes(y = percentile_50_var * 1000), size = 0.5, color = colors$color3, linetype = "solid") +
                        geom_line(aes(y = percentile_75_var * 1000), size = 0.5, color = colors$color3, linetype = "dotted") +
                        geom_line(aes(y = percentile_90_var * 1000), size = 0.5, color = colors$color3, linetype = "dashed") +
                        geom_line(aes(y = percentile_97_var * 1000), size = 0.75, color = colors$color3, linetype = "solid") +
                        geom_point(data = rv$proxy %>% filter(!is.na(Weight)) %>% mutate(AGE = paste0(Week,"w",Day, "d")), aes(x = PMA/7, y = Weight, PMA = AGE, Z = Weight_Z, DOL = DOL), color = colors$color9, size = 2) +
                        geom_line(data = rv$Chou_weight %>% filter(Day %in% (c((as.numeric(input$weight_range[1])*7):(as.numeric(input$weight_range[2])*7)) - rv$offset)), 
                                  mapping = aes(x = ((Day + rv$offset)/7), y = percentile * 1000), size = 0.75, color = colors$color1) +
                        labs(x = "\nPostmenstrual Age (week)", 
                             y = "Weight (gram)\n",
                             title = NULL) + 
                        scale_x_continuous(breaks = 2 * c(10:30), sec.axis = sec_axis(~ (. - (GAn + rv$offset/7)), breaks =c(0:21), name = "Week of Life")) +
                        scale_y_continuous(breaks = 500 * c(0:20)) +
                        theme_bw() +
                        theme(axis.text = element_text(size = 8, color = "black", family = input$font_selection),
                              axis.title.x = element_text(size = 10, hjust = 1, face = "bold", family = input$font_selection),
                              axis.title.y = element_text(size = 10, face = "bold", family = input$font_selection),
                              plot.margin = margin(0.2, 0.05, 0.1, 0.1, "cm"))         
                    ggplotly(tooltip = c("PMA", "DOL", "Weight", "Z"),
                             plots$weight_plot
                    )
                })
                
                
                
                output$length_plot <- renderPlotly({
                    plots$length_plot <-
                        
                    ggplot(rv$Chou_length, aes(x = ((Day + rv$offset)/7))) +
                        geom_line(aes(y = percentile_3_var), size = 0.75, color = colors$color3, linetype = "solid") +
                        geom_line(aes(y = percentile_10_var), size = 0.5, color = colors$color3, linetype = "dashed") +
                        geom_line(aes(y = percentile_25_var), size = 0.5, color = colors$color3, linetype = "dotted") +
                        geom_line(aes(y = percentile_50_var), size = 0.5, color = colors$color3, linetype = "solid") +
                        geom_line(aes(y = percentile_75_var), size = 0.5, color = colors$color3, linetype = "dotted") +
                        geom_line(aes(y = percentile_90_var), size = 0.5, color = colors$color3, linetype = "dashed") +
                        geom_line(aes(y = percentile_97_var), size = 0.75, color = colors$color3, linetype = "solid") +
                        geom_point(data = rv$proxy %>% filter(!is.na(Length)) %>% mutate(AGE = paste0(Week,"w",Day, "d")), aes(x = PMA/7, y = Length, PMA = AGE, Z = Length_Z, DOL = DOL), color = colors$color9, size = 2) +
                        geom_line(data = rv$Chou_length %>% filter(Day %in% (c((as.numeric(input$length_range[1])*7):(as.numeric(input$length_range[2])*7)) - rv$offset)), 
                                  mapping = aes(x = ((Day + rv$offset)/7), y = percentile), size = 0.75, color = colors$color1) +
                        labs(x = "\nPostmenstrual Age (week)", 
                             y = "Length (cm)\n",
                             title = NULL) + 
                        scale_x_continuous(breaks = 2 * c(10:30), sec.axis = sec_axis(~ (. - (GAn + rv$offset/7)), breaks =c(0:21), name = "Week of Life")) +
                        scale_y_continuous(breaks = 2 * c(0:50)) +
                        theme_bw() +
                        theme(axis.text = element_text(size = 8, color = "black", family = input$font_selection),
                              axis.title.x = element_text(size = 10, hjust = 1, face = "bold", family = input$font_selection),
                              axis.title.y = element_text(size = 10, face = "bold", family = input$font_selection),
                              plot.margin = margin(0.2, 0.05, 0.1, 0.1, "cm"))         
                    ggplotly(tooltip = c("PMA", "DOL", "Length", "Z"),
                    plots$length_plot)
                })
                
                output$HC_plot <- renderPlotly({
                    plots$HC_plot <-
                    ggplot(rv$Chou_HC, aes(x = ((Day + rv$offset)/7))) +
                        geom_line(aes(y = percentile_3_var), size = 0.75, color = colors$color3, linetype = "solid") +
                        geom_line(aes(y = percentile_10_var), size = 0.5, color = colors$color3, linetype = "dashed") +
                        geom_line(aes(y = percentile_25_var), size = 0.5, color = colors$color3, linetype = "dotted") +
                        geom_line(aes(y = percentile_50_var), size = 0.5, color = colors$color3, linetype = "solid") +
                        geom_line(aes(y = percentile_75_var), size = 0.5, color = colors$color3, linetype = "dotted") +
                        geom_line(aes(y = percentile_90_var), size = 0.5, color = colors$color3, linetype = "dashed") +
                        geom_line(aes(y = percentile_97_var), size = 0.75, color = colors$color3, linetype = "solid") +
                        geom_point(data = rv$proxy %>% filter(!is.na(HC)) %>% mutate(AGE = paste0(Week,"w",Day, "d")), aes(x = PMA/7, y = HC, PMA = AGE, Z = HC_Z, DOL = DOL), color = colors$color9, size = 2) +
                        geom_line(data = rv$Chou_HC %>% filter(Day %in% (c((as.numeric(input$HC_range[1])*7):(as.numeric(input$HC_range[2])*7)) - rv$offset)), 
                                  mapping = aes(x = ((Day + rv$offset)/7), y = percentile), size = 0.75, color = colors$color1) +
                        labs(x = "\nPostmenstrual Age (week)", 
                             y = "Head Circumference (cm)\n",
                             title = NULL) + 
                        scale_x_continuous(breaks = 2 * c(10:30), sec.axis = sec_axis(~ (. - (GAn + rv$offset/7)), breaks =c(0:21), name = "Week of Life")) +
                        scale_y_continuous(breaks = 2 * c(0:50)) +
                        theme_bw() +
                        theme(axis.text = element_text(size = 8, color = "black", family = input$font_selection),
                              axis.title.x = element_text(size = 10, hjust = 1, face = "bold", family = input$font_selection),
                              axis.title.y = element_text(size = 10, face = "bold", family = input$font_selection),
                              plot.margin = margin(0.2, 0.05, 0.1, 0.1, "cm"))         
                    ggplotly(tooltip = c("PMA", "DOL", "HC", "Z"),
                    plots$HC_plot)
                    
                })
                
                updateBox("weight_box", action = "toggle")
                updateBox("length_box", action = "toggle")
                updateBox("HC_box", action = "toggle")
                updateBox("output_box", action = "restore")
                               
                ### Show buttons
                shinyjs::show("delete_record")
                shinyjs::show("save_data")
                shinyjs::show("calculate") 
            }
            
        } 
    })
    
    observeEvent(input$meas_table_cell_edit, {
        info <- input$meas_table_cell_edit
        
        if (info$col == 3) {
            if (is.na(as.numeric(info$value))) {
                rv$proxy[info$row, "Weight"] <- NA
                #rv$dt[info$row, "Weight"] <- NA
            } else if (!is.na(as.numeric(info$value))) {
                rv$proxy[info$row, "Weight"] <- as.numeric(info$value)
                #rv$dt[info$row, "Weight"] <- as.numeric(info$value)
            }
        } else if (info$col == 5) {
            if (is.na(as.numeric(info$value))) {
                rv$proxy[info$row, "Length"] <- NA
                #rv$dt[info$row, "Length"] <- NA
            } else if (!is.na(as.numeric(info$value))) {
                rv$proxy[info$row, "Length"] <- as.numeric(info$value)
                #rv$dt[info$row, "Length"] <- as.numeric(info$value)
            }
        } else if (info$col == 7) {
            if (is.na(as.numeric(info$value))) {
                rv$proxy[info$row, "HC"] <- NA
                #rv$dt[info$row, "HC"] <- NA
            } else if (!is.na(as.numeric(info$value))) {
                rv$proxy[info$row, "HC"] <- as.numeric(info$value)
                #rv$dt[info$row, "HC"] <- as.numeric(info$value)
            }
        }
    })
    
    observeEvent(input$calculate, {
        if (nrow(rv$proxy)> 0) {

        for (i in c(1:nrow(rv$proxy))) {
            if (!is.na(rv$proxy[i, "Weight"])) {
                CGA_adjust <- as.numeric(input$GA_week) * 7 + as.numeric(input$GA_day) + as.numeric(rv$proxy[i, "DOL"]) - rv$offset
                expected_weight <- rv$Chou_weight[rv$Chou_weight$Day %in% CGA_adjust, "percentile_50_var"]
                expected_sigma  <- rv$Chou_weight[rv$Chou_weight$Day %in% CGA_adjust, "sigma"]

                Z <- (as.numeric(rv$proxy[i, "Weight"]) / 1000 - expected_weight) / expected_sigma
                Percentile <- 100 * pnorm(Z)
                rv$proxy[i, "Weight_Z"] <- paste0(round(digits = 2, Z), " (", round(digits = 1, Percentile), "%ile)")
            } else if (is.na(rv$proxy[i, "Weight"])) {
                rv$proxy[i, "Weight_Z"] <- NA
            }

            if (!is.na(rv$proxy[i, "Length"])) {
                CGA_adjust <- as.numeric(input$GA_week) * 7 + as.numeric(input$GA_day) + as.numeric(rv$proxy[i, "DOL"]) - rv$offset
                expected_length <- rv$Chou_length[rv$Chou_length$Day %in% CGA_adjust, "percentile_50_var"]
                expected_sigma  <- rv$Chou_length[rv$Chou_length$Day %in% CGA_adjust, "sigma"]

                Z <- (as.numeric(rv$proxy[i, "Length"]) - expected_length) / expected_sigma
                Percentile <- 100 * pnorm(Z)
                rv$proxy[i, "Length_Z"] <- paste0(round(digits = 2, Z), " (", round(digits = 1, Percentile), "%ile)")
            } else if (is.na(rv$proxy[i, "Length"])) {
                rv$proxy[i, "Length_Z"] <- NA
            }

            if (!is.na(rv$proxy[i, "HC"])) {
                CGA_adjust <- as.numeric(input$GA_week) * 7 + as.numeric(input$GA_day) + as.numeric(rv$proxy[i, "DOL"]) - rv$offset
                expected_HC <- rv$Chou_HC[rv$Chou_HC$Day %in% CGA_adjust, "percentile_50_var"]
                expected_sigma  <- rv$Chou_HC[rv$Chou_HC$Day %in% CGA_adjust, "sigma"]

                Z <- (as.numeric(rv$proxy[i, "HC"]) - expected_HC) / expected_sigma
                Percentile <- 100 * pnorm(Z)
                rv$proxy[i, "HC_Z"] <- paste0(round(digits = 2, Z), " (", round(digits = 1, Percentile), "%ile)")
            } else if (is.na(rv$proxy[i, "HC"])) {
                rv$proxy[i, "HC_Z"] <- NA
            }
        }

        

### WEIGHT
        
        percentile_df <- rv$proxy %>% 
            filter(PMA >= as.numeric(input$weight_range[1]) * 7, PMA <= as.numeric(input$weight_range[2]) * 7) %>% 
            left_join(rv$Chou_weight %>% select(Day, Predicted_expected, sigma) %>% mutate(PMA = Day + rv$offset) %>% select(-Day))
        percentile <- seq(0.1, 99.9, by = 0.1)

        residual_df <- data.frame()
        
        if (!all(is.na(percentile_df$Weight))) {
            for (j in percentile) {
                percentile_df$percentile <- 1000 * (percentile_df$Predicted_expected + percentile_df$sigma * qnorm(j/100))
                percentile_df$residual <- percentile_df$Weight - percentile_df$percentile
                temp <- data.frame(percentile = j, residual = sum(percentile_df$residual^2, na.rm = TRUE))
                residual_df <- rbind(residual_df, temp)
            }
            rv$percentile_min_weight <- residual_df$percentile[residual_df$residual == min(residual_df$residual, na.rm = TRUE)]
            rv$Chou_weight$percentile <- rv$Chou_weight$Predicted_expected + qnorm(rv$percentile_min_weight/100) * rv$Chou_weight$sigma
            output$weight_trajectory_percentile <- renderUI({
                div(style=paste0("font-size:28px;font-weight:500;color:",colors$color9,";"), HTML(paste0(rv$percentile_min_weight,"<sup>th</sup> percentile")))
            })
            
            weight_WHO_percentile <- rv$WHO_weight[rv$WHO_weight$Percentile %in% rv$percentile_min_weight, "MEAS"]
            if (max(rv$Chou_weight$percentile) < (weight_WHO_percentile/1000)) {
                Percentile_PMA <- NA
            } else if (max(rv$Chou_weight$percentile) >= (weight_WHO_percentile/1000)) {
                Percentile_PMA <- rv$Chou_weight %>% filter(percentile < (weight_WHO_percentile/1000)) %>% slice(n())
                Percentile_PMA <- Percentile_PMA$Day + rv$offset
            }
            
            weight_transition_WHO <- 
                ifelse(is.na(Percentile_PMA), 
                   paste0((44*7+rv$offset)%/%7," weeks ", (44*7+rv$offset)%%7, " days PMA<div style='font-size:12px;'>(Size for percentile not attainable)</div>"),
                   paste0(weight_WHO_percentile, " grams (", Percentile_PMA %/% 7, "w", Percentile_PMA %%7, "d PMA)"))
            
            output$weight_WHO_transition <- renderUI({
                div(style=paste0("font-size:20px;font-weight:300;color:",colors$color9,";"), HTML(weight_transition_WHO))
            })
            
        } else {
            rv$percentile_min_weight <- NA
            rv$Chou_weight$percentile <- as.numeric(NA)
            output$weight_trajectory_percentile <- renderUI({})
            output$weight_WHO_transition <- renderUI({})
        }


### LENGTH

        percentile_df <- rv$proxy %>% 
            filter(PMA >= as.numeric(input$length_range[1]) * 7, PMA <= as.numeric(input$length_range[2]) * 7) %>% 
            left_join(rv$Chou_length %>% select(Day, Predicted_expected, sigma) %>% mutate(PMA = Day + rv$offset) %>% select(-Day))
        residual_df <- data.frame()
        
        if (!all(is.na(percentile_df$Length))) {
            
            for (j in percentile) {
                percentile_df$percentile <- (percentile_df$Predicted_expected + percentile_df$sigma * qnorm(j/100))
                percentile_df$residual <- percentile_df$Length - percentile_df$percentile
                temp <- data.frame(percentile = j, residual = sum(percentile_df$residual^2, na.rm = TRUE))
                residual_df <- rbind(residual_df, temp)
            }
            rv$percentile_min_length <- residual_df$percentile[residual_df$residual == min(residual_df$residual, na.rm = TRUE)]
            rv$Chou_length$percentile <- rv$Chou_length$Predicted_expected + qnorm(rv$percentile_min_length/100) * rv$Chou_length$sigma
            output$length_trajectory_percentile <- renderUI({
                div(style=paste0("font-size:28px;font-weight:500;color:",colors$color9,";"), HTML(paste0(rv$percentile_min_length,"<sup>th</sup> percentile")))
            })
            
            length_WHO_percentile <- rv$WHO_length[rv$WHO_length$Percentile %in% rv$percentile_min_length, "MEAS"]
            if (max(rv$Chou_length$percentile) < (length_WHO_percentile)) {
                Percentile_PMA <- NA
            } else if (max(rv$Chou_length$percentile) >= (length_WHO_percentile)) {
                Percentile_PMA <- rv$Chou_length %>% filter(percentile < (length_WHO_percentile)) %>% slice(n())
                Percentile_PMA <- Percentile_PMA$Day + rv$offset
            }
            
            length_transition_WHO <- 
                ifelse(is.na(Percentile_PMA), 
                       paste0((44*7+rv$offset)%/%7," weeks ", (44*7+rv$offset)%%7, " days PMA<div style='font-size:12px;'>(Size for percentile not attainable)</div>"),
                       paste0(length_WHO_percentile, " cm (", Percentile_PMA %/% 7, "w", Percentile_PMA %%7, "d PMA)"))
            
            output$length_WHO_transition <- renderUI({
                div(style=paste0("font-size:20px;font-weight:300;color:",colors$color9,";"), HTML(length_transition_WHO))
            })
            
        } else {
            rv$percentile_min_length <- NA
            rv$Chou_length$percentile <- as.numeric(NA)
            output$length_trajectory_percentile <- renderUI({})
            output$length_WHO_transition <- renderUI({})
        }
 
### HEAD CIRCUMFERENCE
        
        percentile_df <- rv$proxy  %>% 
            filter(PMA >= as.numeric(input$HC_range[1]) * 7, PMA <= as.numeric(input$HC_range[2]) * 7) %>% 
            left_join(rv$Chou_HC %>% select(Day, Predicted_expected, sigma) %>% mutate(PMA = Day + rv$offset) %>% select(-Day))
        residual_df <- data.frame()
        
        
        if (!all(is.na(percentile_df$HC))) {
            
            for (j in percentile) {
                percentile_df$percentile <- (percentile_df$Predicted_expected + percentile_df$sigma * qnorm(j/100))
                percentile_df$residual <- percentile_df$HC - percentile_df$percentile
                temp <- data.frame(percentile = j, residual = sum(percentile_df$residual^2, na.rm = TRUE))
                residual_df <- rbind(residual_df, temp)
            }
            rv$percentile_min_HC <- residual_df$percentile[residual_df$residual == min(residual_df$residual, na.rm = TRUE)]
            rv$Chou_HC$percentile <- rv$Chou_HC$Predicted_expected + qnorm(rv$percentile_min_HC/100) * rv$Chou_HC$sigma
            output$HC_trajectory_percentile <- renderUI({
                div(style=paste0("font-size:28px;font-weight:500;color:",colors$color9,";"), HTML(paste0(rv$percentile_min_HC,"<sup>th</sup> percentile")))
            })
            
            HC_WHO_percentile <- rv$WHO_HC[rv$WHO_HC$Percentile %in% rv$percentile_min_HC, "MEAS"]
            if (max(rv$Chou_HC$percentile) < (HC_WHO_percentile)) {
                Percentile_PMA <- NA
            } else if (max(rv$Chou_HC$percentile) >= (HC_WHO_percentile)) {
                Percentile_PMA <- rv$Chou_HC %>% filter(percentile < (HC_WHO_percentile)) %>% slice(n())
                Percentile_PMA <- Percentile_PMA$Day + rv$offset
            }
            
            HC_transition_WHO <- 
                ifelse(is.na(Percentile_PMA), 
                       paste0((44*7+rv$offset)%/%7," weeks ", (44*7+rv$offset)%%7, " days PMA<div style='font-size:12px;'>(Size for percentile not attainable)</div>"),
                       paste0(HC_WHO_percentile, " cm (", Percentile_PMA %/% 7, "w", Percentile_PMA %%7, "d PMA)"))
            
            output$HC_WHO_transition <- renderUI({
                div(style=paste0("font-size:20px;font-weight:300;color:",colors$color9,";"), HTML(HC_transition_WHO))
            })
            
        } else {
            rv$percentile_min_length <- NA
            rv$Chou_HC$percentile <- as.numeric(NA)
            output$HC_trajectory_percentile <- renderUI({})
            output$HC_WHO_transition <- renderUI({})
        }
        

        rv$dt <- rv$proxy

        }
    })
    
    # observeEvent(list(input$weight_range, input$length_range, input$HC_range), {
    #     click("calculate")
    # })
    
    observeEvent(input$color_theme_selection, {
        selected_color_theme <- gsub("Option ", "", str_extract(input$color_theme_selection, "Option\\s[0-9]+"))
        
        change_color_palette(selected_color_theme)

    })
    
    observeEvent(input$save_settings, {
        
        change_color_theme()
        change_font(input$font_selection)
        rv$font <- input$font_selection
        # save color theme
        selected_color_theme <- gsub("Option ", "", str_extract(input$color_theme_selection, "Option\\s[0-9]+"))
        id <- read_csv("~/Apps/GTC-Website-Apps/GTC-Website-Registration/www/user.csv")
        id[id$id %in% input$id, "color_theme"] <- as.numeric(selected_color_theme)
        id[id$id %in% input$id, "font"] <- input$font_selection
        write_csv(id, "~/Apps/GTC-Website-Apps/GTC-Website-Registration/www/user.csv")
        
    })
    
    observeEvent(list(input$plot_output_size, input$plot_output_orientation), {
        if (req(input$plot_output_orientation) == "Portrait") {
            if (req(input$plot_output_size) == "Letter") {
                rv$plot_width_x <- 8.5
                rv$plot_height_y <- 11
            } else if (req(input$plot_output_size) == "Legal") {
                rv$plot_width_x <- 11
                rv$plot_height_y <- 17
            } else if (req(input$plot_output_size) == "A4") {
                rv$plot_width_x <- 8.3
                rv$plot_height_y <- 11.7
            } else if (req(input$plot_output_size) == "B5") {
                rv$plot_width_x <- 6.9
                rv$plot_height_y <- 9.8
            }
        } else if (req(input$plot_output_orientation) == "Landscape") {
            if (req(input$plot_output_size) == "Letter") {
                rv$plot_height_y <- 8.5
                rv$plot_width_x <- 11
            } else if (req(input$plot_output_size) == "Legal") {
                rv$plot_height_y <- 11
                rv$plot_width_x <- 17
            } else if (req(input$plot_output_size) == "A4") {
                rv$plot_height_y <- 8.3
                rv$plot_width_x <- 11.7
            } else if (req(input$plot_output_size) == "B5") {
                rv$plot_height_y <- 6.9
                rv$plot_width_x <- 9.8
            }
        }
        
        output$msg <- renderUI({
            span(rv$plot_width_x,"|",rv$plot_height_y, style="color:white;")
        })
        
        output$download_plot_msg <- renderUI({
            div("The plot will be generated in the ", rv$layout, " layout with a size of ", rv$plot_width_x, "in by ", rv$plot_height_y, "in", style="color:red;padding:10px 0 20px 0;")
        })
    })
    
    observeEvent(input$plot_layout_1, {
        rv$layout <- "horizontal"
        plots$layout <- plots$weight_plot / plots$length_plot / plots$HC_plot
        output$download_plot_msg <- renderUI({
            div("The plot will be generated in the ", rv$layout, " layout with a size of ", rv$plot_width_x, "in by ", rv$plot_height_y, "in", style="color:red;padding:10px 0 20px 0;")
        })
    })
    
    observeEvent(input$plot_layout_2, {
        rv$layout <- "vertical"
        plots$layout <- plots$weight_plot | plots$length_plot | plots$HC_plot
        output$download_plot_msg <- renderUI({
            div("The plot will be generated in the ", rv$layout, " layout with a size of ", rv$plot_width_x, "in by ", rv$plot_height_y, "in", style="color:red;padding:10px 0 20px 0;")
        })
    })
    
    observeEvent(input$plot_layout_3, {
        rv$layout <- "plate"
        plots$layout <- plots$weight_plot | (plots$length_plot / plots$HC_plot)
        output$download_plot_msg <- renderUI({
            div("The plot will be generated in the ", rv$layout, " layout with a size of ", rv$plot_width_x, "in by ", rv$plot_height_y, "in", style="color:red;padding:10px 0 20px 0;")
        })
    })
    
    
    output$download_plot <- downloadHandler(
        
        filename = function() {
            paste0(input$id_dropdown, "_plot.pdf")
        },
        content = function(file) {
            

            
            if (rv$hasDisplay == 1) {
                pdf(file, width = rv$plot_width_x, height = rv$plot_height_y)
                if (rv$layout == "vertical") {
                    print(((plots$weight_plot & theme(plot.margin = margin(0.1,0.1,0.1,0.1,"inch"))) / (plots$length_plot & theme(plot.margin = margin(0.1,0.1,0.1,0.1,"inch"))) / (plots$HC_plot & theme(plot.margin = margin(0.1,0.1,0.1,0.1,"inch")))) +
                            plot_annotation(subtitle = "nicugrowth.app", 
                                            title = "Chou's Postnatal Growth Trajectory Curves",
                                            caption = "Contact us at admin@nicugrowth.app for any questions!"))
                } else if (rv$layout == "horizontal") {
                    print(((plots$weight_plot & theme(plot.margin = margin(0.1,0.1,0.1,0.1,"inch"))) | (plots$length_plot & theme(plot.margin = margin(0.1,0.1,0.1,0.1,"inch"))) | (plots$HC_plot & theme(plot.margin = margin(0.1,0.1,0.1,0.1,"inch")))) +
                            plot_annotation(subtitle = "nicugrowth.app", 
                                            title = "Chou's Postnatal Growth Trajectory Curves",
                                            caption = "Contact us at admin@nicugrowth.app for any questions!"))
                } else if (rv$layout == "plate") {
                    print(((plots$weight_plot & theme(plot.margin = margin(0.1,0.1,0.1,0.1,"inch"))) | ((plots$length_plot & theme(plot.margin = margin(0.1,0.1,0.1,0.1,"inch"))) / (plots$HC_plot & theme(plot.margin = margin(0.1,0.1,0.1,0.1,"inch"))))) +
                            plot_annotation(subtitle = "nicugrowth.app", 
                                            title = "Chou's Postnatal Growth Trajectory Curves",
                                            caption = "Contact us at admin@nicugrowth.app for any questions!"))
                }
            }
            dev.off()
        }
    )
    
    observeEvent(input$table_output_format, {
        if (input$table_output_format == "CSV") {
            rv$output_table_format <- "csv"
        } else if (input$table_output_format == "MS Excel") {
            rv$output_table_format <- "xlsx"
        } else if (input$table_output_format == "MS Word") {
            rv$output_table_format <- "docx"
        }
        
        output$msg <- renderUI({
            span(rv$plot_width_x,"|",rv$plot_height_y, style="color:white;")
        })
        output$download_table_msg <- renderUI({
            div("The table will be save in the ", input$table_output_format, " format", style="color:red;padding:10px 0 20px 0;")
        })
    })
    
    table_output <- reactive({
        proxy_df <- rv$proxy
        proxy_df[is.na(proxy_df)] <- ""
        proxy_df  %>% 
            rename(`Weight Z (%ile)` ="Weight_Z",
                   `Length Z (%ile)` = "Length_Z",
                   `HC Z (%ile)` = "HC_Z") %>% 
            select(input$table_output_columns)
            
    })
    
    output$download_table <- downloadHandler(
        
        filename = function() {
            paste0(input$id_dropdown, "_table.", rv$output_table_format)
        },
        content = function(file) {
            
            if (rv$output_table_format == "csv") {
                write_csv(table_output(), file = file, na = "")
            } else if (rv$output_table_format == "xlsx") {
                writexl::write_xlsx(table_output(), file)
            } else if (rv$output_table_format == "docx") {
                officer::read_docx() %>%
                    officer::body_add_table(., table_output()) %>%
                    print(., file)
            }
            
        }
    )
    
    
    
})
