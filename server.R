###########################################################################
###########################################################################
###                                                                     ###
###                          SERVER DEFINITION                          ###
###                                                                     ###
###########################################################################
###########################################################################

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(data.table)
library(highcharter)
library(shinycssloaders)
library(zoo)
library(glue)

source("data.R")
source("utils.R")

server <- function(input, output) {
    
    # JS script to put the drowdownButtons inside the box header
    runjs("$('#restriction_plot .box-header').append($('#moveme1').detach())")
    runjs("$('#predictions .box-header').append($('#moveme2').detach())") 
    
    reactive_data <- reactive({filter(all_data, id==input$country) %>%
            mutate(daily_cases = confirmed - lag(confirmed, default=confirmed[1])) %>%
            mutate(daily_cases_smoothed = moving_average(daily_cases)) %>%
            mutate(daily_deaths = deaths - lag(deaths, default=deaths[1])) %>%
            mutate(daily_deaths_smoothed = moving_average(daily_deaths)) %>%
            mutate(daily_tests = tests - lag(tests, default=deaths[1])) %>%
            mutate(daily_tests_smoothed = moving_average(daily_tests))
    })
    
    #################################################################
    ##                    Define overview plots                    ##
    #################################################################
    
    output$vbox1 <- renderValueBox({
        data <- reactive_data()
        hc <- data  %>%
            hchart("line", hcaes(date, daily_cases_smoothed), name = "Cases Timeline")  %>%
            hc_size(height = 100) %>%
            hc_credits(enabled = FALSE) %>%
            hc_add_theme(hc_theme_sparkline_vb())
        total <- length(data$daily_cases_smoothed)
        today <- data$daily_cases_smoothed[total]
        week_back <- data$daily_cases_smoothed[total - 7]
        percent_week <- ifelse(week_back == 0, 0, round((today - week_back)/week_back * 100))
        vb <- valueBoxSpark(
            value = round(today),
            title = toupper("Daily Cases"),
            sparkobj = hc,
            subtitle = tagList(arrow(percent_week), percent_week, "% Since last week"),
            info = "Daily number of COVID cases",
            width = 4,
            color = "teal",
            href = NULL
        )
        vb
    })
    output$vbox2 <- renderValueBox({
        data <- reactive_data()
        hc <- data  %>%
            hchart("line", hcaes(date, daily_deaths_smoothed), name = "Deaths Timeline")  %>%
            hc_size(height = 100) %>%
            hc_credits(enabled = FALSE) %>%
            hc_add_theme(hc_theme_sparkline_vb())
        total <- length(data$daily_deaths_smoothed)
        today <- data$daily_deaths_smoothed[total]
        week_back <- data$daily_deaths_smoothed[total - 7]
        percent_week <- ifelse(week_back == 0, 0, round((today - week_back)/week_back * 100))
        vb <- valueBoxSpark(
            value = round(today),
            title = toupper("Daily Deaths"),
            sparkobj = hc,
            subtitle = tagList(arrow(percent_week), percent_week, "% Since last week"),
            info = "Daily number of COVID deaths",
            width = 4,
            color = "red",
            href = NULL
        )
        vb
    })
    output$vbox3 <- renderValueBox({
        data <- reactive_data()
        hc <- data  %>%
            hchart("line", hcaes(date, hosp), name = "Hospital Admissions Timeline")  %>%
            hc_size(height = 100) %>%
            hc_credits(enabled = FALSE) %>%
            hc_add_theme(hc_theme_sparkline_vb())
        total <- length(data$hosp)
        today <- data$hosp[total]
        week_back <- data$hosp[total - 7]
        percent_week <- ifelse(week_back == 0, 0, round((today - week_back)/week_back * 100))
        vb <- valueBoxSpark(
            value = round(today),
            title = toupper("Hospital Admissions"),
            sparkobj = hc,
            subtitle = tagList(arrow(percent_week), percent_week, "% Since last week"),
            info = "Number of COVID patients in hospital beds",
            width = 4,
            color = "yellow",
            href = NULL
        )
        vb
    })
    
    #################################################################
    ##                   Define restriction plot                   ##
    #################################################################
    
    output$restriction_plot <- renderPlot({
        data <- reactive_data()
        data <- data %>%
            mutate(to_show = data[[statistics[[input$restriction_plot_stat]]]])
        
        if (!is.null(display_restriction())){
            levels = data[[display_restriction()]]+1
            colors_label = unlist(restrictions_level_to_text[[display_restriction()]], recursive=FALSE)
            breaks = as.character(1:length(colors_label))
            colors = head(colors_name, length(colors_label))
            names(colors) = breaks
            plot <- data %>%
                ggplot(aes(x=date, y=to_show, colour=factor(levels), 
                           group=c(0, cumsum(tail(levels != shift(levels, 1), -1))))) +
                geom_line(size=1.5) +
                scale_colour_manual(values=colors, breaks=breaks , labels=colors_label, 
                                    name="Restrictions", drop=FALSE) +
                xlab("Date") + ylab(input$restriction_plot_stat) +
                theme(legend.position="bottom",
                      legend.text=element_text(size=12),
                      axis.text=element_text(size=12),
                      axis.title=element_text(size=14))
        } else {
            plot <- data %>% 
                ggplot(aes(x=date, y=to_show)) +
                geom_line(size=1.5) +
                xlab("Date") + ylab("Daily cases") +
                theme(axis.text=element_text(size=12),
                      axis.title=element_text(size=14))
        }
        plot <- plot + 
            geom_vline(aes(xintercept=input$restriction_plot_date), linetype = "dashed") +
            annotate("text", x=input$restriction_plot_date, y = 0, label=input$restriction_plot_date, hjust=1.2, vjust=1, color='black')
        return(plot)
    })
    
    ##################################################################
    ##                   Define restrictions list                   ##
    ##################################################################
    
    # This create a button for each measure in place
    output$restrictions <- renderUI({
        categories = tagList()
        data <- reactive_data()
        date_row = filter(data, date==input$restriction_plot_date)
        i = 1
        for (cat in names(restrictions_categories)){
            buttons = tagList()
            j = 1
            for (name in restrictions_categories[[cat]]){
                level = date_row[[name]]
                if (!is.null(level)){
                    buttons[[j]] = actionButton(
                        glue("display_{name}"),
                        restrictions_level_to_text[[name]][[level+1]],
                        style=glue("color: #000; background-color: #{colors[[level+1]]}; border-color: #{colors[[level+1]]}")
                    )
                    j = j+1
                }
            }
            categories[[i]] = box(title=cat, buttons)
            i = i+1
        }
        categories
    })
    
    display_restriction <- reactiveVal(NULL)
    
    # Observe event for all restriction buttons
    observe({
        input_btn <- paste0("display_", names(restrictions_level_to_text))
        lapply(input_btn,
               function(x){
                   observeEvent(
                       input[[x]],
                       {
                           name <- sub("display_", "", x)
                           if (is.null(display_restriction()) || (display_restriction() != name)) {
                               display_restriction(name)
                           } else {
                               display_restriction(NULL)
                           }
                       }
                   )
               }
        )
    })
    
    #################################################################
    ##                   Define forecasting plot                   ##
    #################################################################
    
    output$forecasting <- renderPlot({
        data <- reactive_data()
        Re_start_date = input$forecasting_start
        Re_end_date = Re_start_date + input$forecasting_period
        max_date = max(data$date)
        # The R number is estimated on the two weeks prior to the start date
        base_case = data %>% filter(date == Re_start_date) %>% pull(daily_cases_smoothed)
        old_case = data %>% filter(date == Re_start_date - 14) %>% pull(daily_cases_smoothed)
        R_number = exp((log(base_case) - log(old_case))/2)
        
        data = data %>%
            mutate(forecasting = ifelse(date < Re_start_date, 
                                        daily_cases_smoothed, 
                                        as.integer(base_case*(R_number**(as.numeric(date-Re_start_date)/7)))))
        if (Re_end_date <= max_date) { 
            plot_data <- data.frame(
                date = data$date,
                daily_cases_smoothed = data$daily_cases_smoothed,
                forecasting = ifelse(data$date < Re_start_date, 
                                     data$daily_cases_smoothed, 
                                     as.integer(base_case*(R_number**(as.numeric(data$date-Re_start_date)/7))))
            )
        } else {
            dates =  c(data$date, seq(max_date, Re_end_date, "days"))
            plot_data <- data.frame(
                date = dates,
                daily_cases_smoothed = c(data$daily_cases_smoothed, 
                                         rep(0, length(dates)-length(data$daily_cases_smoothed))),
                forecasting = ifelse(dates < Re_start_date, 
                                     c(data$daily_cases_smoothed, rep(0, length(dates)-length(data$daily_cases_smoothed))), 
                                     as.integer(base_case*(R_number**(as.numeric(dates-Re_start_date)/7))))
            )
        }
        plot = plot_data %>%
            ggplot(aes(x=date)) + 
            geom_line(aes(y=daily_cases_smoothed), size=1.5, filter(data, date < max_date)) +
            geom_line(aes(y=forecasting), color = "red", size=1.5, filter(plot_data, date < Re_end_date & date > Re_start_date)) +
            geom_vline(xintercept = as.numeric(as.Date(Re_start_date, format = "%Y-%m-%d")), colour = "black", linetype = 'dashed') + 
            annotate("text", x=Re_start_date, y = 0, label=Re_start_date, hjust=1.2, vjust=1, color='black') +
            theme(legend.position="none") + theme_bw() +
            xlab("Date") + ylab("Daily cases") +
            theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=14))
        
        return(plot)
        
    })
    
    output$forecast_box <- renderUI({
        data <- reactive_data()
        
        Re_start_date = input$forecasting_start
        Re_end_date = Re_start_date + input$forecasting_period
        max_date = max(data$date)
        base_case = data %>% filter(date == Re_start_date) %>% pull(daily_cases_smoothed)
        old_case = data %>% filter(date == Re_start_date - 14) %>% pull(daily_cases_smoothed)
        R_number = exp((log(base_case) - log(old_case))/2)
        
        # forecast 
        forecast_number = as.integer(base_case*(R_number**(input$forecasting_period/7)))
        if (Re_end_date <= max_date) {
            actual_number = data %>% filter(date == Re_end_date) %>% pull(daily_cases_smoothed)
            difference = actual_number - forecast_number
        } else {
            actual_number = "No data"
            difference = "No data"
        }
        
        list(
            infoBox(
                "R number", format(round(R_number, 2), nsmall = 2), icon = shiny::icon("bar-chart"),
                color = ifelse(R_number > 1, "orange", "green"), width = 6
            ),
            infoBox(
                "Forecasting cases", paste0(" ", forecast_number), icon = shiny::icon("bar-chart"),
                color = "red", width = 6
            ),
            infoBox(
                "Actual cases", paste0(" ", actual_number), icon = shiny::icon("bar-chart"),
                color = "black", width = 6
            ),
            infoBox(
                "Difference", paste0(" ", difference), icon = shiny::icon("bar-chart"),
                color = ifelse(difference > 0, "orange", "green"), width = 6
            )
        )
        
    })
    
}