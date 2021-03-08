###########################################################################
###########################################################################
###                                                                     ###
###                            UI DEFINITION                            ###
###                                                                     ###
###########################################################################
###########################################################################

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)


source("data.R")


# options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

header <- dashboardHeader()
sidebar <- dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(),
    selectInput("country", "Select country",
                choices=abbr, selected="CAN"))
    helpText(
        "Data is taken from https://covid19datahub.io/",
        "Guidotti et al., (2020). COVID-19 Data Hub. Journal of Open Source Software, 5(51), 2376, https://doi.org/10.21105/joss.02376"
    )
body <- dashboardBody(
    useShinyjs(),
    fluidPage(
        fluidRow(
            # withSpinner(valueBoxOutput("vbox1"), image = 'https://drive.google.com/u/0/uc?id=1YLP6tzIv6sB4pVYWQy9fRcUqAd8AYAEn'),
            valueBoxOutput("vbox1"),
            valueBoxOutput("vbox2"),
            valueBoxOutput("vbox3")
        ),
        fluidRow(
            div(
                id='restriction_plot',
                box(solidHeader = TRUE,
                    status = "info",
                    title = "Government measures over time",
                    div(
                        dropdownButton(
                            tags$div(
                                style = "color: black !important;", # for text
                                h3("Plot options")
                            ),
                            selectInput(inputId = 'restriction_plot_stat',
                                        label = shiny::HTML("<p><span style='color: black'>Show evolution of</span></p>"),
                                        selected = "Daily cases",
                                        choices = names(statistics)),
                            dateInput(inputId = 'restriction_plot_date',
                                      label = shiny::HTML("<p><span style='color: black'>See restrictions on</span></p>"),
                                      value = max(all_data$date),
                                      min = min(all_data$date),
                                      max = max(all_data$date)),
                            circle = FALSE,
                            icon = icon("cog"),
                            tooltip = tooltipOptions(title = "Click for plot options")
                        ),
                        class = "box-tools pull-right",
                        id = "moveme1"
                    ),
                    plotOutput("restriction_plot")
                )
            ),
            box(solidHeader = TRUE,
                status = "info",
                title = "Measures in place at selected date",
                uiOutput("restrictions")
            )
        ),
        fluidRow(
            div(
                id='predictions',
                box(solidHeader = TRUE,
                    status = "info",
                    title = "Predictions",
                    div(
                        dropdownButton(
                            tags$div(
                                style = "color: black !important;", # for text
                                h3("Plot options")
                            ),
                            dateInput("forecasting_start",
                                      shiny::HTML("<p><span style='color: black'> Forecasting start date: </span></p>"),
                                      min = min(all_data$date),
                                      max = max(all_data$date),
                                      value = as.Date(max(all_data$date))-30),
                            sliderInput("forecasting_period",
                                        shiny::HTML("<p><span style='color: black'>Forecasting range:</span></p>"),
                                        min = 7,
                                        max = 90,
                                        value = 30),
                            circle = FALSE,
                            icon = icon("cog"),
                            tooltip = tooltipOptions(title = "Click for plot options")
                        ),
                        class = "box-tools pull-right",
                        id = "moveme2"
                    ),
                    plotOutput("forecasting")
                )
            ),
            uiOutput("forecast_box")
        )
    )
)
ui <- dashboardPage(header, sidebar, body)