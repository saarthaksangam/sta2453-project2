##################################################################
##                 Define theme for value boxes                 ##
##################################################################

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)

hc_theme_sparkline_vb <- function(...) {
    
    theme <- list(
        chart = list(
            backgroundColor = NULL,
            margins = c(0, 0, 0, 0),
            spacingTop = 0,
            spacingRight = 0,
            spacingBottom = 0,
            spacingLeft = 0,
            plotBorderWidth = 0,
            borderWidth = 0,
            style = list(overflow = "visible")
        ),
        xAxis = list(
            visible = FALSE, 
            endOnTick = FALSE, 
            startOnTick = FALSE
        ),
        yAxis = list(
            visible = FALSE,
            endOnTick = FALSE, 
            startOnTick = FALSE
        ),
        tooltip = list(
            outside = FALSE,
            shadow = FALSE,
            borderColor = "transparent",
            botderWidth = 0,
            backgroundColor = "transparent",
            style = list(textOutline = "5px white")
        ),
        plotOptions = list(
            series = list(
                marker = list(enabled = FALSE),
                lineWidth = 2,
                shadow = FALSE,
                fillOpacity = 0.25,
                color = "#FFFFFFBF",
                fillColor = list(
                    linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
                    stops = list(
                        list(0.00, "#FFFFFF00"),
                        list(0.50, "#FFFFFF7F"),
                        list(1.00, "#FFFFFFFF")
                    )
                )
            )
        ),
        credits = list(
            enabled = FALSE,
            text = ""
        )
    )
    
    theme <- structure(theme, class = "hc_theme")
    
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(
            theme,
            hc_theme(...)
        )
    }
    
    theme
}

valueBoxSpark <- function(value, title, sparkobj = NULL, subtitle, info = NULL, 
                          icon = NULL, color = "aqua", width = 4, href = NULL){
    
    shinydashboard:::validateColor(color)
    
    if (!is.null(icon))
        shinydashboard:::tagAssert(icon, type = "i")
    
    info_icon <- tags$small(
        tags$i(
            class = "fa fa-info-circle fa-lg",
            title = info,
            `data-toggle` = "tooltip",
            style = "color: rgba(255, 255, 255, 0.75);"
        ),
        # bs3 pull-right 
        # bs4 float-right
        class = "pull-right float-right"
    )
    
    boxContent <- div(
        class = paste0("small-box bg-", color),
        div(
            class = "inner",
            tags$small(title),
            if (!is.null(sparkobj)) info_icon,
            h3(value),
            if (!is.null(sparkobj)) sparkobj,
            p(subtitle)
        ),
        # bs3 icon-large
        # bs4 icon
        if (!is.null(icon)) div(class = "icon-large icon", icon, style = "z-index; 0")
    )
    
    if (!is.null(href)) 
        boxContent <- a(href = href, boxContent)
    
    div(
        class = if (!is.null(width)) paste0("col-sm-", width), 
        boxContent
    )
}

arrow <- function(num) {
    if(num < 0){
        return(HTML("&darr;"))
    }
    else if(num>0){ return(HTML("&uarr;")) }
    else return(HTML("-"))
}