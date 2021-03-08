#################################################################
##                       Data collection                       ##
#################################################################

# Data taken from https://covid19datahub.io/
# Guidotti et al., (2020). COVID-19 Data Hub. Journal of Open Source Software, 5(51), 2376, https://doi.org/10.21105/joss.02376

library("COVID19")
library(tidyverse)

all_data <- covid19(raw=FALSE, verbose=FALSE)
# glimpse(all_data)

abbr <- c(unique(all_data$id))
alias <- c(unique(all_data$administrative_area_level_1))
names(abbr) <- alias

moving_average <- function(x, n = 7){round(stats::filter(x, rep(1 / n, n), sides = 1))}

statistics <- list(
    "Daily cases" = "daily_cases_smoothed",
    "Daily deaths" = "daily_deaths_smoothed",
    "Daily tests" = "daily_tests_smoothed",
    "Hospital patients" = "hosp",
    "ICU patients" = "icu"
)

restrictions <- c("school_closing", 
                  "workplace_closing", 
                  "cancel_events", 
                  "gatherings_restrictions", 
                  "transport_closing", 
                  "stay_home_restrictions", 
                  "internal_movement_restrictions", 
                  "international_movement_restrictions",
                  "information_campaigns", 
                  "testing_policy", 
                  "contact_tracing")

school_closing_text <- list(
    "Open schools",
    "School closing recommended", 
    "School closing required (partial)", 
    "School closing required")
workplace_closing_text <- list(
    "Open workplaces",
    "Workplace closing recommended", 
    "Workplace closing required (partial)", 
    "Workplace closing required (non-essential)")
cancel_events_text <- list(
    "Events maintained",
    "Events cancelling recommended", 
    "Events cancelled")
gathering_restrictions_text <- list(
    "Gatherings allowed",
    "No gatherings above 1000 people",
    "No gathering above 100 people",
    "No gatherings above 10 people",
    "No gatherings (<10 people)")
transport_closing_text <- list(
    "Public transports maintained",
    "Public transports reduced",
    "Public transports closed")
stay_home_restrictions_text <- list(
    "No lockdown",
    "Staying home is recommended",
    "Lockdown (soft)",
    "Lockdown (hard)")
internal_movement_restrictions_text <- list(
    "No internal travel restriction",
    "Internal travel reduced",
    "Internal travel prohibited")
international_movement_restrictions_text <- list(
    "No international travel restriction",
    "Screening (International travel)",
    "High-risk regions quarantine (international)",
    "High-risk regions ban (international)",
    "Total border closure")
information_campaigns_text <- list(
    "No information campaign",
    "Information campaign",
    "Coordinated information campaign")
testing_policy_text <- list(
    "No testing policy",
    "Partial testing of symptomatic people",
    "Testing of symptomatic people",
    "Open public testing")
contact_tracing_text <- list(
    "No contact tracing",
    "Limited contact tracing",
    "Comprehensive contact tracing")
restrictions_categories=list(
    "Lockdown"=list("stay_home_restrictions"),
    "Travel&Transport"=list("international_movement_restrictions", 
                            "internal_movement_restrictions", 
                            "transport_closing"),
    "Schools&Workplaces"=list("school_closing", 
                              "workplace_closing"),
    "Information&Testing"=list("information_campaigns", 
                               "testing_policy", 
                               "contact_tracing"),
    "Events&Gatherings"=list("cancel_events", 
                             "gathering_restrictions")
)

restrictions_level_to_text <- list(
    "school_closing"=school_closing_text,
    "workplace_closing"=workplace_closing_text,
    "cancel_events"=cancel_events_text,
    "gathering_restrictions"=gathering_restrictions_text,
    "transport_closing"=transport_closing_text,
    "stay_home_restrictions"=stay_home_restrictions_text,
    "internal_movement_restrictions"=internal_movement_restrictions_text,
    "international_movement_restrictions"=international_movement_restrictions_text,
    "information_campaigns"=information_campaigns_text,
    "testing_policy"=testing_policy_text,
    "contact_tracing"=contact_tracing_text
)

colors = c("008000", "e6e6e6", "ffff66", "ff9900", "ff5050")
colors_name = c("green", "gray", "yellow", "orange", "red")