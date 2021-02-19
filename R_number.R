library(COVID19)
library(ggplot2)
library(plotly)
data_Canada <- covid19("Canada", raw = FALSE) 
data_Japan <- covid19("Japan", raw = FALSE) 
data_UK <- covid19("United Kingdom", raw = FALSE) 

Re_data <- read.csv("re_estimates_on.csv")

Re_start_date = "2020-04-16"
R_number = Re_data$Re[Re_data["date_start"] == Re_start_date]
base_case = data_Canada$confirmed[data_Canada["date"] == Re_start_date]
Re_start_date = as.Date(Re_start_date,format = "%Y-%m-%d")

g = data_Canada %>%
    mutate(date = as.Date(date,format = "%Y-%m-%d")) %>%
    mutate(roll = rollmean(confirmed, k=7, na.pad = TRUE, align = "center"))  %>%
    mutate(forecasting = ifelse(date < Re_start_date, confirmed, base_case*(R_number**(as.numeric(date-Re_start_date)/7)))) %>%
    ggplot(aes(x=date, y=confirmed, label=date, label1=confirmed)) + 
    geom_point(size = 0.1) + 
    geom_line(aes(y=forecasting), color = "red") +
    geom_line(aes(y=roll)) +
    geom_vline(xintercept = as.numeric(Re_start_date), color = "blue", size=1.5) +
    theme(legend.position="none") + theme_bw() + labs(y="Daily New Cases")+
    theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
    scale_x_date(date_breaks = "28 days")

ggplotly(g, tooltip = c("Date", "confirmed"))
