source("helpers.R")

##### Import libraries
library(shiny)
library(readxl)
library(dplyr)
library(VIM)
library(ggplot2)

# setwd("V:/Forschung/Projekte/PallMeT Projekte_TSteigleder/06 EmpkinS/16 R/GUARDIANApp")
# dataset_path <- "W:/palliativmedizin/science/GDN_Ph2-1_Messung auf Station/Auswertung/Probanden GDN1001_GDN1050/Auswertung/Herzschlag/HR_GDN1001_GDN1050_30sWin_30sStep_herztoene_v1.xlsx"

dataset_path <- "data/HR_GDN1001_GDN1005_30sWin_30sStep_herztoene_v1.xlsx"

patient_ids <- Filter(function(x) x != "Infos", excel_sheets(dataset_path))
patient_data <- read_excel(dataset_path, sheet=2)
dates_unique <- unique(trunc(patient_data$StartSlot, "day"))
dates_unique <- as.character(dates_unique)

# User interface ----
ui <- fluidPage(
  titlePanel("GUARDIAN HR_mean"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("patient_id", 
                  label = "Choose a patient to display",
                  choices = patient_ids,
                  selected = patient_ids[1]),
      selectInput("date",
                  label = "Date to analyze",
                  choices = (dates_unique),
                  selected = dates_unique[1]),
      sliderInput("time_range", 
                  label = "Time of the day interested:",
                  min = 0, 
                  max = 24, value = c(0,24)),
      selectInput("imputation",
                  label = "Select data imputation method",
                  choices = c("None","Mean", "Mode"),)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Server logic
server <- function(input, output) {
  
  getDataInput <- reactive({
    df <- read_excel(dataset_path, sheet=as.character(input$patient_id))
    
    # percentage missing data
    HR_mean <- select(df, HR_mean)
    pctmiss <- colSums(is.na(HR_mean))/nrow(HR_mean)
    
    df <- filter(df, trunc(StartSlot, "day")== as.POSIXct(input$date, tz="UTC"))
    df$StartTimeHR <- as.POSIXct(df$StartTimeHR, format="%H:%M:%S", tz="UTC")
    start_time <- as.character(input$time_range[1])
    end_time <- as.character(input$time_range[2])
    end_time <- as.POSIXct(end_time, format="%H", tz="UTC")
    if (format(end_time, format="%H") == "00") {
      end_time <- as.POSIXct("23:59", format="%H:%M", tz="UTC")
                             }
    
    df <- filter(df,
                 as.POSIXct(df$StartTimeHR, format="%H:%M:%S", tz="UTC") >= as.POSIXct(start_time, format="%H", tz="UTC") &
                 as.POSIXct(df$StartTimeHR, format="%H:%M:%S", tz="UTC") < as.POSIXct(end_time, format="%H:%M", tz="UTC")
                 )
    
    args <- list(data=df, imputation=input$imputation, info=c(input$patient_id, start_time, end_time, format(pctmiss, digits=2)))
  })
  
  output$plot <- renderPlot({
    do.call(HR_mean_plot, getDataInput())
    })
}

# Run the app
shinyApp(ui, server)