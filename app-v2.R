

# Import libraries
library(lubridate)
library(shiny)
library(plotly)

library(tidyverse)
library(tsibble)
library(shinythemes)


setwd("~/Documents/ecommerce_sales_forecasting/")

# Read in the prediction output data and actuals
#model <- readRDS("model_nested_df.rds")
act <- read.csv("actfc_df.csv", header = TRUE) %>% select(c(order_purchase_timestamp,product_category_name_english,sales_volume))
fcst <- read.csv("prediction_output_fbprophet.csv", header = TRUE)
product_category <- c("all",unique(act$product_category_name_english)) #creating a list for the product category dropdown 
combined_df <- right_join(act,fcst, by =c("order_purchase_timestamp"="ds","product_category_name_english") , copy=FALSE) %>%
  mutate(date = order_purchase_timestamp,
         product_category=product_category_name_english,
         actual = sales_volume,
         forecast = yhat) %>% select(c(date,product_category,actual,forecast))

alltemp <- data.frame(combined_df %>% group_by(date) %>% summarise(actual = sum(actual),
                                                        forecast = sum(forecast)))
alldf <- data.frame(date=alltemp$date,product_category="all",alltemp[,2:3])

combined_df <- rbind(combined_df,alldf)
combined_df <- combined_df %>% mutate(forecast = round(ifelse(forecast<0,0,forecast),0))


####################################
# User interface                   #
####################################

ui <- fluidPage(theme = shinytheme("flatly"),

  # Page header
  headerPanel('Ecommerce Forecast vs Actual Dashboard'),
  
  # Input values
  sidebarPanel(
    #HTML("<h3>Input parameters</h3>"),
    tags$label(h3('Input parameters')),
    selectInput("product_category", 
                 label = "Product Category", 
                product_category, selected = "all"),
    sliderInput("date", label = "Start Date", min = as.Date("2017-01-05"), max = as.Date("2018-08-30"), value = as.Date("2017-01-05")),
    actionButton("submitbutton", "Submit", 
                 class = "btn btn-primary")
  ),
  
  mainPanel(
    plotlyOutput('plot1'),
    tableOutput("tabledata") # Prediction results table
  )
)

#runApp("app-v2.R")
####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    ### Rendering the outputs
    Output <- combined_df[combined_df$date >= input$date & combined_df$product_category== input$product_category,]
    print(tail(Output,30))
    })
  
  pltinput <- reactive({
    outputplotdf <- combined_df[combined_df$date >= input$date & combined_df$product_category== input$product_category,] %>% 
      gather(key="type", value="value",-c(date,product_category)) %>% 
      mutate(date = as.Date(date))
  })
  
  #prediction plot
  output$plot1 <- renderPlotly({
    pal <- c("#213ee2", "#06d997")
    fig <- plot_ly(pltinput(), x = ~date, y = ~value, type = 'scatter', mode = 'lines', color = ~type, colors=pal) %>%
      layout(
        title = list(text = paste("Product Category =","'",input$product_category,"'"), y = 0.97, x=0.5),
        titlefont = list(
          size = 28,
          color = '#213ee2'),
        xaxis = list(type = 'date',
                     tickformat = "%B %Y"))
       fig
     })
  
  # Prediction table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
      } 
     })
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)


