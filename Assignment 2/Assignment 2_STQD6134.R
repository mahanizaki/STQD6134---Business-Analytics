# Assignment 2: Business Analytics STQD6134
# Title: Association Rule
# Group members:
# Azrul Zulhilmi bin Ahmad Rosli (P153478)
# Mahani Binti Mohamad Zaki (P147234)

# Load required libraries
library(shiny)
library(arules)
library(arulesViz)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plyr)
library(stringr)
library(RColorBrewer)

# Load the data
ecommerce <- read.csv(file.choose(), header = TRUE, sep = ",")

# Data Cleaning
ecommerce$TransactionNo <- as.character(ecommerce$TransactionNo)
# Remove cancel transaction (associate with C)
ecommerce$TransactionNo[str_detect(ecommerce$TransactionNo, "[^0-9]")] <- NA
ecommerce$TransactionNo <- as.numeric(ecommerce$TransactionNo)
cleaned_ecommerce <- na.omit(ecommerce)
cleaned_ecommerce$ProductName <- tolower(cleaned_ecommerce$ProductName)
cleaned_ecommerce <- distinct(cleaned_ecommerce)

# Prepare data for market basket analysis
itemslist <- data.frame(
  "TransactionID" = cleaned_ecommerce$TransactionNo,
  "Item" = cleaned_ecommerce$ProductName
)

# Convert long format to basket format
basket <- ddply(itemslist, .(TransactionID), summarize, Items = paste(Item, collapse = ","))
basket <- distinct(basket)

# Convert the Items column into a list
basket_list <- strsplit(as.character(basket$Items), ",")

# Create a transactions object
transactions <- as(basket_list, "transactions")

# Define UI
ui <- fluidPage(
  # Custom CSS for black background and text colors
  tags$head(
    tags$style(HTML("
      body {
        background-color: black;
        color: white;
      }
      .well {
        background-color: #333333;
        border-color: #555555;
        color: white;
      }
      h3, h4, p {
        color: white;
      }
      .form-control {
        background-color: #222222;
        color: white;
        border: 1px solid #555555;
      }
      .shiny-input-container {
        color: white;
      }
      .btn {
        background-color: #555555;
        color: white;
        border: none;
      }
      .btn:hover {
        background-color: #777777;
      }
    "))
  ),
  
  titlePanel("Association Rule: Market Basket Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      width = 4, # Increase the width of the sidebar panel
      div(class = "intro-text",
          h3("Introduction"),
          p("This dashboard performs market basket analysis on e-commerce transaction data. The data is from online sales transaction data of a United 
Kingdom e-commerce shop for a year. Kaggle: https://tinyurl.com/3sus65v9 "),
          h3("Data Description"),
          p("The dataset contains sales transactions with columns such as TransactionNo and ProductName. 
Confidence and lift values can be adjusted based on requirements. The top 10 and bottom 10 rules can be 
selected to analyze either the best-selling or least-selling products.")
      ),
      hr(),
      sliderInput("support", "Support Threshold:", min = 0.00, max = 1.00, value = 0.01, step = 0.001),
      sliderInput("confidence", "Confidence Threshold:", min = 0.00, max = 1.00, value = 0.8, step = 0.01),
      radioButtons("sort_order", "Sort Rules by Lift:", 
                   choices = c("Descending" = "desc", "Ascending" = "asc"), 
                   selected = "desc"),
      hr(),
      h4("Total Rules Generated:"),
      verbatimTextOutput("total_rules"),
      h4("Top 10 Association Rules"),
      verbatimTextOutput("rules_output")
    ),
    mainPanel(
      h3("Scatterplot of Rules"),
      plotOutput("scatterplot"),
      h3("10 Rules (Graph)"),
      plotOutput("top10_graph"),
      h3("Created by:"),
      h4("Azrul Zulhilmi bin Ahmad Rosli (P153478)"),
      h4("Mahani Binti Mohamad Zaki (P147234)")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive expression to generate rules
  rules <- reactive({
    Rules <- apriori(transactions, parameter = list(supp = input$support, conf = input$confidence, minlen = 2))
    
    # Sort rules by lift (ascending or descending)
    if (input$sort_order == "desc") {
      Rules <- sort(Rules, by = 'lift', decreasing = TRUE)
    } else {
      Rules <- sort(Rules, by = 'lift', decreasing = FALSE)
    }
    
    Rules
  })
  
  # Display total number of rules generated
  output$total_rules <- renderText({
    paste("Total Rules:", length(rules()))
  })
  
  # Display top 10 rules
  output$rules_output <- renderPrint({
    if (length(rules()) > 0) {
      inspect(head(rules(), 10))  # Show only top 10 rules
    } else {
      "No rules generated."
    }
  })
  
  # Scatterplot of rules
  output$scatterplot <- renderPlot({
    if (length(rules()) > 0) {
      plot(rules(), method = "scatterplot", measure = c("support", "confidence"), shading = "lift",
           main = "Scatterplot of Association Rules", cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)
    } else {
      # Set the background and plot text for "No rules to plot"
      plot.new()
      par(bg = "black") # Black background for the plot
      rect(0, 0, 1, 1, col = "black", border = NA) # Explicitly fill the background
      text(0.5, 0.5, "No rules to plot", cex = 1.5, col = "white")
    }
  })
  
  # Top 10 rules graph
  output$top10_graph <- renderPlot({
    if (length(rules()) > 0) {
      top10Rules <- head(rules(), 10)
      plot(top10Rules, method = "graph", control = list(nodeCol = "lightblue", edgeCol = "darkgray"))
    } else {
      # Set the background and plot text for "No rules to plot"
      plot.new()
      par(bg = "black") # Black background for the plot
      rect(0, 0, 1, 1, col = "black", border = NA) # Explicitly fill the background
      text(0.5, 0.5, "No rules to plot", cex = 1.5, col = "white")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
