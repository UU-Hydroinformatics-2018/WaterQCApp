

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyUI(fluidPage(
  
  # App title ----
  titlePanel("Uploading Utah DWQ Buoy Files for QC"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(("Use the URL Link for Downloading New Data from Utah DWQ Buoy Stations Email:DWQ@DWQ.gov and PW:DWQ"),
                 uiOutput("tab"),
                 # Horizontal line ----
                 tags$hr(),
                 
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File (<5 MB)",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Amount of Data to Display/Download",
                   choices = c(Head = "head", All = "all"),
                   selected = "head"),
      
      actionButton("Load", "Load the QC File/Plots"),
      
      # Horizontal line ----
      tags$hr(),
      
      downloadButton("downloadData.csv", "Download QC'ed CSV"), width = 4),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
      # Output: Data file ----
      tabPanel("Loaded Table",tableOutput("contents")),
      tabPanel("QC Table",tableOutput("my_output_data")),
      tabPanel("Temperature Plot",plotOutput("plot_temp"),tableOutput("table1")),
      tabPanel("Sp Cond Plot",plotOutput("plot_sp"),tableOutput("table2")),
      tabPanel("pH Plot",plotOutput("plot_pH"),tableOutput("table3")),
      tabPanel("Turbidity Plot",plotOutput("plot_turb"),tableOutput("table4")),
      tabPanel("Chlorophyll Plot",plotOutput("plot_chlor"),tableOutput("table5")),
      tabPanel("Dissolved Oxygen Plot",plotOutput("plot_do"),tableOutput("table6")),
      tabPanel("Phycocyanin Plot",plotOutput("plot_phy"),tableOutput("table7")))
      
    )
    
  )
))