library(shiny)

shinyUI(fluidPage(
  titlePanel("US Storms/Weather Events and Damages"),
  
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style(type='text/css', "h1 { color: #586e75;}"),
	tags$style(type='text/css', ".well { background-color: #eee8d5; }"),
        tags$style(type="text/css", "select { max-width: 275px; }"),
        tags$style(type="text/css", "textarea { max-width: 200px; }"),
        tags$style(type="text/css", ".jslider { max-width: 220px; }"),
        tags$style(type='text/css', ".well { max-width: 300px; }"),
        tags$style(type='text/css', ".span4 { max-width: 300px; }")
      ),
      helpText("At initialization, when switch tabs, or when change of state or county selection could take few secs to update the screen."),
      helpText("Please Wait..."),
      br(),
      br(), 
      helpText("Data From:"),      
      helpText("U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database for Storms and other severe weather events"),
      br(),
      br(),      
      helpText("Select USA tab to see the total damages due to severe weather events in Entire United States"),
      br(),
      br(),
    
#       textInput("symb", "Text", "Text"),
#       selectInput("select", label = "State", 
#                   choices = list("AK"="AK", "AL"="AL", "AM"="AM", "AN"="AN", "AR"="AR", "AS"="AS", "AZ"="AZ", "CA"="CA", "CO"="CO", "CT"="CT", "DC"="DC", "DE"="DE", "FL"="FL", "GA"="GA", "GM"="GM", "GU"="GU", "HI"="HI", "IA"="IA", "ID"="ID", "IL"="IL", "IN"="IN", "KS"="KS", "KY"="KY", "LA"="LA", "LM"="LM", "MA"="MA", "MD"="MD", "ME"="ME", "MI"="MI", "MN"="MN", "MO"="MO", "MS"="MS", "MT"="MT", "NC"="NC", "ND"="ND", "NE"="NE", "NH"="NH","NJ"="NJ", "NM"="NM", "NV"="NV", "NY"="NY", "OH"="OH", "OK"="OK", "OR"="OR", "PA"="PA", "PH"="PH", "PK"="PK", "PR"="PR", "PZ"="PZ", "RI"="RI", "SC"="SC", "SD"="SD", "TN"="TN", "TX"="TX", "UT"="UT", "VA"="VA", "VI"="VI", "VT"="VT", "WA"="WA", "WI"="WI", "WV"="WV", "WY"="WY", "LH"="LH", "LS"="LS", "LC"="LC", "LE"="LE", "LO"="LO", "ST"="ST", "MH"="MH", "SL"="SL", "XX"="XX", "PM"="PM"), 
#                   selected = "SC"),
      
      helpText("Select State tab to see the total damages due to severe weather events in the selected State"),
      uiOutput("choose_State"),
      br(),
      br(),
      helpText("Select County tab to see the total damages due to severe weather events in the selected County of the State"),
      uiOutput("choose_County"),
      br(),
      br(),
      br(),
      br()      
      
#       br(),
#     
#       dateRangeInput("dates", 
#         "Date range",
#         start = "2013-01-01", 
#         end = as.character(Sys.Date())),
#    
#       actionButton("get", "Get Stock"),
#       
#       br(),
#       br(),
#       
#       checkboxInput("log", "Plot y axis on log scale", 
#         value = FALSE),
#       
#       checkboxInput("adjust", 
#         "Adjust prices for inflation", value = FALSE)
    ),
    
#    mainPanel(plotOutput("plot"))
     mainPanel( 
         tabsetPanel(type = "tabs", 
              tabPanel("USA", 
                        plotOutput(
                         outputId = "plot", 
                         height = "1440px",
                         width = "1584px")),
              tabPanel("State", 
                       plotOutput(
                         outputId = "plot2", 
                         height = "1440px",
                         width = "1584px")),
              tabPanel("County", 
                       plotOutput(
                         outputId = "plot3", 
                         height = "1440px",
                         width = "1584px"))              
         )
     )
  )
))