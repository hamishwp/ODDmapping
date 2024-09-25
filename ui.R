library(leaflet)
library(shinythemes)

# Choices for drop-downs
hazards<-c(
  "All"="All",
  "Earthquake"="EQ",
  "Drought"="DR",
  "Tropical Cyclones"="TC",
  "Violent Wind"="VW",
  "Flood"="FL",
  "Volcanic eruption"="VO"
)

ODDz<-c(
  "Displaced People"="Disp",
  "Affected Population"="Population",
  "GDP-PPP per Capita"="GDP",
  "Hazard Footprint 1"="hazMean1",
  "Hazard Footprint 2"="hazMean2",
  "Hazard Footprint 3"="hazMean3",
  "Max. Driving Distance"="isochrone"
)

thazards<-c(
  "All"=NULL,
  "Earthquake"="Earthquake",
  "Mass movement"="Mass movement",
  "Drought"="Drought",
  "Tropical Cyclones"="Tropical Cyclones",
  "Violent Wind"="Violent Wind",
  "Flood"="Flood",
  "Volcanic eruption"="Volcanic eruption"
)


navbarPage("ODDRIN", id="nav", theme = shinytheme("flatly"),

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 50, bottom = "auto",
        width = 330, height = "auto",

        h4("Select Event:"),
        
        # tags$div("Select bounding box by either:",
        #   tags$ol(
        #     tags$li("Zooming in on map"),
        #     tags$li("Using map rectangle tool (top-left)")
        #   )
        # ),
        
        selectInput("haz", "Hazard:", hazards),
        
        # sliderInput("alertvals","GDACS Alertscore Range:",value=c(0,4),
        #             min = 0,max = 4,round = -2, dragRange = F,step = 0.1),
        
        # airDatepickerInput(
        #   inputId = "dates",
        #   label = "Date Range (yyyy-mm-dd):",
        #   range = TRUE, value = c("2008-01-02", as.character(Sys.Date())),
        #   separator = "   to   ",
        #   minDate = "2008-01-01",
        #   maxDate = Sys.Date()
        #   # view = c("days", "months", "years"),
        #   # clearButton = FALSE,
        #   # todayButton = FALSE
        # ),
        dateRangeInput("Date Range (yyyy-mm-dd):", inputId = "dates",
                       start = "2008-01-01",
                       end = Sys.Date(),
                       format = "yyyy-mm-dd",
                       autoclose = T,
                       max = Sys.Date()),
        
        selectInput("ODDvariable", "Select Variable to Plot:", ODDz),
        
        uiOutput("places"),
        uiOutput("shapes"),
        
        textOutput("tmp"),
        
        uiOutput("sumLocation"),
        
        tableOutput("predictedDisp"),
        
        tableOutput("affectedpop"),
        
        uiOutput("shelter"),
        
        textOutput("disttime")
        
        # plotOutput("IDPs", height = 200)
        
        # plotOutput("scatterCollegeIncome", height = 250)
      ),

      tags$div(id="cite",
        '.................................Developed by Hamish Patten, Department of Statistics., University of Oxford.'
      )
    )
  ),

  ############### DATA EXPLORER ###############
  
  tabPanel("Data explorer",
    fluidRow(
      column(3,
        # selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
        selectInput("iso3s", "Country", unique(DispData$iso3), multiple=TRUE)
      ),
      column(3,
        # conditionalPanel("input.states",
        selectInput("tablehazard", "Hazard:", thazards, multiple=TRUE)
        # )
      ),
      column(3,
        # conditionalPanel("input.states",
          # selectInput("tabledates", "Country", unique(DispData$iso3), multiple=TRUE)
        dateRangeInput("Date Range (yyyy-mm-dd):", inputId = "tabledates",
                       start = "2008-01-01",
                       end = Sys.Date(),
                       format = "yyyy-mm-dd",
                       autoclose = T,
                       max = Sys.Date())
        # )
      )
    ),
    # fluidRow(
    #   column(1,
    #     numericInput("minScore", "Min. Displacement", min=0, max=100, value=0)
    #   ),
    #   column(1,
    #     numericInput("maxScore", "Max. Displacement", min=0, max=100, value=100)
    #   )
    # ),
    hr(),
    DT::dataTableOutput("disptable")
  ),
  
  ############### Media Disaster Reports ###############
  
  tabPanel("Media Disaster Reports",
           titlePanel(h3(em("Oxford Disaster Displacement Real-time Information Network"))),
           br(),
           br(),
           sidebarLayout(position="right",
                         sidebarPanel(width=6,
                                      tags$head(
                                        # tags$style(type="text/css", "select { max-width: 540px; }"),
                                        # tags$style(type="text/css", ".span4 { max-width: 190px; }"),
                                        tags$style(type="text/css", ".well { max-width: 380px; }")
                                      ),
                                      p("Developed at the Department of Statistics, University of Oxford, by Hamish Patten."),
                                      a(href="https://www.stats.ox.ac.uk/",img(src = "OUlogo.png", height = 100.5, width = "auto")) ,             
                                      br(),
                                      br(),
                                      p("In collaboration with"),
                                      a(href="https://www.internal-displacement.org/",img(src = "IDMClogo.png", height = 68, width = "auto")),              
                                      br(),
                                      br(),
                                      p("Funding from"),
                                      a(href="https://epsrc.ukri.org/",img(src = "EPSRClogo.png", height = 122, width = "auto"))
                         ),
                         mainPanel(width=5,
                                   h2("The ODDRIN statistical software tool"),
                                   "Methodology described in the following article:")
           )
  ),
  
  ############### ABOUT THE TOOL ###############
  
  tabPanel("About ODDRIN",
           titlePanel(h3(em("Oxford Disaster Displacement Real-time Information Network"))),
           br(),
           br(),
           sidebarLayout(position="right",
             sidebarPanel(width=6,
                          tags$head(
                            # tags$style(type="text/css", "select { max-width: 540px; }"),
                            # tags$style(type="text/css", ".span4 { max-width: 190px; }"),
                            tags$style(type="text/css", ".well { max-width: 380px; }")
                          ),
                          p("Developed at the Department of Statistics, University of Oxford, by Hamish Patten."),
                          a(href="https://www.stats.ox.ac.uk/",img(src = "OUlogo.png", height = 100.5, width = "auto")) ,             
                          br(),
                          br(),
                          p("In collaboration with"),
                          a(href="https://www.internal-displacement.org/",img(src = "IDMClogo.png", height = 68, width = "auto")),              
                          br(),
                          br(),
                          p("Funding from"),
                          a(href="https://epsrc.ukri.org/",img(src = "EPSRClogo.png", height = 122, width = "auto"))
                          ),
             mainPanel(width=5,
                       h2("The ODDRIN statistical software tool"),
                       "Methodology described in the following article:")
           )
  ),
  
  conditionalPanel("false", icon("crosshair"))
)
