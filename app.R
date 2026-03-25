library(shiny)
library(rhandsontable)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(ggnewscale)
library(ggrepel)
library(scales)
library(patchwork)
library(ggiraph)

# global ------------------------------------------------------------------

data_details <- read_rds("data/processed/data_details.RDS")
data_compartments <- read_rds("data/processed/data_compartments.RDS")
data_totloads <- read_rds("data/processed/data_totloads.RDS")

data_peacou <- read_rds("data/processed/data_peacou.RDS")


#data_betas <- read_rds("data/processed/data_betas.RDS")
#data_example <- read_rds("data/processed/data_example.RDS")

# Source utility functions (rose plot, distribution plot)
source("R/utils.R")

# ui ----------------------------------------------------------------------


ui <- shinydashboard::dashboardPage(
  ###### Header ##################################################################
  shinydashboard::dashboardHeader(title = "PIE"),
  
  ###### Sidebar #################################################################
  shinydashboard::dashboardSidebar(
    ### Menu ###
    shinydashboard::sidebarMenu(
      id = "sidebar_menu",
      menuItem("  Welcome", tabName = "welcome", icon = icon("campground")),
      menuItem("  Load Calculator", tabName = "sys", icon = icon("bug")),
      menuItem(
        "  Single Compound View",
        tabName = "single",
        icon = icon("flask")
      ),
      menuItem(
        "  Compound Comparison View",
        tabName = "double",
        icon = icon("flask-vial")
      )
      
    ),
    
    # # Rose plot option for detailed figure or not, single
    # conditionalPanel(
    #   condition = "input.sidebar_menu == 'single'",
    #   h4("Plot Options"),
    #   checkboxInput("detailed_view", "Detailed plot view", value = FALSE),
    #   div(
    #     style = "padding-left: 30px; padding-right: 30px; color: white; font-size: 12px;",
    #     p("• Quality of the data ranges from 1 (low) to 5 (high)"),
    #     p("• Data may be missing (X, dashed filling) or not reported (blank)"),
    #   )
    # ),
    
    # Rose plot option for detailed figure or not, double
    conditionalPanel(
      condition = "input.sidebar_menu == 'double'",
      h4("Plot Options"),
      checkboxInput("detailed_view2", "Detailed plot view", value = FALSE),
      div(
        style = "padding-left: 30px; padding-right: 30px; color: white; font-size: 12px;",
        p("• Quality of the data ranges from 1 (low) to 5 (high)"),
        p("• Data may be missing (X, dashed filling) or not reported (blank)"),
      )
    ),
    
    # Pesticide data entry specific sidebar content
    conditionalPanel(
      condition = "input.sidebar_menu == 'sys'",
      br(),
      div(
        style = "padding-left: 15px; padding-right: 15px; text-align: center;",
        actionButton("add_row", "Add Row", class = "btn-primary btn-sm", style = "margin-bottom: 10px;"),
        actionButton("remove_row", "Remove Row", class = "btn-warning btn-sm", style = "margin-bottom: 10px;")
      )
    ),
    
    
    
    ### Credit info, ADOPT IPM logo ###
    div(
      style = "position: fixed;
               bottom: 15px;
               left: 15px;
               font-size: 12px;
               color: #888;
               z-index: 1000;",
      # Try different approaches for the image
      # # Option 1: Standard approach (what you have)
      img(
        src = "adopt-ipm_logo-clean.png",
        #src = "test.png",
        height = "50px",
        width = "auto",
        style = "margin-bottom: 5px;",
        onerror = "this.style.display='none'; console.log('Image failed to load');"
      ),
      br(),
      HTML(
        "<a href='https://adopt-ipm.eu/' target='_blank'>adopt-ipm.eu</a><br>
         Nichols et al. (2026)<br>
         Last updated: March 2026<br>"
      )
    )
  ),
  #--end of sidebar
  
  
  ###### Body ####################################################################
  shinydashboard::dashboardBody(
    tags$head(tags$style(
      HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
      ")
    )),
    
    tabItems(
      
      ###### Welcome tab ######
      tabItem(tabName = "welcome", fluidRow(
        box(
          title = "Welcome to the Pesticide Impact Explorer (PIE) tool",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          h3("Getting Started", icon("person-walking")),
          p(
            "Welcome to our dashboard! Below is an overview of the tabs and some useful resources:",
            style = "font-size: 16px; margin-bottom: 20px;"
          ),
          
          h4("Dashboard Contents", style = "color: #2c3e50; margin-top: 25px;"),
          tags$ul(
            style = "line-height: 1.8; font-size: 15px;",
            tags$li(
              tags$strong("Load Calculator", style = "color: #f39c12;"),
              " allows users to calculate the load and societal costs resulting from a pesticide package (as calculated by the ",
              tags$em("Harmonized Pesticide Load Index", style = "color: #8e44ad;"),
              " and the ",
              tags$em("Pesticide Environmental Accounting (PEA)", style = "color: #8e44ad;"),
              " methodologies)"
            ),
            tags$li(
              tags$strong("Single Compound View", style = "color: #eb5e23;"),
              " presents detailed information on the impact of substances used in agricultural settings",
            ),
            tags$li(
              tags$strong("Compound Comparison View", style = "color: #27ae60;"),
              " allows side-by-side comparison of substance impacts"
            )
          ),
          
          hr(style = "margin: 30px 0; border-top: 2px solid #bdc3c7;"),
          
          h4("Additional Resources", style = "color: #2c3e50; margin-bottom: 15px;"),
          tags$ul(
            style = "line-height: 2; font-size: 15px;",
            tags$li(
              "Visit the website hosting the ",
              tags$strong("Pesticide Properties DataBase", style = "color: #2980b9;"),
              " hosted by the University of Hertfordshire: ",
              tags$a(
                "PPDB",
                href = "https://sitem.herts.ac.uk/aeru/ppdb/en/index.htm",
                target = "_blank",
                style = "color: #eb5e23; text-decoration: none; font-weight: bold;
                          border-bottom: 1px dotted #eb5e23;"
              )
            ),
            tags$li(
              "Read the publication describing the ",
              tags$strong("Pesticide Properties Database", style = "color: #2980b9;"),
              " in detail: ",
              tags$a(
                "Lewis et al. 2015",
                href = "https://www.tandfonline.com/doi/full/10.1080/10807039.2015.1133242",
                target = "_blank",
                style = "color: #eb5e23; text-decoration: none; font-weight: bold;
                          border-bottom: 1px dotted #eb5e23;"
              )
            ),
            tags$li(
              "Read the dissertation describing the development of the ",
              tags$strong("Harmonized Pesticide Load Index", style = "color: #2980b9;"),
              " in detail: ",
              tags$a(
                "Vandevoorde 2025",
                href = "https://sytra.be/publication/three-tools-reduction-pesticide-impacts/",
                target = "_blank",
                style = "color: #eb5e23; text-decoration: none; font-weight: bold;
                          border-bottom: 1px dotted #eb5e23;"
              )
            ),
            tags$li(
              "Read the publication describing the calculation of the ",
              tags$strong("Harmonized Pesticide Load Index", style = "color: #2980b9;"),
              " in detail: ",
              tags$a(
                "Vandevoorde et al. 2025",
                href = "https://iopscience.iop.org/article/10.1088/1748-9326/ae269b",
                target = "_blank",
                style = "color: #eb5e23; text-decoration: none; font-weight: bold;
                          border-bottom: 1px dotted #eb5e23;"
              )
            ),
            tags$li(
              "Read the publication describing the calculation of societal costs of pesticides using the ",
              tags$strong("Pesticide Environmental Accounting (PEA)", style = "color: #2980b9;"),
              " tool here:",
              tags$a(
                "Leach and Mumford 2008",
                href = "https://www.sciencedirect.com/science/article/abs/pii/S0269749107001492?via%3Dihub",
                target = "_blank",
                style = "color: #eb5e23; text-decoration: none; font-weight: bold;
                          border-bottom: 1px dotted #eb5e23;"
              )
            ),
            tags$li(
              "Read the accompanying publication for the",
              tags$strong("PIE tool", style = "color: #2980b9;"),
              " here: ",
              tags$a(
                "Publication in progress, here is the project website",
                href = "https://adopt-ipm.eu/",
                target = "_blank",
                style = "color: #eb5e23; text-decoration: none; font-weight: bold;
                          border-bottom: 1px dotted #27ae60;"
              )
            )
          ),
          
          div(
            style = "margin-top: 30px; padding: 15px; background-color: #ecf0f1; border-radius: 5px;",
            h5("TL;DR?", style = "color: #2c3e50; margin-bottom: 10px;"),
            p(
              "Navigate through the different tabs using the sidebar to explore all available features.
                Each section provides detailed insights into pesticide impacts and agricultural management systems.",
              style = "margin-bottom: 0; font-size: 14px; color: #34495e;"
            )
          )
        )
      )),
      #--end welcome tab
      
      ###### Calculate load tab ######
      tabItem(
        tabName = "sys",
        ###### guidance ######
        fluidRow(
          # box(
          #   title = "How do I enter data into the table?",
          #   status = "success",
          #   solidHeader = TRUE,
          #   width = 12,
          #   height = "250px",
            div(
              style = "font-size: 18px; line-height: 1.8; padding-left: 30px;",
              p(
                "Click on the first cell under the ",
                tags$strong(style = "color: #d9534f;", "Compound"),
                " column, start typing in the name of the active ingredient, and select it from the drop-down menu. The ",
                tags$strong(style = "color: #d9534f;", "load"),
                " will automatically fill in."
              ),
              p(
                "Enter the amount of the ",
                tags$strong(style = "color: #d9534f;", "active ingredient"),
                "that was applied in kg per area, most commonly ",
                tags$strong(style = "color: #d9534f;", "kg/ha"),
                ", noting that it is the amount of ",
                tags$strong(style = "color: #d9534f;", "active ingredient"),
                ", NOT THE AMOUNT OF PRODUCT! "
              ),
              p(
                "Continue adding active ingredients until you have entered everything that was applied."
              ),
              p(
                "If you need more rows, add them using the buttons located in the ",
                tags$strong(style = "color: #d9534f;", "left sidebar"),
                " (under the tab names)."
              )
            )
            #)
          ),
        
        # First system
        fluidRow(
          box(
            title = "Pesticides applied",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            height = "275px",
            rHandsontableOutput("pest_hottable")
          ),
          box(
            title = "Pesticides insight",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            height = "275px",
            fluidRow(column(12, verbatimTextOutput("pest_insight"))),
            fluidRow(column(
              12,
              div(
                style = "display: flex; align-items: center; justify-content: center; padding-top: 20px;",
                downloadButton(
                  "download_pest_table",
                  "Download Table (TSV)",
                  class = "btn-link btn-lg",
                  icon = icon("download"),
                  style = "font-size: 16px;"
                )
              )
            ))
          )
        ),
        
        ###### impacts summary ######
        fluidRow(
          box(
            title = "Pesticides impact summary",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = "350px",
            fluidRow(column(
              12, valueBoxOutput("pest_totalload", width = 12)
            )),
            fluidRow(
              column(2, valueBoxOutput("pest_ecoaqu", width = 12)),
              column(2, valueBoxOutput("pest_ecoterr", width = 12)),
              column(4, valueBoxOutput("pest_envpers", width = 12)),
              column(4, valueBoxOutput("pest_humhea", width = 12)),
            )
          )
        ),
        
        #--for making the dropdown text bigger
        tags$head(
          tags$style(HTML("
    #costs_gdp + div .selectize-input {
      font-size: 20px !important;
      min-height: 45px;
      padding: 18px 18px;
    }
    #costs_gdp + div .selectize-input input {
      font-size: 20px !important;
    }
    #costs_gdp + div .selectize-dropdown {
      font-size: 20px !important;
    }
    #costs_gdp + div .selectize-dropdown-content {
      font-size: 20px !important;
    }
  "))
        ),
        
        ###### costs summary ######
        fluidRow(
          box(
            title = "Pesticides societal costs summary",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = "175px",
            fluidRow(
              column(5, valueBoxOutput("pest_costs", width = 12)),
              column(2, selectizeInput(
                "costs_gdp",
                label = NULL,
                choices = NULL,
                multiple = FALSE,
                selected = NULL,
                options = list(placeholder = "Select a GDP adjuster")
              ) %>% tagAppendAttributes(style = "font-size: 18px;")
              ),
              column(5, valueBoxOutput("pest_costs_new", width = 12))
            )
          )
        ),
        
        
        ###### information ######
        fluidRow(
          box(
            title = "Important information",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            height = "750px",
            h4("Where does the societal cost come from?", style = "color: #3c8dbc; font-weight: bold;"),
            tags$ul(
              style = "font-size: 15px; line-height: 1.8; list-style-type: disc;",
              tags$li(
                "They are calculated using the original approach used by ",
                tags$a(
                  "Pretty et al. 2010",
                  href = "https://your-url-here.com",
                  target = "_blank",
                  style = "color: #d9534f; font-weight: bold; text-decoration: underline;"
                ),
                " in Euros adjusted to 2025 prices"
              ),
              tags$li(
                "Includes costs stemming from the impacts of:",
                tags$ul(
                  style = "list-style-type: circle; margin-top: 5px;",
                  tags$li("Pesticides in sources of drinking water"),
                  tags$li("Pollution incidents and cleanup"),
                  tags$li("Fish deaths and monitoring"),
                  tags$li("Biodiversity/wildlife losses"),
                  tags$li("Cultural/landscape/tourism losses"),
                  tags$li("Bee colony losses"),
                  tags$li("Acute effects of pesticides on human health")
                )
              )
            ),
            br(),
            h4("What is the GDP adjuster doing?", style = "color: #3c8dbc; font-weight: bold;"),
            div(
              style = "font-size: 15px; line-height: 1.8;",
              p(
                "• By default, the societal costs are presented in ",
                tags$strong(style = "color: #d9534f;", "Euros"),
                " per hectare"
              ),
              p(
                "• The adjuster uses the selected country's ",
                tags$strong(style = "color: #d9534f;", "population and Gross Domestic Product (GDP)"),
                " to adjust the value"
              )
            ),
            br(),
            h4("What does 00_not listed mean?", style = "color: #3c8dbc; font-weight: bold;"),
            div(
              style = "font-size: 15px; line-height: 1.8;",
              p(
                "• The list of compounds represents those with values for at least ",
                tags$strong(style = "color: #d9534f;", "60%"),
                " of the metrics used in the calculations"
              ),
              p(
                "• Compounds with too much missing data are therefore ",
                tags$strong(style = "color: #d9534f;", "not listed")
              ),
              p(
                "• Without data to prove otherwise, the methodology assumes a ",
                tags$strong("worst-case scenario"),
                " for those compounds (consistent with the ",
                tags$strong("precautionary principle"),
                ")"
              ),
              p(
                "• If your compound is not listed, you can select ",
                tags$strong(style = "color: #d9534f;", "00_not listed"),
                " and it will assume ",
                tags$strong(style = "color: #d9534f;", "a load of 1")
              )
            ),
            br(),
            h4("What does 00_biopesticide mean?", style = "color: #3c8dbc; font-weight: bold;"),
            div(
              style = "font-size: 15px; line-height: 1.8;",
              p(
                "• The ",
                tags$a(
                  href = "https://sitem.herts.ac.uk/aeru/bpdb/index.htm",
                  # URL
                  target = "_blank",
                  tags$strong("Biopesticides Database"),
                  style = "color: #eb5e23; text-decoration: none; font-weight: bold;
                          border-bottom: 1px dotted #eb5e23;"
                ),
                " is not yet included in this methodology"
              ),
              p(
                "• Biopesticide data is ",
                tags$strong("sparse"),
                ", and requires additional considerations"
              ),
              p(
                "• Without data to prove otherwise, the methodology currently assumes ",
                tags$strong("a load of 0"),
                " for biopesticides"
              ),
              p(
                "• If you used a biopesticide, you can select ",
                tags$strong(style = "color: #5cb85c;", "00_biopesticide"),
                " and it will assume ",
                tags$strong(style = "color: #5cb85c;", "a load of 0")
              )
            )
          )
        )
        
      ),
      #--end of tab
      
      ###### Single substance tab ######
      tabItem(
        tabName = "single",
        
        ## First row
        fluidRow(
          # Substance selection box
          box(
            title = "Substance Selection",
            status = "primary",
            # "info",
            solidHeader = TRUE,
            width = 4,
            height = "275px",
            # Added consistent height
            
            # Filter options
            selectizeInput(
              "substance_category",
              label = NULL,
              choices = NULL,
              # populated from data in the server
              multiple = TRUE,
              selected = NULL,
              options = list(placeholder = "Filter by category")
            ),
            selectizeInput(
              "substance_origins",
              label = NULL,
              choices = NULL,
              # populated from data in the server
              multiple = TRUE,
              selected = NULL,
              options = list(placeholder = "Filter by origin")
            ),
            
            # Substance selection
            selectInput(
              "substance_single",
              "Select Substance:",
              choices = NULL,
              # populated from data in the server
              selected = NULL
            )
          ),
          # Substance information box
          box(
            title = "Substance Information",
            status = "primary",
            # "info",
            solidHeader = TRUE,
            width = 8,
            height = "275px",
            # Added consistent height
            verbatimTextOutput("substance_info")
          )
          
        ),
        ## Second row, two graphs (one rose and one distribution)
        fluidRow(
          #--Distribution box
          box(
            title = "Load Score Relative to All Substances",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            #height = "400px",
            #plotlyOutput("dist_plot", height = "400px")
            girafeOutput("dist_plot", height = "475px")
          ),
          
          #--Rose plot box
          box(
            title = "Load Scores by Compartment",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            
            # Checkbox positioned just below the title bar
            div(
              style = "margin-top: -10px; margin-bottom: 10px; margin-left: 5px;",
              checkboxInput(
                "detailed_view", 
                tags$span(style = "font-size: 16px; font-weight: 500;", "Detailed view"),
                value = FALSE
              )
            ),
            
            
            # Center the plot
            div(
              style = "text-align: center;",
              plotOutput("rose_plot", height = "400px")
            ),
            
            # Right-aligned download button at the bottom
            div(
              style = "text-align: right; margin-top: 10px;",
              downloadButton(
                "download_rose_plot",
                "Download Plot",
                class = "btn-primary btn-sm",
                icon = icon("download")
              )
            )
          )
        ),
        
        #--third row
        fluidRow(
          # Download Data box
          box(
            title = "Download Load Score Details",
            status = "success",
            solidHeader = TRUE,
            width = 4,
            height = "500px",
            # Added consistent height
            div(
              style = "padding: 15px;",
              p(
                "Load scores represent a relative toxicity burden, also known as a hazard score."
              ),
              br(),
              h4("Useful Links"),
              tags$ul(tags$li(
                tags$a(
                  "Pesticide Properties Database",
                  href = "https://sitem.herts.ac.uk/aeru/ppdb/",
                  target = "_blank"
                )
              )),
              tags$ul(tags$li(
                tags$a(
                  "Original paper on estimating societal costs of pesticides (Pretty et al. 2000)",
                  href = "https://www.sciencedirect.com/science/article/abs/pii/S0308521X00000317",
                  target = "_blank"
                )
              )),
              br(),
              div(
                style = "text-align: center; padding: 20px;",
                p("Download the detailed load score data for the selected substance:"),
                br(),
                downloadButton(
                  "download_ai_data",
                  "Download Data (TSV)",
                  class = "btn-success btn-lg",
                  # Changed to green
                  icon = icon("download"),
                  style = "background-color: #ffd74a; border-color: #ffd74a;"  # Custom green color
                )
              )
              
              
            )
          ),
          
          
          #--societal costs box
          box(
            title = "Societal Costs",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            height = "500px",
            
            # Center the plot
            div(style = "text-align: center;", plotOutput("cost_plot", height = "400px")),
            
            # Right-aligned download button at the bottom
            div(
              style = "text-align: right; margin-top: 10px;",
              downloadButton(
                "download_cost_plot",
                "Download Plot",
                class = "btn-primary btn-sm",
                icon = icon("download")
              )
            )
            
            
          )
        ), 
        ###### information ######
        fluidRow(
          box(
            title = "Important information",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            height = "200px",
            h4("What are the numbers that appear in the 'Detailed view'?", style = "color: #000000; font-weight: bold;"),
            div(
              style = "font-size: 15px; line-height: 1.8;",
              p(
                "• The values range from 1 to 5 and represent a ",
                tags$strong(style = "color: #d9534f;", "data quality rating"),
                ", with 1 being the lowest, and 5 being highest"
              ),
              p(
                "• Missing data is indicated with a ",
                tags$strong(style = "color: #d9534f;", "diagonal fill pattern"),
                " and a data quality score of ",
                tags$strong(style = "color: #d9534f;", "X"),
              )
            )
            )
          )
        
      ),
      #--end of tab
      
      
      ######Double substance tab ######
      tabItem(
        tabName = "double",
        fluidRow(
          # Substance1 selection
          box(
            title = "First substance selection",
            status = "primary",
            # "info",
            solidHeader = TRUE,
            width = 6,
            height = "275px",
            
            # Filter options
            selectizeInput(
              "substance_category1",
              label = NULL,
              choices = NULL,
              # populated from data in the server
              multiple = TRUE,
              selected = NULL,
              options = list(placeholder = "Filter by category")
            ),
            selectizeInput(
              "substance_origins1",
              label = NULL,
              choices = NULL,
              # populated from data in the server
              multiple = TRUE,
              selected = NULL,
              options = list(placeholder = "Filter by origin")
            ),
            selectInput(
              "substance_double1",
              "Select Substance:",
              choices = NULL,
              # populated from data in the server
              selected = NULL
            )
          ),
          
          # Substance2 selection
          box(
            title = "Second substance selection",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            height = "275px",
            
            # Filter options
            selectizeInput(
              "substance_category2",
              label = NULL,
              choices = NULL,
              # populated from data in the server
              multiple = TRUE,
              selected = NULL,
              options = list(placeholder = "Filter by category")
            ),
            selectizeInput(
              "substance_origins2",
              label = NULL,
              choices = NULL,
              # populated from data in the server
              multiple = TRUE,
              selected = NULL,
              options = list(placeholder = "Filter by origin")
            ),
            
            # Substance selection
            selectInput(
              "substance_double2",
              "Select Substance:",
              choices = NULL,
              # populated from data in the server
              selected = NULL
            )
          )
          
          
        ),
        
        #--Rose plots
        fluidRow(
          # Rose plot first substance
          box(
            title = "First Substance Load Scores",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            div(style = "text-align: center;", plotOutput("rose_plot1", height = "400px")),
            # Right-aligned download button at the bottom
            div(
              style = "text-align: right; margin-top: 10px;",
              downloadButton(
                "download_rose_plot1",
                "Download Plot",
                class = "btn-primary btn-sm",
                icon = icon("download")
              )
            )
          ),
          
          # Rose plot second substance
          box(
            title = "Second Substance Load Scores",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            div(style = "text-align: center;", plotOutput("rose_plot2", height = "400px")),
            # Right-aligned download button at the bottom
            div(
              style = "text-align: right; margin-top: 10px;",
              downloadButton(
                "download_rose_plot2",
                "Download Plot",
                class = "btn-primary btn-sm",
                icon = icon("download")
              )
            )
          )
        ),
        
        #--Societal costs plots
        fluidRow(
          # Societal costs, first substance
          box(
            title = "First Substance Societal Costs",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            div(style = "text-align: center;", plotOutput("cost_plot1", height = "400px")),
            # Right-aligned download button at the bottom
            div(
              style = "text-align: right; margin-top: 10px;",
              downloadButton(
                "download_cost_plot1",
                "Download Plot",
                class = "btn-primary btn-sm",
                icon = icon("download")
              )
            )
          ),
          
          # Cost plot second substance
          box(
            title = "Second Substance Societal Costs",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            div(style = "text-align: center;", plotOutput("cost_plot2", height = "400px")),
            # Right-aligned download button at the bottom
            div(
              style = "text-align: right; margin-top: 10px;",
              downloadButton(
                "download_cost_plot2",
                "Download Plot",
                class = "btn-primary btn-sm",
                icon = icon("download")
              )
            )
          )
        )
        
      
      
      )
    ) #--end of dashboard body
  ) #--end of dashboard page
)



# server ------------------------------------------------------------------


server <- function(input, output, session) {
  # Single substance tab =======================================================
  
  #--Populate filter lists (runs once at app startup)
  
  
  observeEvent(TRUE, {
    # Substance category filter
    updateSelectInput(
      session,
      "substance_category",
      choices = unique(data_details$compound_category) |>
        sort()
    )
    # Substance origin filter
    updateSelectInput(session,
                      "substance_origins",
                      choices = unique(data_details$compound_origin) |>
                        sort())
  }, once = TRUE)
  
  
  
  ###### Populate list of substance (reacts on filters) ######
  #--data for second tab, 1st choice
  substance_choices <- reactive({
    data_details_filtered <- data_details
    
    # Filter by origin only if an origin is selected
    if (!is.null(input$substance_origins) &&
        length(input$substance_origins) > 0) {
      data_details_filtered <-
        data_details_filtered |>
        dplyr::filter(compound_origin %in% input$substance_origins)
    }
    
    # Filter by category only if a category is selected
    if (!is.null(input$substance_category) &&
        length(input$substance_category) > 0) {
      data_details_filtered <-
        data_details_filtered |>
        dplyr::filter(compound_category %in% input$substance_category)
    }
    
    
    # Format final substance list
    data_details_filtered |>
      dplyr::pull(compound) |>
      unique() |>
      sort()
  })
  
  ###### Selected substance based on user choice ######
  observe({
    choices <- substance_choices()
    selected <- isolate(input$substance_single)
    if (!is.null(selected))
      selected <- selected[selected %in% choices]
    updateSelectInput(session,
                      "substance_single",
                      choices = choices,
                      selected = selected)
    updateSelectInput(session, "substances_compare", choices = choices)
  })
  
  # If current selection is no longer valid (e.g. after a new filter is applied), clear it
  observe({
    valid_choices <- substance_choices()
    current <- input$substance_single
    if (!is.null(current) && !current %in% valid_choices) {
      updateSelectInput(session, "substance_single", selected = "")
      #updateSelectInput(session, "substances_compare", selected = "")
    }
  })
  
  ###### Reduce data based on selected substance ######
  single_substance_data <- reactive({
    req(input$substance_single)
    d1 <- data_details[data_details$compound == input$substance_single, ]
    #d1 <- data_details[data_details$compound == "diquat", ]
    d2 <- distinct(d1[, c(
      "compound",
      "cas",
      "compound_type",
      "compound_origin",
      "compound_group",
      "tot_load_score"
    )])
    #--try to add costs...
    data_cost <- data_totloads
    adj <- pull(data_peacou[data_peacou$country == "EU", 2]) #could make a drop down at some point
    data_cost$euros_kg <- data_cost$totcost_euros_kg_ref * adj
    d3 <- data_cost[data_cost$compound == input$substance_single, ]
    #d3 <- data_cost[data_cost$compound == "diquat", ]
    combined_data <-
      merge(d2,
            d3,
            by = c("compound", "tot_load_score"),
            all = TRUE) |>
      mutate(across(all_of(c(
        "tot_load_score", "euros_kg"
      )), as.numeric))
    
    return(combined_data)
  })
  
  
  ###### Display substance data, single ######
  output$substance_info <- renderText({
    # Make it reactive to both inputs
    choices <- substance_choices()
    selected <- input$substance_single
    # Clear out if nothing selected or selection invalid
    if (is.null(selected) ||
        selected == "" || !selected %in% choices) {
      return("")
    }
    # Normal case (if a substance is selected)
    data_sub <- single_substance_data()
    if (nrow(data_sub) > 0) {
      paste0(
        "    Substance: ",
        input$substance_single,
        "\n\n",
        "          CAS: ",
        unique(data_sub$cas),
        "\n",
        "     Category: ",
        unique(data_sub$compound_type),
        "\n",
        "       Origin: ",
        unique(data_sub$compound_origin),
        "\n",
        #" Sub type: ", unique(data_sub$sub_compound_category), "\n",
        "       Family: ",
        unique(data_sub$compound_group),
        "\n\n",
        "         Load: ",
        round(unique(data_sub$tot_load_score), 2),
        "\n",
        "Societal cost: €",
        round(unique(data_sub$euros_kg), 2),
        "/kg"
      )
    }
  })
  
  ###### Display rose plot, single ######
  output$rose_plot <- renderPlot({
    req(input$substance_single)
    if (input$detailed_view) {
      fxn_Make_Detailed_Rose_Plot(compound_name = input$substance_single,
                                  data = data_details)
    } else {
      fxn_Make_Rose_Plot(compound_name = input$substance_single,
                         data = data_compartments)
    }
    
    
  })
  
  ###### Download rose plot, single ######
  output$download_rose_plot <- downloadHandler(
    filename = function() {
      paste0("rose_plot_",
             input$substance_single,
             "_",
             Sys.Date(),
             ".png")
    },
    content = function(file) {
      req(input$substance_single)
      
      # Create the same plot as in renderPlot
      p <- if (input$detailed_view) {
        fxn_Make_Detailed_Rose_Plot(compound_name = input$substance_single,
                                    data = data_details)
      } else {
        fxn_Make_Rose_Plot(compound_name = input$substance_single,
                           data = data_compartments)
      }
      
      #--Explicitly add a white background
      p <- p + theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
      
      # Save the plot
      ggsave(
        file,
        plot = p,
        device = "png",
        width = 10,
        height = 8,
        dpi = 300,
        bg = "white"
      )
    }
  )
  
  ###### Display load distribution ######
  # output$dist_plot <- renderPlotly({
  #   req(input$substance_single)
  #      p <-  fxn_Make_Distribution_Plot(compound_names = input$substance_single,
  #                        data = data_details) 
  #      ggplotly(p)
  # })
  
  output$dist_plot <- renderGirafe({
    req(input$substance_single)
    p <-  fxn_Make_Reactive_Distribution_Plot(compound_names = input$substance_single,
                                     data = data_details) 
    girafe(ggobj = p)
  })
  
  ###### Download data option ######
  output$download_ai_data <- downloadHandler(
    filename = function() {
      req(input$substance_single)
      paste0(
        "load_score_details_",
        gsub("[^A-Za-z0-9]", "_", input$substance_single),
        "_",
        Sys.Date(),
        ".tsv"
      )
    },
    content = function(file) {
      req(input$substance_single)
      data_sub <- single_substance_data()
      display_data <-
        data_sub |>
        dplyr::mutate_if(is.numeric, round, 3) #|>
      #dplyr::select(-xmax, -xmin, -xmid, -trunk)
      
      write.table(
        display_data,
        file,
        sep = "\t",
        row.names = FALSE,
        col.names = TRUE,
        quote = FALSE
      )
    }
  )
  
  
  ###### Display costs ######
  output$cost_plot <- renderPlot({
    req(input$substance_single)
    fxn_Make_Costs_Plot(
      compound_name = input$substance_single,
      data = data_compartments,
      data2 = data_peacou,
      country_adjuster = "EU"
    )
  })
  
  ###### Download costs plot ######
  output$download_cost_plot <- downloadHandler(
    filename = function() {
      paste0("cost_plot_",
             input$substance_single,
             "_",
             Sys.Date(),
             ".png")
    },
    content = function(file) {
      req(input$substance_single)
      
      p <- fxn_Make_Costs_Plot(
        compound_name = input$substance_single,
        data = data_compartments,
        data2 = data_peacou,
        country_adjuster = "EU"
      )
      
      #--Explicitly add a white background
      p <- p + theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
      
      # Save the plot
      ggsave(
        file,
        plot = p,
        device = "png",
        width = 10,
        height = 8,
        dpi = 300,
        bg = "white"
      )
    }
  )
  
  
  
  # Double substances tab =====================================================
  
  ###### Populate filter lists (runs once at app startup) ######
  
  observeEvent(TRUE, {
    # Substance category filter
    updateSelectInput(
      session,
      "substance_category1",
      choices = unique(data_details$compound_category) |>
        sort()
    )
    # Substance origin filter
    updateSelectInput(
      session,
      "substance_origins1",
      choices = unique(data_details$compound_origin) |>
        sort()
    )
    
  }, once = TRUE)
  
  observeEvent(TRUE, {
    # Substance type filter
    updateSelectInput(
      session,
      "substance_category2",
      choices = unique(data_details$compound_category) |>
        sort()
    )
    # Substance origin filter
    updateSelectInput(
      session,
      "substance_origins2",
      choices = unique(data_details$compound_origin) |>
        sort()
    )
    
  }, once = TRUE)
  
  
  
  ###### Populate list of substance (reacts on filters) ######
  #--data for second tab, 1st choice
  substance_choices1 <- reactive({
    data_details_filtered1 <- data_details
    
    # Filter by origin only if an origin is selected
    if (!is.null(input$substance_origins1) &&
        length(input$substance_origins1) > 0) {
      data_details_filtered1 <-
        data_details_filtered1 |>
        dplyr::filter(compound_origin %in% input$substance_origins1)
    }
    
    # Filter by type only if a category is selected
    if (!is.null(input$substance_category1) &&
        length(input$substance_category1) > 0) {
      data_details_filtered1 <-
        data_details_filtered1 |>
        dplyr::filter(compound_category %in% input$substance_category1)
    }
    
    
    
    # Format final substance list
    data_details_filtered1 |>
      dplyr::pull(compound) |>
      unique() |>
      sort()
  })
  
  #--data for second tab, 2nd choice
  substance_choices2 <- reactive({
    data_details_filtered2 <- data_details
    
    # Filter by origin only if an origin is selected
    if (!is.null(input$substance_origins2) &&
        length(input$substance_origins2) > 0) {
      data_details_filtered2 <-
        data_details_filtered2 |>
        dplyr::filter(compound_origin %in% input$substance_origins2)
    }
    
    # Filter by type only if a category is selected
    if (!is.null(input$substance_category2) &&
        length(input$substance_category2) > 0) {
      data_details_filtered2 <-
        data_details_filtered2 |>
        dplyr::filter(compound_category %in% input$substance_category2)
    }
    
    
    
    # Format final substance list
    data_details_filtered2 |>
      dplyr::pull(compound) |>
      unique() |>
      sort()
  })
  
  ###### Selected substance1 based on user choice ######
  observe({
    choices1 <- substance_choices1()
    selected1 <- isolate(input$substance_double1)
    if (!is.null(selected1))
      selected1 <- selected1[selected1 %in% choices1]
    updateSelectInput(session,
                      "substance_double1",
                      choices = choices1,
                      selected = selected1)
  })
  
  # If current selection is no longer valid (e.g. after a new filter is applied), clear it
  observe({
    valid_choices <- substance_choices1()
    current <- input$substance_double1
    if (!is.null(current) && !current %in% valid_choices) {
      updateSelectInput(session, "substance_double1", selected = "")
    }
  })
  
  ###### Selected substance2 based on user choice ######
  observe({
    choices2 <- substance_choices2()
    selected2 <- isolate(input$substance_double2)
    if (!is.null(selected2))
      selected2 <- selected2[selected2 %in% choices2]
    updateSelectInput(session,
                      "substance_double2",
                      choices = choices2,
                      selected = selected2)
  })
  
  # If current selection is no longer valid (e.g. after a new filter is applied), clear it
  observe({
    valid_choices <- substance_choices2()
    current <- input$substance_double2
    if (!is.null(current) && !current %in% valid_choices) {
      updateSelectInput(session, "substance_double2", selected = "")
    }
  })
  
  
  # output$rose_plot <- renderPlot({
  #   req(input$substance_single)
  #   if (input$detailed_view) {
  #     fxn_Make_Detailed_Rose_Plot(compound_name = input$substance_single,
  #                                 data = data_details)
  #   } else {
  #     fxn_Make_Rose_Plot(compound_name = input$substance_single,
  #                        data = data_compartments)
  #   }
  #
  #
  # })
  
  ###### Display rose plots ######
  output$rose_plot1 <- renderPlot({
    req(input$substance_double1)
    if (input$detailed_view2) {
      fxn_Make_Detailed_Rose_Plot(compound_name = input$substance_double1,
                                  data = data_details)
    } else {
      fxn_Make_Rose_Plot(compound_name = input$substance_double1,
                         data = data_compartments)
    }
    
  })
  
  output$rose_plot2 <- renderPlot({
    req(input$substance_double2)
    if (input$detailed_view2) {
      fxn_Make_Detailed_Rose_Plot(compound_name = input$substance_double2,
                                  data = data_details)
    } else {
      fxn_Make_Rose_Plot(compound_name = input$substance_double2,
                         data = data_compartments)
    }
    
  })
  
  # output$dist_plot_both <- renderPlot({
  #   req(input$substance_double1)
  #   fxn_Make_Distribution_Plot(
  #     compound_names = c(input$substance_double1, input$substance_double2),
  #     data = data_details
  #   )
  # })
  
  
  output$download_rose_plot1 <- downloadHandler(
    filename = function() {
      paste0("rose_plot1_",
             input$substance_double1,
             "_",
             Sys.Date(),
             ".png")
    },
    content = function(file) {
      req(input$substance_double1)
      
      # Create the same plot as in renderPlot
      p <- if (input$detailed_view2) {
        fxn_Make_Detailed_Rose_Plot(compound_name = input$substance_double1,
                                    data = data_details)
      } else {
        fxn_Make_Rose_Plot(compound_name = input$substance_double1,
                           data = data_compartments)
      }
      
      #--Explicitly add a white background
      p <- p + theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
      
      # Save the plot
      ggsave(
        file,
        plot = p,
        device = "png",
        width = 10,
        height = 8,
        dpi = 300,
        bg = "white"
      )
    }
  )
  
  
  output$download_rose_plot2 <- downloadHandler(
    filename = function() {
      paste0("rose_plot2_",
             input$substance_double2,
             "_",
             Sys.Date(),
             ".png")
    },
    content = function(file) {
      req(input$substance_double2)
      
      # Create the same plot as in renderPlot
      p <- if (input$detailed_view2) {
        fxn_Make_Detailed_Rose_Plot(compound_name = input$substance_double2,
                                    data = data_details)
      } else {
        fxn_Make_Rose_Plot(compound_name = input$substance_double2,
                           data = data_compartments)
      }
      
      #--Explicitly add a white background
      p <- p + theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
      
      # Save the plot
      ggsave(
        file,
        plot = p,
        device = "png",
        width = 10,
        height = 8,
        dpi = 300,
        bg = "white"
      )
    }
  )
  
  ###### Display costs1 ######
  output$cost_plot1 <- renderPlot({
    req(input$substance_double1)
    fxn_Make_Costs_Plot(
      compound_name = input$substance_double1,
      data = data_compartments,
      data2 = data_peacou,
      country_adjuster = "EU"
    )
  })
  
  ###### Download costs1 plot ######
  output$download_cost_plot1 <- downloadHandler(
    filename = function() {
      paste0("cost_plot1_",
             input$substance_double1,
             "_",
             Sys.Date(),
             ".png")
    },
    content = function(file) {
      req(input$substance_double1)
      
      p <- fxn_Make_Costs_Plot(
        compound_name = input$substance_double1,
        data = data_compartments,
        data2 = data_peacou,
        country_adjuster = "EU"
      )
      
      #--Explicitly add a white background
      p <- p + theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
      
      # Save the plot
      ggsave(
        file,
        plot = p,
        device = "png",
        width = 10,
        height = 8,
        dpi = 300,
        bg = "white"
      )
    }
  )
  ###### Display costs2 ######
  output$cost_plot2 <- renderPlot({
    req(input$substance_double2)
    fxn_Make_Costs_Plot(
      compound_name = input$substance_double2,
      data = data_compartments,
      data2 = data_peacou,
      country_adjuster = "EU"
    )
  })
  
  ###### Download costs2 plot ######
  output$download_cost_plot2 <- downloadHandler(
    filename = function() {
      paste0("cost_plot1_",
             input$substance_double2,
             "_",
             Sys.Date(),
             ".png")
    },
    content = function(file) {
      req(input$substance_double2)
      
      p <- fxn_Make_Costs_Plot(
        compound_name = input$substance_double2,
        data = data_compartments,
        data2 = data_peacou,
        country_adjuster = "EU"
      )
      
      #--Explicitly add a white background
      p <- p + theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
      
      # Save the plot
      ggsave(
        file,
        plot = p,
        device = "png",
        width = 10,
        height = 8,
        dpi = 300,
        bg = "white"
      )
    }
  )
  
  
  ###### Download data option ######
  #--something is funky here
  # output$download_data2 <- downloadHandler(
  #   filename = function() {
  #     req(input$substance_double1)
  #     req(input$substance_double2)
  #     paste0(
  #       "load_score_details_", #--make sure only allowed characters in name
  #       gsub(
  #         "[^A-Za-z0-9]",
  #         input$substance_double1),
  #         "_",
  #       gsub(
  #           "[^A-Za-z0-9]",
  #           input$substance_double2),
  #       "_",
  #       Sys.Date(),
  #       ".tsv"
  #     )
  #   },
  #   content = function(file) {
  #     req(input$substance_double1)
  #     #--what should go here?
  #     data_sub <-
  #       data_details |>
  #       filter(compound_name %in% c(input$substance_double1, input$substance_double2))
  #
  #     display_data2 <-
  #       data_sub |>
  #       dplyr::mutate_if(is.numeric, round, 3) |>
  #       dplyr::select(
  #         compound,
  #         compound_type,
  #         env_raw,
  #         eco.terr_raw,
  #         eco.aqua_raw,
  #         hum_raw,
  #         load_score,
  #         missing_share
  #       )
  #
  #     write.table(
  #       display_data2,
  #       file,
  #       sep = "\t",
  #       row.names = FALSE,
  #       col.names = TRUE,
  #       quote = FALSE
  #     )
  #   }
  # )
  
  # Calculate load =======================================================
  
  # Update costs_gdp selectizeInput with choices from your dataset
  observe({
    choices_vector <- unique(data_peacou$country)
    
    updateSelectizeInput(
      session,
      "costs_gdp",
      choices = choices_vector,
      selected = "EU"  # Set default selection here
    )
  })
  
  # Initialize reactive values for both tables
  values <- reactiveValues()
  
  # Initialize data frame
  observe({
    if (is.null(values$data)) {
      initial_rows <- 5
      values$data <- data.frame(
        Compound = rep("", initial_rows),
        Compound_Load = rep(0, initial_rows),
        SocietalCost = rep(0, initial_rows),
        ecotoxicity_aquatic = rep(0, initial_rows),
        ecotoxicity_terrestrial = rep(0, initial_rows),
        environmental_fate = rep(0, initial_rows),
        human_health = rep(0, initial_rows),
        QuantAppl_kgperarea = rep(0, initial_rows),
        EcoAqu_Load = rep(0, initial_rows),
        EcoTerr_Load = rep(0, initial_rows),
        EnvPers_Load = rep(0, initial_rows),
        HumHea_Load = rep(0, initial_rows),
        Total_Load = rep(0, initial_rows),
        Total_SocietalCosts = rep(0, initial_rows),
        stringsAsFactors = FALSE
      )
    }
  })
  
  # Add row functionality
  observeEvent(input$add_row, {
    if (nrow(values$data) < 50) {
      new_row <- data.frame(
        Compound = "",
        Compound_Load = 0,
        SocietalCost = 0,
        ecotoxicity_aquatic = 0,
        ecotoxicity_terrestrial = 0,
        environmental_fate = 0,
        human_health = 0,
        QuantAppl_kgperarea = 0,
        EcoAqu_Load = 0,
        EcoTerr_Load = 0,
        EnvPers_Load = 0,
        HumHea_Load = 0,
        Total_Load = 0,
        Total_SocietalCosts = 0,
        stringsAsFactors = FALSE
      )
      values$data <- rbind(values$data, new_row)
    }
  })
  
  # Remove row functionality
  observeEvent(input$remove_row, {
    if (nrow(values$data) > 1) {
      values$data <- values$data[-nrow(values$data), ]
    }
  })
  
  # Helper function to update calculations
  update_calculations <- function(data) {
    for (i in 1:nrow(data)) {
      if (data$Compound[i] != "" && !is.na(data$Compound[i])) {
        matching_row <- data_totloads[data_totloads$compound == data$Compound[i], ]
        if (nrow(matching_row) > 0) {
          # Populate hidden intermediate values
          data$ecotoxicity_aquatic[i] <- matching_row$ecotoxicity_aquatic[1]
          data$ecotoxicity_terrestrial[i] <- matching_row$ecotoxicity_terrestrial[1]
          data$environmental_fate[i] <- matching_row$environmental_fate[1]
          data$human_health[i] <- matching_row$human_health[1]
          data$Compound_Load[i] <- matching_row$tot_load_score[1]
          data$SocietalCost[i] <- matching_row$totcost_euros_kg_ref[1] * 0.5701703
          
          # Calculate loads only if quantity is applied
          if (!is.na(data$QuantAppl_kgperarea[i]) &&
              data$QuantAppl_kgperarea[i] > 0) {
            data$EcoAqu_Load[i] <- data$ecotoxicity_aquatic[i] * data$QuantAppl_kgperarea[i]
            data$EcoTerr_Load[i] <- data$ecotoxicity_terrestrial[i] * data$QuantAppl_kgperarea[i]
            data$EnvPers_Load[i] <- data$environmental_fate[i] * data$QuantAppl_kgperarea[i]
            data$HumHea_Load[i] <- data$human_health[i] * data$QuantAppl_kgperarea[i]
            data$Total_Load[i] <- data$Compound_Load[i] * data$QuantAppl_kgperarea[i]
            data$Total_SocietalCosts[i] <- data$SocietalCost[i] * data$QuantAppl_kgperarea[i]
          } else {
            data$EcoAqu_Load[i] <- 0
            data$EcoTerr_Load[i] <- 0
            data$EnvPers_Load[i] <- 0
            data$HumHea_Load[i] <- 0
            data$Total_Load[i] <- 0
            data$Total_SocietalCosts[i] <- 0
          }
        }
      } else {
        data$EcoAqu_Load[i] <- 0
        data$EcoTerr_Load[i] <- 0
        data$EnvPers_Load[i] <- 0
        data$HumHea_Load[i] <- 0
        data$Total_Load[i] <- 0
        data$Total_SocietalCosts[i] <- 0
      }
    }
    return(data)
  }
  
  # Render table - ONLY SHOW COLUMNS YOU WANT VISIBLE
  output$pest_hottable <- renderRHandsontable({
    if (!is.null(values$data)) {
      values$data <- update_calculations(values$data)
      
      # Select only the columns to display (hidden columns won't show)
      display_data <- values$data[, c(
        "Compound",
        "Compound_Load",
        "QuantAppl_kgperarea",
        #"EcoAqu_Load",
        #"EcoTerr_Load",
        #"EnvPers_Load",
        #"HumHea_Load",
        "Total_Load",
        "Total_SocietalCosts"
      )]
      
      rhandsontable(
        display_data,
        rowHeaders = TRUE,
        height = 250,
        colWidths = c(
          160,
          160,
          160,
          #100, 100, 100, 100,
          160,
          160
        )
      ) %>%
        hot_col(
          "Compound",
          type = "dropdown",
          source = as.character(unique(data_totloads$compound)),
          halign = "htCenter",
          allowInvalid = FALSE
        ) %>%
        hot_col(
          "Compound_Load",
          readOnly = TRUE,
          halign = "htCenter",
          format = "0.000"
        ) %>%
        hot_col(
          "QuantAppl_kgperarea",
          type = "numeric",
          halign = "htCenter",
          format = "0.000"
        ) %>%
        #hot_col("EcoAqu_Load", readOnly = TRUE, format = "0.000") %>%
        #hot_col("EcoTerr_Load", readOnly = TRUE, format = "0.000") %>%
        #hot_col("EnvPers_Load", readOnly = TRUE, format = "0.000") %>%
        #hot_col("HumHea_Load", readOnly = TRUE, format = "0.000") %>%
        hot_col("Total_Load", readOnly = TRUE, format = "0.000") %>%
        hot_col(
          "Total_SocietalCosts",
          readOnly = TRUE,
          halign = "htCenter",
          format = "0.00"
        ) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    }
  })
  
  
  # Update data when table is edited
  observeEvent(input$pest_hottable, {
    if (!is.null(input$pest_hottable)) {
      updated_data <- hot_to_r(input$pest_hottable)
      
      # Merge the updated visible columns back with the full data
      values$data$Compound <- updated_data$Compound
      values$data$QuantAppl_kgperarea <- updated_data$QuantAppl_kgperarea
      
      # Trigger recalculation
      values$data <- update_calculations(values$data)
    }
  })
  
  
  # Summary output
  output$pest_insight <- renderText({
    if (!is.null(values$data)) {
      # Filter to only filled rows (compounds that have been selected)
      filled_data <- values$data[values$data$Compound != "" &
                                   !is.na(values$data$Compound), ]
      
      if (nrow(filled_data) > 0) {
        grand_total <- sum(values$data$Total_Load, na.rm = TRUE)
        
        # Find min and max risk scores among filled rows
        load_min <- min(filled_data$Compound_Load, na.rm = TRUE)
        load_max <- max(filled_data$Compound_Load, na.rm = TRUE)
        risk_min <- min(filled_data$Total_Load, na.rm = TRUE)
        risk_max <- max(filled_data$Total_Load, na.rm = TRUE)
        
        # Find compounds with min and max risk scores
        min_compound <- filled_data$Compound[which(filled_data$Compound_Load == load_min)[1]]
        max_compound <- filled_data$Compound[which(filled_data$Compound_Load == load_max)[1]]
        
        # Find applications with min and max risk scores
        min_applic <- filled_data$Compound[which(filled_data$Total_Load == risk_min)[1]]
        max_applic <- filled_data$Compound[which(filled_data$Total_Load == risk_max)[1]]
        
        paste(
          # "Lowest Load Compound:",
          # "\n",
          # min_compound,
          # " (",
          # format(load_min, digits = 2, nsmall = 3),
          # ")",
          "Highest Load Compound:",
          "\n",
          max_compound,
          " (",
          format(load_max, digits = 2, nsmall = 3),
          ")",
          "\n",
          
          # "\nLowest Load Application:",
          # "\n",
          # min_applic,
          # " (",
          # format(risk_min, digits = 1, nsmall = 2),
          # " ha-1 )",
          "\n\nHighest Load Application:",
          "\n",
          max_applic,
          " (",
          format(risk_max, digits = 1, nsmall = 2),
          " ha-1 )"
          
        )
      } else {
        "No compounds have been selected yet."
      }
    }
  })
  
  
  # Value boxes for dashboard display
  #--total
  output$pest_totalload <- renderValueBox({
    if (!is.null(values$data)) {
      grand_total <- sum(values$data$Total_Load, na.rm = TRUE)
      valueBox(
        value = format(grand_total, digits = 2, nsmall = 0),
        subtitle = "Total Package Load Per Hectare",
        icon = icon("exclamation-triangle"),
        color = "red"
      )
    }
  })
  
  #--EcoAqu
  output$pest_ecoaqu <- renderValueBox({
    if (!is.null(values$data)) {
      grand_total <- sum(values$data$EcoAqu_Load, na.rm = TRUE)
      valueBox(
        value = format(grand_total, digits = 2, nsmall = 0),
        subtitle = "Ecotox-Aquatic Load (1/6 weight)",
        icon = icon("fish"),
        color = "blue"
      )
    }
  })
  
  #--EcoTerr
  output$pest_ecoterr <- renderValueBox({
    if (!is.null(values$data)) {
      grand_total <- sum(values$data$EcoTerr_Load, na.rm = TRUE)
      valueBox(
        value = format(grand_total, digits = 2, nsmall = 0),
        subtitle = "Ecotox-Terrestrial Load (1/6 weight)",
        icon = icon("crow"),
        color = "aqua"
      )
    }
  })
  
  
  #--EnvPers
  output$pest_envpers <- renderValueBox({
    if (!is.null(values$data)) {
      grand_total <- sum(values$data$EnvPers_Load, na.rm = TRUE)
      valueBox(
        value = format(grand_total, digits = 2, nsmall = 0),
        subtitle = HTML("&nbsp;<br>Environmental Persistance Load (1/3 weight)"),
        #subtitle = "Environ Persis Load (1/3 weight)\nBLANK",
        icon = icon("glass-water"),
        color = "yellow"
      )
    }
  })
  
  output$pest_humhea <- renderValueBox({
    if (!is.null(values$data)) {
      total_items <- sum(values$data$HumHea_Load, na.rm = TRUE)
      valueBox(
        value = format(total_items, digits = 2, nsmall = 0),
        subtitle = HTML("&nbsp;<br>Human Health Load (1/3 weight)"),
        #subtitle = "Human Health Load (1/3 weight)",
        icon = icon("person-breastfeeding"),
        color = "orange"
      )
    }
  })
  
  output$pest_costs <- renderValueBox({
    if (!is.null(values$data)) {
      total_costs <- round(sum(values$data$Total_SocietalCosts, na.rm = TRUE), 2)
      valueBox(
        value = paste(total_costs, "€/ha"),
        subtitle = "Total Societal Costs of Package Per Hectare",
        icon = icon("coins"),
        color = "green"
        #color = "red"
      )
    }
  })
  
  # Add this for pest_costs_new
  output$pest_costs_new <- renderValueBox({
    # Ensure both dependencies are available
    req(values$data, input$costs_gdp)
    
    # Calculate base total costs (same as pest_costs)
    total_costs <- round(sum(values$data$Total_SocietalCosts, na.rm = TRUE), 2)
    
    # Get GDP adjuster for selected country from data_peacou
    selected_country_data <- data_peacou[data_peacou$country == input$costs_gdp, ]
    
    # Get the adjustment factor
    gdp_adjuster <- selected_country_data$GDP_percapita_multiplier[1]
    gdp_EU <- data_peacou[data_peacou$country == "EU", ]$GDP_percapita_multiplier
    
    # Calculate adjusted costs
    adjusted_costs <- round(total_costs * gdp_adjuster / gdp_EU, 2)
    
    valueBox(
      value = paste(adjusted_costs, "€/ha (", input$costs_gdp, ")"),
      subtitle = paste0("Costs adjusted for ", input$costs_gdp, " population and GDP"),
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  output$download_pest_table <- downloadHandler(
    filename = function() {
      paste0("pesticide_load_table_", Sys.Date(), ".tsv")
    },
    content = function(file) {
      if (!is.null(values$data)) {
        # Get the full data with all calculations
        export_data <- values$data
        
        # Filter to only show rows with compounds selected
        export_data <- export_data[export_data$Compound != "" &
                                     !is.na(export_data$Compound), ]
        
        # Round numeric columns for cleaner export
        export_data <- export_data %>%
          mutate(across(where(is.numeric), ~ round(.x, 3)))
        
        write.table(
          export_data,
          file,
          sep = "\t",
          row.names = FALSE,
          col.names = TRUE,
          quote = FALSE
        )
      }
    }
  )
  
}

# run app -----------------------------------------------------------------
shinyApp(ui = ui, server = server)
