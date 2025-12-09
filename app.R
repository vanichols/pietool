library(shiny)
library(rhandsontable)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(ggnewscale)
library(ggrepel)
library(scales)
library(patchwork)

# global ------------------------------------------------------------------

data_details <- read_rds("data/processed/data_details.RDS")
data_compartments <- read_rds("data/processed/data_compartments.RDS")
data_totloads <- read_rds("data/processed/data_totloads.RDS")

data_peacountry <- read_rds("data/processed/data_peacountry.RDS")


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
      menuItem(
        "  Single Substance View",
        tabName = "single",
        icon = icon("flask")
      ),
      menuItem(
        "  Susbtance Comparison View",
        tabName = "double",
        icon = icon("flask-vial")
      ),
      menuItem("  System Insights", tabName = "sys", icon = icon("bug"))
    ),
    
    # Rose plot option for detailed figure or not
    conditionalPanel(
      condition = "input.sidebar_menu == 'single'",
      h4("Plot Options"),
      checkboxInput("detailed_view", "Detailed plot view", value = FALSE),
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
      h4("Table Instructions", style = "padding-left: 15px; color: white;"),
      div(
        style = "padding-left: 25px; padding-right: 25px; color: white; font-size: 12px;",
        p("• Select a compound from the dropdown"),
        p("• Load score will auto-populate"),
        p(
          "• Enter the quantity of compound applied (in consistent units for the entire table)"
        ),
        p("• Compound's risk score will be calculated automatically")
      ),
      br(),
      div(
        style = "padding-left: 15px;",
        actionButton("add_row", "Add Row", class = "btn-primary btn-sm", style = "margin-bottom: 10px;"),
        br(),
        actionButton("remove_row", "Remove Row", class = "btn-warning btn-sm", style = "margin-bottom: 15px;"),
        # br(),
        # numericInput("max_rows", "Max Rows:", value = 5, min = 1, max = 50, width = "150px")
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
        #src = "adopt-ipm_logo-clean.png",
        src = "test.png",
        height = "50px",
        width = "auto",
        style = "margin-bottom: 5px;",
        onerror = "this.style.display='none'; console.log('Image failed to load');"
      ),
      br(),
      HTML(
        "<a href='https://adopt-ipm.eu/' target='_blank'>adopt-ipm.eu</a><br>
         Nichols and Vandevoorde (2025)<br>
         Last updated: Nov 2025<br>"
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
        # Custom green title
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
              tags$strong("Single substance view", style = "color: #eb5e23;"),
              " presents detailed information on the impact of substances used in agricultural settings (as calculated by the ",
              tags$em("Harmonized Pesticide Load Index", style = "color: #8e44ad;"),
              " and the ",
              tags$em("Pesticide Environmental Accounting (PEA)", style = "color: #8e44ad;"),
              " methodologies)"
            ),
            tags$li(
              tags$strong("Substance comparison view", style = "color: #27ae60;"),
              " allows side-by-side comparison of substance impacts"
            ),
            tags$li(
              tags$strong("System insights", style = "color: #f39c12;"),
              " allows users to see the most and least toxic components of a pesticide package"
            )
          ),
          
          hr(style = "margin: 30px 0; border-top: 2px solid #bdc3c7;"),
          
          h4("Additional Resources", style = "color: #2c3e50; margin-bottom: 15px;"),
          tags$ul(
            style = "line-height: 2; font-size: 15px;",
            tags$li(
              "Read the ",
              tags$strong("dissertation", style = "color: #2980b9;"),
              " describing the development of the ",
              tags$em("Harmonized Pesticide Load Index", style = "color: #8e44ad;"),
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
              "Read the ",
              tags$strong("publication", style = "color: #2980b9;"),
              " describing the calculation of the ",
              tags$em("Harmonized Pesticide Load Index", style = "color: #8e44ad;"),
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
              "Read the ",
              tags$strong("publication", style = "color: #2980b9;"),
              " describing the calculation of societal costs of pesticide use using the ",
              tags$em("Pesticide Environmental Accounting (PEA)", style = "color: #8e44ad;"),
              " tool:",
              tags$a(
                "Leach and Mumford 2008",
                href = "https://www.sciencedirect.com/science/article/abs/pii/S0269749107001492?via%3Dihub",
                target = "_blank",
                style = "color: #eb5e23; text-decoration: none; font-weight: bold;
                          border-bottom: 1px dotted #eb5e23;"
              )
            ),
            tags$li(
              "Read the ",
              tags$strong("accompanying publication for the PIE and PESTO tools", style = "color: #2980b9;"),
              " to this dashboard: ",
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
      #--end tab
      
      ###### Single Substance Tab ######
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
            width = 6,
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
          
          # Download Data box - replaced the data table
          box(
            title = "Download Load Score Details",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            height = "275px",
            # Added consistent height
            div(
              style = "text-align: center; padding: 20px;",
              p("Download the detailed load score data for the selected substance:"),
              br(),
              downloadButton(
                "download_data",
                "Download Data (TSV)",
                class = "btn-success btn-lg",
                # Changed to green
                icon = icon("download"),
                style = "background-color: #ffd74a; border-color: #ffd74a;"  # Custom green color
              )
              
            )
          )
        ),
        ## Second row, two graphs (one rose and one distribution), blank area not sure what to do with
        fluidRow(
          #--Rose plot box
          box(
            title = "Load Scores by Compartment",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("rose_plot", height = "400px")
          ),
          # Substance information box
          box(
            title = "Substance Information",
            status = "primary",
            # "info",
            solidHeader = TRUE,
            width = 6,
            height = "400px",
            # Added consistent height
            verbatimTextOutput("substance_info")
          )
        ),
        
        #--third row
        fluidRow(
          #--Distribution box
          box(
            title = "Load Score Relative to All Substances",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("dist_plot", height = "400px")
          ),
          box(
            title = "Societal Costs",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("cost_plot", height = "400px")
          )
        ),
         
        #--fourth row
        fluidRow(
          # Information and links box
          box(
            title = "Additional Resources",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            height = "300px",
            div(
              style = "padding: 15px;",
              h4("About Load Scores"),
              p("Load scores represent a relative toxicity burden, also known as a hazard score."),
              br(),
              h4("Useful Links"),
              tags$ul(tags$li(
                tags$a(
                  "Pesticide Properties Database",
                  href = "https://sitem.herts.ac.uk/aeru/ppdb/",
                  target = "_blank"
                )
              )),
              br(),
            )
          )
        )
        
      ),
      #--end of tab
      
      ######Substance comparison tab ######
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
            status = "primary",
            # "info",
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
        
        fluidRow(
          # Rose plot first substance
          box(
            title = "First Substance Load Scores",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("rose_plot1", height = "400px")
          ),
          
          # Rose plot second substance
          box(
            title = "Second Substance Load Scores",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("rose_plot2", height = "400px")
          )
        ),
        
        fluidRow(
          #--figure with distributions
          box(
            title = "Load Score(s) Relative to All Substances",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotOutput("dist_plot_both", height = "500px")
          )
        )
        
        
      ),
      #--end of tab
      
      
      ###### System insights tab ######
      tabItem(
        tabName = "sys",
        # First system
        fluidRow(
          box(
            title = "Pesticides applied",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            height = "300px",
            rHandsontableOutput("pest_hottable")
          ),
          box(
            title = "Pesticides insight",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            height = "300px",
            verbatimTextOutput("pest_insight")
          )
        ),
        fluidRow(
          box(
            title = "Pesticides impact summary",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = "175px",
            fluidRow(
              column(4, valueBoxOutput("pest_totalrisk", width = 12)),
              column(4, valueBoxOutput("pest_itemcount", width = 12)),
              column(4, valueBoxOutput("pest_rows", width = 12))
            )
          )
        )
      )
      #--end of tab
      
      
      
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
    data_details <- data_details
    data_details[data_details$compound == input$substance_single, ]
  })
  
  ###### Display substance data ######
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
        "Substance: ",
        input$substance_single,
        "\n\n",
        "      CAS: ",
        unique(data_sub$cas),
        "\n",
        " Category: ",
        unique(data_sub$compound_type),
        "\n",
        "   Origin: ",
        unique(data_sub$compound_origin),
        "\n",
        #" Sub type: ", unique(data_sub$sub_compound_category), "\n",
        "   Family: ",
        unique(data_sub$compound_group),
        "\n\n",
        "     Load: ",
        round(unique(data_sub$tot_load_score), 2)
      )
    }
  })
  
  ###### Display load visualization as rose plot ######
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
  
  
  ###### Display load on distribution ######
  output$dist_plot <- renderPlot({
    req(input$substance_single)
    fxn_Make_Distribution_Plot(compound_names = input$substance_single,
                               data = data_details)
  })
  
  ###### Display costs ######
  output$cost_plot <- renderPlot({
    req(input$substance_single)
    fxn_Make_Costs_Plot(compound_name = input$substance_single,
                               data = data_compartments)
  })
  
  ###### Download data option ######
  output$download_data <- downloadHandler(
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
        dplyr::mutate_if(is.numeric, round, 3) |>
        dplyr::select(-xmax, -xmin, -xmid, -trunk)
      
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
  
  # double substances tab =====================================================
  
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
  
  
  ###### Display HPL visualisation graph ######
  output$rose_plot1 <- renderPlot({
    req(input$substance_double1)
    fxn_Make_Rose_Plot(compound_name = input$substance_double1,
                       data = data_compartments)
  })
  
  output$rose_plot2 <- renderPlot({
    req(input$substance_double2)
    fxn_Make_Rose_Plot(compound_name = input$substance_double2,
                       data = data_compartments)
  })
  
  output$dist_plot_both <- renderPlot({
    req(input$substance_double1)
    fxn_Make_Distribution_Plot(
      compound_names = c(input$substance_double1, input$substance_double2),
      data = data_details
    )
  })
  
  
  
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
  
  # System insights =======================================================
  # Initialize reactive values for both tables
  values <- reactiveValues()
  
  # Initialize both data frames
  observe({
    if (is.null(values$data)) {
      initial_rows <- 8
      values$data <- data.frame(
        Compound = rep("", initial_rows),
        Load_Score = rep(0, initial_rows),
        Quantity_Applied = rep(0, initial_rows),
        Risk_Score = rep(0, initial_rows),
        stringsAsFactors = FALSE
      )
    }
    
  })
  
  # Add row functionality - affects both tables
  observeEvent(input$add_row, {
    if (nrow(values$data) < 50) {
      new_row <- data.frame(
        Compound = "",
        Load_Score = 0,
        Quantity_Applied = 0,
        Risk_Score = 0,
        stringsAsFactors = FALSE
      )
      values$data <- rbind(values$data, new_row)
    }
  })
  
  # Remove row functionality - affects both tables
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
          data$Load_Score[i] <- matching_row$tot_load_score[1]
          # Calculate Risk_Score
          if (!is.na(data$Quantity_Applied[i]) &&
              data$Quantity_Applied[i] > 0) {
            data$Risk_Score[i] <- data$Load_Score[i] * data$Quantity_Applied[i]
          } else {
            data$Risk_Score[i] <- 0
          }
        }
      } else {
        data$Load_Score[i] <- 0
        data$Risk_Score[i] <- 0
      }
    }
    return(data)
  }
  
  # Render table
  output$pest_hottable <- renderRHandsontable({
    if (!is.null(values$data)) {
      values$data <- update_calculations(values$data)
      
      rhandsontable(
        values$data,
        rowHeaders = TRUE,
        height = 250,
        colWidths = c(180, 100, 140, 100)
      ) %>%
        hot_col(
          "Compound",
          type = "dropdown",
          source = as.character(unique((
            data_details$compound
          ))),
          allowInvalid = FALSE
        ) %>%
        hot_col(
          "Load_Score",
          readOnly = TRUE,
          type = "numeric",
          format = "0.000"
        ) %>%
        hot_col("Quantity_Applied", type = "numeric", format = "0.000") %>%
        hot_col("Risk_Score", readOnly = TRUE, format = "0.000") %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    }
  })
  
  # Update data when table is edited
  observeEvent(input$pest_hottable, {
    if (!is.null(input$pest_hottable)) {
      # Get the updated data
      updated_data <- hot_to_r(input$pest_hottable)
      # Process each row for auto-population and calculations
      for (i in 1:nrow(updated_data)) {
        if (!is.na(updated_data$Compound[i]) &&
            updated_data$Compound[i] != "") {
          # Auto-populate load score based on Compound
          matching_row <- data_totloads[data_totloads$compound == updated_data$Compound[i], ]
          if (nrow(matching_row) > 0) {
            updated_data$Load_Score[i] <- matching_row$tot_load_score[1]
          }
          
          # Calculate Risk_Score (Load_Score * Quantity_Applied)
          if (!is.na(updated_data$Quantity_Applied[i]) &&
              updated_data$Quantity_Applied[i] > 0) {
            updated_data$Risk_Score[i] <- updated_data$Load_Score[i] * updated_data$Quantity_Applied[i]
          } else {
            updated_data$Risk_Score[i] <- 0
          }
        } else {
          # Reset values1 if no Compound selected
          updated_data$Load_Score[i] <- 0
          updated_data$Risk_Score[i] <- 0
        }
      }
      
      values$data <- updated_data
    }
  })
  
  
  # Summary output
  output$pest_insight <- renderText({
    if (!is.null(values$data)) {
      # Filter to only filled rows (compounds that have been selected)
      filled_data <- values$data[values$data$Compound != "" &
                                   !is.na(values$data$Compound), ]
      
      if (nrow(filled_data) > 0) {
        grand_total <- sum(values$data$Risk_Score, na.rm = TRUE)
        
        # Find min and max risk scores among filled rows
        risk_min <- min(filled_data$Risk_Score, na.rm = TRUE)
        risk_max <- max(filled_data$Risk_Score, na.rm = TRUE)
        
        # Find applications with min and max risk scores
        min_applic <- filled_data$Compound[which(filled_data$Risk_Score == risk_min)[1]]
        max_applic <- filled_data$Compound[which(filled_data$Risk_Score == risk_max)[1]]
        
        paste(
          "Lowest Risk Application:",
          "\n",
          min_applic,
          " (",
          format(risk_min, digits = 2, nsmall = 2),
          ")",
          "\n\nHighest Risk Application:",
          "\n",
          max_applic,
          " (",
          format(risk_max, digits = 2, nsmall = 2),
          ")"
        )
      } else {
        "No compounds have been selected yet."
      }
    }
  })
  
  
  # Value boxes for dashboard display
  output$pest_totalrisk <- renderValueBox({
    if (!is.null(values$data)) {
      grand_total <- sum(values$data$Risk_Score, na.rm = TRUE)
      valueBox(
        value = format(grand_total, digits = 2, nsmall = 0),
        subtitle = "Total Risk Score",
        icon = icon("exclamation-triangle"),
        color = "red"
      )
    }
  })
  
  output$pest_itemcount <- renderValueBox({
    if (!is.null(values$data)) {
      total_items <- sum(values$data$Quantity_Applied, na.rm = TRUE)
      valueBox(
        value = format(total_items, digits = 2, nsmall = 2),
        subtitle = "Total Quantity of Compounds Applied",
        icon = icon("cubes"),
        color = "blue"
        #color = "red"
      )
    }
  })
  
  output$pest_rows <- renderValueBox({
    if (!is.null(values$data)) {
      filled_rows <- sum(values$data$Compound != "", na.rm = TRUE)
      valueBox(
        value = filled_rows,
        subtitle = "Number of Compound Applications Entered",
        icon = icon("list"),
        color = "yellow"
        #color = "red"
      )
    }
  })
  
  
}

# run app -----------------------------------------------------------------
shinyApp(ui = ui, server = server)
