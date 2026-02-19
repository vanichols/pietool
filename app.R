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
        p("• The load of the application score will be calculated automatically")
      ),
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
         Nichols and Vandevoorde (2026)<br>
         Last updated: Feb 2026<br>"
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
          # Substance information box
          box(
            title = "Substance Information",
            status = "primary",
            # "info",
            solidHeader = TRUE,
            width = 6,
            height = "275px",
            # Added consistent height
            verbatimTextOutput("substance_info")
          )
          
        ),
        ## Second row, two graphs (one rose and one distribution), blank area not sure what to do with
        fluidRow(
         
          #--Distribution box
          box(
            title = "Load Score Relative to All Substances",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            plotOutput("dist_plot", height = "400px")
          ),
          
          #--Rose plot box
          box(
            title = "Load Scores by Compartment",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            #height = "500px",
            plotOutput("rose_plot", height = "400px")
          )
        ),
        
        #--third row
        fluidRow(
          # Download Data box
          box(
            title = "Download Load Score Details",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            height = "500px",
            # Added consistent height
            div(
              style = "padding: 15px;",
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
          
          # box(
          #   title = "Additional Resources",
          #   status = "info",
          #   solidHeader = TRUE,
          #   width = 3,
          #   height = "500px",
          #   div(
          #     style = "padding: 15px;",
          #     h4("About Load Scores"),
          #     p("Load scores represent a relative toxicity burden, also known as a hazard score."),
          #     br(),
          #     h4("Useful Links"),
          #     tags$ul(tags$li(
          #       tags$a(
          #         "Pesticide Properties Database",
          #         href = "https://sitem.herts.ac.uk/aeru/ppdb/",
          #         target = "_blank"
          #       )
          #     )),
          #     br(),
          #   )
          # ), 
          
          
          box(
            title = "Societal Costs",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            height = "500px",
            plotOutput("cost_plot", height = "400px")
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
      
      
      ###### Calculate load tab ######
      tabItem(
        tabName = "sys",
        # First system
        fluidRow(
          box(
            title = "Pesticides applied",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            height = "300px",
            rHandsontableOutput("pest_hottable")
          ),
          box(
            title = "Pesticides insight",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
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
            height = "500px",
            fluidRow(
              column(8, valueBoxOutput("pest_totalload", width = 12)),
              column(4, valueBoxOutput("pest_costs", width = 12))
            ),
            fluidRow(
              column(2, valueBoxOutput("pest_ecoaqu", width = 12)),
              column(2, valueBoxOutput("pest_ecoterr", width = 12)),
              column(2, valueBoxOutput("pest_envpers", width = 12)),
              column(2, valueBoxOutput("pest_humhea", width = 12)),
              column(4, 
                     div(
                       style = "display: flex; align-items: center; justify-content: center; height: 100%; padding-top: 20px;",
                       downloadButton("download_pest_table", 
                                      "Download Table (TSV)", 
                                      class = "btn-link btn-lg", 
                                      icon = icon("download"),
                                      style = "font-size: 16px;")
                     )
              )
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
    d1 <- data_details[data_details$compound == input$substance_single, ]
    #d1 <- data_details[data_details$compound == "diquat", ]    
    d2 <- distinct(d1[, c("compound", "cas", "compound_type", "compound_origin", "compound_group", "tot_load_score")])
    #--try to add costs...
    data_cost <- data_totloads
    adj <- pull(data_peacou[data_peacou$country == "EU", 2]) #could make a drop down at some point
    data_cost$euros <- data_cost$totcost_euros_kg_ref * adj
    d3 <- data_cost[data_cost$compound == input$substance_single, ]
    #d3 <- data_cost[data_cost$compound == "diquat", ]
    combined_data <- 
      merge(d2, d3, by = c("compound", "tot_load_score"), all = TRUE) |> 
      mutate(across(all_of(c("tot_load_score", "euros")), as.numeric))
    
    return(combined_data)
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
        round(unique(data_sub$euros), 2), "/kg")
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
                               data = data_compartments,
                        data2 = data_peacou,
                        country_adjuster = "EU")
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
  
  # Calculate load =======================================================
  # Initialize reactive values for both tables
  values <- reactiveValues()
  
  # Initialize data frame
  observe({
    if (is.null(values$data)) {
      initial_rows <- 8
      values$data <- data.frame(
        Compound = rep("", initial_rows),
        Compound_Load = rep(0, initial_rows),
        SocietalCost = rep(0, initial_rows),
        ecotoxicity_aquatic = rep(0, initial_rows),
        ecotoxicity_terrestrial = rep(0, initial_rows),
        environmental_fate = rep(0, initial_rows),
        human_health = rep(0, initial_rows),
        Quantity_Applied = rep(0, initial_rows),
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
        Quantity_Applied = 0,
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
          if (!is.na(data$Quantity_Applied[i]) && data$Quantity_Applied[i] > 0) {
            data$EcoAqu_Load[i] <- data$ecotoxicity_aquatic[i] * data$Quantity_Applied[i]
            data$EcoTerr_Load[i] <- data$ecotoxicity_terrestrial[i] * data$Quantity_Applied[i]
            data$EnvPers_Load[i] <- data$environmental_fate[i] * data$Quantity_Applied[i]
            data$HumHea_Load[i] <- data$human_health[i] * data$Quantity_Applied[i]
            data$Total_Load[i] <- data$Compound_Load[i] * data$Quantity_Applied[i]
            data$Total_SocietalCosts[i] <- data$SocietalCost[i] * data$Quantity_Applied[i]
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
      display_data <- values$data[, c("Compound", 
                                      "Compound_Load", 
                                      "Quantity_Applied", 
                                      "EcoAqu_Load", 
                                      "EcoTerr_Load", 
                                      "EnvPers_Load", 
                                      "HumHea_Load",
                                      "Total_Load",
                                      "Total_SocietalCosts")]
      
      rhandsontable(
        display_data,
        rowHeaders = TRUE,
        height = 250,
        colWidths = c(160, 120, 120, 100, 100, 100, 100, 100, 120)
      ) %>%
        hot_col(
          "Compound",
          type = "dropdown",
          source = as.character(unique(data_totloads$compound)),
          allowInvalid = FALSE
        ) %>%
        hot_col("Compound_Load", readOnly = TRUE, format = "0.000") %>%
        hot_col("Quantity_Applied", type = "numeric", format = "0.000") %>%
        hot_col("EcoAqu_Load", readOnly = TRUE, format = "0.000") %>%
        hot_col("EcoTerr_Load", readOnly = TRUE, format = "0.000") %>%
        hot_col("EnvPers_Load", readOnly = TRUE, format = "0.000") %>%
        hot_col("HumHea_Load", readOnly = TRUE, format = "0.000") %>%
        hot_col("Total_Load", readOnly = TRUE, format = "0.000") %>%
        hot_col("Total_SocietalCosts", readOnly = TRUE, format = "0.00") %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    }
  })
  
  
  # Update data when table is edited
  observeEvent(input$pest_hottable, {
    if (!is.null(input$pest_hottable)) {
      updated_data <- hot_to_r(input$pest_hottable)
      
      # Merge the updated visible columns back with the full data
      values$data$Compound <- updated_data$Compound
      values$data$Quantity_Applied <- updated_data$Quantity_Applied
      
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
  
  
  # Value boxes for dashboard display---NEED TO ADD THE COMPARTMENTS
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
        subtitle = "Environ Persis Load (1/3 weight)",
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
        subtitle = "Human Health Load (1/3 weight)",
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
  
  output$download_pest_table <- downloadHandler(
    filename = function() {
      paste0("pesticide_load_table_", Sys.Date(), ".tsv")
    },
    content = function(file) {
      if (!is.null(values$data)) {
        # Get the full data with all calculations
        export_data <- values$data
        
        # Filter to only show rows with compounds selected
         export_data <- export_data[export_data$Compound != "" & !is.na(export_data$Compound), ]
        
        # Round numeric columns for cleaner export
        export_data <- export_data %>%
          mutate(across(where(is.numeric), ~round(.x, 3)))
        
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
