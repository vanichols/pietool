library(shiny)
library(rhandsontable)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(ggnewscale)
library(ggrepel)

#mmissing some comma or something

# global ------------------------------------------------------------------

# Source utility functions (rose plot, distribution plot)
source("R/utils.R")

data_hpli <- read_rds("data/processed/data_hpli.RDS")
data_betas <- read_rds("data/processed/data_betas.RDS")
data_example <- read_rds("data/processed/data_example.RDS")

# ui ----------------------------------------------------------------------


ui <- shinydashboard::dashboardPage(
  ###### Header ##################################################################
  shinydashboard::dashboardHeader(title = "ADOPT-IPM online performance assessment tool"),
  
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
        tabName = "subcomp",
        icon = icon("flask-vial")
      ),
      menuItem(
        "  Single System Insights",
        tabName = "sys",
        icon = icon("bug")
      ),
      menuItem(
        "  System Comparison",
        tabName = "syscomp",
        icon = icon("bugs")
      ),
      menuItem(
        "  Example case study",
        tabName = "example",
        icon = icon("bacon")
      )
      
    ),
    
    # Pesticide data entry specific sidebar content
    conditionalPanel(
      condition = "input.sidebar_menu == 'sys'",
      br(),
      h4("Table Instructions", style = "padding-left: 15px; color: white;"),
      div(
        style = "padding-left: 15px; color: white; font-size: 12px;",
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
          title = "Welcome to PESTO",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          h3("Getting Started"),
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
              ")"
            ),
            tags$li(
              tags$strong("Substance comparison view", style = "color: #eb5e23;"),
              " allows side-by-side comparison of substance impacts"
            ),
            tags$li(
              tags$strong("Single system insights", style = "color: #f39c12;"),
              " presents a wholistic performance of a management system (based on the ",
              tags$em("Harmonized Pesticide Load Index", style = "color: #8e44ad;"),
              ")"
            ),
            tags$li(
              tags$strong("System comparison", style = "color: #f39c12;"),
              " allows side-by-side comparison of performances"
            ),
            tags$li(
              tags$strong("Example case study", style = "color: #27ae60;"),
              " presents an example comparing field cropping and strip cropping in the Netherlands"
            )
          ),
          
          hr(style = "margin: 30px 0; border-top: 2px solid #bdc3c7;"),
          
          h4("Additional Resources", style = "color: #2c3e50; margin-bottom: 15px;"),
          tags$ul(
            style = "line-height: 2; font-size: 15px;",
            tags$li(
              "Read the ",
              tags$strong("dissertation", style = "color: #2980b9;"),
              " describing calculation of the ",
              tags$em("Harmonized Pesticide Load Index", style = "color: #8e44ad;"),
              " (publication is in review): ",
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
              tags$strong("EU Horizon 2020 project deliverable", style = "color: #2980b9;"),
              " describing the performance tool this methodology is based on: ",
              tags$a(
                "Benefits of IPM to endusers",
                href = "https://cordis.europa.eu/project/id/633999/results",
                target = "_blank",
                style = "color: #f39c12; text-decoration: none; font-weight: bold;
                          border-bottom: 1px dotted #f39c12;"
              )
            ),
            tags$li(
              "Read the ",
              tags$strong("accompanying publication", style = "color: #2980b9;"),
              " to this dashboard: ",
              tags$a(
                "Publication in progress, here is the project website",
                href = "https://adopt-ipm.eu/",
                target = "_blank",
                style = "color: #27ae60; text-decoration: none; font-weight: bold;
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
      )
      ), 
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
            width = 4,
            height = "275px",
            # Added consistent height
            verbatimTextOutput("substance_info")
          ),
          
          # Download Data box - replaced the data table
          box(
            title = "Download Load Score Details",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
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
            width = 4,
            plotOutput("rose_plot", height = "500px")
          ),
          #--Distribution box
          box(
            title = "Load Score Relative to All Substances",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            plotOutput("dist_plot", height = "500px")
          ),
          # Information and links box
          box(
            title = "Additional Resources",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            div(
              style = "padding: 15px;",
              h4("About Load Scores"),
              p("Load scores represent a relative toxicity burden ."),
              p(
                "The visualization shows a substance's load scores for each compartment, as calculated by Vandervoode et al. (in review)"
              ),
              br(),
              h4("Useful Links"),
              tags$ul(tags$li(
                tags$a(
                  "Pesticide Properties Database",
                  href = "https://sitem.herts.ac.uk/aeru/ppdb/",
                  target = "_blank"
                )
              ), tags$li(
                tags$a(
                  "PhD manuscript with more details and background",
                  href = "https://sytra.be/publication/three-tools-reduction-pesticide-impacts/",
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
        tabName = "subcomp",
        fluidRow(
          # Substance1 selection
          box(
            title = "First substance selection",
            status = "primary",
            # "info",
            solidHeader = TRUE,
            width = 4,
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
            width = 4,
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
          ),
          
          # Blank space
          column(width = 4)
          
          # # Download Data box (replaced the blank space)
          # box(
          #   title = "Download Load Score Details",
          #   status = "primary",
          #   solidHeader = TRUE,
          #   width = 4,
          #   height = "275px",
          #   # Added consistent height
          #   div(
          #     style = "text-align: center; padding: 20px;",
          #     p("Download the detailed load score data for the selected substance:"),
          #     br(),
          #     downloadButton(
          #       "download_data2",
          #       "Download Data (TSV)",
          #       class = "btn-success btn-lg",
          #       # Changed to green
          #       icon = icon("download"),
          #       style = "background-color: #ffd74a; border-color: #ffd74a;"  # Custom green color
          #     )
          #
          #   )
          # )
          
        ),
        
        fluidRow(
          # Rose plot first substance
          box(
            title = "First Substance Load Scores",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            plotOutput("rose_plot1", height = "500px")
          ),
          
          # Rose plot second substance
          box(
            title = "Second Substance Load Scores",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            plotOutput("rose_plot2", height = "500px")
          ),
          #--figure with distributions
          box(
            title = "Load Score(s) Relative to All Substances",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            plotOutput("dist_plot_both", height = "500px")
          ),
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
            height = "225px",
            rHandsontableOutput("pest_hottable")
          ),
          box(
            title = "Pesticides insight",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            height = "225px",
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
      ),
      #--end of tab
      
      ###### Two System comparison tab ######
      tabItem(tabName = "syscomp", fluidRow(
        # Custom green title
        box(
          title = "Coming soon",
          status = "primary",
          solidHeader = TRUE,
          width = 12
        )
      )),
      #--end tab
      
      
      ###### Example case study tab ######
      tabItem(tabName = "example", fluidRow(
        # Custom green title
        box(
          title = "Coming soon",
          status = "primary",
          solidHeader = TRUE,
          width = 12
        )
      )) #--end tab
      
    ) #--end of dashboard body
  ) #--end of dashboard page
)



# server ------------------------------------------------------------------


server <- function(input, output, session) {
  # Single substance tab =======================================================
  
  #--Populate filter lists (runs once at app startup)
  
  
  observeEvent(TRUE, {
    # Substance category filter
    updateSelectInput(session,
                      "substance_category",
                      choices = unique(data_hpli$compound_category) |>
                        sort())
    # Substance origin filter
    updateSelectInput(session,
                      "substance_origins",
                      choices = unique(data_hpli$compound_origin) |>
                        sort())
  }, once = TRUE)
  
  
  
  ###### Populate list of substance (reacts on filters) ######
  #--data for second tab, 1st choice
  substance_choices <- reactive({
    data_hpli_filtered <- data_hpli
    
    # Filter by origin only if an origin is selected
    if (!is.null(input$substance_origins) &&
        length(input$substance_origins) > 0) {
      data_hpli_filtered <-
        data_hpli_filtered |>
        dplyr::filter(compound_origin %in% input$substance_origins)
    }
    
    # Filter by category only if a category is selected
    if (!is.null(input$substance_category) &&
        length(input$substance_category) > 0) {
      data_hpli_filtered <-
        data_hpli_filtered |>
        dplyr::filter(compound_category %in% input$substance_category)
    }
    
    
    # Format final substance list
    data_hpli_filtered |>
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
    data_hpli <- data_hpli
    data_hpli[data_hpli$compound == input$substance_single, ]
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
        round(unique(data_sub$load_score), 3)
      )
    }
  })
  
  ###### Display load visualization as rose plot ######
  output$rose_plot <- renderPlot({
    req(input$substance_single)
    fxn_Make_Rose_Plot(compound_name = input$substance_single,
                       data = data_hpli)
  })
  
  ###### Display load on distribution ######
  output$dist_plot <- renderPlot({
    req(input$substance_single)
    fxn_Make_Distribution_Plot(compound_names = input$substance_single,
                               data = data_hpli)
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
        dplyr::select(
          compound,
          compound_type,
          env_raw,
          eco.terr_raw,
          eco.aqua_raw,
          hum_raw,
          load_score,
          missing_share
        )
      
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
  # System insights =======================================================
  # Initialize reactive values for both tables
  values <- reactiveValues()
  
  # Initialize both data frames
  observe({
    if (is.null(values$data)) {
      initial_rows <- 5
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
        # Look up load score based on Compound
        matching_row <- data_hpli[data_hpli$compound == data$Compound[i], ]
        if (nrow(matching_row) > 0) {
          data$Load_Score[i] <- matching_row$load_score[1]
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
          source = as.character(data_hpli$compound),
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
          matching_row <- data_hpli[data_hpli$compound == updated_data$Compound[i], ]
          if (nrow(matching_row) > 0) {
            updated_data$Load_Score[i] <- matching_row$load_score[1]
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
        
        # Find compounds with min and max risk scores
        min_compound <- filled_data$Compound[which(filled_data$Risk_Score == risk_min)[1]]
        max_compound <- filled_data$Compound[which(filled_data$Risk_Score == risk_max)[1]]
        
        paste(
          "Lowest Risk Application:",
          "\n",
          min_compound,
          " (",
          format(risk_min, digits = 2, nsmall = 2),
          ")",
          "\n\nHighest Risk Application:",
          "\n",
          max_compound,
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
        #color = "blue"
        color = "red"
      )
    }
  })
  
  output$pest_rows <- renderValueBox({
    if (!is.null(values$data)) {
      filled_rows <- sum(values$data$Compound != "", na.rm = TRUE)
      valueBox(
        value = filled_rows,
        subtitle = "Number of Applications Entered",
        icon = icon("list"),
        #color = "yellow"
        color = "red"
      )
    }
  })
  
  
  # System comparison =======================================================
  # Initialize reactive values for both tables
  values1 <- reactiveValues()
  values2 <- reactiveValues()
  
  # Initialize both data frames
  observe({
    if (is.null(values1$data)) {
      initial_rows <- 5
      values1$data <- data.frame(
        Compound = rep("", initial_rows),
        Load_Score = rep(0, initial_rows),
        Quantity_Applied = rep(0, initial_rows),
        Risk_Score = rep(0, initial_rows),
        stringsAsFactors = FALSE
      )
    }
    
    if (is.null(values2$data)) {
      initial_rows <- 5
      values2$data <- data.frame(
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
    if (nrow(values1$data) < 50) {
      new_row <- data.frame(
        Compound = "",
        Load_Score = 0,
        Quantity_Applied = 0,
        Risk_Score = 0,
        stringsAsFactors = FALSE
      )
      values1$data <- rbind(values1$data, new_row)
      values2$data <- rbind(values2$data, new_row)
    }
  })
  
  # Remove row functionality - affects both tables
  observeEvent(input$remove_row, {
    if (nrow(values1$data) > 1) {
      values1$data <- values1$data[-nrow(values1$data), ]
      values2$data <- values2$data[-nrow(values2$data), ]
    }
  })
  
  # Helper function to update calculations
  update_calculations <- function(data) {
    for (i in 1:nrow(data)) {
      if (data$Compound[i] != "" && !is.na(data$Compound[i])) {
        # Look up load score based on Compound
        matching_row <- data_hpli[data_hpli$compound == data$Compound[i], ]
        if (nrow(matching_row) > 0) {
          data$Load_Score[i] <- matching_row$load_score[1]
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
  
  # Render table 1
  output$hot_table1 <- renderRHandsontable({
    if (!is.null(values1$data)) {
      values1$data <- update_calculations(values1$data)
      
      rhandsontable(
        values1$data,
        rowHeaders = TRUE,
        height = 250,
        colWidths = c(180, 100, 140, 100)
      ) %>%
        hot_col(
          "Compound",
          type = "dropdown",
          source = as.character(data_hpli$compound),
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
  
  # Render table 2
  output$hot_table2 <- renderRHandsontable({
    if (!is.null(values2$data)) {
      values2$data <- update_calculations(values2$data)
      
      rhandsontable(
        values2$data,
        rowHeaders = TRUE,
        height = 250,
        colWidths = c(180, 100, 140, 100)
      ) %>%
        hot_col(
          "Compound",
          type = "dropdown",
          source = as.character(data_hpli$compound),
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
  observeEvent(input$hot_table1, {
    if (!is.null(input$hot_table1)) {
      # Get the updated data
      updated_data <- hot_to_r(input$hot_table1)
      
      # Process each row for auto-population and calculations
      for (i in 1:nrow(updated_data)) {
        if (!is.na(updated_data$Compound[i]) &&
            updated_data$Compound[i] != "") {
          # Auto-populate load score based on Compound
          matching_row <- data_hpli[data_hpli$compound == updated_data$Compound[i], ]
          if (nrow(matching_row) > 0) {
            updated_data$Load_Score[i] <- matching_row$load_score[1]
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
      
      values1$data <- updated_data
    }
  })
  
  # Update data when table is edited
  observeEvent(input$hot_table2, {
    if (!is.null(input$hot_table2)) {
      # Get the updated data
      updated_data <- hot_to_r(input$hot_table2)
      
      # Process each row for auto-population and calculations
      for (i in 1:nrow(updated_data)) {
        if (!is.na(updated_data$Compound[i]) &&
            updated_data$Compound[i] != "") {
          # Auto-populate load score based on Compound
          matching_row <- data_hpli[data_hpli$compound == updated_data$Compound[i], ]
          if (nrow(matching_row) > 0) {
            updated_data$Load_Score[i] <- matching_row$load_score[1]
          }
          
          # Calculate Risk_Score (Load_Score * Quantity_Applied)
          if (!is.na(updated_data$Quantity_Applied[i]) &&
              updated_data$Quantity_Applied[i] > 0) {
            updated_data$Risk_Score[i] <- updated_data$Load_Score[i] * updated_data$Quantity_Applied[i]
          } else {
            updated_data$Risk_Score[i] <- 0
          }
        } else {
          # Reset values2 if no Compound selected
          updated_data$Load_Score[i] <- 0
          updated_data$Risk_Score[i] <- 0
        }
      }
      
      values2$data <- updated_data
    }
  })
  
  # Summary output
  output$summary1 <- renderText({
    if (!is.null(values1$data)) {
      # Filter to only filled rows (compounds that have been selected)
      filled_data <- values1$data[values1$data$Compound != "" &
                                    !is.na(values1$data$Compound), ]
      
      if (nrow(filled_data) > 0) {
        grand_total <- sum(values1$data$Risk_Score, na.rm = TRUE)
        
        # Find min and max risk scores among filled rows
        risk_min <- min(filled_data$Risk_Score, na.rm = TRUE)
        risk_max <- max(filled_data$Risk_Score, na.rm = TRUE)
        
        # Find compounds with min and max risk scores
        min_compound <- filled_data$Compound[which(filled_data$Risk_Score == risk_min)[1]]
        max_compound <- filled_data$Compound[which(filled_data$Risk_Score == risk_max)[1]]
        
        paste(
          "Lowest Risk Application:",
          "\n",
          min_compound,
          " (",
          format(risk_min, digits = 2, nsmall = 2),
          ")",
          "\n\nHighest Risk Application:",
          "\n",
          max_compound,
          " (",
          format(risk_max, digits = 2, nsmall = 2),
          ")"
        )
      } else {
        "No compounds have been selected yet."
      }
    }
  })
  
  output$summary2 <- renderText({
    if (!is.null(values2$data)) {
      # Filter to only filled rows (compounds that have been selected)
      filled_data <- values2$data[values2$data$Compound != "" &
                                    !is.na(values2$data$Compound), ]
      
      if (nrow(filled_data) > 0) {
        grand_total <- sum(values2$data$Risk_Score, na.rm = TRUE)
        
        # Find min and max risk scores among filled rows
        risk_min <- min(filled_data$Risk_Score, na.rm = TRUE)
        risk_max <- max(filled_data$Risk_Score, na.rm = TRUE)
        
        # Find compounds with min and max risk scores
        min_compound <- filled_data$Compound[which(filled_data$Risk_Score == risk_min)[1]]
        max_compound <- filled_data$Compound[which(filled_data$Risk_Score == risk_max)[1]]
        
        paste(
          "Lowest Risk Application:",
          "\n",
          min_compound,
          " (",
          format(risk_min, digits = 2, nsmall = 2),
          ")",
          "\n\nHighest Risk Application:",
          "\n",
          max_compound,
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
  output$total_risk1 <- renderValueBox({
    if (!is.null(values1$data)) {
      grand_total <- sum(values1$data$Risk_Score, na.rm = TRUE)
      valueBox(
        value = format(grand_total, digits = 2, nsmall = 0),
        subtitle = "Total Risk Score",
        icon = icon("exclamation-triangle"),
        color = "red"
      )
    }
  })
  
  output$item_count1 <- renderValueBox({
    if (!is.null(values1$data)) {
      total_items <- sum(values1$data$Quantity_Applied, na.rm = TRUE)
      valueBox(
        value = format(total_items, digits = 2, nsmall = 2),
        subtitle = "Total Quantity of Compounds Applied",
        icon = icon("cubes"),
        #color = "blue"
        color = "red"
      )
    }
  })
  
  output$filled_rows1 <- renderValueBox({
    if (!is.null(values1$data)) {
      filled_rows <- sum(values1$data$Compound != "", na.rm = TRUE)
      valueBox(
        value = filled_rows,
        subtitle = "Number of Applications Entered",
        icon = icon("list"),
        #color = "yellow"
        color = "red"
      )
    }
  })
  
  output$total_risk2 <- renderValueBox({
    if (!is.null(values2$data)) {
      grand_total <- sum(values2$data$Risk_Score, na.rm = TRUE)
      valueBox(
        value = format(grand_total, digits = 2, nsmall = 0),
        subtitle = "Total Risk Score",
        icon = icon("exclamation-triangle"),
        color = "yellow"
      )
    }
  })
  
  output$item_count2 <- renderValueBox({
    if (!is.null(values2$data)) {
      total_items <- sum(values2$data$Quantity_Applied, na.rm = TRUE)
      valueBox(
        value = format(total_items, digits = 2, nsmall = 2),
        subtitle = "Total Quantity of Compounds Applied",
        icon = icon("cubes"),
        #color = "blue"
        color = "yellow"
      )
    }
  })
  
  output$filled_rows2 <- renderValueBox({
    if (!is.null(values2$data)) {
      filled_rows <- sum(values2$data$Compound != "", na.rm = TRUE)
      valueBox(
        value = filled_rows,
        subtitle = "Number of Applications Entered",
        icon = icon("list"),
        #color = "yellow"
        color = "yellow"
      )
    }
  })
  
  # subcomp substances tab =====================================================
  
  ###### Populate filter lists (runs once at app startup) ######
  
  observeEvent(TRUE, {
    # Substance category filter
    updateSelectInput(
      session,
      "substance_category1",
      choices = unique(data_hpli$compound_category) |>
        sort()
    )
    # Substance origin filter
    updateSelectInput(session,
                      "substance_origins1",
                      choices = unique(data_hpli$compound_origin) |>
                        sort())
    
  }, once = TRUE)
  
  observeEvent(TRUE, {
    # Substance type filter
    updateSelectInput(
      session,
      "substance_category2",
      choices = unique(data_hpli$compound_category) |>
        sort()
    )
    # Substance origin filter
    updateSelectInput(session,
                      "substance_origins2",
                      choices = unique(data_hpli$compound_origin) |>
                        sort())
    
  }, once = TRUE)
  
  
  
  ###### Populate list of substance (reacts on filters) ######
  #--data for second tab, 1st choice
  substance_choices1 <- reactive({
    data_hpli_filtered1 <- data_hpli
    
    # Filter by origin only if an origin is selected
    if (!is.null(input$substance_origins1) &&
        length(input$substance_origins1) > 0) {
      data_hpli_filtered1 <-
        data_hpli_filtered1 |>
        dplyr::filter(compound_origin %in% input$substance_origins1)
    }
    
    # Filter by type only if a category is selected
    if (!is.null(input$substance_category1) &&
        length(input$substance_category1) > 0) {
      data_hpli_filtered1 <-
        data_hpli_filtered1 |>
        dplyr::filter(compound_category %in% input$substance_category1)
    }
    
    
    
    # Format final substance list
    data_hpli_filtered1 |>
      dplyr::pull(compound) |>
      unique() |>
      sort()
  })
  
  #--data for second tab, 2nd choice
  substance_choices2 <- reactive({
    data_hpli_filtered2 <- data_hpli
    
    # Filter by origin only if an origin is selected
    if (!is.null(input$substance_origins2) &&
        length(input$substance_origins2) > 0) {
      data_hpli_filtered2 <-
        data_hpli_filtered2 |>
        dplyr::filter(compound_origin %in% input$substance_origins2)
    }
    
    # Filter by type only if a category is selected
    if (!is.null(input$substance_category2) &&
        length(input$substance_category2) > 0) {
      data_hpli_filtered2 <-
        data_hpli_filtered2 |>
        dplyr::filter(compound_category %in% input$substance_category2)
    }
    
    
    
    # Format final substance list
    data_hpli_filtered2 |>
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
                       data = data_hpli)
  })
  
  output$rose_plot2 <- renderPlot({
    req(input$substance_double2)
    fxn_Make_Rose_Plot(compound_name = input$substance_double2,
                       data = data_hpli)
  })
  
  output$dist_plot_both <- renderPlot({
    req(input$substance_double1)
    fxn_Make_Distribution_Plot(
      compound_names = c(input$substance_double1, input$substance_double2),
      data = data_hpli
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
  #       data_hpli |>
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
  
  
}

# run app -----------------------------------------------------------------
shinyApp(ui = ui, server = server)
