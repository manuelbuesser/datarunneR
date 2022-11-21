# datarunneR v2
# last updated: 21/11/2022
# Github link: https://github.com/manuelbuesser/datarunneR

#### START #####################################################################


#### * Packages ################################################################

# load packages
library(dplyr)                            # data manipulation
library(shiny)                            # shiny environment
library(shinythemes)                      # CSS/bootstrap theme
library(shinyWidgets)                     # for lots of custom input widgets
library(shinyBS)                          # widget tooltips
library(shinyjqui)                        # to dynamically expand chart
library(DT)                               # to display tables
library(srvyr)                            # engine to run survey data analysis
library(highcharter)                      # charts
library(rlang)                            # to define additional functions
library(leaflet)                          # to display maps
library(leaflet.extras)                   # additional options for leaflet
library(vtable)                           # to display dataset overview
library(purrr)                            # for multiple choice loop
library(htmlwidgets)                      # modifications on leaflet map
library(shinybusy)                        # loading animations

#### * Formulas ################################################################

# define function to detect binary variables
is.binary <- function(x) {all(na.omit(x) %in% 0:1)}

# define functions for conditional dplyr piping (taken from tidytidbits package)
execute_in_pipeline <- function(.data, .language)
{
  .language <- enquo(.language)
  pipeline_env <- get_env(.language)
  
  magrittr_call <- quo(`%>%`(., !!.language))
  magrittr_call <- quo_squash(magrittr_call)
  
  magrittr_call <- new_quosure(magrittr_call, env=pipeline_env)
  fseq <- eval_tidy(magrittr_call)
  rlang::invoke(fseq, list(.data))
}

execute_if <- function(.data, .predicate, .language)
{
  .predicate <- enquo(.predicate)
  if (eval_tidy(.predicate, data = .data))
  {
    .language <- enquo(.language)
    execute_in_pipeline(.data, !!.language)
  }
  else
    .data
}


#### * Additional Inputs ################################################################

# define color palette for plot
cols1 <- c("#ee5859", "#58585a", "#f7acac", "#acacad", "#a5c9a1", "#d2e4d0", "#56b3cd",
           "#abd9e6", "#f69e61", "#fbcfb0", "#fff67a", "#fffbbd", "#d2cbb8", "#e9e5dc")

# increase max upload file size to 30MB
options(shiny.maxRequestSize = 30*1024^2)


#### USER INTERFACE ############################################################

ui <- navbarPage(title = HTML("<i class='fas fa-running'></i> datarunneR"),
                 windowTitle = "datarunneR",
                 theme = shinytheme("simplex"),
                 
                 #### * Upload tab #############################################
                 
                 tabPanel("Upload",
                          icon = icon("upload"),
                          
                          tags$head(includeHTML(("google-analytics.html"))),
                          
                          sidebarLayout(
                            
                            sidebarPanel(
                              
                              tags$h4("Upload dataset"),
                              
                              fileInput("file1", "Choose CSV file:",
                                        multiple = FALSE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")),
                              
                              prettyRadioButtons("sep", "Separator:",
                                                 choices = c(Comma = ",",
                                                             Semicolon = ";",
                                                             Tab = "\t"),
                                                 selected = ",",
                                                 status = "danger"),
                              
                              prettyRadioButtons("quote", "Quote:",
                                                 choices = c(None = "",
                                                             "Double Quote" = '"',
                                                             "Single Quote" = "'"),
                                                 selected = '"',
                                                 status = "danger"),
                              
                              
                              # dropdown panel with additional information
                              absolutePanel(id = "dropdown", top = 26, left = 165, width = 100, fixed=FALSE, draggable = FALSE, height = "auto",
                                            dropdown(
                                              h4("Data Upload"),
                                              p(h6("Please upload only .csv files. Excel files can currently not be uploaded.")),
                                              p(h6("Once the dataset is uploaded, you will see an overview table on the right. Only change the 'separator' and 'quote' specifications if the dataset did not seem to be read correctly.")),
                                              p(h6("In case the upload does not work, ensure that your dataset does not include any special characters, as those may break down the application.")),
                                              width = "400px",
                                              tooltip = tooltipOptions(title = "Click for more details on data upload", placement = "top"),
                                              size = "xs",
                                              up = FALSE,
                                              style = "jelly", icon = icon("info"),
                                              animate = animateOptions(
                                                enter = "fadeInDown",
                                                exit  = "fadeOutUp",
                                                duration = 0.5)
                                            )
                              ),
                              
                              width = 3,
                            ),
                            
                            mainPanel(
                              dataTableOutput("summary")                     # summary table with info about uploaded dataset
                            )
                          )
                 ),
                 
                 
                 #### * Analysis tab ###########################################
                 
                 
                 tabPanel("Analysis",
                          icon = icon("chart-column"),
                          
                          sidebarLayout(
                            sidebarPanel(
                              
                              #tags$style(HTML('table.dataTable tr.active td {background-color: #58585a !important;}')),
                              
                              tags$h4("Analysis Parameters"),
                              
                              #dropdown panel with additional information about analysis inputs
                              absolutePanel(id = "dropdown2", top = 26, left = 205, width = 100, fixed=FALSE, draggable = FALSE, height = "auto",
                                            dropdown(
                                              h4("Analysis Parameters"),
                                              p(h6("Select an indicator under 'Indicator' and click 'Run!' to display the results.")),
                                              p(h6("Click on the red gear icon to find more visualization options.")),
                                              p(h6("Refer to the info tab more more detailed explanations of the app's functions.")),
                                              tooltip = tooltipOptions(title = "Click for more details on how to run an analysis", placement = "top"),
                                              width = "400px", size = "xs", up = FALSE, style = "jelly", icon = icon("info"),
                                              animate = animateOptions(enter = "fadeInDown", exit  = "fadeOutUp", duration = 0.5)
                                            )
                              ),
                              
                              # indicator selection for all indicator (except multiple choice indicators)
                              conditionalPanel(condition = "input.multiple_choice == 0",
                                               pickerInput("select_indicator",
                                                           label = "Indicator:",   
                                                           choices = NULL,
                                                           selected = NULL,
                                                           multiple = FALSE,
                                                           options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
                                               )
                              ),
                              
                              
                              # indicator selection for multiple choice indicators
                              conditionalPanel(condition = "input.multiple_choice == 1",
                                               pickerInput("select_indicator_mc",
                                                           label = "Indicators:",   
                                                           choices = NULL,
                                                           selected = NULL,
                                                           multiple = TRUE,
                                                           options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
                                               )
                              ),
                              
                              # selection of disaggregation variable
                              pickerInput("select_group",
                                          label = "Disaggregate by:",
                                          choices = NULL,
                                          selected = NULL,
                                          multiple = TRUE,
                                          options = pickerOptions(maxOptions = 1, title = "Grouping variable 1", actionsBox = TRUE, liveSearch = TRUE)
                              ),
                              
                              # selection of second disaggregation variable (conditional on first disaggregation)
                              conditionalPanel(condition = "input.select_group != '' & input.multiple_choice == 0",
                                               pickerInput("select_group2",
                                                           #label = "Grouping variable 2:",   
                                                           choices = NULL,
                                                           selected = NULL,
                                                           multiple = TRUE,
                                                           options = pickerOptions(maxOptions = 1, title = "Grouping variable 2", actionsBox = TRUE, liveSearch = TRUE)
                                               )
                              ),
                              
                              # strata filter option if strata variable exists
                              conditionalPanel(condition = "input.type == 'strat'",
                                               pickerInput("select_strata",
                                                           label = "Filter by strata:",   
                                                           choices = NULL,
                                                           selected = NULL,
                                                           multiple = TRUE,
                                                           options = pickerOptions(title = "Strata", actionsBox = TRUE, liveSearch = TRUE)
                                               )
                              ),
                              
                              # selection of filter indicator
                              pickerInput("select_ind",
                                          label = "Filter by indicator:",   
                                          choices = NULL,
                                          selected = NULL,
                                          multiple = TRUE,
                                          options = pickerOptions(maxOptions = 1, title = "Select indicator", actionsBox = TRUE, liveSearch = TRUE)
                              ),
                              
                              # dynamic selection of filter indicator values
                              uiOutput("filter_ind"),
                              
                              # second filter variable and value selection
                              conditionalPanel(condition = "input.select_ind != ''",
                                               pickerInput("select_ind2",
                                                           label = "Filter by indicator 2:",   
                                                           choices = NULL,
                                                           selected = NULL,
                                                           multiple = TRUE,
                                                           options = pickerOptions(maxOptions = 1, title = "Select indicator 2", actionsBox = TRUE, liveSearch = TRUE)
                                               ),
                                               uiOutput("filter_ind2"),
                              ),
                              
                              hr(),
                              
                              # button to initiate analysis as per inputs
                              actionButton("run", "Run!", icon = icon("running"), class = "btn-primary", style="margin-bottom:4px"),
                              
                              # button to reset parameter inputs
                              actionButton("table_reset", "Reset selection", icon = icon("undo-alt"), style="margin-bottom:4px"),
                              
                              # button to download analysis results data
                              downloadButton("downloadData", "Download as CSV", style="margin-bottom:4px"),
                              
                              # button to show additional input options
                              conditionalPanel(condition = "input.more_options == 0 | input.more_options == input.less_options",
                                               actionButton("more_options", "More options", icon = icon("caret-down"), style="margin-bottom:4px")
                              ),
                              
                              # button to hide additional input options
                              conditionalPanel(condition = "input.more_options > input.less_options",
                                               actionButton("less_options", "Less options", icon = icon("caret-up"), style="margin-bottom:4px")
                              ),
                              
                              # conditional panel with additional input options
                              conditionalPanel(condition = "input.more_options > input.less_options", br(),
                                               
                                               # switch for multiple choice option (analyse multiple indicators at once)
                                               prettySwitch("multiple_choice",
                                                            label = "Multiple choice",
                                                            status = "success",
                                                            fill = TRUE
                                               ),
                                               
                                               # switch to interpret numeric variables as factors
                                               prettySwitch("factor",
                                                            label = "Numeric as factor",
                                                            status = "success",
                                                            fill = TRUE
                                               ),
                                               
                                               # switch to include NA in analysis
                                               prettySwitch("na",
                                                            label = "Include missing values",
                                                            status = "success",
                                                            fill = TRUE
                                               ),
                                               
                                               # input to change confidence level for analysis
                                               prettyRadioButtons("confidence", "Confidence level:",
                                                                  choices = c("90%" = 0.9,
                                                                              "95% (default)" = 0.95,
                                                                              "99%" = 0.99),
                                                                  selected = 0.95,
                                                                  status = "danger"),
                                               
                                               # input to switch from mean to median analysis
                                               prettyRadioButtons("statistic", "Statistic (for numerical var.):",
                                                                  choices = c("Mean" = "mean",
                                                                              "Median" = "median"),
                                                                  selected = "mean",
                                                                  status = "danger"),
                                               
                                               hr(),
                                               
                                               # inout to define weighted analysis
                                               prettyRadioButtons("type", "Sample type:",
                                                                  choices = c("Unweighted sample"         = "un",
                                                                              "Stratified sample"         = "strat",
                                                                              "One-stage cluster sample"  = "clust1",
                                                                              "Two-stage cluster sample"  = "clust2",
                                                                              "Stratified cluster sample" = "strat_clust"),
                                                                  selected = "un",
                                                                  status = "danger"),
                                               
                                               conditionalPanel(condition = "input.type == 'strat'",
                                                                tags$i(h5("Please make sure your dataset includes the following two variables: (1) 'strata' specifying the strata names, and (2) 'pop' specifying the corresponding population size.", style = "color: red;")),
                                               ),
                                               
                                               conditionalPanel(condition = "input.type == 'clust1'",
                                                                tags$i(h5("Please make sure your dataset includes the following two variables: (1) 'cluster' specifying the cluster names, and (2) 'pop' specifying the corresponding population size.", style = "color: red;")),
                                               ),
                                               
                                               conditionalPanel(condition = "input.type == 'clust2'",
                                                                tags$i(h5("Please make sure your dataset includes the following four variables: (1) 'fsu' specifying the first-stage cluster units, (2) 'ssu' specifying the second-stage cluster units, (3) 'pop_fsu' specifying the corresponding first-stage cluster population size, and (4) 'pop_ssu' specifying the corresponding second-stage cluster population size.", style = "color: red;")),
                                               ),
                                               
                                               conditionalPanel(condition = "input.type == 'strat_clust'",
                                                                tags$i(h5("Please make sure your dataset includes the following three variables: (1) 'strata' specifying the strata names, (2) 'cluster' specifying the cluster names, and (3) 'pop' specifying the corresponding population size.", style = "color: red;")),
                                               )
                                               
                              ),
                              
                              # pop up alert in case of faulty selection
                              useSweetAlert(),
                              
                              width = 3
                            ),
                            
                            mainPanel(
                              align = "left",
                              
                              # add loading animation
                              add_busy_spinner(spin = "fading-circle", color = "#56b3cd", position = "top-right",
                                               margins = c("200", "34%"), height = "100px", width = "100px"),
                              
                              # map output
                              conditionalPanel(condition = "input.select_map == 1",
                                               leafletOutput("map", width = "100%", height = "800")
                              ),
                              
                              conditionalPanel(condition = "input.select_map == 0",
                                               
                                               # chart output
                                               jqui_resizable(highchartOutput("plot")),
                                               
                                               # table output
                                               conditionalPanel(condition = "input.scatter != 1",
                                                                dataTableOutput("table")
                                               )
                              ),
                              
                              # dropdown panel for chart display options
                              absolutePanel(
                                id = "settings", fixed = FALSE, draggable = FALSE,
                                top = "0", left = "0", right = "auto", bottom = "auto",
                                
                                dropdown(
                                  tags$h4("Chart options:"),
                                  tags$h5("Type:"),
                                  tags$i(h6("numeric:",
                                            style="color:grey;text-align:justify")),
                                  
                                  prettySwitch("histogram",
                                               label = "Histogram",
                                               status = "success",
                                               fill = TRUE
                                  ),
                                  
                                  prettySwitch("density",
                                               label = "Density",
                                               status = "success",
                                               fill = TRUE
                                  ),
                                  
                                  prettySwitch("boxplot",
                                               label = "Boxplot",
                                               status = "success",
                                               fill = TRUE
                                  ),
                                  
                                  prettySwitch("heatmap",
                                               label = "Heatmap",
                                               status = "success",
                                               fill = TRUE
                                  ),
                                  
                                  prettySwitch("scatter",
                                               label = "Scatterplot",
                                               status = "success",
                                               fill = TRUE
                                  ),
                                  
                                  conditionalPanel(condition = "input.timeseries == 1",
                                                   hr()
                                  ),
                                  
                                  prettySwitch("timeseries",
                                               label = "Timeseries",
                                               status = "success",
                                               fill = TRUE
                                  ),
                                  
                                  conditionalPanel(condition = "input.timeseries == 1",
                                                   pickerInput("select_date",
                                                               label = "Select date variable:",   
                                                               choices = NULL,
                                                               selected = NULL,
                                                               multiple = FALSE,
                                                               options = pickerOptions(title = "Date variable", actionsBox = TRUE, liveSearch = TRUE)
                                                   ),
                                                   
                                                   hr()
                                  ),

                                  tags$i(h6("categorical:",
                                            style="color:grey;text-align:justify")),
                                  
                                  prettySwitch("stacked",
                                               label = "Stacked bar plot",
                                               status = "success",
                                               fill = TRUE
                                  ),
                                  
                                  prettySwitch("donut",
                                               label = "Donut",
                                               status = "success",
                                               fill = TRUE
                                  ),
                                  
                                  tags$h5("Display options:"),
                                  
                                  prettySwitch("count",
                                               label = "Counts (cat.)",
                                               status = "success",
                                               fill = TRUE
                                  ),
                                  
                                  prettySwitch("interval",
                                               label = "Conf. intervals (num.)",
                                               status = "success",
                                               fill = TRUE
                                  ),
                                  
                                  hr(),
                                  
                                  tags$h4("Table options:"),
                                  
                                  prettySwitch("select_ci",
                                               label = "Show confidence interval",
                                               status = "success",
                                               fill = TRUE
                                  ),
                                  
                                  numericInput("rounding", "Decimals:", 1, min = 0, max = 10, step = 1, width = "60px"),
                                  bsTooltip(id = "rounding", title = "Define how many decimals you want there to be shown in the table and plot.",
                                            placement = "right", trigger = "hover", options = list(delay = list(show=1000))),
                                  
                                  hr(),
                                  
                                  tags$h4("Map options:"),
                                  
                                  prettySwitch("select_map",
                                               label = "Map GPS data (num.)",
                                               status = "success",
                                               fill = TRUE
                                  ),
                                  
                                  conditionalPanel(condition = "input.select_map == 1",
                                                   pickerInput("select_latitude",
                                                               label = "Select latitude:",   
                                                               choices = NULL,
                                                               selected = NULL,
                                                               multiple = FALSE,
                                                               options = pickerOptions(title = "Latitude", actionsBox = TRUE, liveSearch = TRUE)
                                                   ),
                                                   
                                                   pickerInput("select_longitude",
                                                               label = "Select longitude:",   
                                                               choices = NULL,
                                                               selected = NULL,
                                                               multiple = FALSE,
                                                               options = pickerOptions(title = "Longitude", actionsBox = TRUE, liveSearch = TRUE)
                                                   )
                                  ),
                                  
                                  width = "230px", size = "xs", up = FALSE, status = "danger",
                                  style = "jelly", icon = icon("gear"),
                                  animate = animateOptions(enter = "fadeInDown", exit  = "fadeOutUp", duration = 0.5)
                                )
                              ),
                              
                              width = 9
                            )
                          ) #sidebarlayout
                 ), #tabpanel
                 
                 
                 #### * Regression tab ###########################################
                 
                 tabPanel("Regression",
                          icon = icon("chart-line"),
                          
                          sidebarLayout(
                            
                            sidebarPanel(
                              
                              tags$h4("Regression Inputs"),
                              
                              # regression model selection
                              pickerInput("select_model",
                                          label = "Model:",   
                                          choices = c("Simple linear regression"),
                                          selected = "Simple linear regression",
                                          multiple = FALSE,
                                          options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
                              ),
                              
                              # dependent variable selection
                              pickerInput("select_dependent",
                                          label = "Dependent variable:",   
                                          choices = NULL,
                                          selected = NULL,
                                          multiple = FALSE,
                                          options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
                              ),
                              
                              # independent variables selection
                              pickerInput("select_independent",
                                          label = "Independent variable(s):",   
                                          choices = NULL,
                                          selected = NULL,
                                          multiple = TRUE,
                                          options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
                              ),
                              
                              # fixed effect variable selection
                              #pickerInput("select_fe",
                              #            label = "Fixed effects variable:",   
                              #            choices = NULL,
                              #            selected = NULL,
                              #            multiple = FALSE,
                              #            options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
                              #),
                              
                              hr(),
                              
                              # action button to run regression analysis
                              actionButton("run_reg", "Run!", icon = icon("running"), class = "btn-primary", style="margin-bottom:4px"),
                              
                              width = 3
                            ),
                            
                            # main panel with regression output
                            mainPanel(
                              conditionalPanel(condition = "input.run_reg >= 1",
                                               verbatimTextOutput("regression"))
                            )
                          )
                 ),  #tabpanel
                 
                 
                 #### * Info tab ###########################################
                 
                 
                 tabPanel("Info",
                          icon = icon("info"),
                          
                          # load in external html file (created with R markdown)
                          tags$iframe(src = './info.html',
                                      frameborder = 0, scrolling = 'auto', style='width:100vw;height:100vh;'
                          )
                 ) #tabpanel
) #navbarpage


#### SERVER ####################################################################

server <- function(input, output, session) {
  
  #### * Data upload ###########################################################
  
  # interpret uploaded data
  data_upload <- reactive({
    req(input$file1)
    tryCatch(
      {
        data_upload <- read.csv(input$file1$datapath,
                                header = TRUE,
                                sep = input$sep,
                                quote = input$quote,
                                na.strings=c("", "NA", " "),
                                stringsAsFactors = TRUE) %>%
          mutate_if(is.binary, as.factor) # find and change class of binary (0/1) variables
        
        
      },
      error = function(e) {
        stop(safeError(e))
      }
      
    )
  })
  
  # render table with dataset summary
  output$summary <- renderDT({
    
    summary <- vtable(data_upload(), out = "return")
    
    summary <- DT::datatable(
      summary,
      options = list(dom = "ft", paging = FALSE),
      style = "bootstrap",
      class = "table-condensed table-hover table-striped"
    )
    
  })

  
  #### * Dynamic UI elements ###################################################
  
  
  # create reference lists
  indicators <- reactive({names(data_upload())})
  indicators_cat <- reactive({data_upload() %>% select_if(~class(.) == 'factor') %>% names()})
  indicators_num <- reactive({data_upload() %>% select_if(~class(.) != 'factor') %>% names()})
  strata <- reactive({as.character(sort(unique(data_upload()$strata)))})
  list1 <- reactive({as.character(sort(unique(data_upload()[[input$select_ind]])))})
  list2 <- reactive({as.character(sort(unique(data_upload()[[input$select_ind2]])))})
  
  
  # update choice lists with values from uploaded file
  observe({
    input$file1
    updatePickerInput(session, "select_indicator", choices = indicators())
    updatePickerInput(session, "select_indicator_mc", choices = indicators())
    updatePickerInput(session, "select_indicator_donut", choices = indicators())
    updatePickerInput(session, "select_group", choices = indicators())
    updatePickerInput(session, "select_group2", choices = indicators())
    updatePickerInput(session, "select_strata", choices = strata())
    updatePickerInput(session, "select_ind", choices = indicators())
    updatePickerInput(session, "select_ind2", choices = indicators())
    updatePickerInput(session, "select_latitude", choices = indicators())
    updatePickerInput(session, "select_longitude", choices = indicators())
    updatePickerInput(session, "select_date", choices = indicators())
    updatePickerInput(session, "select_dependent", choices = indicators_num())
    updatePickerInput(session, "select_independent", choices = indicators())
    updatePickerInput(session, "select_dependent_test", choices = indicators_num())
    updatePickerInput(session, "select_independent_test", choices = indicators())
    updatePickerInput(session, "select_fe", choices = indicators_cat())
  })
  
  
  # define dynamic filter variable
  output$filter_ind <- renderUI({
    
    req(input$select_ind != "")
    
    pickerInput("select_ind_value",
                choices = list1(),
                selected = NULL,
                multiple = TRUE,
                options = pickerOptions(title = "Select filter values", actionsBox = TRUE, liveSearch = TRUE)
    )
  })
  
  # define 2nd dynamic filter variable
  output$filter_ind2 <- renderUI({
    
    req(input$select_ind2 != "")
    
    pickerInput("select_ind_value2",
                #label = "",   
                choices = list2(),
                selected = NULL,
                multiple = TRUE,
                options = pickerOptions(title = "Select filter values", actionsBox = TRUE, liveSearch = TRUE)
    )
  })
  
  
  # make sure factor switch goes back to default when a new indicator is selected
  observe({
    input$select_indicator
    updatePrettySwitch(session, "factor", value = FALSE)
  })
  
  
  # make sure CI switch goes back to default when count is displayed
  observe({
    input$count
    updatePrettySwitch(session, "interval", value = FALSE)
  })
  
  
  # define reset button parameters
  observe({
    input$table_reset
    updatePrettySwitch(session, "factor", value = FALSE)
    updatePrettySwitch(session, "na", value = FALSE)
    updatePickerInput(session, "select_group", selected = "")
    updatePickerInput(session, "select_group2", selected = "")
    updatePickerInput(session, "select_strata", selected = "")
    updatePickerInput(session, "select_ind", selected = "")
    updatePickerInput(session, "select_ind_value", selected = "")
    updatePickerInput(session, "select_ind2", selected = "")
    updatePickerInput(session, "select_ind_value2", selected = "")
    updatePrettyRadioButtons(session, "confidence", selected = 0.95)
    updatePrettyRadioButtons(session, "statistic", selected = "mean")
    updatePrettySwitch(session, "count", value = FALSE)
    updatePrettySwitch(session, "interval", value = FALSE)
    updatePrettySwitch(session, "stacked", value = FALSE)
    updateNumericInput(session, "rounding", value = 1)
  })
  
  
  #### * Design & analysis ##############################################
  
  # apply dynamic filters to datasets
  data_filter <- eventReactive(input$run, {
    data_upload() %>%
      execute_if(input$factor, mutate_at(input$select_indicator, as.factor)) %>%
      filter(if(input$type == "strat") is.null(input$select_strata) | strata  %in% input$select_strata else TRUE) %>%
      filter(if(!is.null(input$select_ind)) is.null(input$select_ind_value) | get(input$select_ind) %in% input$select_ind_value else TRUE) %>%
      filter(if(!is.null(input$select_ind) & !is.null(input$select_ind2)) is.null(input$select_ind_value2) | get(input$select_ind2) %in% input$select_ind_value2 else TRUE)
  })
  
  
  # define survey design
  design <- reactive({
    data_filter() %>%
      execute_if(input$type == "un", as_survey_design(ids = 1)) %>%
      execute_if(input$type == "strat", as_survey_design(ids = 1, strata = strata, fpc = pop)) %>%
      execute_if(input$type == "clust1", as_survey_design(ids = cluster, fpc = pop)) %>%
      execute_if(input$type == "clust2", as_survey_design(ids = c(fsu, ssu), fpc = c(pop_fsu, pop_ssu))) %>%
      execute_if(input$type == "strat_clust", as_survey_design(ids = cluster, strata = strata, fpc = pop))
  })
  
  
  # run analysis
  results <- eventReactive(input$run, {
    
    input$run
    
    # for single indicators
    if (!input$multiple_choice){
      
      # factor variables (character or binary):
      if (is.factor(data_filter()[[input$select_indicator]])) {
        
        results <- design() %>%
          execute_if(!input$na, filter(!is.na(get(input$select_indicator)))) %>%
          group_by_(if(is.null(input$select_group)) input$select_indicator else TRUE) %>%
          execute_if(!is.null(input$select_group), group_by_(input$select_group, input$select_indicator)) %>%
          execute_if(!is.null(input$select_group) & !is.null(input$select_group2), group_by_(input$select_group2, input$select_group, input$select_indicator)) %>%
          summarize(stat = survey_mean(na.rm = TRUE, vartype = "ci", level = as.numeric(input$confidence), proportion = TRUE),
                    count = unweighted(n()))
        
      # numeric variables:
      } else if (!input$scatter){
        
        results <- design() %>%
          filter(!is.na(input$select_indicator)) %>%
          group_by_(if(!is.null(input$select_group)) input$select_group else TRUE) %>%
          execute_if(!is.null(input$select_group) & !is.null(input$select_group2), group_by_(input$select_group2, input$select_group)) %>%
          execute_if(input$statistic == "mean", summarize(stat = survey_mean(get(input$select_indicator), na.rm=TRUE, vartype = "ci", level = as.numeric(input$confidence)),
                                                          count = unweighted(sum(!is.na(get(input$select_indicator)))))) %>%
          execute_if(input$statistic == "median", summarize(stat = survey_median(get(input$select_indicator), na.rm=TRUE, vartype = "ci", level = as.numeric(input$confidence)),
                                                            count = unweighted(sum(!is.na(get(input$select_indicator)))))) %>%
          execute_if(is.null(input$select_group) , select(-"TRUE"))
        
      }
      
    # for multiple choice (multiple indicators)
    } else if(input$multiple_choice){
      
      # without disaggregation
      if(is.null(input$select_group)){
        results_single <- map(input$select_indicator_mc, ~
                                design() %>%
                                group_by_at(vars(.x)) %>%
                                summarize(stat = survey_mean(na.rm = TRUE, vartype = "ci", level = as.numeric(input$confidence), proportion = TRUE),
                                          count = unweighted(n())) %>%
                                mutate(var = colnames(.)[1]) %>%
                                filter_at(1, all_vars(. != 0)) %>%
                                select(-1) %>%
                                select(var, everything())
                              )
        
        results <- map_dfr(results_single, bind_rows) 
        
      # with disaggregation
      } else if(!is.null(input$select_group)){
        results_single <- map(input$select_indicator_mc, ~
                                design() %>%
                                group_by_at(vars(input$select_group, .x)) %>%
                                summarize(stat = survey_mean(na.rm = TRUE, vartype = "ci", level = as.numeric(input$confidence), proportion = TRUE),
                                          count = unweighted(n())) %>%
                                mutate(var = colnames(.)[2]) %>%
                                filter_at(2, all_vars(. != 0)) %>%
                                select(-2) %>%
                                select(input$select_group, var, everything())
                              )
        
        results <- map_dfr(results_single, bind_rows) %>%
          arrange(input$select_group)
        
      }
    }
  })
  
  
  # error message in case an empty column is called
  observeEvent(input$run, {
    if (!input$multiple_choice & all(is.na(data_filter()[[input$select_indicator]]))) {
      sendSweetAlert(
        session = session,
        title = "No data!",
        text = "There is no data for this selection. Select another indicator.",
        type = "error",
        btn_colors = "#454747"
      )
    }
  })
  
  # add tooltips for input widgets
  addTooltip(session, id = 'type', title = "Only change this input if you use stratified or cluster sampling.",
             placement = "top", trigger = "hover", options = list(delay = list(show=1000)))
  addTooltip(session, id = 'sep', title = "Do not change this input if the preview table is displayed correctly on the right.",
             placement = "bottom", trigger = "hover", options = list(delay = list(show=1000)))
  addTooltip(session, id = 'quote', title = "Do not change this input if the preview table is displayed correctly on the right.",
             placement = "top", trigger = "hover", options = list(delay = list(show=1000)))
  addTooltip(session, id = 'run', title = "Click to run the analysis as per your inputs.",
             placement = "top", trigger = "hover", options = list(delay = list(show=1000)))
  addTooltip(session, id = 'factor', title = "Interpret numeric values as categorical.",
             placement = "top", trigger = "hover", options = list(delay = list(show=1000)))
  addTooltip(session, id = 'na', title = "Include empty values in analysis.",
             placement = "top", trigger = "hover", options = list(delay = list(show=1000)))
  addTooltip(session, id = 'confidence', title = "Change the confidence level.",
             placement = "top", trigger = "hover", options = list(delay = list(show=1000)))
  addTooltip(session, id = 'statistic', title = "Display mean/median in the table and graph.",
             placement = "top", trigger = "hover", options = list(delay = list(show=1000)))
  addTooltip(session, id = "count", title = "Display the number of observations in the plot (as opposed to mean/median).",
             placement = "top", trigger = "hover", options = list(delay = list(show=1000)))
  addTooltip(session, id = "interval", title = "The confidence interval depends on the selected confidence level.",
             placement = "top", trigger = "hover", options = list(delay = list(show=1000)))
  addTooltip(session, id = "histogram", title = "Plot a numeric variable in a histogram.",
             placement = "top", trigger = "hover", options = list(delay = list(show=1000)))
  addTooltip(session, id = "density", title = "Display a numeric variable in a density plot.",
             placement = "top", trigger = "hover", options = list(delay = list(show=1000)))
  addTooltip(session, id = "boxplot", title = "Display a numeric variable in a boxplot. You can add a disaggregation.",
             placement = "top", trigger = "hover", options = list(delay = list(show=1000)))
  addTooltip(session, id = "heatmap", title = "Display a numeric variable in a heatmap. Two disaggregations are required.",
             placement = "top", trigger = "hover", options = list(delay = list(show=1000)))
  addTooltip(session, id = "stacked", title = "Display bar plot.",
             placement = "top", trigger = "hover", options = list(delay = list(show=1000)))
  addTooltip(session, id = "select_map", title = "If you have latitude/longitude in your data, you can display any numeric variables on a map.",
             placement = "top", trigger = "hover", options = list(delay = list(show=1000)))
  addTooltip(session, id = "select_ci", title = "Display confidence interval in the table.",
             placement = "top", trigger = "hover", options = list(delay = list(show=1000)))
  
  
  #### * Output: Map #########################################################
  
  
  # render map
  output$map <- renderLeaflet({
    
    input$run
    
    isolate(req(input$select_map == TRUE))
    
    map_data <- isolate(data_filter())
    
    pal <- isolate(colorNumeric(
      palette = "Reds",
      na.color = "transparent",
      domain = map_data[[input$select_indicator]]
      ))
    
    pal2 <- isolate(colorFactor(topo.colors(6), map_data$housing))
    
    # define map output using leaflet
    map <- isolate(leaflet(data = map_data, options = leafletOptions(attributionControl=FALSE, zoomControl = FALSE)) %>%
                     execute_if(!is.factor(map_data[[input$select_indicator]]),
                                addCircleMarkers(
                                  lng = ~get(input$select_longitude), lat = ~get(input$select_latitude),
                                  radius = 5,
                                  color = ~pal(map_data[[input$select_indicator]]),
                                  stroke = FALSE
                                )) %>%
                     execute_if(is.factor(map_data[[input$select_indicator]]),
                                addCircleMarkers(
                                  lng = ~get(input$select_longitude), lat = ~get(input$select_latitude),
                                  radius = 5,
                                  #color = ~pal2(map_data[[input$select_indicator]]),
                                  #color = ~pal2(housing),
                                  stroke = FALSE
                                )) %>%
                     execute_if(!is.factor(map_data[[input$select_indicator]]) , addLegend("bottomright", pal = pal, values = map_data[[input$select_indicator]])) %>%
                     #execute_if(is.factor(map_data[[input$select_indicator]]) , addLegend("bottomright", pal = pal2, values = map_data[[input$select_indicator]])) %>%
                     addProviderTiles(providers$CartoDB.Positron) %>%
                     htmlwidgets::onRender("function(el, x) { L.control.zoom({ position: 'topright' }).addTo(this) }")
    )
  })
  
  
  #### * Output: Table #########################################################
  
  # render output table
  output$table <- renderDT({
    
    input$run
    
    # remove confidence interval
    table <- isolate(results() %>%
      execute_if(!input$select_ci, select(-"stat_low", -"stat_upp")))
    
    # define variable
    table <- isolate(DT::datatable(
      table,
      options = list(dom = "t", paging = FALSE),
      rownames = FALSE,
      style = "bootstrap",
      class = "table-condensed table-hover table-striped") %>%
        execute_if(input$select_ci & is.factor(data_filter()[[input$select_indicator]]), formatPercentage(c("stat", "stat_low", "stat_upp"), input$rounding)) %>%
        execute_if(input$select_ci & !is.factor(data_filter()[[input$select_indicator]]), formatRound(c("stat", "stat_low", "stat_upp"), input$rounding)) %>%
        execute_if(!input$select_ci & is.factor(data_filter()[[input$select_indicator]]), formatPercentage(c("stat"), input$rounding)) %>%
        execute_if(!input$select_ci & !is.factor(data_filter()[[input$select_indicator]]), formatRound(c("stat"), input$rounding)) %>%
        execute_if(input$multiple_choice, formatPercentage(c("stat"), input$rounding))
    )
  })
  
  
  #### * Output: Plot ##########################################################
  
  # render plot
  output$plot <- renderHighchart({
    
    input$run
    
    if (all(is.na(data_filter()[[isolate(input$select_indicator)]]))) {
      return(NULL)
    } else {
      
      # factor variable - no disaggregation
      isolate(if ((is.factor(data_filter()[[input$select_indicator]]) | input$multiple_choice) & is.null(input$select_group)) {
        
        if (!input$donut){
          results1 <- results() %>%
            execute_if(!input$multiple_choice, mutate(var = get(input$select_indicator)))
          plot <- results1 %>%
            execute_if(!input$stacked & !input$count, hchart("column", hcaes(x = var, y = stat*100))) %>%
            execute_if(!input$stacked & !input$count & input$interval, hc_add_series(name = "confidence interval", data = list_parse(mutate(results1, low = stat_low*100, high = stat_upp*100)),
                                                                                     type = "errorbar", color = "black", stemWidth = 1)) %>%
            execute_if(!input$stacked & input$count, hchart("column", hcaes(x = var, y = count))) %>%
            execute_if(input$stacked, hchart("bar", hcaes(y = stat*100, group = var))) %>%
            execute_if(input$stacked, hc_plotOptions(series=list(stacking='normal'))) %>%
            execute_if(input$stacked, hc_legend(reversed = TRUE))  %>%
            execute_if(input$stacked, hc_xAxis(title="", visible = FALSE))  %>%
            execute_if(!input$count, hc_yAxis(min = 0, max = 100, title = list(text=""), labels = list(format = "{value}%"))) %>%
            execute_if(!input$stacked & !input$count, hc_tooltip(headerFormat = "<span style = 'font-size:10'>{point.key}</span><br>", pointFormat = "<b>{point.y:.1f}%</b> ({point.count} counts)"))  %>%
            execute_if(!input$stacked & input$count, hc_tooltip(headerFormat = "<span style = 'font-size:10'>{point.key}</span><br>", pointFormat = "<b>{point.count}</b>"))  %>%
            execute_if(input$stacked, hc_tooltip(headerFormat = "<span style = 'color:{series.color}'>\u25CF</span> {series.name}<br>", pointFormat = "<b>{point.percentage:.1f}%</b> ({point.count} counts)"))
        
        # donut chart
        } else if(input$donut){
          results1 <- results() %>%
            mutate(var = get(input$select_indicator))
          
          plot <- results1 %>%
            hchart("pie", hcaes(x = var, y = stat*100)) %>%
            hc_plotOptions(pie = list(innerSize = "60%",
                                      allowPointSelect = TRUE,
                                      dataLabels = list(enabled = TRUE,
                                                        format = '<b>{point.name}</b>: {point.percentage:.1f} %'))) %>%
            hc_tooltip(headerFormat = "<span style = 'color:{point.color}'>\u25CF</span> {point.key}<br>", pointFormat = "<b>{point.percentage:.1f}%</b> ({point.count} counts)")
        }
        
      # factor variable - 1 disaggregation variable
      } else if ((is.factor(data_filter()[[input$select_indicator]]) | input$multiple_choice) & !is.null(input$select_group) & is.null(input$select_group2)) {
        
        results1 <- results() %>%
          execute_if(!input$multiple_choice, mutate(var = get(input$select_indicator),
                                                    var2 = get(input$select_group))) %>%
          execute_if(input$multiple_choice, mutate(var2 = get(input$select_group)))
        plot <- results1 %>%
          execute_if(!input$count, hchart("column", hcaes(x = var2, y = stat*100, group = var))) %>%
          execute_if(input$count, hchart("column", hcaes(x = var2, y = count, group = var))) %>%
          execute_if(!input$multiple_choice, hc_plotOptions(series=list(stacking='normal')))  %>%
          execute_if(!input$count, hc_yAxis(min = 0, max = 100, title = list(text=""), labels = list(format = "{value}%"))) %>%
          execute_if(!input$count & !input$multiple_choice, hc_tooltip(headerFormat = "<span style = 'font-size:10'>{point.key}</span><br>", pointFormat = "<span style = 'color:{series.color}'>\u25CF</span> {series.name}<br><b>{point.percentage:.1f}%</b> ({point.count} counts)")) %>%
          execute_if(!input$count & input$multiple_choice, hc_tooltip(headerFormat = "<span style = 'font-size:10'>{point.key}</span><br>", pointFormat = "<span style = 'color:{series.color}'>\u25CF</span> {series.name}<br><b>{point.y:.1f}%</b> ({point.count} counts)")) %>%
          execute_if(input$count, hc_tooltip(headerFormat = "<span style = 'font-size:10'>{point.key}</span><br>", pointFormat = "<span style = 'color:{series.color}'>\u25CF</span> {series.name}<br><b>{point.count}</b>"))  
        
      # factor variable - 2 disaggregation variables
      } else if (is.factor(data_filter()[[input$select_indicator]]) & !is.null(input$select_group) & !is.null(input$select_group2)) {
        results1 <- results() %>%
          mutate(var = get(input$select_indicator),
                 var2 = paste0(get(input$select_group2), " - ", get(input$select_group)))
        plot <- results1 %>%
          execute_if(!input$count, hchart("column", hcaes(x = var2, y = stat*100, group = var))) %>%
          execute_if(input$count, hchart("column", hcaes(x = var2, y = count, group = var))) %>%
          hc_plotOptions(series=list(stacking='normal'))  %>%
          execute_if(!input$count, hc_yAxis(min = 0, max = 100, title = list(text=""), labels = list(format = "{value}%"))) %>%
          hc_tooltip(headerFormat = "<span style = 'font-size:10'>{point.key}</span><br>", pointFormat = "<span style = 'color:{series.color}'>\u25CF</span> {series.name}<br><b>{point.percentage:.1f}%</b> ({point.count} counts)")
        
      # numeric variable - no disaggregation
      } else if(is.null(input$select_group)) {
        
        if(!input$histogram & !input$density & !input$boxplot & !input$timeseries) {
          plot <- hchart(results(), "column", hcaes(x = "1", y = stat)) %>%
            execute_if(input$interval, hc_add_series(name = "confidence interval", data = list_parse(mutate(results(), x = "1", low = stat_low, high = stat_upp)),
                                                     type = "errorbar", color = "black", stemWidth = 1))  %>%
            hc_tooltip(headerFormat = "", pointFormat = "<b>{point.y:.1f}</b> ({point.count} counts)") %>%
            hc_xAxis(title="", visible = FALSE) %>%
            execute_if(input$statistic == "mean", hc_yAxis(title = list(text="mean"))) %>%
            execute_if(input$statistic == "median", hc_yAxis(title = list(text="median")))
          
        # histogram
        } else if (input$histogram){
          plot <- hchart(data_filter()[[input$select_indicator]]) %>% hc_legend(enabled = FALSE)
          
        # density
        } else if (input$density){
          plot <- hchart(density(na.omit(data_filter()[[input$select_indicator]])), type = "area", name = "Weight") %>%
            hc_tooltip(headerFormat = "<span style = 'font-size:10'>{point.y:.3f}</span><br>", pointFormat = "<span style = 'color:{series.color}'>\u25CF</span> {series.name}<br>{point.x:.2f}")
        
        # boxplot
        } else if (input$boxplot){
          plot <- hcboxplot(var = "1", x = data_filter()[[input$select_indicator]], outliers = FALSE) %>%
            hc_chart(type = "column") %>%
            hc_plotOptions(boxplot = list(fillColor = "#e9e9e9", lineWidth = 1, lineColor = "#5c5c5c",
                                          medianWidth = 2, medianColor = "#d9230f", stemColor = "#5c5c5c", stemWidth = 1,
                                          whiskerColor = "#5c5c5c", whiskerLength = "0%", whiskerWidth = 1
                                          )
                           ) %>%
            hc_tooltip(useHTML = TRUE, headerFormat = "",
                       pointFormat = "<table>
                                        <tr><td>Max: </td<td>{point.high}</td></tr>
                                        <tr><td>Q3:  </td><td>{point.q3}</td></tr>
                                        <tr><td>Med: </td><td>{point.median}</td></tr>
                                        <tr><td>Q1:  </td><td>{point.q1}</td></tr>
                                        <tr><td>Min: </td><td>{point.low}</td></tr>
                                      </table>"
                       ) %>%
            hc_xAxis(title="", visible = FALSE)
          
        # timeseries
        } else if(input$timeseries){
          
          timeseries_data <- data_filter() %>%
            mutate(select_date = as.Date(get(input$select_date), format = "%Y-%m-%d"),
                   select_ind = get(input$select_indicator)) %>%
            arrange(select_date)
          plot <- timeseries_data %>%
            hchart("line", hcaes(x = select_date, y = select_ind)) %>%
            hc_chart(zoomType = "x") %>%
            hc_tooltip() %>%
            hc_yAxis(title = "") %>%
            hc_tooltip(headerFormat = "<span style = 'font-size:10'>{point.key}</span><br>", pointFormat = "<b>{point.y:.0f}</b>")
        }
        
      # numeric variable - 1 disaggregation variable
      } else if(!is.null(input$select_group) & is.null(input$select_group2)) {
        
        if(!input$boxplot & !input$scatter & !input$timeseries) {
          
          results1 <- results() %>%
            mutate(var = get(input$select_group))
          plot <- hchart(results1, "column", hcaes(x = var, y = stat)) %>%
            execute_if(input$interval, hc_add_series(name = "confidence interval", data = list_parse(mutate(results1, low = stat_low, high = stat_upp)),
                                                     type = "errorbar", color = "black", stemWidth = 1)) %>%
            hc_tooltip(headerFormat = "<span style = 'font-size:10'>{point.key}</span><br>", pointFormat = "<b>{point.y:.1f}</b> ({point.count} counts)") %>%
            execute_if(input$statistic == "mean", hc_yAxis(title = list(text="mean"))) %>%
            execute_if(input$statistic == "median", hc_yAxis(title = list(text="median")))
        
        # boxplot
        } else if(input$boxplot){
          
          plot <- hcboxplot(x = data_filter()[[input$select_indicator]], var = data_filter()[[input$select_group]], outliers = FALSE) %>%
            hc_chart(type = "column") %>%
            hc_plotOptions(boxplot = list(fillColor = "#e9e9e9", lineWidth = 1, lineColor = "#5c5c5c",
                                          medianWidth = 2, medianColor = "#d9230f", stemColor = "#5c5c5c", stemWidth = 1,
                                          whiskerColor = "#5c5c5c", whiskerLength = "0%", whiskerWidth = 1)
                           ) %>%
            hc_tooltip(useHTML = TRUE, pointFormat = "<table>
                                                        <tr><td>Max: </td<td>{point.high}</td></tr>
                                                        <tr><td>Q3: </td><td>{point.q3}</td></tr>
                                                        <tr><td>Med: </td><td>{point.median}</td></tr>
                                                        <tr><td>Q1: </td><td>{point.q1}</td></tr>
                                                        <tr><td>Min: </td><td>{point.low}</td></tr>
                                                     </table>")
        
        # scatterplot
        } else if (input$scatter){
          
          data_scatter <- data_filter() %>%
            mutate(var = get(input$select_indicator),
                   var2 = get(input$select_group))
          
          plot <- data_scatter %>%
            hchart('scatter', hcaes(x = var, y = var2)) %>%
            hc_plotOptions(series = list(marker = list(radius = 3))) %>%
            hc_yAxis(title = list(text = input$select_group)) %>%
            hc_tooltip(headerFormat = "", pointFormat = "X: <b>{point.x:.1f}</b><br>Y: <b>{point.y:.1f}</b>")
          
        # timeseries
        } else if(input$timeseries){
          
          timeseries_data <- data_filter() %>%
            mutate(select_date = as.Date(get(input$select_date), format = "%Y-%m-%d"),
                   select_ind = get(input$select_indicator),
                   select_group = get(input$select_group)) %>%
            arrange(select_date)
          plot <- timeseries_data %>%
            hchart("line", hcaes(x = select_date, y = select_ind, group = select_group)) %>%
            hc_yAxis(title = "") %>%
            hc_chart(zoomType = "x")
          
        }
        
      # numeric variable - 2 disaggregation variables
      } else if(!is.null(input$select_group) & !is.null(input$select_group2)) {
        
        if(!input$heatmap & !input$scatter){
          
          results1 <- results() %>%
            mutate(var = paste0(get(input$select_group2), " - ", get(input$select_group)),
                   var2 = get(input$select_group2))
          plot <- hchart(results1, "column", hcaes(x = var, y = stat, group = var2)) %>%
            hc_plotOptions(series=list(stacking='normal')) %>%
            execute_if(input$interval, hc_add_series(name = "confidence interval", data = list_parse(mutate(results1, low = stat_low, high = stat_upp)),
                                                     type = "errorbar", color = "black", stemWidth = 1)) %>%
            hc_tooltip(headerFormat = "<span style = 'font-size:10'>{point.key}</span><br>", pointFormat = "<b>{point.y:.1f}</b> ({point.count} counts)") %>%
            execute_if(input$statistic == "mean", hc_yAxis(title = list(text="mean"))) %>%
            execute_if(input$statistic == "median", hc_yAxis(title = list(text="median")))
          
        # heatmap
        } else if(input$heatmap){
          results1 <- results() %>%
            mutate(var = get(input$select_group),
                   var2 = get(input$select_group2))
          
          # tooltip
          fntltp <- JS("function(){
                  return this.series.xAxis.categories[this.point.x] + ' ~ ' +
                         this.series.yAxis.categories[this.point.y] + ': <b>' +
                         Highcharts.numberFormat(this.point.value, 2)+'</b>';
               ; }")
          
          plot <- hchart(results1, "heatmap", hcaes(x = var, y = var2, value = stat)) %>%
            hc_yAxis(title="") %>%
            hc_tooltip(formatter = fntltp)
          
        # scatterplot
        } else if (input$scatter){
          
          data_scatter <- data_filter() %>%
            mutate(var = get(input$select_indicator),
                   var2 = get(input$select_group),
                   var3 = get(input$select_group2))
          
          plot <- data_scatter %>%
            hchart('scatter', hcaes(x = var, y = var2, group = var3)) %>%
            hc_plotOptions(series = list(marker = list(radius = 3))) %>%
            hc_yAxis(title = list(text = input$select_group))
        }
      })
      
      plot <- isolate(plot %>%
                        #execute_if(is.factor(data_filter()[[input$select_indicator]]) & !input$count, hc_tooltip(valueDecimals = (input$rounding + 2))) %>%
                        #execute_if(!is.factor(data_filter()[[input$select_indicator]]) & !input$count, hc_tooltip(valueDecimals = input$rounding)) %>%
                        execute_if(input$count, hc_yAxis(min = 0, title = list(text="count"))) %>%
                        hc_colors(cols1) %>%
                        hc_title(
                          text = input$select_indicator,
                          style = list(fontSize = "12px")
                        ) %>%
                        hc_xAxis(title = "") %>%
                        hc_exporting(
                          enabled = TRUE,
                          filename = paste0("plot_export-", Sys.Date()),
                          buttons = list(contextButton = list(menuItems = list("downloadPNG", "downloadPDF", "downloadCSV")))
                        )
      )
    }
  })
  
  
  #### * Data download #########################################################
  
  # define download button parameters
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("datarunneR-", input$select_indicator, "-", Sys.Date(),".csv")
    },
    content = function(file) {
      write.csv(results(), file, row.names = FALSE, na = "")
    }
  )
  
  
  #### * Output: Regression ##########################################################
  
  fit <- eventReactive(input$run_reg, {
    fit <- lm(reformulate(input$select_independent, input$select_dependent), data = data_upload(), na.action = na.exclude)
  })
  
  output$regression <- renderPrint({
    summary(fit())
  })
  
  
}

shinyApp(ui = ui, server = server)
