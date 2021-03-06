# datarunneR v1 (beta)
# last update: 06/03/2021
# Github link: https://github.com/manuelbuesser/datarunneR

#### START #####################################################################

# increase max upload file size to 30MB
options(shiny.maxRequestSize = 30*1024^2)


#### * Packages ################################################################

# load packages
library(dplyr)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(srvyr)
library(highcharter)
library(rlang)


# define color palette for plot
cols1 <- c("#ee5859", "#58585a", "#f7acac", "#acacad", "#a5c9a1", "#d2e4d0", "#56b3cd",
           "#abd9e6", "#f69e61", "#fbcfb0", "#fff67a", "#fffbbd", "#d2cbb8", "#e9e5dc")


#### * Formulas ################################################################

# define function to detect binary variables
#is.binary <- function(v) {
#  x <- unique(v)
#  length(x) - sum(is.na(x)) <= 2L
#}

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


#### USER INTERFACE ############################################################

ui <- navbarPage(title = HTML("<i class='fas fa-running'></i> datarunneR"),
                 theme = shinytheme("simplex"),
                 
                 
                 #### * Inputs tab #############################################
                 
                 tabPanel("Inputs",
                          icon = icon("upload"),
                          sidebarLayout(
                            
                            sidebarPanel(
                              
                              tags$h5("Upload dataset:"),
                              
                              fileInput("file1", "Choose CSV file",
                                        multiple = FALSE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")),
                              
                              prettyRadioButtons("sep", "Separator",
                                                 choices = c(Comma = ",",
                                                             Semicolon = ";",
                                                             Tab = "\t"),
                                                 selected = ",",
                                                 status = "danger"),
                              
                              prettyRadioButtons("quote", "Quote",
                                                 choices = c(None = "",
                                                             "Double Quote" = '"',
                                                             "Single Quote" = "'"),
                                                 selected = '"',
                                                 status = "danger"),
                              
                              hr(),
                              
                              prettyRadioButtons("type", "Sample type",
                                                 choices = c("Unweighted sample"         = "un",
                                                             "Stratified sample"         = "strat",
                                                             "One-stage cluster sample"  = "clust1",
                                                             "Two-stage cluster sample"  = "clust2",
                                                             "Stratified cluster sample" = "strat_clust"),
                                                 selected = "un",
                                                 status = "danger"),
                              
                              prettyRadioButtons("statistic", "Statistic (for numerical variables)",
                                                 choices = c("Mean" = "mean",
                                                             "Median" = "median"),
                                                 selected = "mean",
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
                              ),
                              
                              width = 3
                            ),
                            
                            mainPanel(
                              tableOutput("contents_data")
                            )
                          )
                 ),
                 
                 
                 #### * Analysis tab ###########################################
                 
                 tabPanel("Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              
                              pickerInput("select_indicator",
                                          label = "Indicator:",   
                                          choices = NULL,
                                          selected = NULL,
                                          multiple = FALSE,
                                          options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE)
                              ),
                              
                              prettySwitch("factor",
                                           label = "Format as factor",
                                           status = "success",
                                           fill = TRUE
                              ),
                              
                              prettySwitch("na",
                                           label = "Include missing values",
                                           status = "success",
                                           fill = TRUE
                              ),
                              
                              hr(),
                              
                              pickerInput("select_group",
                                          label = "Disaggregate by:",   
                                          choices = NULL,
                                          selected = NULL,
                                          multiple = TRUE,
                                          options = pickerOptions(maxOptions = 1, title = "Grouping variable 1", actionsBox = TRUE, liveSearch = TRUE)
                              ),
                              
                              pickerInput("select_group2",
                                          #label = "Grouping variable 2:",   
                                          choices = NULL,
                                          selected = NULL,
                                          multiple = TRUE,
                                          options = pickerOptions(maxOptions = 1, title = "Grouping variable 2", actionsBox = TRUE, liveSearch = TRUE)
                              ),
                              
                              hr(),
                              
                              conditionalPanel(condition = "input.type == 'strat'",
                                               pickerInput("select_strata",
                                                           label = "Filter by strata:",   
                                                           choices = NULL,
                                                           selected = NULL,
                                                           multiple = TRUE,
                                                           options = pickerOptions(title = "Strata", actionsBox = TRUE, liveSearch = TRUE)
                                               )
                              ),
                              
                              pickerInput("select_ind",
                                          label = "Filter by indicator:",   
                                          choices = NULL,
                                          selected = NULL,
                                          multiple = TRUE,
                                          options = pickerOptions(maxOptions = 1, title = "Select indicator", actionsBox = TRUE, liveSearch = TRUE)
                              ),
                              
                              uiOutput("filter_ind"),
                              
                              hr(),
                              
                              actionButton("table_reset", "Reset selection"),
                              
                              downloadButton("downloadData", "Download as CSV"),
                              
                              width = 3
                            ),
                            
                            mainPanel(
                              align = "center",
                              dataTableOutput("table"),
                              tags$i(h5(textOutput("no_data"), style = "color: red;")),
                              highchartOutput("plot"),
                              width = 9
                            )
                          ) #sidebarlayout
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
                                stringsAsFactors = TRUE)
        
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
  })
  
  
  #### * Dynamic UI elements ###################################################
  
  # render dataset for preview
  output$contents_data <- renderTable({
    req(input$file1)
    return(head(data_upload(), 5))
  }, na = "")
  
  
  # create reference lists
  indicators <- reactive({names(data_upload())})
  strata <- reactive({as.character(sort(unique(data_upload()$strata)))})
  list1 <- reactive({as.character(sort(unique(data_upload()[[input$select_ind]])))})
  
  
  # update choice lists with values from uploaded file
  observe({
    input$file1
    updatePickerInput(session, "select_indicator", choices = indicators())
    updatePickerInput(session, "select_group", choices = indicators())
    updatePickerInput(session, "select_group2", choices = indicators())
    updatePickerInput(session, "select_strata", choices = strata())
    updatePickerInput(session, "select_ind", choices = indicators())
  })
  
  
  # define dynamic filter variable
  output$filter_ind <- renderUI({
    
    req(input$select_ind != "")
    
    pickerInput("select_ind_value",
                #label = "",   
                choices = list1(),
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
  })
  
  
  #### * Design & analysis ##############################################
  
  # apply dynamic filters to datasets
  data_filter <- reactive({
    data_upload() %>%
      mutate_if(is.binary, as.factor) %>% # change class of binary variables
      execute_if(input$factor, mutate_at(input$select_indicator, as.factor)) %>%
      #execute_if(input$type == "strat", filter(is.null(input$select_strata) | strata  %in% input$select_strata)) %>%
      filter(if(input$type == "strat") is.null(input$select_strata) | strata  %in% input$select_strata else TRUE) %>%
      #execute_if(!is.null(input$select_ind), filter(is.null(input$select_ind_value) | get(input$select_ind) %in% input$select_ind_value))
      filter(if(!is.null(input$select_ind)) is.null(input$select_ind_value) | get(input$select_ind) %in% input$select_ind_value else TRUE)
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
  results <- reactive({
    
    if (is.factor(data_filter()[[input$select_indicator]])) {
      # factor variables (character or binary):
      results <- design() %>%
        execute_if(!input$na, filter(!is.na(get(input$select_indicator)))) %>%
        group_by_(if(is.null(input$select_group)) input$select_indicator else TRUE) %>%
        execute_if(!is.null(input$select_group), group_by_(input$select_group, input$select_indicator)) %>%
        execute_if(!is.null(input$select_group) & !is.null(input$select_group2), group_by_(input$select_group2, input$select_group, input$select_indicator)) %>%
        summarize(stat = survey_mean(na.rm = TRUE, vartype = "ci", proportion = TRUE),
                  count = unweighted(n()))
      
    } else {
      # numeric variables:
      results <- design() %>%
        filter(!is.na(input$select_indicator)) %>%
        group_by_(if(!is.null(input$select_group)) input$select_group else TRUE) %>%
        execute_if(!is.null(input$select_group) & !is.null(input$select_group2), group_by_(input$select_group2, input$select_group)) %>%
        execute_if(input$statistic == "mean", summarize(stat = survey_mean(get(input$select_indicator), na.rm=TRUE, vartype = "ci"),
                                                        count = unweighted(sum(!is.na(get(input$select_indicator)))))) %>%
        execute_if(input$statistic == "median", summarize(stat = survey_median(get(input$select_indicator), na.rm=TRUE, vartype = "ci"),
                                                          count = unweighted(sum(!is.na(get(input$select_indicator))))))
    }
  })

  
  # define error message in case there is no data
  output$no_data <- renderText({
    
    req(input$select_indicator != "")
    
    if (all(is.na(data_filter()[[input$select_indicator]]))) {
      "There is no data for this selection. Select another indicator."} else {NULL}
  })
  
  
  #### * Output: Table #########################################################
  
  # render output table
  output$table <- renderDT({
    
    req(input$select_indicator != "")
    
    table <- DT::datatable(
      results(),
      options = list(dom = 't', paging = FALSE#, order = list(1, 'desc')
      ),
      rownames = FALSE,
      style = 'bootstrap',
      class = 'table-condensed table-hover table-striped') %>%
      execute_if(is.factor(data_filter()[[input$select_indicator]]), formatPercentage(c("stat", "stat_low", "stat_upp"), 1)) %>%
      execute_if(!is.factor(data_filter()[[input$select_indicator]]), formatRound(c("stat", "stat_low", "stat_upp"), 1))
  })
  
  
  #### * Output: Plot ##########################################################
  
  # render plot
  output$plot <- renderHighchart({
    
    if (all(is.na(data_filter()[[input$select_indicator]]))) {
      return(NULL)
    } else {
      
      # factor variable - no disaggregation
      if (is.factor(data_filter()[[input$select_indicator]]) & is.null(input$select_group)) {
        results1 <- results() %>%
          mutate(var = get(input$select_indicator))
        plot <- hchart(results1, "column", hcaes(x = var, y = stat))
        
        # factor variable - 1 disaggregation variable
      } else if (is.factor(data_filter()[[input$select_indicator]]) & !is.null(input$select_group) & is.null(input$select_group2)) {
        results1 <- results() %>%
          mutate(var = get(input$select_indicator),
                 var2 = get(input$select_group))
        plot <- hchart(results1, "column", hcaes(x = var2, y = stat, group = var)) %>%
          hc_plotOptions(series=list(stacking='normal'))
        
        # factor variable - 2 disaggregation variables
      } else if (is.factor(data_filter()[[input$select_indicator]]) & !is.null(input$select_group) & !is.null(input$select_group2)) {
        results1 <- results() %>%
          mutate(var = get(input$select_indicator),
                 var2 = paste0(get(input$select_group2), " - ", get(input$select_group)))
        plot <- hchart(results1, "column", hcaes(x = var2, y = stat, group = var)) %>%
          hc_plotOptions(series=list(stacking='normal'))
        
        # numeric variable - no disaggregation
      } else if(is.null(input$select_group)) {
        plot <- hchart(results(), "column", hcaes(x = "1", y = stat))  
        
        # numeric variable - 1 disaggregation variable
      } else if(!is.null(input$select_group) & is.null(input$select_group2)) {
        results1 <- results() %>%
          mutate(var = get(input$select_group))
        plot <- hchart(results1, "column", hcaes(x = var, y = stat))  
        
        # numeric variable - 2 disaggregation variables
      } else if(!is.null(input$select_group) & !is.null(input$select_group2)) {
        results1 <- results() %>%
          mutate(var = paste0(get(input$select_group2), " - ", get(input$select_group)),
                 var2 = get(input$select_group2))
        plot <- hchart(results1, "column", hcaes(x = var, y = stat, group = var2)) %>%
          hc_plotOptions(series=list(stacking='normal'))
      }
      
      plot <- plot %>%
        execute_if(is.factor(data_filter()[[input$select_indicator]]), hc_yAxis(min = 0, max = 1, title = list(text="proportion"))) %>%
        execute_if(!is.factor(data_filter()[[input$select_indicator]]) & input$statistic == "mean", hc_yAxis(title = list(text="mean"))) %>%
        execute_if(!is.factor(data_filter()[[input$select_indicator]]) & input$statistic == "median", hc_yAxis(title = list(text="median"))) %>%
        hc_tooltip(valueDecimals = 3) %>%
        hc_colors(cols1) %>%
        hc_title(
          text = input$select_indicator,
          style = list(fontSize = "12px")
        ) %>%
        hc_xAxis(title = "") %>%
        hc_exporting(
          enabled = TRUE,
          filename = paste0("plot_export-", Sys.Date()),
          buttons = list(contextButton = list(menuItems = list("downloadPNG", "downloadPDF", "downloadCSV"))),
          #sourceWidth = 1000,
          sourceHeight = 700
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
  
}

shinyApp(ui = ui, server = server)
