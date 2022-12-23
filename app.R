###############################################################################
#
# Authors:      Jessica Nephin
# Affiliation:  Fisheries and Oceans Canada (DFO)
# Contact:      e-mail: jessica.nephin@dfo-mpo.gc.ca
#
# Overview:
# A shiny app for dive and transect logging in the field. Created for the 
# Non-Destructive Survey Tool program. The goal is to collect all relevant 
# metadata associated with ROV dives.
#
# Requirements:
# *  SQLite
# *  R version 4.2.1 or greater
#
# Notes: Based on this example
# https://www.nielsvandervelden.com/blog/editable-datatables-in-r-shiny-using-sql/
#
###############################################################################


## To do
# - 1) change equip configuration input to variable:value pairs
#   try adding a list of possible variables to set up:
#   c('pan','tilt','yaw','offset_aft', 'offset_fwd', offset_strd', 'offset_port', 'zoom', 'aperture', 'x','y')
#   maybe this could work as a select box with an adjacent text box and an input button
#   when the button is clicked it would have to save the input to a {} string and 
#   then reset the select box and text for another input. The input could be sent to a renderText{} uiOutput
#   which could be edited if needed and that would be used as the configuration input$field
# - 2) Maybe change 'leg' to numeric with a numeric input 
# - 3) Move issues in annotations to gitlab issues 


# Notes:
# - when you delete a dive it does not delete any of its transects


# Packages
library(shiny)
library(DT)
library(RSQLite)
library(pool)
library(shinyjs)
library(uuid)
library(dplyr)
library(shinydashboard)
library(purrr)


##############################
#           Set-up           #
##############################


# Connect to db
dbname <- 'divelogging-db.sqlite'
db <- dbConnect(RSQLite::SQLite(), dbname)
# Load all tables from db into workspace
alltabs <- lapply(setNames(nm = dbListTables(db)), dbReadTable, conn = db)
#list2env(alltabs, envir=environment())
dbDisconnect(db)


# Clone the existing db
pool <- dbPool(db)



###############################
#       Common functions      #
###############################

# Label mandatory fields function
labelMandatory <- function(label) {
  tagList(
    label,
    span('*', class = 'mandatory_star')
  )
}

appCSS <- list('.unique_star'='color: red',
               '.mandatory_star'='color: red')
               




##############################
#             UI             #
##############################

# Dashboard UI content

# Header
header <- dashboardHeader(title = 'NDST Dive Logging App',
                          tags$li(class = 'dropdown', 
                                  style='position:fixed;right:10px;top:10px;',
                                  actionButton("export", "Export", 
                                               icon('file-export'))
                          )
)

# Sidebar
sidebar <- dashboardSidebar( 
  sidebarMenu(
    menuItem('Cruise', tabName = 'cruisetab', icon = icon('ship')),
    menuItem('Personnel', tabName = 'peopletab', icon = icon('user-group')),
    menuItem('Equipment list', tabName = 'equiptab', icon = icon('list-ul')),
    menuItem('Equipment configurations', tabName = 'econfigtab', icon = icon('screwdriver-wrench')),
    menuItem('Dive configurations', tabName = 'dconfigtab', icon = icon('sliders')),
    menuItem('Dives', tabName = 'divetab', icon = icon('water')),
    menuItem('Transects', tabName = 'trantab', icon = icon('arrow-right'))
    
  )
)
# Body
body <- body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'divetab',
            h2('Dives'),
            fluidPage(
              shinyjs::useShinyjs(),
              shinyjs::inlineCSS(appCSS),
              fluidRow(
                actionButton('add_button_dives', 'Add', icon('plus')),
                actionButton('edit_button_dives', 'Edit', icon('edit')),
                actionButton('delete_button_dives', 'Delete', icon('trash-alt'))
              ),
              br(),
              fluidRow(width='100%',
                       dataTableOutput('responses_dives', width = '100%')
              )
            )
    ),
    tabItem(tabName = 'trantab',
            h2('Transects'),
            fluidPage(
              shinyjs::useShinyjs(),
              shinyjs::inlineCSS(appCSS),
              fluidRow(
                actionButton('edit_button_transects', 'Edit', icon('edit')),
                actionButton('delete_button_transects', 'Delete', icon('trash-alt'))
              ),
              br(),
              fluidRow(width='100%',
                       dataTableOutput('responses_transects', width = '100%')
              )
            )
    ),
    tabItem(tabName = 'cruisetab',
            h2('Cruise'),
            fluidPage(
              shinyjs::useShinyjs(),
              shinyjs::inlineCSS(appCSS),
              fluidRow(
                actionButton('add_button_cruise', 'Add', icon('plus')),
                actionButton('edit_button_cruise', 'Edit', icon('edit')),
                actionButton('delete_button_cruise', 'Delete', icon('trash-alt'))
              ),
              br(),
              fluidRow(width='100%',
                       dataTableOutput('responses_cruise', width = '100%')
              )
            )
    ),
    tabItem(tabName = 'peopletab',
            h2('Personnel'),
            fluidPage(
              shinyjs::useShinyjs(),
              shinyjs::inlineCSS(appCSS),
              fluidRow(
                actionButton('add_button_people', 'Add', icon('plus')),
                actionButton('edit_button_people', 'Edit', icon('edit')),
                actionButton('delete_button_people', 'Delete', icon('trash-alt'))
              ),
              br(),
              fluidRow(width='100%',
                       dataTableOutput('responses_people', width = '100%')
              )
            )
    ),
    tabItem(tabName = 'equiptab',
            h2('Equipment list'),
            fluidPage(
              shinyjs::useShinyjs(),
              shinyjs::inlineCSS(appCSS),
              fluidRow(
                actionButton('add_button_equipment', 'Add', icon('plus')),
                actionButton('edit_button_equipment', 'Edit', icon('edit')),
                actionButton('delete_button_equipment', 'Delete', icon('trash-alt'))
              ),
              br(),
              fluidRow(width='100%',
                       dataTableOutput('responses_equipment', width = '100%')
              )
            )
    ), tabItem(tabName = 'econfigtab',
               h2('Instrument and platform configurations'),
               fluidPage(
                 shinyjs::useShinyjs(),
                 shinyjs::inlineCSS(appCSS),
                 fluidRow(
                   actionButton('add_button_equipconfig', 'Add', icon('plus')),
                   actionButton('edit_button_equipconfig', 'Edit', icon('edit')),
                   actionButton('delete_button_equipconfig', 'Delete', icon('trash-alt'))
                 ),
                 br(),
                 fluidRow(width='100%',
                          dataTableOutput('responses_equipconfig', width = '100%')
                 )
               )
    ), tabItem(tabName = 'dconfigtab',
               h2('Dive configurations'),
               fluidPage(
                 shinyjs::useShinyjs(),
                 shinyjs::inlineCSS(appCSS),
                 fluidRow(
                   actionButton('add_button_diveconfig', 'Add', icon('plus')),
                   actionButton('edit_button_diveconfig', 'Edit', icon('edit')),
                   actionButton('delete_button_diveconfig', 'Delete', icon('trash-alt'))
                 ),
                 br(),
                 fluidRow(width='100%',
                          dataTableOutput('responses_diveconfig', width = '100%')
                 )
               )
    )
  )
)


# Dashboard ui
ui <-  dashboardPage(header, sidebar, body)




##############################
#           SERVER           #
##############################


# Server
server <- function(input, output, session) {

  
  ##############################
  #          Dashboard         #
  ##############################
  
  # Export all tables as csv
  observeEvent(input$export, {
    # Load all tables from db into workspace
    alltabs <- lapply(setNames(nm = dbListTables(pool)), dbReadTable, conn = pool)
    # Make export folder
    datefolder <- as.character(format(Sys.Date(), format='%Y-%m-%d')) 
    if(!dir.exists(file.path('Exports',datefolder))) 
      dir.create(file.path('Exports',datefolder), recursive = T)
    # Export all tables to csv
    for(i in names(alltabs)){
      name <- paste0(alltabs[['cruise']]$name[1], '_', i, '.csv')
      write.csv(alltabs[[i]], file.path('Exports', datefolder, name), row.names = F)
    }
  })
  
  
  ##############################
  #           Tables           #
  ##############################
  
  # Make inputs reactive and load tables
  makeReactive <- function(table) {
    # reactive
    tmp <- reactive({
      # Make reactive
      input[[paste0('submit_', table)]]
      input[[paste0('submit_edit_', table)]]
      input[[paste0('yes_delete_', table)]]
      # Load table
      dbReadTable(pool, table)
    })
    
    # Return
    return(tmp)
  }
  
  # Make tables reactive
  dives_df <- makeReactive('dives')
  transects_df <- makeReactive('transects')
  cruise_df <- makeReactive('cruise')
  equipment_df <- makeReactive('equipment')
  people_df <- makeReactive('people')
  equipconfig_df <- makeReactive('equipconfig')
  diveconfig_df <- makeReactive('diveconfig')
  
  
  # Display dives table
  output$responses_dives <- DT::renderDataTable({
    table <- dives_df() %>% select(-row_id) 
    table <- datatable(table, rownames = FALSE, selection = 'single',
                       options = list(searching = FALSE, lengthChange = FALSE)
    )
  })
  
  # Display transects table
  output$responses_transects <- DT::renderDataTable({
    table <- transects_df() %>% select(-row_id) 
    table <- datatable(table, rownames = FALSE, selection = 'single',
                       options = list(searching = FALSE, lengthChange = FALSE)
    )
  })
  
  # Display cruise table
  output$responses_cruise <- DT::renderDataTable({
    table <- cruise_df() %>% select(-row_id)
    table <- datatable(table, rownames = FALSE, selection = 'single',
                       options = list(searching = FALSE, lengthChange = FALSE)
    )
  })
  
  # Display equipment table
  output$responses_equipment <- DT::renderDataTable({
    table <- equipment_df() %>% select(-row_id)
    table <- datatable(table, rownames = FALSE, selection = 'single',
                       options = list(searching = FALSE, lengthChange = FALSE)
    )
  })
  
  # Display personnel table
  output$responses_people <- DT::renderDataTable({
    table <- people_df() %>% select(-row_id)
    table <- datatable(table, rownames = FALSE, selection = 'single',
                       options = list(searching = FALSE, lengthChange = FALSE)
    )
  })
  
  # Display equip config table
  output$responses_equipconfig <- DT::renderDataTable({
    table <- equipconfig_df() %>% select(-row_id)
    table <- datatable(table, rownames = FALSE, selection = 'single',
                       options = list(searching = FALSE, lengthChange = FALSE)
    )
  })
  
  # Display dive config table
  output$responses_diveconfig <- DT::renderDataTable({
    table <- diveconfig_df() %>% select(-row_id)
    table <- datatable(table, rownames = FALSE, selection = 'single',
                       options = list(searching = FALSE, lengthChange = FALSE)
    )
  })
  
  
  ##############################
  #        Form data           #
  ##############################
  
  # Save form data as data.frame, reactive to changes in input
  
  # Dives
  dives_FormData <- reactive({
    dftable <- data.frame(row_id = UUIDgenerate(),
                          cruise_name = input$dive_cruisename,
                          leg = input$dive_cruiseleg,
                          name = input$dive_name, 
                          pilot = input$dive_pilot,
                          start_time = input$dive_starttime,
                          end_time = input$dive_endtime,
                          site_name = input$dive_sitename,
                          dive_config = input$dive_diveconfig,
                          objective = input$dive_objective,
                          summary = input$dive_summary,
                          note = input$dive_note,
                          stringsAsFactors = FALSE)
    return(dftable)
  })
  
  # Transects
  transects_FormData <- reactive({
    dftable <- data.frame(row_id = UUIDgenerate(),
                          cruise_name = input$transect_cruisename,
                          leg = input$transect_cruiseleg,
                          dive_name = input$transect_divename, 
                          name = input$transect_name, 
                          start_time = input$transect_starttime,
                          end_time = input$transect_endtime,
                          objective = input$transect_objective,
                          summary = input$transect_summary,
                          note = input$transect_note,
                          stringsAsFactors = FALSE)
    return(dftable)
  })
  
  
  # Cruise
  cruise_FormData <- reactive({
    dftable <- data.frame(row_id = UUIDgenerate(),
                          name = input$cruise_name,
                          leg = input$cruise_leg,
                          objective = input$cruise_objective,
                          summary = input$cruise_summary,
                          note = input$cruise_note,
                          stringsAsFactors = FALSE)
    return(dftable)
  })
  
  # equipment
  equipment_FormData <- reactive({
    dftable <- data.frame(row_id = UUIDgenerate(),
                          short_code = input$equip_shortcode,
                          brand = input$equip_brand,
                          model = input$equip_model,
                          serial_number = input$equip_serialnumber,
                          type = input$equip_type,
                          note = input$equip_note,
                          stringsAsFactors = FALSE)
    return(dftable)
  })
  
  # people
  people_FormData <- reactive({
    dftable <- data.frame(row_id = UUIDgenerate(),
                          initials = input$initials,
                          first_name = input$first_name,
                          last_name = input$last_name,
                          email = input$email,
                          stringsAsFactors = FALSE)
    return(dftable)
  })
  
  # equip config
  equipconfig_FormData <- reactive({
    dftable <- data.frame(row_id = UUIDgenerate(),
                          name = input$equipconfig_name,
                          short_code = input$equipconfig_shortcode,
                          type = input$equipconfig_type,
                          configuration = input$equipconfig_configuration,
                          note = input$equipconfig_note,
                          stringsAsFactors = FALSE)
    return(dftable)
  })

    # dive config
  diveconfig_FormData <- reactive({
    dftable <- data.frame(row_id = UUIDgenerate(),
                          name = input$diveconfig_name,
                          ship_config = input$ship_config,
                          sub_config = input$sub_config,
                          ship_instrument_configs = paste0(input$ship_instrument_configs,collapse=';'),
                          sub_instrument_configs = paste0(input$sub_instrument_configs,collapse=';'),
                          note = input$diveconfig_note,
                          stringsAsFactors = FALSE)
    return(dftable)
  })
 

  ##############################
  #        Input forms         #
  ##############################
  
  # Mandatory fields for submission function
  obsFieldsMandatory <- function(table, fields){
    # Define which input fields are mandatory 
    observe({
      mandatoryFilled <- vapply(fields ,function(x) {!is.null(input[[x]]) && input[[x]][1] != ''}, logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      shinyjs::toggleState(id = paste0('submit_',table), condition = mandatoryFilled)
    })
  }
  
  # Set mandatory fields
  obsFieldsMandatory(table='dives', fields=c('dive_cruisename', 'dive_cruiseleg', 'dive_name', 'dive_pilot', 
                                             'dive_diveconfig','dive_starttime', 'dive_endtime'))
  obsFieldsMandatory(table='transects', fields=c('transect_cruisename', 'transect_cruiseleg', 'transect_divename', 
                                                 'transect_name','transect_starttime', 'transect_endtime'))
  obsFieldsMandatory(table='cruise', fields=c('cruise_name', 'cruise_leg', 'cruise_objective'))
  obsFieldsMandatory(table='equipment', fields=c('equip_shortcode', 'equip_brand', 'equip_model','equip_serialnumber',
                                                 'equip_type'))
  obsFieldsMandatory(table='people', fields=c('initials', 'first_name', 'last_name','email'))
  obsFieldsMandatory(table='equipconfig', fields=c('equipconfig_name', 'equipconfig_shortcode', 'equipconfig_type'))
  obsFieldsMandatory(table='diveconfig', fields=c('diveconfig_name', 'ship_config', 'sub_config', 
                                                  'ship_instrument_configs','sub_instrument_configs'))
  
  
  # Form for dive data entry
  dives_entryform <- function(button_id){
    showModal(
      modalDialog(
        h2('Dive'),
        div(id=('dives_entryform'),
            tags$head(tags$style('.modal-dialog{ width:450px}')),
            tags$head(tags$style(HTML('.shiny-split-layout > div {overflow: visible}'))),
            fluidPage(
              fluidRow(
                splitLayout(
                  cellWidths = c('200px', '200px'),
                  cellArgs = list(style = 'vertical-align: top'),
                  # Use the last record for cruise name and leg as defaults
                  textInput('dive_cruisename', labelMandatory('Cruise name'), cruise_df()$name[nrow(cruise_df())]),
                  textInput('dive_cruiseleg', labelMandatory('Leg'),  cruise_df()$leg[nrow(cruise_df())])
                ),
                splitLayout(
                  cellWidths = c('200px', '200px'),
                  cellArgs = list(style = 'vertical-align: top'),
                  textInput('dive_name', labelMandatory('Dive name'),  ''),
                  actionButton('check_dives', 'Check', icon = icon('key')),
                ),
                splitLayout(
                  cellWidths = c('200px', '200px'),
                  cellArgs = list(style = 'vertical-align: top'),
                  selectInput('dive_diveconfig', labelMandatory('Dive config'), 
                              choices=diveconfig_df()$name, selected=diveconfig_df()$name[nrow(diveconfig_df())]),
                  selectInput('dive_pilot', labelMandatory('Pilot'), people_df()$initials),
                ),
                splitLayout(
                  cellWidths = c('300px', '100px'),
                  cellArgs = list(style = 'vertical-align: top'),
                  uiOutput('out_dive_starttime'),
                  actionButton('set_dive_starttime', 'Start', icon = icon("clock"), class = "btn-primary"),
                ),
                splitLayout(
                  cellWidths = c('150px', '250px'),
                  cellArgs = list(style = 'vertical-align: top'),
                  actionButton('add_button_transects', 'Add Transect', icon('plus'), class = 'btn-warning'),
                  textOutput('Text_transect')
                ),
                br(), #br(),
                splitLayout(
                  cellWidths = c('300px', '100px'),
                  cellArgs = list(style = 'vertical-align: top'),
                  uiOutput('out_dive_endtime'),
                  actionButton('set_dive_endtime', 'End', icon = icon("clock"), class = "btn-primary"),
                ),
                tags$style(type='text/css', '#set_dive_starttime { width:100%; margin-top: 25px}'),
                tags$style(type='text/css', '#set_dive_endtime { width:100%; margin-top: 25px}'),
                tags$style(type='text/css', '#check_dives { width:100%; margin-top: 25px}'),
                tags$style(type='text/css', '#Text_transect { width:100%; margin-top: 5px; font-size: 16px}'),
                textInput('dive_sitename', 'Site name', ''),
                textInput('dive_objective', 'Objective', ''),
                textInput('dive_summary', 'Summary', ''),
                textInput('dive_note', 'Note', ''),
                helpText(labelMandatory(''), paste('Mandatory field')),
                actionButton(button_id, 'Submit', class = 'btn-warning')
              ),
              easyClose = FALSE
            )
        )
      )
    )
  }
  
  # Form for transect data entry
  transects_entryform <- function(button_id){
    # Use the current record for cruise name and leg as defaults
    showModal(
      modalDialog(
        h2('Transect'),
        div(id=('transects_entryform'),
            tags$head(tags$style('.modal-dialog{ width:450px}')),
            tags$head(tags$style(HTML('.shiny-split-layout > div {overflow: visible}'))),
            fluidPage(
              fluidRow(
                splitLayout(
                  cellWidths = c('200px', '200px'),
                  cellArgs = list(style = 'vertical-align: top'),
                  textInput('transect_cruisename', labelMandatory('Cruise name'), current_dive[1, 'cruise_name']),
                  textInput('transect_cruiseleg', labelMandatory('Leg'),  current_dive[1, 'leg'])
                ),
                splitLayout(
                  cellWidths = c('200px', '200px'),
                  cellArgs = list(style = 'vertical-align: top'),
                  textInput('transect_divename', labelMandatory('Dive name'), current_dive[1, 'name'] ),
                  textInput('transect_name', labelMandatory('Transect name'),  paste0(current_dive[1, 'name'],'_1'))
                ),
                splitLayout(
                  cellWidths = c('300px', '100px'),
                  cellArgs = list(style = 'vertical-align: top'),
                  uiOutput('out_transect_starttime'),
                  actionButton('set_transect_starttime', 'Start', icon = icon("clock"), class = "btn-primary"),
                ),
                splitLayout(
                  cellWidths = c('300px', '100px'),
                  cellArgs = list(style = 'vertical-align: top'),
                  uiOutput('out_transect_endtime'),
                  actionButton('set_transect_endtime', 'End', icon = icon("clock"), class = "btn-primary"),
                ),
                tags$style(type='text/css', '#set_transect_starttime { width:100%; margin-top: 25px}'),
                tags$style(type='text/css', '#set_transect_endtime { width:100%; margin-top: 25px}'),
                textInput('transect_objective', 'Objective', ''),
                textInput('transect_summary', 'Summary', ''),
                textInput('transect_note', 'Note', ''),
                helpText(labelMandatory(''), paste('Mandatory field')),
                actionButton(button_id, 'Submit', class = 'btn-warning')
              ),
              easyClose = FALSE
            )
        )
      )
    )
  }
  
  # Form for cruise data entry
  cruise_entryform <- function(button_id){
    showModal(
      modalDialog(
        h2('Cruise'),
        div(id=('cruise_entryform'),
            tags$head(tags$style('.modal-dialog{ width:450px}')),
            tags$head(tags$style(HTML('.shiny-split-layout > div {overflow: visible}'))),
            fluidPage(
              fluidRow(
                splitLayout(
                  cellWidths = c('250px', '150px'),
                  cellArgs = list(style = 'vertical-align: top'),
                  textInput('cruise_name', labelMandatory('Cruise name'), ''),
                  textInput('cruise_leg', labelMandatory('Leg'),  '')
                ),
                textInput('cruise_objective', labelMandatory('Objective'), ''),
                textInput('cruise_summary', 'Summary', ''),
                textInput('cruise_note', 'Note', ''),
                helpText(labelMandatory(''), paste('Mandatory field')),
                actionButton(button_id, 'Submit', class = 'btn-warning')
              ),
              easyClose = FALSE
            )
        )
      )
    )
  }
  
  # Form for equipment data entry
  equipment_entryform <- function(button_id){
    showModal(
      modalDialog(
        h2('Equipment'),
        div(id=('equipment_entryform'),
            tags$head(tags$style('.modal-dialog{ width:400px}')),
            tags$head(tags$style(HTML('.shiny-split-layout > div {overflow: visible}'))),
            fluidPage(
              fluidRow(
                textInput('equip_shortcode', labelMandatory('Short code'), ''),
                textInput('equip_brand', labelMandatory('Brand'),  ''),
                textInput('equip_model', labelMandatory('Model'), ''),
                textInput('equip_serialnumber', labelMandatory('Serial number'), ''),
                textInput('equip_type',labelMandatory('Type'), ''),
                textInput('equip_note', 'Note', ''),
                helpText(labelMandatory(''), paste('Mandatory field')),
                actionButton(button_id, 'Submit', class = 'btn-warning')
              ),
              easyClose = FALSE
            )
        )
      )
    )
  }
  
  # Form for personnel data entry
  people_entryform <- function(button_id){
    showModal(
      modalDialog(
        h2('Personnel'),
        div(id=('people_entryform'),
            tags$head(tags$style('.modal-dialog{ width:400px}')),
            tags$head(tags$style(HTML('.shiny-split-layout > div {overflow: visible}'))),
            fluidPage(
              fluidRow(
                textInput('initials', labelMandatory('Initials'), ''),
                textInput('first_name', labelMandatory('First name'),  ''),
                textInput('last_name', labelMandatory('Last name'), ''),
                textInput('email', labelMandatory('Email'), ''),
                helpText(labelMandatory(''), paste('Mandatory field')),
                actionButton(button_id, 'Submit', class = 'btn-warning')
              ),
              easyClose = FALSE
            )
        )
      )
    )
  }
  
  # Form for equipment configuration data entry
  equipconfig_entryform <- function(button_id){
    showModal(
      modalDialog(
        h2('Equipment configuration'),
        div(id=('equipconfig_entryform'),
            tags$head(tags$style('.modal-dialog{ width:400px}')),
            tags$head(tags$style(HTML('.shiny-split-layout > div {overflow: visible}'))),
            fluidPage(
              fluidRow(
                selectInput('equipconfig_shortcode', labelMandatory('Short code'), equipment_df()$short_code),
                selectInput('equipconfig_type', labelMandatory('Type'), c('platform','instrument')),
                textInput('equipconfig_name', labelMandatory('Name'), ''),
                textInput('equipconfig_configuration', 'Configuration', ''),
                textInput('equipconfig_note', 'Note', ''),
                helpText(labelMandatory(''), paste('Mandatory field')),
                actionButton(button_id, 'Submit', class = 'btn-warning')
              ),
              easyClose = FALSE
            )
        )
      )
    )
  }
  
  # Form for dive configuration data entry
  diveconfig_entryform <- function(button_id){
    showModal(
      modalDialog(
        h2('Dive configuration'),
        div(id=('diveconfig_entryform'),
            tags$head(tags$style('.modal-dialog{ width:400px}')),
            tags$head(tags$style(HTML('.shiny-split-layout > div {overflow: visible}'))),
            fluidPage(
              fluidRow(
                textInput('diveconfig_name', labelMandatory('Name'), ''),
                selectizeInput('ship_config', labelMandatory('Ship'), choices=equipconfig_df()$name, multiple=T, options = list(maxItems = 1)),
                selectizeInput('sub_config', labelMandatory('Sub'), choices=equipconfig_df()$name, multiple=T, options = list(maxItems = 1)),
                selectInput('ship_instrument_configs', labelMandatory('Ship instruments'), choices=equipconfig_df()$name, multiple=T),
                selectInput('sub_instrument_configs', labelMandatory('Sub instruments'), choices=equipconfig_df()$name, multiple=T),
                textInput('diveconfig_note', 'Note', ''),
                helpText(labelMandatory(''), paste('Mandatory field')),
                actionButton(button_id, 'Submit', class = 'btn-warning')
              ),
              easyClose = FALSE
            )
        )
      )
    )
  }
  
  
  
  
  ##################################
  #   Start and End time buttons   #
  ##################################
  
  # Make reactive
  datetimes <- reactiveValues(dive_start = '',
                              dive_end = '',
                              transect_start = '',
                              transect_end = '')

  # For dives
  
  # Event observers for datetimes
  observeEvent(input$set_dive_starttime,{
    datetimes$dive_start <- as.character(format(Sys.time(), format='%Y-%m-%dT-%TZ', tz='GMT'))
  })
  observeEvent(input$set_dive_endtime,{
    datetimes$dive_end <- as.character(format(Sys.time(), format='%Y-%m-%dT-%TZ', tz='GMT'))
  })

  # Generate the textInput for datetimes
  output$out_dive_starttime <- renderUI({
    textInput('dive_starttime', labelMandatory('Start time'), datetimes$dive_start)
  })
  output$out_dive_endtime <- renderUI({
    textInput('dive_endtime', labelMandatory('End time'), datetimes$dive_end)
  })

 # For transects
  
  # Event observers for datetimes
  observeEvent(input$set_transect_starttime,{
    datetimes$transect_start <- as.character(format(Sys.time(), format='%Y-%m-%dT-%TZ', tz='GMT'))
  })
  observeEvent(input$set_transect_endtime,{
    datetimes$transect_end <- as.character(format(Sys.time(), format='%Y-%m-%dT-%TZ', tz='GMT'))
  })
  
  # Generate the textInput for datetimes
  output$out_transect_starttime <- renderUI({
    textInput('transect_starttime', labelMandatory('Start time'), datetimes$transect_start)
  })
  output$out_transect_endtime <- renderUI({
    textInput('transect_endtime', labelMandatory('End time'), datetimes$transect_end)
  })
  
  
  
  ##############################
  #       Observe Events       #
  ##############################
  
  
  # Function for common observe events
  # Row last clicked is the row selected because only one row can be selected at a time
  obsEvents <- function(table){
    
    # Observe event for opening the form, high priority to ensure no reactive values
    # are updated until the event is finished.
    observeEvent(input[[paste0('add_button_', table)]], priority = 20,{
      # If add transects button is clicked, then same dive forms data before opening transect form
      if(table == 'transects')  current_dive <<- dives_FormData()
      if(table == 'dives')  current_dive <<- data.frame()
      # Open form
      get(paste0(table,'_entryform'))(paste0('submit_', table))
      # Disable submit, is enabled when mandatory field is entered
      shinyjs::disable(id = paste0('submit_',table))
    })
    
    # Observe event for opening the delete modal
    observeEvent(input[[paste0('delete_button_', table)]], priority = 20,{
      showModal(
        # If row selected
        if(length(input[[paste0('responses_',table,'_rows_selected')]])==1 ){
          modalDialog(
            title = 'Delete selected row?',
            'Warning: This action cannot be undone!',
            footer = tagList(
              actionButton(paste0('yes_delete_',table), 'Yes, Delete'),
              modalButton('Cancel')
            ), 
            easyClose = TRUE
          )
        } else {
          modalDialog(
            paste('Please select a row' ),
            easyClose = TRUE
          )
        }
      )
    })
    
    # Observe event for deleting selected rows
    observeEvent(input[[paste0('yes_delete_',table)]], priority = 20,{
      # Read table from db pool
      SQL_df <- dbReadTable(pool, table)
      row_selection <- SQL_df[input[[paste0('responses_',table,'_row_last_clicked')]], 'row_id']
      # Delete row
      dbExecute(pool, sprintf('DELETE FROM "%s" WHERE "row_id" == ("%s")', table, row_selection))
      removeModal()
    })
    
    
    # Observe event for submiting form data, append to db and reset _entryform
    # FormData() table needs to be called within observe event to be reactive
    observeEvent(input[[paste0('submit_', table)]], priority = 20,{
      # Get form data
      formdata <- get(paste0(table,'_FormData'))
      # Query
      quary <- sqlAppendTable(pool, table, formdata(), row.names = FALSE)
      dbExecute(pool, quary)
      # Reset 
      if( table == 'dives'){
        datetimes$dive_start <- ''
        datetimes$dive_end <- ''
      }
      if( table == 'transects'){
        datetimes$transect_start <- ''
        datetimes$transect_end <- ''
      }
      removeModal()
      shinyjs::reset(paste0(table,'_entryform'))
    })
  }
  
  
  # Dive observer events
  obsEvents(table='dives')
  obsEvents(table='transects')
  obsEvents(table='cruise')
  obsEvents(table='equipment')
  obsEvents(table='people')
  obsEvents(table='equipconfig')
  obsEvents(table='diveconfig')
  

  
#######################################
#     Dives Checks, Edit, Restore     #
#######################################

  # Check for unique dive names
  observeEvent(input$check_dives,{
    if( !(input$dive_name %in% dives_df()$name) ){
      updateActionButton(session, 'check_dives', label='Checked', icon=icon('check'))
      shinyjs::addClass(id = 'check_dives', class='btn-success')
    } else {
      updateActionButton(session, 'check_dives', label='Duplicate, try again', icon=icon('xmark'))
      shinyjs::removeClass(id = 'check_dives', class='btn-success')
    }
  })
  
  
  # Add transect text to dive form after transect is submitted
  output$Text_transect <- renderText({
    # Reacts to
    req(input$submit_transects) 
    input$add_button_dives
    input$edit_button_dives
    # If current dive table is empty
    if ( nrow(current_dive) == 0 ){
      out <- ''
    } else {
      # Current dive name
      divename <- current_dive[1, 'name']
      # Read table from db pool
      SQL_df <- dbReadTable(pool, 'transects')
      # Get transects
      trans <- SQL_df[SQL_df$dive_name == divename, 'name']
      out <- paste('Transects:', paste(trans, collapse = ', '))
    } 
    out
  })
  

# Edit data
# !!! Odd behaviour when you start app and open form, start and end times will appear empty
  #   if you dismiss the form and open the row again it will show the correct datetimes
# Update form values in the selected row.
  observeEvent(input$edit_button_dives, priority = 20,{
    # Fetch db data
    SQL_df <- dbReadTable(pool, 'dives')
    # Warnings for selection
    showModal(
      if(length(input$responses_dives_rows_selected) < 1){
        modalDialog(
          paste('Please select a row' ),easyClose = TRUE)
      })
    # If one row is selected open form and update
    if(length(input$responses_dives_rows_selected) == 1 ){
      # Form
      dives_entryform('submit_edit_dives')
      # Update
      updateTextInput(session, 'dive_cruisename', value = SQL_df[input$responses_dives_rows_selected, 'cruise_name'])
      updateTextInput(session,'dive_cruiseleg', value = SQL_df[input$responses_dives_rows_selected, 'leg'])
      updateTextInput(session,'dive_name', value = SQL_df[input$responses_dives_rows_selected, 'name'])
      updateTextInput(session,'dive_sitename',  value = SQL_df[input$responses_dives_rows_selected, 'site_name'])
      updateSelectInput(session,'dive_diveconfig',  selected = SQL_df[input$responses_dives_rows_selected, 'dive_config'])
      updateSelectInput(session,'dive_pilot',  selected = SQL_df[input$responses_dives_rows_selected, 'pilot'])
      updateTextInput(session,'dive_starttime', value = SQL_df[input$responses_dives_rows_selected, 'start_time'])
      updateTextInput(session,'dive_endtime', value = SQL_df[input$responses_dives_rows_selected, 'end_time'])
      updateTextInput(session,'dive_objective', value = SQL_df[input$responses_dives_rows_selected, 'objective'])
      updateTextInput(session,'dive_summary', value = SQL_df[input$responses_dives_rows_selected, 'summary'])
      updateTextInput(session, 'dive_note', value = SQL_df[input$responses_dives_rows_selected, 'note'])
      # current dive
      current_dive <<- SQL_df[input$responses_dives_rows_selected,]
    }
  })

# Updates the selected row with the values that were entered in the form, based on the row last clicked.
observeEvent(input$submit_edit_dives, priority = 20, {
  # Get db data
  SQL_df <- dbReadTable(pool, 'dives')
  row_id <- SQL_df[input$responses_dives_row_last_clicked, 'row_id']
  dbExecute(pool, sprintf('UPDATE "dives" SET "cruise_name" = ?, "leg" = ?, "name" = ?, "pilot" = ?, "start_time" = ?,
                            "end_time" = ?, "site_name" = ?, "dive_config" = ? , "objective" = ?, "summary" = ?,
                            "note" = ? WHERE "row_id" = ("%s")', row_id),
            param = list(input$dive_cruisename,
                         input$dive_cruiseleg,
                         input$dive_name,
                         input$dive_pilot,
                         input$dive_starttime,
                         input$dive_endtime,
                         input$dive_sitename,
                         input$dive_diveconfig,
                         input$dive_objective,
                         input$dive_summary,
                         input$dive_note))
  # Reset
  datetimes$dive_start <- ''
  datetimes$dive_end <- ''
  removeModal()
  
})


# Restore dive entry form modal, after submitting transect record
observeEvent(input$submit_transects, {
  # Open form
  dives_entryform('submit_dives')
  # Update
  updateTextInput(session, 'dive_cruisename', value = current_dive[1, 'cruise_name'])
  updateTextInput(session,'dive_cruiseleg', value = current_dive[1, 'leg'])
  updateTextInput(session,'dive_name', value = current_dive[1, 'name'])
  updateTextInput(session,'dive_sitename',  value = current_dive[1, 'site_name'])
  updateSelectInput(session,'dive_diveconfig',  selected = current_dive[1, 'dive_config'])
  updateSelectInput(session,'dive_pilot',  selected = current_dive[1, 'pilot'])
  updateTextInput(session,'dive_starttime', value = current_dive[1, 'start_time'])
  updateTextInput(session,'dive_endtime', value = current_dive[1, 'end_time'])
  updateTextInput(session,'dive_objective', value = current_dive[1, 'objective'])
  updateTextInput(session,'dive_summary', value = current_dive[1, 'summary'])
  updateTextInput(session, 'dive_note', value = current_dive[1, 'note'])

})


##################################
#         Transects Edit         #
##################################

# Edit data
observeEvent(input$edit_button_transects, priority = 20,{
  # Fetch db data
  SQL_df <- dbReadTable(pool, 'transects')
  # Warnings for selection
  showModal(
    if(length(input$responses_transects_rows_selected) < 1){
      modalDialog(
        paste('Please select a row' ),easyClose = TRUE)
    })
  # If one row is selected open form and update
  if(length(input$responses_transects_rows_selected) == 1 ){
    # Form
    transects_entryform('submit_edit_transects')
    # Update
    updateTextInput(session, 'transect_cruisename', value = SQL_df[input$responses_transects_rows_selected, 'cruise_name'])
    updateTextInput(session,'transect_cruiseleg', value = SQL_df[input$responses_transects_rows_selected, 'leg'])
    updateTextInput(session,'transect_divename', value = SQL_df[input$responses_transects_rows_selected, 'dive_name'])
    updateTextInput(session,'transect_name',  value = SQL_df[input$responses_transects_rows_selected, 'name'])
    updateTextInput(session,'transect_starttime', value = SQL_df[input$responses_transects_rows_selected, 'start_time'])
    updateTextInput(session,'transect_endtime', value = SQL_df[input$responses_transects_rows_selected, 'end_time'])
    updateTextInput(session,'transect_objective', value = SQL_df[input$responses_transects_rows_selected, 'objective'])
    updateTextInput(session,'transect_summary', value = SQL_df[input$responses_transects_rows_selected, 'summary'])
    updateTextInput(session, 'transect_note', value = SQL_df[input$responses_transects_rows_selected, 'note'])
  }
})

# Updates the selected row with the values that were entered in the form, based on the row last clicked.
observeEvent(input$submit_edit_transects, priority = 20, {
  # Get db data
  SQL_df <- dbReadTable(pool, 'transects')
  row_id <- SQL_df[input$responses_transects_row_last_clicked, 'row_id']
  dbExecute(pool, sprintf('UPDATE "transects" SET "cruise_name" = ?, "leg" = ?, "dive_name" = ?, "name" = ?, "start_time" = ?,
                            "end_time" = ?, "objective" = ?, "summary" = ?, "note" = ? WHERE "row_id" = ("%s")', row_id),
            param = list(input$transect_cruisename,
                         input$transect_cruiseleg,
                         input$transect_divename,
                         input$transect_name,
                         input$transect_starttime,
                         input$transect_endtime,
                         input$transect_objective,
                         input$transect_summary,
                         input$transect_note))
  # Reset
  datetimes$transect_start <- ''
  datetimes$transect_end <- ''
  removeModal()
  
})


###################################
#           Cruise Edit           #
###################################

# Edit data
# Update form values in the selected row.
observeEvent(input$edit_button_cruise, priority = 20,{
  # Fetch db data
  SQL_df <- dbReadTable(pool, 'cruise')
  # Warnings for selection
  showModal(
    if(length(input$responses_cruise_rows_selected) < 1){
      modalDialog(
        paste('Please select a row' ),easyClose = TRUE)
    })     # If one row is selected open form and update
  if(length(input$responses_cruise_rows_selected) == 1 ){
  # Form
  cruise_entryform('submit_edit_cruise')
  # Update
  updateTextInput(session, 'cruise_name', value = SQL_df[input$responses_cruise_rows_selected, 'name'])
  updateTextInput(session,'cruise_leg', value = SQL_df[input$responses_cruise_rows_selected, 'leg'])
  updateTextInput(session,'cruise_objective', value = SQL_df[input$responses_cruise_rows_selected, 'objective'])
  updateTextInput(session,'cruise_summary', value = SQL_df[input$responses_cruise_rows_selected, 'summary'])
  updateTextInput(session, 'cruise_note', value = SQL_df[input$responses_cruise_rows_selected, 'note'])
  }
})

# Updates the selected row with the values that were entered in the form, based on the row last clicked.
observeEvent(input$submit_edit_cruise, priority = 20, {
  # Get db data
  SQL_df <- dbReadTable(pool, 'cruise')
  row_id <- SQL_df[input$responses_cruise_row_last_clicked, 'row_id']
  dbExecute(pool, sprintf('UPDATE "cruise" SET "name" = ?, "leg" = ?, "objective" = ?, "summary" = ?,
                            "note" = ? WHERE "row_id" = ("%s")', row_id),
            param = list(input$cruise_name,
                         input$cruise_leg,
                         input$cruise_objective,
                         input$cruise_summary,
                         input$cruise_note))
  removeModal()
  
})


###################################
#          Equipment Edit         #
###################################

# Edit data
# Update form values in the selected row.
observeEvent(input$edit_button_equipment, priority = 20,{
  # Fetch db data
  SQL_df <- dbReadTable(pool, 'equipment')
  # Warnings for selection
  showModal(
    if(length(input$responses_equipment_rows_selected) < 1){
      modalDialog(
        paste('Please select a row' ),easyClose = TRUE)
    })     # If one row is selected open form and update
  if(length(input$responses_equipment_rows_selected) == 1 ){
    # Form
    equipment_entryform('submit_edit_equipment')
    # Update
    updateTextInput(session, 'equip_shortcode', value = SQL_df[input$responses_equipment_rows_selected, 'short_code'])
    updateTextInput(session,'equip_brand', value = SQL_df[input$responses_equipment_rows_selected, 'brand'])
    updateTextInput(session,'equip_model', value = SQL_df[input$responses_equipment_rows_selected, 'model'])
    updateTextInput(session,'equip_serialnumber', value = SQL_df[input$responses_equipment_rows_selected, 'serial_number'])
    updateTextInput(session, 'equip_type', value = SQL_df[input$responses_equipment_rows_selected, 'type'])
    updateTextInput(session, 'equip_note', value = SQL_df[input$responses_equipment_rows_selected, 'note'])
  }
})

# Updates the selected row with the values that were entered in the form, based on the row last clicked.
observeEvent(input$submit_edit_equipment, priority = 20, {
  # Get db data
  SQL_df <- dbReadTable(pool, 'equipment')
  row_id <- SQL_df[input$responses_equipment_row_last_clicked, 'row_id']
  dbExecute(pool, sprintf('UPDATE "equipment" SET "short_code" = ?, "brand" = ?, "model" = ?, "serial_number" = ?,
                            "type" = ?, "note" = ? WHERE "row_id" = ("%s")', row_id),
            param = list(input$equip_shortcode,
                         input$equip_brand,
                         input$equip_model,
                         input$equip_serialnumber,
                         input$equip_type,
                         input$equip_note))
  removeModal()
  
})



###################################
#           People Edit           #
###################################

# Edit data
# Update form values in the selected row.
observeEvent(input$edit_button_people, priority = 20,{
  # Fetch db data
  SQL_df <- dbReadTable(pool, 'people')
  # Warnings for selection
  showModal(
    if(length(input$responses_people_rows_selected) < 1){
      modalDialog(
        paste('Please select a row' ),easyClose = TRUE)
    })     # If one row is selected open form and update
  if(length(input$responses_people_rows_selected) == 1 ){
    # Form
    people_entryform('submit_edit_people')
    # Update
    updateTextInput(session, 'initials', value = SQL_df[input$responses_people_rows_selected, 'initials'])
    updateTextInput(session,'first_name', value = SQL_df[input$responses_people_rows_selected, 'first_name'])
    updateTextInput(session,'last_name', value = SQL_df[input$responses_people_rows_selected, 'last_name'])
    updateTextInput(session,'email', value = SQL_df[input$responses_people_rows_selected, 'email'])
  }
})

# Updates the selected row with the values that were entered in the form, based on the row last clicked.
observeEvent(input$submit_edit_people, priority = 20, {
  # Get db data
  SQL_df <- dbReadTable(pool, 'people')
  row_id <- SQL_df[input$responses_people_row_last_clicked, 'row_id']
  dbExecute(pool, sprintf('UPDATE "people" SET "initials" = ?, "first_name" = ?, "last_name" = ?, "email" = ?
                          WHERE "row_id" = ("%s")', row_id),
            param = list(input$initials,
                         input$first_name,
                         input$last_name,
                         input$email))
  removeModal()
  
})


###################################
#        Equipconfig Edit         #
###################################

# Edit data
# Update form values in the selected row.
observeEvent(input$edit_button_equipconfig, priority = 20,{
  # Fetch db data
  SQL_df <- dbReadTable(pool, 'equipconfig')
  # Warnings for selection
  showModal(
    if(length(input$responses_equipconfig_rows_selected) < 1){
      modalDialog(
        paste('Please select a row' ),easyClose = TRUE)
    })     # If one row is selected open form and update
  if(length(input$responses_equipconfig_rows_selected) == 1 ){
    # Form
    equipconfig_entryform('submit_edit_equipconfig')
    # Update
    updateSelectInput(session, 'equipconfig_shortcode', selected = SQL_df[input$responses_equipconfig_rows_selected, 'short_code'])
    updateSelectInput(session, 'equipconfig_type', selected = SQL_df[input$responses_equipconfig_rows_selected, 'type'])
    updateTextInput(session,'equipconfig_name', value = SQL_df[input$responses_equipconfig_rows_selected, 'name'])
    updateTextInput(session,'equipconfig_configuration', value = SQL_df[input$responses_equipconfig_rows_selected, 'configuration'])
    updateTextInput(session,'equipconfig_note', value = SQL_df[input$responses_equipconfig_rows_selected, 'note'])
  }
})

# Updates the selected row with the values that were entered in the form, based on the row last clicked.
observeEvent(input$submit_edit_equipconfig, priority = 20, {
  # Get db data
  SQL_df <- dbReadTable(pool, 'equipconfig')
  row_id <- SQL_df[input$responses_equipconfig_row_last_clicked, 'row_id']
  dbExecute(pool, sprintf('UPDATE "equipconfig" SET "name" = ?, "short_code" = ?, "type" = ?, "configuration" = ?, "note" = ?
                          WHERE "row_id" = ("%s")', row_id),
            param = list(input$equipconfig_name,
                         input$equipconfig_shortcode,
                         input$equipconfig_type,
                         input$equipconfig_configuration,
                         input$equipconfig_note))
  removeModal()
  
})


###################################
#        Diveconfig Edit         #
###################################

# Edit data
# Update form values in the selected row.
observeEvent(input$edit_button_diveconfig, priority = 20,{
  # Fetch db data
  SQL_df <- dbReadTable(pool, 'diveconfig')
  # Warnings for selection
  showModal(
    if(length(input$responses_diveconfig_rows_selected) < 1){
      modalDialog(
        paste('Please select a row' ),easyClose = TRUE)
    })     # If one row is selected open form and update
  if(length(input$responses_diveconfig_rows_selected) == 1 ){
    # Form
    diveconfig_entryform('submit_edit_diveconfig')
    # extract multiple values
    ship_intrum <- SQL_df[input$responses_diveconfig_rows_selected, 'ship_instrument_configs']
    ship_intrum_split <- strsplit(ship_intrum, ';')[[1]]
    sub_intrum <- SQL_df[input$responses_diveconfig_rows_selected, 'sub_instrument_configs']
    sub_intrum_split <- strsplit(sub_intrum, ';')[[1]]
    # Update
    updateTextInput(session,'diveconfig_name', value = SQL_df[input$responses_diveconfig_rows_selected, 'name'])
    updateSelectizeInput(session, 'ship_config', selected = SQL_df[input$responses_diveconfig_rows_selected, 'ship_config'])
    updateSelectizeInput(session, 'sub_config', selected = SQL_df[input$responses_diveconfig_rows_selected, 'sub_config'])
    updateSelectInput(session,'ship_instrument_configs', selected = ship_intrum_split)
    updateSelectInput(session,'sub_instrument_configs', selected = sub_intrum_split)
    updateTextInput(session,'diveconfig_note', value = SQL_df[input$responses_diveconfig_rows_selected, 'note'])
  }
})

# Updates the selected row with the values that were entered in the form, based on the row last clicked.
observeEvent(input$submit_edit_diveconfig, priority = 20, {
  # Get db data
  SQL_df <- dbReadTable(pool, 'diveconfig')
  row_id <- SQL_df[input$responses_diveconfig_row_last_clicked, 'row_id']
  dbExecute(pool, sprintf('UPDATE "diveconfig" SET "name" = ?, "ship_config" = ?, "sub_config" = ?, 
                          "ship_instrument_configs" = ?, "sub_instrument_configs" = ?, "note" = ?
                          WHERE "row_id" = ("%s")', row_id),
            param = list(input$diveconfig_name,
                         input$ship_config,
                         input$sub_config,
                         paste0(input$ship_instrument_configs,collapse=';'),
                         paste0(input$sub_instrument_configs,collapse=';'),
                         input$diveconfig_note))
  removeModal()
  
})


} # End server


##############################
#             APP            #
##############################

# Run the application 
shinyApp(ui = ui, server = server)
