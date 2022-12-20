library(shiny)
library(DT)
library(RSQLite)
library(pool)
library(shinyjs)
library(uuid)
library(dplyr)
library(shinydashboard)
#library(shinyTime)

# Based on this example
# https://www.nielsvandervelden.com/blog/editable-datatables-in-r-shiny-using-sql/


# still to do
# - make certain fields unique keys
# - somehow need to have transects as a subform of dive


##############################
#           Set-up           #
##############################


# Connect to db
dbname <- 'divelogging-db.sqlite'
db <- dbConnect(RSQLite::SQLite(), dbname)
# Load all tables from db into workspace
alltabs <- lapply(setNames(nm = dbListTables(db)), dbReadTable, conn = db)
list2env(alltabs, envir=environment())
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

# Label unique values required
labelUnique <- function(label) {
  tagList(
    label,
    span('**', class = 'unique_star')
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
    menuItem('People', tabName = 'peopletab', icon = icon('user-group')),
    menuItem('Equipment list', tabName = 'equiptab', icon = icon('list-ul')),
    menuItem('Equipment configurations', tabName = 'econfigtab', icon = icon('screwdriver-wrench')),
    menuItem('Dive configurations', tabName = 'dconfigtab', icon = icon('sliders')),
    menuItem('Dives', tabName = 'divetab', icon = icon('water'))
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
    tabItem(tabName = 'cruisetab',
            h2('Cruise name and legs'),
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
            h2('Personnel')
    ),
    tabItem(tabName = 'equiptab',
            h2('Equipment list')
    ),
    tabItem(tabName = 'econfigtab',
            h2('Set instrument and platform configurations')
    ),
    tabItem(tabName = 'dconfigtab',
            h2('Set dive configuration')
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
      name <- paste0(alltabs[['cruise']]$name, '_', i, '.csv')
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
      # Make reactive to
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
  cruise_df <- makeReactive('cruise')
  
  
  # Display dives table
  output$responses_dives <- DT::renderDataTable({
    table <- dives_df() %>% select(-row_id)
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
  
  
  
  ##############################
  #        Form data           #
  ##############################
  
  # Save form data into data_frame format, reactive to changes in input
  dives_FormData <- reactive({
    divesFormData <- data.frame(row_id = UUIDgenerate(),
                                cruise_name = input$dive_cruisename,
                                leg = input$dive_cruiseleg,
                                name = input$dive_name, 
                                pilot = input$dive_pilot,
                                start_time = as.character(format(input$dive_starttime, format='%Y-%m-%dT')),
                                end_time = input$dive_endtime,
                                site_name = input$dive_sitename,
                                dive_config = input$dive_diveconfig,
                                objective = input$dive_objective,
                                summary = input$dive_summary,
                                note = input$dive_note,
                                stringsAsFactors = FALSE)
    return(divesFormData)
  })
  
  
  # Save form data into data_frame format, reactive to changes in input
  cruise_FormData <- reactive({
    cruiseFormData <- data.frame(row_id = UUIDgenerate(),
                                 name = input$cruise_name,
                                 leg = input$cruise_leg,
                                 objective = input$cruise_objective,
                                 summary = input$cruise_summary,
                                 note = input$cruise_note,
                                 stringsAsFactors = FALSE)
    return(cruiseFormData)
  })
  
  
  
  ##############################
  #        Input forms         #
  ##############################
  
  # Mandatory fields for submission function
  obsFieldsMandatory <- function(table, fields){
    # Define which input fields are mandatory 
    observe({
      mandatoryFilled <- vapply(fields ,function(x) {!is.null(input[[x]]) && input[[x]] != ''}, logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      shinyjs::toggleState(id = paste0('submit_',table), condition = mandatoryFilled)
    })
  }
  
  
  # Set mandatory fields
  obsFieldsMandatory(table='dives', fields=c('cruise_name', 'cruise_leg','dive_name', 
                                             'dive_pilot', 'dive_diveconfig','dive_objective'))
  obsFieldsMandatory(table='cruise', fields=c('cruise_name', 'cruise_leg'))
  
  
  # Form for dive data entry
  dives_entryform <- function(button_id){
    showModal(
      modalDialog(
        div(id=('dives_entryform'),
            tags$head(tags$style('.modal-dialog{ width:450px}')),
            tags$head(tags$style(HTML('.shiny-split-layout > div {overflow: visible}'))),
            fluidPage(
              fluidRow(
                splitLayout(
                  cellWidths = c('250px', '150px'),
                  cellArgs = list(style = 'vertical-align: top'),
                  # Use the last record for cruise name and leg as defaults
                  textInput('dive_cruisename', labelMandatory('Cruise name'), cruise_df()$name[nrow(cruise_df())]),
                  textInput('dive_cruiseleg', labelMandatory('Leg'),  cruise_df()$leg[nrow(cruise_df())])
                ),
                splitLayout(
                  cellWidths = c('200px', '200px'),
                  cellArgs = list(style = 'vertical-align: top'),
                  textInput('dive_name', labelUnique('Dive name'),  ''),
                  textInput('dive_sitename', 'Site name', ''),
                ),
                splitLayout(
                  cellWidths = c('200px', '200px'),
                  cellArgs = list(style = 'vertical-align: top'),
                  selectInput('dive_diveconfig', labelMandatory('Dive config'), c('d1','d2','d3')),
                  selectInput('dive_pilot', labelMandatory('Pilot'), people$initials),
                ),
                splitLayout(
                  cellWidths = c('300px', '100px'),
                  cellArgs = list(style = 'vertical-align: top'),
                  uiOutput('out_dive_starttime'),
                  actionButton('set_dive_starttime', 'Start', icon = icon("clock"), class = "btn-primary"),
                ),
                splitLayout(
                  cellWidths = c('300px', '100px'),
                  cellArgs = list(style = 'vertical-align: top'),
                  uiOutput('out_dive_endtime'),
                  actionButton('set_dive_endtime', 'End', icon = icon("clock"), class = "btn-primary"),
                ),
                tags$style(type='text/css', '#set_dive_starttime { width:100%; margin-top: 25px}'),
                tags$style(type='text/css', '#set_dive_endtime { width:100%; margin-top: 25px}'),
                textInput('dive_objective', labelMandatory('Objective'), ''),
                textInput('dive_summary', 'Summary', ''),
                textInput('dive_note', 'Note', ''),
                helpText(labelMandatory(''), paste('Mandatory field')),
                helpText(labelUnique(''), paste('Unique key')),
                actionButton('check_dives', 'Check', icon = icon("refresh")),
                actionButton(button_id, 'Submit', class = "btn-warning")
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
                helpText(labelMandatory(''), paste('Mandatory field.')),
                actionButton(button_id, 'Submit')
              ),
              easyClose = FALSE
            )
        )
      )
    )
  }
  
  
  ##############################
  #       Observe Events       #
  ##############################
  
  
  # Function for common observe events
  # Row last clicked is the row selected because only one row can be selected at a time
  obsEvents <- function(table){
    
    # Observe event for opening the form, high priority to ensure no reactive values
    # are updated until the event is finished.
    observeEvent(input[[paste0('add_button_', table)]], priority = 20,{
      get(paste0(table,'_entryform'))(paste0('submit_', table))
    })
    
    # Observe event for opening the delete modal
    observeEvent(input[[paste0('delete_button_', table)]], priority = 20,{
      showModal(modalDialog(
        title = 'Delete selected row?',
        'Warning: This action cannot be undone!',
        footer = tagList(
          actionButton(paste0('yes_delete_',table), 'Yes, Delete'),
          modalButton('Cancel')
        ), 
        easyClose = TRUE
      ))
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
      formdata <- get(paste0(table,'_FormData'))
      quary <- sqlAppendTable(pool, table, formdata(), row.names = FALSE)
      dbExecute(pool, quary)
      shinyjs::reset(paste0(table,'_entryform'))
      removeModal()
    })
  }
  
  
  # Dive observer events
  obsEvents(table='dives')
  obsEvents(table='cruise')
  
  
  ##################################
  #   Start and End time buttons   #
  ##################################

  # Starting empty field
  emptytime <- ''
  
  # Make reactive
  datetimes <- reactiveValues(dive_start = emptytime,
                              dive_end = emptytime)
  
  # Event observers for datetimes
  observeEvent(input$set_dive_starttime,{
    datetimes$dive_start <- as.character(format(Sys.time(), format='%Y-%m-%dT-%TZ', tz='GMT'))
  })
  observeEvent(input$set_dive_endtime,{
    datetimes$dive_end <- as.character(format(Sys.time(), format='%Y-%m-%dT-%TZ', tz='GMT'))
    })
  
  # Generate the textInput for datetimes
  output$out_dive_starttime <- renderUI({
    textInput('dive_starttime', 'Start time:', datetimes$dive_start)
  })
  output$out_dive_endtime <- renderUI({
    textInput('dive_endtime', 'End time:', datetimes$dive_end)
  })


  
##################################
#    Dives Checks and  Edit      #
##################################

  # Dive name must be unique, toggle submit until unique
  # !!! Other options could be change colour or text on check button to indicate a pass,
  # while also toggling submit
  observeEvent(input$check_dives,{
    cond <-!(input$dive_name %in% dives_df()$name)
    shinyjs::toggleState(id = 'submit_dives', condition = cond)
  })
  
  

  # Use this code to create sub-form for transect that opens from dive form
  
  # # Dive names must be unique, check and open new model with result
  # observeEvent(input$check_dives,{ 
  #   # Save form data temporarily
  #   tmp <<- dives_FormData()
  #   showModal(
  #     if( input$dive_name %in% dives_df()$name ){
  #       modalDialog(
  #         title = "Error!",
  #         "Dive name must be a unique value. Try again.",
  #         easyClose=FALSE,
  #         footer = actionButton("restoreModal", label = "Okay")
  #       )
  #     } else {
  #       modalDialog(
  #         title = "Success!",
  #         "All checks have passed",
  #         easyClose=FALSE,
  #         footer = actionButton("restoreModal", label = "Okay")
  #       )
  #     }
  #   )
  # })
  # # Restore dive entry form modal, after check is performed
  # observeEvent(input$restoreModal, {
  #   # Open form
  #   dives_entryform('submit_dives')
  #   # Update
  #   updateTextInput(session, 'dive_cruisename', value = tmp[1, 'cruise_name'])
  #   updateTextInput(session,'dive_cruiseleg', value = tmp[1, 'leg'])
  #   updateTextInput(session,'dive_name', value = tmp[1, 'name'])
  #   updateTextInput(session,'dive_sitename',  value = tmp[1, 'site_name'])
  #   updateSelectInput(session,'dive_diveconfig',  selected = tmp[1, 'dive_config'])
  #   updateSelectInput(session,'dive_pilot',  selected = tmp[1, 'pilot'])
  #   updateDateInput(session,'dive_starttime', value = tmp[1, 'start_time'])
  #   updateDateInput(session,'dive_endtime', value = tmp[1, 'end_time'])
  #   updateTextInput(session,'dive_objective', value = tmp[1, 'objective'])
  #   updateTextInput(session,'dive_summary', value = tmp[1, 'summary'])
  #   updateTextInput(session, 'dive_note', value = tmp[1, 'note'])
  #   
  # })
  

# Edit data
# Update form values in the selected row.
observeEvent(input$edit_button_dives, priority = 20,{
  # Fetch db data
  SQL_df <- dbReadTable(pool, 'dives')
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
  # Form
  cruise_entryform('submit_edit_cruise')
  # Update
  updateTextInput(session, 'cruise_name', value = SQL_df[input$responses_cruise_rows_selected, 'name'])
  updateTextInput(session,'cruise_leg', value = SQL_df[input$responses_cruise_rows_selected, 'leg'])
  updateTextInput(session,'cruise_objective', value = SQL_df[input$responses_cruise_rows_selected, 'objective'])
  updateTextInput(session,'cruise_summary', value = SQL_df[input$responses_cruise_rows_selected, 'summary'])
  updateTextInput(session, 'cruise_note', value = SQL_df[input$responses_cruise_rows_selected, 'note'])
  
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


}


##############################
#             APP            #
##############################

# Run the application 
shinyApp(ui = ui, server = server)
