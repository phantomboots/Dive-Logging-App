library(shiny)
library(DT)
library(RSQLite)
library(pool)
library(shinyjs)
library(uuid)
library(dplyr)
library(shinydashboard)

# Based on this example
# https://www.nielsvandervelden.com/blog/editable-datatables-in-r-shiny-using-sql/
  

# still to do
# - test with 2 tables
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

# # Add data function
# appendData <- function(table, data){
#   quary <- sqlAppendTable(pool, table, data, row.names = FALSE)
#   dbExecute(pool, quary)
# }

# Label mandatory fields function
labelMandatory <- function(label) {
  tagList(
    label,
    span('*', class = 'mandatory_star')
  )
}
appCSS <- '.mandatory_star { color: red; }'




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
                               end_time = as.character(format(input$dive_endtime, format='%Y-%m-%dT')),
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
  
  # Set mandatory fields for dives form
  obsFieldsMandatory(table='dives', fields=c('dive_name', 'dive_pilot', 'dive_diveconfig'))

  # Form for dive data entry
  dives_entryform <- function(button_id){
    showModal(
      modalDialog(
        div(id=('dives_entryform'),
            tags$head(tags$style('.modal-dialog{ width:500px}')),
            tags$head(tags$style(HTML('.shiny-split-layout > div {overflow: visible}'))),
            fluidPage(
              fluidRow(
                splitLayout(
                  cellWidths = c('250px', '150px'),
                  cellArgs = list(style = 'vertical-align: top'),
                  # Use the last record for cruise name and leg as defaults
                  textInput('dive_cruisename', labelMandatory('Cruise name'), cruise$name[nrow(cruise)]),
                  textInput('dive_cruiseleg', labelMandatory('Leg'),  cruise$leg[nrow(cruise)])
                ),
                splitLayout(
                  cellWidths = c('200px', '200px'),
                  cellArgs = list(style = 'vertical-align: top'),
                  textInput('dive_name', labelMandatory('Dive name'),  ''),
                  textInput('dive_sitename', 'Site name', ''),
                ),
                splitLayout(
                  cellWidths = c('200px', '200px'),
                  cellArgs = list(style = 'vertical-align: top'),
                  selectInput('dive_diveconfig', labelMandatory('Dive config'), c('d1','d2','d3')),
                  selectInput('dive_pilot', labelMandatory('Pilot'), people$initials),
                ),
                dateInput('dive_starttime', label = 'Start date:', format = "yyyy-mm-dd"),
                dateInput('dive_endtime', label = 'End date', format = "yyyy-mm-dd"),
                textInput('dive_objective', labelMandatory('Objective'), ''),
                textInput('dive_summary', 'Summary', ''),
                textInput('dive_note', 'Note', ''),
                helpText(labelMandatory(''), paste('Mandatory field.')),
                actionButton(button_id, 'Submit')
              ),
              easyClose = TRUE
            )
        )
      )
    )
  }

  # Set mandatory fields for dives form
  obsFieldsMandatory(table='cruise', fields=c('cruise_name', 'cruise_leg'))
  
  # Form for cruise data entry
  cruise_entryform <- function(button_id){
    showModal(
      modalDialog(
        div(id=('cruise_entryform'),
            tags$head(tags$style('.modal-dialog{ width:500px}')),
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
              easyClose = TRUE
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
  obsEvents <- function(table, data){
    
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

  
  
  ##############################
  #  Dives Edit form and save  #
  ##############################


  # Edit data
  # Update form values in the selected row. Errors are displayed if there are non or more then 1 row selected.
  observeEvent(input$edit_button_dives, priority = 20,{
    # Fetch db data
    SQL_df <- dbReadTable(pool, 'dives')
    # Warnings for selection
    showModal(
      if(length(input$responses_dives_rows_selected) > 1 ){
        modalDialog(
          title = 'Warning',
          paste('Please select only one row.' ),easyClose = TRUE)
      } else if(length(input$responses_dives_rows_selected) < 1){
        modalDialog(
          title = 'Warning',
          paste('Please select a row.' ),easyClose = TRUE)
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
      updateDateInput(session,'dive_starttime', value = SQL_df[input$responses_dives_rows_selected, 'start_time'])
      updateDateInput(session,'dive_endtime', value = SQL_df[input$responses_dives_rows_selected, 'end_time'])
      updateTextInput(session,'dive_objective', value = SQL_df[input$responses_dives_rows_selected, 'objective'])
      updateTextInput(session,'dive_summary', value = SQL_df[input$responses_dives_rows_selected, 'summary'])
      updateTextInput(session, 'dive_note', value = SQL_df[input$responses_dives_rows_selected, 'note'])
    }
  })



  # Updates the selected row with the values that were entered in the form, based on the row last clicked.
  #  Note that for identifying the selected row_id the "row_last_clicked" function is used instead of "rows_selected".
  # This is because upon showing the form module the row is deselected which results in a NULL when the rows_selected
  # function is used.
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
                           as.character(format(input$dive_starttime, format='%Y-%m-%dT')),
                           as.character(format(input$dive_endtime, format='%Y-%m-%dT')),
                           input$dive_sitename,
                           input$dive_diveconfig,
                           input$dive_objective,
                           input$dive_summary,
                           input$dive_note))
    removeModal()

  })

  
# Still need to add observer events for cruise

}


##############################
#             APP            #
##############################

# Run the application 
shinyApp(ui = ui, server = server)
