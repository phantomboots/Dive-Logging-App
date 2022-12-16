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
# - write dive table functions to take dive as input then use for transect as well
# - test with 2 tables
# - make certain fields unique keys
# - somehow need to have transects as a subform of dive

# Connect to db
dbname <- 'divelogging-db.sqlite'
db <- dbConnect(RSQLite::SQLite(), dbname)
# Load all tables from db into workspace
alltabs <- lapply(setNames(nm = dbListTables(db)), dbReadTable, conn = db)
list2env(alltabs, envir=environment())
dbDisconnect(db)


# Clone the existing db
pool <- dbPool(db)


# Label mandatory fields function
labelMandatory <- function(label) {
  tagList(
    label,
    span('*', class = 'mandatory_star')
  )
}
appCSS <- '.mandatory_star { color: red; }'



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
    menuItem('Cruise', tabName = 'cruisetab', icon = icon('th')),
    menuItem('People', tabName = 'peopletab', icon = icon('th')),
    menuItem('Equipment', tabName = 'equiptab', icon = icon('th')),
    menuItem('Instruments', tabName = 'intrutab', icon = icon('th')),
    menuItem('Platforms', tabName = 'plattab', icon = icon('th')),
    menuItem('Dive Configuration', tabName = 'configtab', icon = icon('th')),
    menuItem('Dive', tabName = 'divetab', icon = icon('th')),
    menuItem('Transect', tabName = 'transtab', icon = icon('th'))
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
              actionButton('add_button', 'Add', icon('plus')),
              actionButton('edit_button', 'Edit', icon('edit')),
              actionButton('delete_button', 'Delete', icon('trash-alt'))
            ),
            br(),
            fluidRow(width='100%',
                     dataTableOutput('responses_dives', width = '100%')
            )
    )),
    tabItem(tabName = 'transtab',
            h2('Transects')
    )
  )
)
 
 
  # Dashboard ui
ui <-  dashboardPage(header, sidebar, body)



# Server
server <- function(input, output, session) {
  
  # Load dives and make reactive to inputs  
  dives_df <- reactive({
    
    # Make reactive to
    input$submit
    input$submit_edit
    input$yes_delete
    
    # Load dive table as dives_df
    dbReadTable(pool, 'dives')
    
  })  
  
  # List of mandatory fields for submission
  fieldsMandatory <- c('cruise_name', 'leg', 'name', 'pilot', 'dive_config', 'objective')
  
  # Define which input fields are mandatory 
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ''
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = 'submit', condition = mandatoryFilled)
  })
  
  # Form for data entry
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
                  textInput('cruise_name', labelMandatory('Cruise name'), cruise$name),
                  textInput('leg', labelMandatory('Leg'),  cruise$leg)
                ),
                splitLayout(
                  cellWidths = c('200px', '200px'),
                  cellArgs = list(style = 'vertical-align: top'),
                  textInput('name', labelMandatory('Dive name'),  ''),
                  textInput('site_name', 'Site name', ''),
                ),
                splitLayout(
                  cellWidths = c('200px', '200px'),
                  cellArgs = list(style = 'vertical-align: top'),
                  selectInput('dive_config', labelMandatory('Dive config'), c('d1','d2','d3')),
                  selectInput('pilot', labelMandatory('Pilot'), people$initials),
                ),
                dateInput('start_time', label = 'Start date:', format = "yyyy-mm-dd"),
                dateInput('end_time', label = 'End date',format = "yyyy-mm-dd"),
                textInput('objective', labelMandatory('Objective'), ''),
                textInput('summary', 'Summary', ''),
                textInput('note', 'Note', ''),
                helpText(labelMandatory(''), paste('Mandatory field.')),
                actionButton(button_id, 'Submit')
              ),
              easyClose = TRUE
            )
        )
      )
    )
  }

  # Save form data into data_frame format, reactive to changes in input
  diveFormData <- reactive({
    diveFormData <- data.frame(row_id = UUIDgenerate(),
                               cruise_name = input$cruise_name,
                               leg = input$leg,
                               name = input$name, 
                               pilot = input$pilot,
                               start_time = as.character(format(input$start_time, format='%Y-%m-%dT')),
                               end_time = as.character(format(input$end_time, format='%Y-%m-%dT')),
                               site_name = input$site_name,
                               dive_config = input$dive_config,
                               objective = input$objective,
                               summary = input$summary,
                               note = input$note,
                               stringsAsFactors = FALSE)
    return(diveFormData)
  })
  
  # Add data function
  appendData <- function(table, data){
    quary <- sqlAppendTable(pool, table, data, row.names = FALSE)
    dbExecute(pool, quary)
  }
  
  # Observe event for opening the form, high priority to ensure no reactive values 
  # are updated until the event is finished.
  observeEvent(input$add_button, priority = 20,{
    dives_entryform('submit')
  })
  
  # Observe event for submiting form data, append to db and reset dives_entryform
  observeEvent(input$submit, priority = 20,{
    appendData(table='dives', data=diveFormData())
    shinyjs::reset('dives_entryform')
    removeModal()
  })

  # Observe event for opening the delete modal
  observeEvent(input$delete_button, priority = 20,{
    showModal(
      # If row selected
      if(length(input$responses_dives_rows_selected)==1 ){
        modalDialog(
          title = 'Delete selected row?',
          'Warning: This action cannot be undone!',
          footer = tagList(
            actionButton('yes_delete', 'Yes, Delete'),
            modalButton('Cancel')
          ),
          easyClose = TRUE
        )
      } else {
        modalDialog(
          title = 'Warning',
          paste('Please select a single row' ),
          easyClose = TRUE
        )
      }
    )
  })

  # Observe event for deleting selected rows
  observeEvent(input$yes_delete, priority = 20,{
    deleteData(table='dives', rowid=row_clicked())
    removeModal()
  })
  
  # React to the row last clicked, save index
  row_clicked <- reactive({
    input$responses_dives_row_last_clicked
  })
  
  # Delete data function
  deleteData <- function(table, rowid){
    # Read table from db pool
    SQL_df <- dbReadTable(pool, table)
    row_selection <- SQL_df[rowid, 'row_id']
    # Delete row
    dbExecute(pool, sprintf('DELETE FROM "%s" WHERE "row_id" == ("%s")', table, row_selection))
    
  }
  
  # Edit data
  # Update form values in the selected row. Errors are displayed 
  # if there are non or more then 1 row selected.
  observeEvent(input$edit_button, priority = 20,{
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
      dives_entryform('submit_edit')
      # Update
      updateTextInput(session, 'cruise_name', value = SQL_df[input$responses_dives_rows_selected, 'cruise_name'])
      updateTextInput(session,'leg', value = SQL_df[input$responses_dives_rows_selected, 'leg'])
      updateTextInput(session,'name', value = SQL_df[input$responses_dives_rows_selected, 'name'])
      updateTextInput(session,'site_name',  value = SQL_df[input$responses_dives_rows_selected, 'site_name'])
      updateSelectInput(session,'dive_config',  selected = SQL_df[input$responses_dives_rows_selected, 'dive_config'])
      updateSelectInput(session,'pilot',  selected = SQL_df[input$responses_dives_rows_selected, 'pilot'])
      updateDateInput(session,'start_time', value = SQL_df[input$responses_dives_rows_selected, 'start_time'])
      updateDateInput(session,'end_time', value = SQL_df[input$responses_dives_rows_selected, 'end_time'])
      updateTextInput(session,'objective', value = SQL_df[input$responses_dives_rows_selected, 'objective'])
      updateTextInput(session,'summary', value = SQL_df[input$responses_dives_rows_selected, 'summary'])
      updateTextInput(session, 'note', value = SQL_df[input$responses_dives_rows_selected, 'note'])
    }
  })
  
  # Updates the selected row with the values that were entered in the form, based on the row last clicked.
  #  Note that for identifying the selected row_id the "row_last_clicked" function is used instead of "rows_selected". 
  # This is because upon showing the form module the row is deselected which results in a NULL when the rows_selected
  # function is used.
  observeEvent(input$submit_edit, priority = 20, {
    # Get db data
    SQL_df <- dbReadTable(pool, 'dives')
    row_selection <- SQL_df[input$responses_dives_row_last_clicked, 'row_id'] 
    dbExecute(pool, sprintf('UPDATE "dives" SET "cruise_name" = ?, "leg" = ?, "name" = ?, "pilot" = ?, "start_time" = ?, 
                            "end_time" = ?, "site_name" = ?, "dive_config" = ? , "objective" = ?, "summary" = ?,
                            "note" = ? WHERE "row_id" = ("%s")', row_selection), 
              param = list(input$cruise_name,
                           input$leg,
                           input$name, 
                           input$pilot,
                           input$start_time,
                           input$end_time,
                           input$site_name,
                           input$dive_config,
                           input$objective,
                           input$summary,
                           input$note))
    removeModal()
    
  })
  
  
  # Display the dives table
  output$responses_dives <- DT::renderDataTable({
    table <- dives_df() %>% select(-row_id) 
    table <- datatable(table, 
                       rownames = FALSE,
                       options = list(searching = FALSE, lengthChange = FALSE)
    )
  })
  
  # Export button
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)
