# Build base database

# Packages
library(RSQLite)
library(uuid)


# Select name of db
# Edit 'cruisename'
dbname <- 'cruisename-divelogging-db.sqlite'



# Create new db or return error
if(!file.exists(dbname)){
  
  # Create db
  mydb <- dbConnect(RSQLite::SQLite(), dbname)
  
  # Create cruise table
  cruise <- data.frame(row_id=character(),name=character(0),leg=character(0),
                       objective=character(0), summary=character(0), note=character(0))
  
  # Load people and equipment list
  people <- read.csv('people.csv')
  equipment <- read.csv('equipment.csv')
  
  # Add row_id fields to people and equip lists
  people <- data.frame(row_id=UUIDgenerate(n=nrow(people)), people)
  equipment <- data.frame(row_id=UUIDgenerate(n=nrow(equipment)), equipment)
  
  # Create empty tables to fill
  dives <- data.frame(row_id=character(),cruise_name=character(0),leg=character(0),
                      name=character(0),pilot=character(0),start_time=character(0),
                      end_time=character(0),site_name=character(0),dive_config=character(0),
                      objective=character(0),summary=character(0),note=character(0))
  transects <- data.frame(row_id=character(),cruise_name=character(0),leg=character(0),
                          dive_name=character(0),name=character(0),start_time=character(0),
                          end_time=character(0),objective=character(0),summary=character(0),note=character(0))
  equipconfig <- data.frame(row_id=character(),name=character(0),short_code=character(0),
                            type=character(0),configuration=character(0),note=character(0))
  diveconfig <- data.frame(row_id=character(),name=character(0),ship_config=character(0),
                           sub_config=character(0), ship_instrument_configs=character(0), 
                           sub_instrument_configs=character(0),note=character(0))
  
  # Write tables
  dbWriteTable(mydb, 'cruise', cruise, overwrite=F)
  dbWriteTable(mydb, 'people', people, overwrite=F)
  dbWriteTable(mydb, 'equipment', equipment, overwrite=F)
  dbWriteTable(mydb, 'dives', dives, overwrite=F)
  dbWriteTable(mydb, 'transects', transects, overwrite=F)
  dbWriteTable(mydb, 'equipconfig', equipconfig, overwrite=F)
  dbWriteTable(mydb, 'diveconfig', diveconfig, overwrite=F)
  
  
  # Disconnect
  dbDisconnect(mydb)
  
} else {
  stop('"', dbname, '" already exists! ', 'Rename and try again.')
}
