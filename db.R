library(RSQLite)

# Name of db
dbname <- 'divelogging-db.sqlite'

# Create db
mydb <- dbConnect(RSQLite::SQLite(), dbname)

# Create cruise table
cruise <- data.frame(row_id=character(),name=character(0),leg=numeric(0),
                     objective=character(0), summary=character(0), note=character(0))

# Load people and equipment list
people <- read.csv('people.csv')
equipment <- read.csv('equipment.csv')

# Create empty table to fill
dives <- data.frame(row_id=character(),cruise_name=character(0),leg=numeric(0),name=character(0),
                    pilot=character(0),start_time=character(0),
                    end_time=character(0),site_name=character(0), 
                    dive_config=character(0), objective=character(0), 
                    summary=character(0), note=character(0))
transect <- data.frame(row_id=character(),cruise_name=character(0),leg=numeric(0),dive_name=character(0), 
                       name=character(0), start_time=character(0),
                       end_time=character(0), objective=character(0), 
                       summary=character(0), note=character(0))

# Write tables
dbWriteTable(mydb, "cruise", cruise, overwrite=T)
dbWriteTable(mydb, "people", people, overwrite=T)
dbWriteTable(mydb, "equipment", equipment, overwrite=T)
dbWriteTable(mydb, "dives", dives, overwrite=T)
dbWriteTable(mydb, "transect", transect, overwrite=T)


# Disconnect
dbDisconnect(mydb)
