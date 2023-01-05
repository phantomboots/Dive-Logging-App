# NDST Dive Logging App

__Main author:__  Jessica Nephin     
__Contact:__      jessica.nephin@dfo-mpo.gc.ca

- [Objective](#objective)
- [Summary](#summary)
- [Status](#status)
- [Contents](#contents)
- [Methods](#methods)
- [Requirements](#requirements)

## Objective
To provide a user interface for logging survey events (dives and transects) during field operations by the Non-Destructive Survey Tools program

## Summary
The user interface is written with R shiny. The app allows users to add, edit and delete dives and transects and associated metadata (e.g., personnel, configurations). All data logged with the app are saved in an sqlite database. Data can also be exported to csv using the export button on the dashboard header.

## Status
Under development

## Contents

### db.R
Script to initilize an empty sqlite database. This should be only be run at the beginning of each survey. It takes inputs equipment.csv and people.csv which contain lists of current NDST equipment and personnel. 

### app.R
The R shiny app. The app allows users to add, edit and delete records in cruise, personnel, equipment, equipment configuration, dive configuration, dives, and transect tables. The app can be started and stopped multiple times without resulting in data loss. The data from each session are continually saved to the sqlite db. No data loss will result from a crash unless any form data was left un-submitted. Inputs are the sqlite db created in db.R and config-variables.csv file. 


## Requirements
R version 4.2.1    
shiny 1.7.4    
sqlite3    

