# Author Luke Wilde
# Date April 28 2022

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#load packages, install if needed
library(tidyverse)
library(lubridate)
library(sf)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(leaflet)

#------------------------------------------------------------------------#
#### Skip this -- Run only to prep dataset -- files not shared to Git ####
#------------------------------------------------------------------------#

# 
# source("H:/My Drive/Wind_River_visproj/WindRiver_vis_proj/MiscFunctions/standard_functions.R")
# 
# ##
# #Use: in the case that 'htmltools' or 'rgeoboundaries' does not install
# #remotes::install_github("wmgeolab/rgeoboundaries")
# #install.packages("htmltools") #may need to update
# 
# setUp(c("sf",
#         "dplyr",
#         "leaftime",
#         "htmltools",
#         "shiny",
#         "leaflet",
#         "RColorBrewer",
#         "shinyWidgets",
#         "MODIStsp", 
#         "tidyverse", 
#         "lme4", 
#         "rgeoboundaries", 
#         "ggplot2", 
#         "maps", 
#         "remotes", 
#         "devtools", 
#         "tigris", 
#         "raster", 
#         "here", 
#         "viridis", 
#         "viridisLite", 
#         "RColorBrewer"))
# 
#library(devtools)
#library(remotes)
#install_github("jmerkle1/MerkleLab-Code-Repository", subdir="MoveTools")
#library(MoveTools)

# #pre processing
# 
# 
# ## Settings for fetching locations on Telonics Collars ONLY ##
# # 
# # fldr_out = "C:\\Users\\14064\\Documents\\GitHub\\Wilde_DataViz_BuildingShinyForMapDataSharing\\Wilde_ShinyAppForDataSharing\\TelonicsTemp"## Where you want the temp files stored, this will be emptied after the data are brought into the R environment
# # days = 3 #the number of days worth of daily locations you want (will only affec thow long the "tail" following points is; think tadpoles)
# # username = "AnnaOrtega" #log in info for RDH
# # password = "n6rW2QSw" #log in info for RDH
# # TDC_path= "C:\\Program Files (x86)\\Telonics\\Data Converter\\TDC.exe" #the default area where apps are saved, go to https://www.telonics.com/software/tdc.php to download the TDC and work from there!
# # 
# # 
# # ## Load where the deer ID metadata are stored
# # #Set working directory and import csv file
# # DeerID.Data <- read.table("./data/Wilde_metadata.csv", header=TRUE, sep=",")
# # 
# # 
# # #now load the gps files from Telonics
# # 
# # if(file.exists(fldr_out)){print(length(list.files(fldr_out))==0)} else{dir.create(file.path(fldr_out))} # should be [1] TRUE or no output
# # 
# # fixes <- fetch_telonics_gstar_positions( username = username,password = password,
# #                                          fldr_out = fldr_out,
# #                                          TDC_path = TDC_path,
# #                                          keep.reports = FALSE,   # if FALSE, will remove all temp files at the end
# #                                          start_date = NULL)  # if you put NULL here, you'll get ALL data, if you change the first number to 1, you get only todays data Sys.time() - days * 24 * 3600
# 
# #fixes will save to a directory, and you can store that location below as the dir.tdc object. Or you can integrate in the case that Jerod's code loads directly into the R environment
# 
# 
# #Import Telonics Globalstar data directly from TDC and clean the data #
# #just click through this section, no changes
# gps <- fixes
# 
# #Extract GPS fixes that have been successful
# gps<-gps[gps$GPS.Fix.Attempt!="Failed", ]
# gps<-gps[gps$GPS.Fix.Attempt!="Unresolved QFP", ]
# gps<-gps[gps$GPS.Fix.Attempt!="", ]
# table(gps$GPS.Fix.Attempt)
# 
# #Ensure that there are the correct number of unique Telonics collars (n = 65)
# unique.cllr<-unique(gps$CllrSrN)
# 
# #Create new GPS dataframe
# t<-gps
# 
# #Split date and time column into two separate columns
# t$tempdate<- sapply(t$Acquisition.Time, FUN = function(x){unlist(strsplit(x, " "))[1]})
# t$temptime<- sapply(t$Acquisition.Time, FUN = function(x){unlist(strsplit(x, " "))[2]})
# 
# #Split date column into year, month, and day columns
# t$Year<-substr(t$tempdate, 1, 4) 
# t$Month<-substr(t$tempdate, 6, 7) 
# t$Day<-substr(t$tempdate, 9, 10) 
# 
# #Combine month, day, and year column into a single date column with correct format
# t$date<- paste(t$Year, t$Month, t$Day,  sep="-")
# 
# #Clean up all date and time columns
# t$Hour <- format(strptime(t$temptime,"%H:%M:%S"),'%H')
# t$Minute <- sprintf(paste("%02d"), 00)
# t$Sec <- sprintf(paste("%02d"), 00)
# t$Time <- paste(t$Hour, t$Minute, t$Sec, sep=":")
# t$Date <- paste(t$Year, t$Month, t$Day, sep="-")
# t$dt <- paste(as.character(t$Date), t$Time, sep=" ")
# t$POSIXct <- as.POSIXct(strptime(t$dt, format="%Y-%m-%d %H:%M:%S"), tz="UTC")
# attr(t$POSIXct, "tzone") <- "GMT"
# attr(t$POSIXct, "tzone") <- "MST"
# 
# #Reconvert date and time data to match MST time zone
# t$Month <- month(t$POSIXct)
# t$Month<- sprintf(paste("%02d"), t$Month)
# t$Day <- day(t$POSIXct)
# t$Year <- year(t$POSIXct)
# t$Hour <- hour(t$POSIXct)
# t$Minute <- sprintf(paste("%02d"), 00)
# t$Sec <- sprintf(paste("%02d"), 00)
# t$Time <- paste(t$Hour, t$Minute, t$Sec, sep=":")
# t$Date <- paste(t$Year, t$Month, t$Day, sep="-")
# t$dt <- paste(as.character(t$Date), t$Time, sep=" ")
# t$POSIXct <- as.POSIXct(strptime(t$dt, format="%Y-%m-%d %H:%M:%S"), tz="MST")
# t$DayOfYear <- yday(t$POSIXct)
# head(t)
# names(t)
# 
# t <- st_transform(t, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# 
# tt <- as.data.frame(t %>% st_drop_geometry())
# names(tt)
# 
# #Remove unneccessary columns, rename column headers, organize columns
# tt <- tt[,c(6:7,8,16,19:21,26:27,29:30)]
# colnames(tt)<-c("Lat","Long","HDOP","CllrSrN","Year","Month","Day","Time","Date","POSIXct","DayOfYear")
# head(tt)
# tt$Altitude<-"N/A"
# tt$NumSats<-"N/A"
# tt<-tt[order(tt$CllrSrN,tt$POSIXct),]
# final.t <- tt[c("CllrSrN","POSIXct","Month","Day", "Year", "DayOfYear","Date","Time","Lat","Long","Altitude","HDOP","NumSats")]
# head(final.t)
# 
# #Note, I did not remove any HDOP errors, because unlike LOTEK...I can't quite figure out what values of HDOP are truly errors
# 
# #Omit POSIXct values with null values
# final.t<-final.t[!is.na(final.t[,2]),] 
# 
# #Remove any rows containing zero values for Latitude and Longitude
# final.t<-final.t[final.t$Long!=0 & final.t$Lat!=0, ]
# head(final.t)
# 
# rm(gps,t)
# 
# 
# 
# #Import master ID list ##
# #Merge DeerIDs based on unique CllrSrN and range of dates
# #Format date and time columns
# DeerID.Data$StartDate <- as.POSIXct(DeerID.Data$StartDate, format="%m/%d/%Y", origin = "1970-01-01")
# DeerID.Data$StopDate <- as.POSIXct(DeerID.Data$StopDate,format="%m/%d/%Y", origin = "1970-01-01")
# 
# DeerID.Data$Startdt <- paste(as.character(DeerID.Data$StartDate), DeerID.Data$StartTime, sep=" ")
# DeerID.Data$StartDateTime <- as.POSIXct(strptime(DeerID.Data$Startdt, format="%Y-%m-%d %H:%M:%S"), origin = "1970-01-01", tz="MST")
# 
# DeerID.Data$Stopdt <- paste(as.character(DeerID.Data$StopDate), DeerID.Data$StopTime, sep=" ")
# DeerID.Data$StopDateTime <- as.POSIXct(strptime(DeerID.Data$Stopdt, format="%Y-%m-%d %H:%M:%S"), origin = "1970-01-01", tz="MST")
# names(DeerID.Data)
# 
# #Remove unncessary columns
# DeerID.Data<- DeerID.Data[c(1:7,10)]
# head(DeerID.Data)
# 
# #Sort unique collar serial numbers; make sure the serial numbers look correct
# sort(unique(final.t$CllrSrN))
# sort(unique(DeerID.Data$CllrSrN))
# 
# #Merge all GPS collar data with Deer ID csv by CllrSrN                
# mydata <- merge(final.t, DeerID.Data, by=c("CllrSrN"), all=TRUE) 
# 
# #Organize dataframe by CllrSrN, POSIXct, and AID
# mydata<-mydata[order(mydata$CllrSrN, mydata$POSIXct, mydata$AID),]
# 
# #Omit any deer IDs that are N/A
# mydata<-mydata[!is.na(mydata[,11]),] 
# 
# #Convert date and time columns into character data type
# mydata$StartDateTime<-as.character(mydata$StartDateTime)
# mydata$StopDateTime<-as.character(mydata$StopDateTime)
# mydata$POSIXct<-as.character(mydata$POSIXct)
# 
# #Subset data based on StopDateTime (NULL values present vs. NULL values not present)
# my.data.Null<-subset(mydata, is.na(mydata$StopDateTime))
# my.data.NoNull<-subset(mydata, !is.na(mydata$StopDateTime))
# 
# #Identify unique IDs and create empty output dataframe for data in which StopDateTime contains NULL values
# Deer.ID.1 <- unique(my.data.Null$AID)
# Deer.CllrSrN.1 <- unique(my.data.Null$CllrSrN) 
# df.1<- data.frame()
# df.1<-as.data.frame(my.data.Null)
# full.df.1<-df.1[0, ]
# 
# starttime <- as.POSIXct(Sys.time())
# 
# #Create for loop that creates unique Deer IDs with associated CllrSrN based on range of dates
# for (i in 1:length (Deer.ID.1)){
#     temp.2<-subset(my.data.Null, AID==Deer.ID.1[i])
#     for (j in 1:length (Deer.CllrSrN.1)){
#         temp.3<-subset(temp.2, CllrSrN==Deer.CllrSrN.1[j])
#         sub.1<-subset(temp.3, temp.3$POSIXct >= temp.3$StartDateTime)
#         full.df.1<-rbind(full.df.1,sub.1)
#     }
# }
# 
# final.gps <- full.df.1
# 
# final.gps <- final.gps %>% dplyr::select(AID, Mgtry, TagColor, TagNum, CllrSrN, Frequency, Lat, Long, POSIXct, Date, Time)
# 
# #### Bring in migration data ##
# mig.timing <- read.table("./data/Wilde_migdata.csv", header=TRUE, sep=",")
# 
# #create a year column in the gps data
# final.gps <- final.gps %>% mutate(Year = year(POSIXct))
# 
# final.gps <- final.gps %>% filter(Year < 2021)
# 
# final.gps <- final.gps %>% filter(AID %in% sample(unique(final.gps$AID),14))
# 
# 
# 
# 
# # keep only columns with start and end of migration in Spring and Fall
# mig.timing <- mig.timing %>% dplyr::select(AID, Year, SpringMS, SpringME, FallMS, FallME)
# 
# #clean up, replace character of missing data with blanks
# mig.timing <- mig.timing %>% replace(. == "N/A", NA)
# 
# #change format of datetime
# mig.timing$SpringMS <- as.POSIXct(mig.timing$SpringMS, format="%m/%d/%Y %H:%M")
# mig.timing$SpringME <- as.POSIXct(mig.timing$SpringME, format="%m/%d/%Y %H:%M")
# mig.timing$FallMS <- as.POSIXct(mig.timing$FallMS, format="%m/%d/%Y %H:%M")
# mig.timing$FallME <- as.POSIXct(mig.timing$FallME, format="%m/%d/%Y %H:%M")
# 
# #join migratinon data to gps
# final.gps <- final.gps %>% left_join(mig.timing, by=c("AID", "Year"))
# 
# final.gps$POSIXct <- as.POSIXct(final.gps$POSIXct, format = "%Y-%m-%d %H:%M:%S")
# spring.mig.gps <- final.gps %>% filter(POSIXct > SpringMS & POSIXct < SpringME)
# fall.mig.gps <- final.gps %>% filter(POSIXct > FallMS & POSIXct < FallME)
# summer.gps <- final.gps %>% filter(POSIXct > SpringME & POSIXct < FallMS)
# winter.gps <- final.gps %>% filter(POSIXct > FallME)
# 
# spring.mig.gps$season <- "Spring Migration"
# fall.mig.gps$season <- "Fall Migration"
# summer.gps$season <- "Summer Range"
# winter.gps$season <- "Winter Range"
# 
# final.app.gps <- rbind(spring.mig.gps, fall.mig.gps, winter.gps, summer.gps)
# 
# # random scatter to hide real data
# x <- round(runif(nrow(final.app.gps), 100, 10000),0)
# y <- round(runif(nrow(final.app.gps), 100, 10000),0)
# 
# final.app.gps$Lat <- final.app.gps$Lat+x
# final.app.gps$Long <- final.app.gps$Long+y
# # #make spatial again
# final.app.sf <- st_as_sf(final.app.gps,
#                      coords = c("Long", "Lat"), # note x goes first
#                      crs = "+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +no_defs", # projection, this is NAD83
#                      remove = FALSE) # don't remove lat/lon cols from dataframe
# 
# head(final.app.gps)
# final.app.sf
# 
# write.csv(final.app.gps, "./Shiny/muledeer_test_data.csv", row.names = F)
# 
# st_write(final.app.sf, "./Shiny/muledeer_test_data.shp", append=F)
# 





#-------------------------------------------------------#
#### Start Here ####
#-------------------------------------------------------#

#data is only from a sample of deer, daily locations from satellite service (Proprietary reasons)

getwd() #make sure this is the app.R file location

#load the data table and shp file
gps.data <- read.csv("./muledeer_test_data.csv")
gps.sf <- st_read("./muledeer_test_data.shp")

gps.sf$POSIXct <- as.POSIXct(paste(gps.sf$POSIXct, gps.sf$Time, " "), format= "%Y-%m-%d %H:%M:%S", tz="MST")

#check if over
head(gps.data)

gps.sf <- st_transform(gps.sf, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Define UI for application 
ui <- dashboardPage(
  dashboardHeader(title="Red Desert Mule Deer Project -- Data sharing tool"),
  dashboardSidebar(
    sidebarMenu(
      pickerInput("Year", "Choose year:",choices=unique(gps.sf$Year), multiple=F, options=list('actions-box' = TRUE), selected=unique(gps.sf$Year)[1]),
      pickerInput("Mig", "Choose movement strategy:",choices=unique(gps.sf$Mgtry), multiple=T, options=list('actions-box' = TRUE)),
      pickerInput("Type", "Choose season range:", choices=unique(gps.sf$season), multiple=T, options=list('actions-box' = TRUE)),
               sliderInput("range",
                  "Dates to view/export:",
                  min=min(gps.sf$POSIXct, na.rm = TRUE),
                  max=max(gps.sf$POSIXct, na.rm = TRUE),
                  value = range(gps.sf$POSIXct, na.rm = FALSE),
                  step = 1)
        ),
    textInput("desn","Download folder:", value = NULL),
    #actionButton("download_shp", "Download shp file"),
    actionButton("download_csv", "Download csv file")
  ),
  dashboardBody(leafletOutput("map", width="100%", height=800))
)

#define server with reactive functionality
#as written now, the reactivity has to occur in a certain order, and cant iteratively react... need to fix
server <- function(input, output, session) {
  filter_year <- reactive(
    gps.sf %>% filter(Year %in% input$Year)
  )
  
  observeEvent(input$Year, {
    updatePickerInput(
      session=session,
      inputId = "Mig",
      choices = unique(filter_year()$Mgtry)
    )
  })
  
  filter_mig <- reactive(
    filter_year() %>% filter(Mgtry %in% input$Mig)
  )
  
  observeEvent(input$Mig, {
    updatePickerInput(
      session=session,
      inputId = "Type",
      choices = unique(filter_mig()$season)
    )
  })
  
  filter_type <- reactive(
    filter_mig() %>% filter(season %in% input$Type)
  )
  
  observeEvent(input$Type,{
    updateSliderInput(
      session = session,
      inputId = "range",
      min = min(filter_type()$POSIXct),
      max = max(filter_type()$POSIXct),
      value = range(filter_type()$POSIXct, na.rm = FALSE)
    )
  }
)
  
  filter_range <- reactive(
    filter_type() %>% filter(POSIXct >= input$range[1] & POSIXct <= input$range[2])
  )
  
  observe({ pal <- colorFactor(palette = 'Dark2', domain = filter_range()$AID)
  
  output$map <- renderLeaflet({
    leaflet(gps.sf) %>% 
      addTiles() %>% 
      addCircleMarkers(data = filter_range(), color="grey", fillColor = ~pal(AID),fillOpacity = 1, popup=~as.character(POSIXct))
  })})
  
    output$download_csv <- downloadHandler(
    filename = function(){
      paste(input$desn,str_replace_all(Sys.Date(),pattern="-",replacement=""),".csv",sep="")
    },
    content = function(file){
      write.csv(as.data.frame(filter_range()), file, row.names=F)
    }
  )
  
  # output$download_shp <- downloadHandler(
  #   filename=paste("RDMD_datadownload_",str_replace_all(Sys.Date(),pattern="-",replacement=""),sep=""),
  #   
  #   content = function(file) {
  #     data = filter_range() # I assume this is a reactive object
  #     # create a temp folder for shp files
  #     temp_shp <- getwd()
  #     # write shp files
  #     st_write(data, paste(filename,".shp",sep=""),append=F)
  #     # zip all the shp files
  #     zip_file <- file.path(temp_shp, paste(filename,".zip",sep=""))
  #     shp_files <- list.files(temp_shp,
  #                             filename,
  #                             full.names = TRUE)
  #     # the following zip method works for me in linux but substitute with whatever method working in your OS 
  #     zip_command <- paste("zip -j", 
  #                          zip_file, 
  #                          paste(shp_files, collapse = " "))
  #     system(zip_command)
  #     # copy the zip file to the file argument
  #     file.copy(zip_file, file)
  #     # remove all the files created
  #     file.remove(zip_file, shp_files)
  #   }
  #)
  
  
}

shinyApp(ui, server)
