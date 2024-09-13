

#-----THINGS TO DO WITH GPS SOFTWARE:


# NEED TO ADDRESS THIS!!! - NRFCLOUD:Starting July 1st, we will begin enforcing our stated limits on the free developer use of nRF Cloud. Prior to July 1st, we will be offering upgrade plans for commercial users. See the documentation about our limits. Close

# is the watchdog working? unit just seemed to stop after the first burst, even after charging all night.

# what happens when the units "hangs"?

# is there an info message when the unit had to restart?


# need meaningful accelerometer data
# make entries consistent (not absolutely necessary)
# consider other gps data strings (more informative)
# what is the projection/coordinate system of the GPS data?
# what format is the message.time string in? It's not seconds since 1970...
# measure actual battery status (volt or similar - ideally multiple measurements)
# reduce inter-burst interval
# increase burst-length
# allow on-board storage of data (for when there is no data link)
# what is AGPS? No coordinates?


#--battery consumption seems to still be pretty high (<1500 fixes per 800 mAh)





#{#### SETUP: RUN ALL
#---NEEDED ON RICHARD'S MACHINE
#


rm(list = ls())
.libPaths(new = c(.libPaths(), "C:\\DIV\\R-4.0.5\\library"))

#
#install.packages(c("crosstalk","leaflet.extras")),lib="C:\\DIV\\R-4.0.5\\library")
#library(raster)
#citation("raster")

#remotes::install_github("hypertidy/anglr",lib="C:\\DIV\\R-4.0.5\\library")

## ------ IMPORT REQUIRED LIBRARIES ------
#library(scales)
library(crosstalk)
library(tidyverse)
library(leaflet.extras)
library(stringr)
library(httr)
library(rasterVis)
library(gridExtra)
library(reshape2)
library(OpenStreetMap)
library(osmdata)
library(rjson)
library(rgl)
library(plotKML)
library(suncalc)
library(ggplot2)
library(ggmap)
#library(googlesheets4)
library(anglr) #---to convert sp and sf to mesh etc. for rgl plotting
library(sp)#,lib="C:/Users/richbi/OneDrive - Norwegian University of Life Sciences/PROJECTS/LOCAL R LIBRARY")
library(sf)


library(ggmap)
library(mgcv)
library(maptools)
library(fields)
library(shape)
library(gdistance)
library("ape")#--for minimum spanning tree function (mst)
library(allelematch)
library(abind)
library(PBSmapping)
library(maptools)#  for lunar and sun schedules
library(rgdal)
#library(R2jags)
library(secr)
library(rgeos)
library(boot)
library(splines)
library(sp)
#library(spatstat)
library(raster)
#library(adehabitat)
library(RColorBrewer)
library(grDevices)
library(R2OpenBUGS)
library(adehabitatHR)
library(RgoogleMaps)
library(ggmap)
#library(weatherData)
library(overlap)
library(activity)
library(ggsn)
library(rgdal)
library(raster)
library(sp)
#library(rjags)
library(spdep)
library(rgeos)
library(maptools)
library(stringr)
library(boot)
library(abind)
library(R.utils)
library(snow)
library(ggmap)
library(RgoogleMaps)
library(readxl)
library(GPSeqClus)#---identify GPS clusters
library(dplyr)
library(rayshader)
library(qdapRegex)#rm_white function
library(adehabitatHR)
library(sp)
library(BBMM)
library(moveVis)
#library(move)
library(leaflet)
library(move)
library(magick)

## ------ SET REQUIRED WORKING DIRECTORIES ------
#source("C:/My_documents/rovquant/analyses/Rgit/RovQuant/Temp/CM/myWorkingDirectories.R")
#source("C:/My_documents/RovQuant/Temp/PD/myWorkingDirectories.R")
source(
  "C:/Users/richbi/OneDrive - Norwegian University of Life Sciences/PROJECTS/rovquant/Temp/RB/myWorkingDirectories.R"
)


## ------ SOURCE THE REQUIRED FUNCTIONS ------
sourceDirectory(dir.function, modifiedOnly = FALSE)
#sourceDirectory("C:/Users/richbi/OneDrive - Norwegian University of Life Sciences/PROJECTS/RovQuant/Temp/RB/FUNCTIONS", modifiedOnly = FALSE)
#sourceDirectory("C:/Users/richbi/OneDrive - Norwegian University of Life Sciences/PROJECTS/RovQuant/Source_Nimble", modifiedOnly = FALSE)

sourceDirectory(
  "C:/Users/richbi/OneDrive - Norwegian University of Life Sciences/PROJECTS/Reveprosjekt/Source",
  modifiedOnly = FALSE
)

#sourceDirectory("C:/Users/richbi/OneDrive - Norwegian University of Life Sciences/PROJECTS/cattrack/Source/source2021", modifiedOnly = FALSE)


## ----------------------------------------------------------------------------------------------
## ------ 0.SET ANALYSIS CHARACTERISTICS -----
## ----------------------------------------------------------------------------------------------
### ==== 1. GENERAL VARIABLES DECLARATION ====
myVars <- list(WD = "C:/Users/richbi/OneDrive - Norwegian University of Life Sciences/PROJECTS/Rgit",
               #---[RB] Need to start using shorter names, otherwise issues with file operations from R.
               modelName = "RED DEER ON SVANOYA 2023",
               plot.check = TRUE)


path <- file.path(myVars$WD, myVars$modelName, "FIGURES")
dir.create(path, recursive = TRUE)
path <- file.path(myVars$WD, myVars$modelName, "DATA")
dir.create(path, recursive = TRUE)


#---DEFINE PROJECTIONS
proj4.latlong <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4.utm <- CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")


###########################################################
#       LOAD CAPTURE DATA
###########################################################

path <-
  file.path(myVars$WD,
            myVars$modelName,
            "GPS TRACKING/GPS tagging records.xlsx")
deer.tagging.data <-
  read_excel(path)#,fileEncoding="UTF-16LE") #nned to specify the encoding otherwise issues importing the (coded) Norwegian symbols

# #---setting to Oslo time
# fox.capture.data<-fox.capture.data[!is.na(fox.capture.data$capture.datetime),]
# fox.capture.data[, grep("datetime", colnames(fox.capture.data))] <-
#   fox.capture.data[, grep("datetime", colnames(fox.capture.data))] - 1 * 60 *
#   60



#----------------------------------------------------------
#----------------------------------------------------------
#               START DEER LOOP
#----------------------------------------------------------
#----------------------------------------------------------


path <-file.path(myVars$WD,myVars$modelName,
                                "GPS TRACKING/nrf files")

file.list <- list.files(path)


file.list<-grep(".json",file.list,value=TRUE)
file.list<-gsub(".json","",file.list)

file.list<-file.list[file.list%in%deer.tagging.data$file.name]

deer.col <- rainbow(length(file.list))
names(deer.col) <- file.list

# #---FOR TESTING GPS UNITS:
# 
# path <-file.path(myVars$WD,myVars$modelName,
#                  "DATA/nrf files")
# 
# file.list <- list.files(path)
# file.list<-grep(".json",file.list,value=TRUE)
# file.list<-gsub(".json","",file.list)
# file.list<-grep("ardufox",file.list,value=TRUE)
# 
# #file.list<-c("GPStest_6751","GPStest_6835","GPStest_4178")

file.list<-c("deer2.2_gps4137")

label <- ID <- file.list[1]




#file.list <- ID <- "fox36_gps020" 
# #---zoom into time window to find drop time:
# fox.capture.data$release.datetime[fox.capture.data$file.name=="fox38_gps160"]<- 
#   as.POSIXct("2023-10-21 23:00:00", tz="UTC")
# 
# fox.capture.data$tracking.end[fox.capture.data$file.name=="fox38_gps160"]<- 
#   as.POSIXct("2023-10-23 00:00:00", tz="UTC")


# file.list <- c(
#   "ardufox-351358814196884 (5)",
#   "ardufox-351358814196827 (7)"
# 
# )


ID<-file.list[1]

#ID <- "fox40_gps6728"
for (ID in file.list) {
  label <- ID
  ###########################################################
  #       LOAD AND PROCESS NRF DATA
  ###########################################################
  

  print(label)
  path <-
    file.path(myVars$WD,
              myVars$modelName,
              "GPS TRACKING/NRF FILES",
              paste(label, ".json", sep = ""))
  
  ID <- label
  #path<-file.path(myVars$WD,myVars$modelName,"DATA/ardufox-351358814194020.json")
  
  result <- rjson::fromJSON(file = path)
  
  rm(dat)
  
  
  
  # res = GET("https://api.nrfcloud.com/v1/openapi.json")
  # res
  #
  # temp<-res$content
  #
  # sample(temp,100)
  ###########################################################
  #       FORMAT/PARSE JSON FILE
  ###########################################################
  
  #----IDENTIFY ALL FIELD NAMES (CHANGE BETWEEN ENTRIES)
  temp <- lapply(result, function(x) {
    x <- data.frame(x)
    names(x)
  })
  all.field.names <- unique(unlist(temp))
  
  
  
  #----UNRAVEL LIST
  #x<-result[[764]]
  temp <- lapply(result, function(x) {
    x <- as.data.frame(x)
    x[, all.field.names[!all.field.names %in% names(x)]] <-
      NA #---augment with missing fields
    x <- x[, all.field.names] #-- order columns consistently
    x[1, ]#---some entries result in multiple entries (see message$data$types); use only the first one
  })
  
  dat <- do.call(rbind, temp)
  
  
  
  
  # #----- GPS unit time (milliseconds since 1970-01-01) has some issues (missing digits, duplicate times, etc.)
  # #----looks like some entries result in dupliate records in the list object?
  # #json_data_frame <- as.data.frame(result)
  #
  # #--fixing some errors with the numbers
  # temp<-(max(str_length(dat$message.timestamp),na.rm=TRUE)-str_length(dat$message.timestamp))
  #
  # i<-454
  # temp<-unlist(lapply(1:length(temp),function(i){
  #   ifelse(temp[i]==0 | is.na(temp[i]),dat$message.timestamp[i],paste(dat$message.timestamp[i],paste(rep(0,temp[i]),collapse=""),sep=""))
  # }))
  #
  #
  # temp<- as.POSIXct("1970-01-01",tz="UTC" )+as.numeric(as.character(temp))/1000# milliseconds since 1970-01-01
  #
  # table(str_length(temp))
  # sort(temp)
  #
  # dat$datetime.posix<-temp
  
  #---until we figure out the actual time string from the unit/sat:
  
  temp <- gsub("T", " ", dat$receivedAt)
  temp <- gsub("Z", "", temp)
  
  
  dat$datetime.posix <- as.POSIXct(strptime(temp, "%Y-%m-%d %H:%M:%S")
                                   , tz = "UTC")
  
  attr(dat$datetime.posix, "tzone") <- "Europe/Paris"
  
  #dat<-dat[dat$datetime.posix>=as.POSIXct("2023-02-03 12:00:00",tz="UCT"),]
  
  
  if(length(deer.tagging.data$datetime.tagged[deer.tagging.data$file.name ==
                                              ID])>0){
  dat <-
    dat[dat$datetime.posix >= deer.tagging.data$datetime.tagged[deer.tagging.data$file.name ==
                                                                  ID], ]
  }
  
  if(length(deer.tagging.data$datetime.tagged[deer.tagging.data$file.name ==
                                              ID])>0 ){
    if(!is.na(deer.tagging.data$tracking.end[deer.tagging.data$file.name ==
                                             ID])){
    
  dat <-
    dat[dat$datetime.posix <= deer.tagging.data$tracking.end[deer.tagging.data$file.name ==
                                                                  ID], ]

    }
  }
  
  
  
  dat$battery.volt <- as.numeric(as.character(dat$message.battery))
  
  
  dim(dat)
  
  #------ACCELEROMETER STRING FORMATTING/PARSING
  
  
  temp <- strsplit(dat$message.accelerometer_xyz, ", ")
  accm.data <- as.data.frame(do.call(rbind, temp))
  accm.data <- data.frame(lapply(accm.data, as.numeric))
  names(accm.data) <- c("x", "y", "z")
  dat <- cbind(dat, accm.data)
  
  
  
  full.dat <- dat
  
  #---split into separate data frames, based on data type:
  
  i <- 1:10
  dat <-
    tapply(1:dim(dat)[1], INDEX = list(dat$message.appId), function(i) {
      return(dat[i, ])
      
    })
  
  dat$Accelerometer <-
    full.dat[full.dat$message.appId %in% c("Status", "GPS"), ]
  
  dat$All <- full.dat
  
  names(dat)

  
  dat$Resets <- full.dat[full.dat$message %in% c("Ardufox started!"), ]
  
  
  # temp<-as.numeric(difftime(dat$Resets$datetime.posix[-1],dat$Resets$datetime.posix[-length(dat$Resets$datetime.posix)]))
  # 
  # hist(temp)
  
  
  ###########################################################
  #       GGA STRING FORMATTING/PARSING
  ###########################################################
  
  #---see gga string components at: https://www.hemispheregnss.com/technical-resource-manual/Import_Folder/GPGGA_Message.htm
  
  # EXAMPLE: $GPGGA,001038.00,3334.2313457,N,11211.0576940,W,2,04,5.4,354.682,M,- 26.574,M,7.0,0138*79
  # FIELDS: $GPGGA,HHMMSS.SS,DDMM.MMMMM,K,DDDMM.MMMMM,L,N,QQ,PP.P,AAAA.AA,M,±XX.XX,M, SSS,RRRR*CC<CR><LF>
  
  temp <- strsplit(dat$GPS$message.data, ",")
  gga.data <- as.data.frame(do.call(rbind, temp))
  names(gga.data) <-
    strsplit(
      "GGAtype,HHMMSS.SS,DDMM.MMMMM,K,DDDMM.MMMMM,L,N,QQ,PP.P,AAAA.AA,M,±XX.XX,M, SSS,RRRR*CC<CR><LF>",
      ","
    )[[1]]
  dim(gga.data)
  head(gga.data)
  
  #gga.data$time.posix<-strptime(gga.data$HHMMSS.SS,"%H%M%S")
  
  gga.data$lat <-
    as.numeric(substr(gga.data$DDMM.MMMMM, 1, 2)) + as.numeric(substr(gga.data$DDMM.MMMMM, 3, str_length(gga.data$DDMM.MMMMM))) /
    60
  gga.data$lon <-
    as.numeric(substr(gga.data$DDDMM.MMMMM, 1, 3)) + as.numeric(substr(gga.data$DDDMM.MMMMM, 4, str_length(gga.data$DDDMM.MMMMM))) /
    60
  
  gga.data$altitude <- as.numeric(as.character(gga.data$AAAA.AA))
  
  gga.data$lat <- ifelse(gga.data$K %in% "N", gga.data$lat, -gga.data$lat)
  gga.data$lon <- ifelse(gga.data$L %in% "E", gga.data$lon, -gga.data$lon)
  
  gga.data$datetime.posix <- dat$GPS$datetime.posix
  
  gga.data <- gga.data[order(gga.data$datetime.posix), ]
  
  #------REMOVE COMPLETELY IDENTICAL DATETIMES - ALL BUT THE FIRST
  gga.data <- gga.data[!duplicated(gga.data$datetime.posix),]
  
  table(table(gga.data$datetime.posix))
  
  gps.sf <- st_as_sf(gga.data, coords = c("lon", "lat"))
  gps.sf <- st_set_crs(gps.sf, proj4.latlong)
  #gps.sf<-st_transform(gps.sf,proj4.utm)
  
  ggplot() +
    geom_sf(data = gps.sf) +
    ggtitle("GPS positions")#+
  # theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank())
  
  
  
  
  
  dat$gps.sf <- gps.sf
  
  #dat$Accelerometer
  
  #dat$Resets
  
  ###########################################################
  #      SOME INITIAL FILTERING/CLEANING/OUTLIER REMOVAL
  ###########################################################
  
  # dat$gps.sf <-
  #   dat$gps.sf[dat$gps.sf$QQ > 0 &
  #                (dat$gps.sf$altitude > 50 & dat$gps.sf$altitude < 200), ]
  #dat$gps.sf<-dat$gps.sf[dat$gps.sf$datetime.posix>=fox.capture.data$release.datetime[fox.capture.data$file.name==ID],]
  
  
  #temppp<-dat$gps.sf
  
  #dat$GPS
  
  
  ###########################################################
  #      MAPPING
  ###########################################################
  
  # # ggplot() +
  # #   geom_sf(data = dat$gps.sf)
  #
  # temp<-st_transform(dat$gps.sf,proj4.utm)
  # e <- extent(temp) #---eventually, delineate the study area based on all cat GPS positions
  #
  # #---IN CASE ONLY 1 DATA POINT
  #
  #
  # if(e[1]-e[2]==0){
  #   e<-extent(c(e[1]-1, e[2]+1, e[3]-1, e[4]+1))
  # }
  #
  # #--increase the size... ADD HERE
  # r<-raster(e,resolution=1)
  # r<-raster::extend(r,250)
  # # coerce to a SpatialPolygons object
  # e.poly <- as(extent(r), 'SpatialPolygons')
  #
  # proj4string(e.poly)<-proj4.utm
  # #---need to do stuff as latlon
  # temp2<-spTransform(e.poly,proj4.latlong)
  # #plot(temp)
  # e<-extent(temp2)
  #
  # # plot(e.poly)
  # # plot(temp,add=TRUE)
  #
  # lower.left<-c(e[3],e[1])
  # upper.right<-c(e[4], e[2])
  #
  #
  # osv.bb<-cbind(rev(lower.left),rev(upper.right))# instead of: getbb("Ås Norway")
  # template<-getbb("Ås Norway")
  # template[]<-osv.bb
  # osv.bb<-template
  #
  # osv_map <- get_map(osv.bb, maptype = "hybrid",source="google")
  # ggplot.osvmap <-   ggmap(osv_map) + geom_point(data = data.frame(st_coordinates(dat$gps.sf)), aes(x = X , y = Y))
  ggplot.osvmap <-
    ggplot() + geom_point(data = data.frame(st_coordinates(dat$gps.sf)), aes(x = X , y = Y))
  
  ########################################
  #       EXPORT RAW GPS DATA TO KML (GOOGLE EARTH)
  ########################################
  
  ID <- label
  path <- file.path(myVars$WD, myVars$modelName, "GPS TRACKING/KML FILES")
  dir.create(path, recursive = TRUE)
  
  in.path <- file.path(path, paste(ID, "_traj.kml", sep = ""))
  st_combine(dat$gps.sf) %>% st_cast("LINESTRING") %>% st_write(in.path, delete_layer =
                                                                  TRUE)
  
  in.path <- file.path(path, paste(ID, "_pts.kml", sep = ""))
  st_combine(dat$gps.sf) %>% st_write(in.path, delete_layer = TRUE)
  
  
  ########################################
  #       TRACK AND MAP FOR SHARING
  ########################################
  
  # lines.sf<-st_combine(dat$gps.sf) %>% st_cast("LINESTRING")
  #
  # poly.sf<-st_buffer(lines.sf,dist = 10)
  #
  # ggplot(poly.sf)+geom_sf()
  
  ########################################
  #       EVALUATE POWER CONSUMPTION (meaningless unless voltage is measured before power management)
  ########################################
  names(dat$Status)
  aes(group = NA)
  
  # ggplot.battery<-ggplot()+
  #   geom_line(data=dat$Status,aes(x=datetime.posix,y=battery.volt, color=message.interval)) +
  #   aes(group=NA) +
  #   geom_point(data=dat$Status,aes(x=datetime.posix,y=battery.volt, color=message.interval),alpha=0.5) +
  #   geom_vline(xintercept = dat$Resets$datetime.posix,color="black") +
  #   ylim(min(c(3000,min(dat$Status$battery.volt)),na.rm=TRUE),max(c(4200,dat$Status$battery.volt),na.rm=TRUE))+
  #   ggtitle("Power consumption over time")
  
  # ggplot.battery<-ggplot()+
  #   geom_line(data=dat$Status,aes(x=datetime.posix,y=battery.volt)) +
  #   geom_point(data=dat$Status,aes(x=datetime.posix,y=battery.volt, color=message.interval),alpha=0.5) +
  #   geom_vline(xintercept = dat$Resets$datetime.posix,color="black") +
  #   ylim(min(c(3000,min(dat$Status$battery.volt)),na.rm=TRUE),max(c(4200,dat$Status$battery.volt),na.rm=TRUE))+
  #   ggtitle("Power consumption over time")
  
  bat.status <- dat$Status[order(dat$Status$datetime.posix), ]
  bat.status$index <- 1:dim(bat.status)[1]
  dat$GPS$index <-
    findInterval(dat$GPS$datetime.posix, bat.status$datetime.posix)
  
  temp <- aggregate(datetime.posix ~ index, data = dat$GPS, length)
  names(temp) <- c("index", "n.fixes")
  bat.status <- merge(bat.status, temp, all.x = TRUE)
  bat.status$n.fixes[is.na(bat.status$n.fixes)] <- 0
  bat.status <- bat.status[order(bat.status$datetime.posix), ]
  bat.status$n.fixes <- cumsum(bat.status$n.fixes)
  
  ggplot.batteryfixes <- ggplot() +
    geom_line(data = bat.status, aes(x = n.fixes, y = battery.volt)) +
    ylim(min(c(3000, min(
      dat$Status$battery.volt
    )), na.rm = TRUE), max(c(4200, dat$Status$battery.volt), na.rm = TRUE)) +
    #geom_vline(xintercept = dat$Resets$datetime.posix,color="red") +
    ggtitle("Power consumption by fix")
  
  # ########################################
  # #       VISUALIZE STATUS (MODE)
  # ########################################
  #
  # #----INDIVIDUAL FIXES
  #
  # dat$Status$datetime.posix
  #
  # GPS<-dat$GPS[order(dat$GPS$datetime.posix),]
  # dat$GPS$fix.id<-1:length(dat$GPS[,1])
  #
  # now<-Sys.time()
  #
  # ggplot.fixes<-ggplot()+
  #   geom_line(data=dat$Status,aes(x=datetime.posix,y=fix.id)) +
  #   geom_vline(xintercept = dat$Resets$datetime.posix,color="red") +
  #   geom_vline(xintercept = now,color="blue")+
  #   ggtitle("Positition fixes")
  ########################################
  #       VISUALIZE POSITION FIX ACCUMULATION
  ########################################
  
  #----INDIVIDUAL FIXES
  
  dat$GPS <- dat$GPS[order(dat$GPS$datetime.posix), ]
  dat$GPS$fix.id <- 1:length(dat$GPS[, 1])
  
  now <- Sys.time()
  
  ggplot.fixes <- ggplot() +
    geom_line(data = dat$GPS, aes(x = datetime.posix, y = fix.id)) +
    geom_vline(xintercept = dat$Resets$datetime.posix,
               color = "red") +
    geom_vline(xintercept = now, color = "blue") +
    ggtitle("Positition fixes")
  
  
  
  
  
  
  #---COMBINED GRAPH FIXES AND BATTERY PROFILE
  
  
  fix.status <- dat$GPS[order(dat$GPS$datetime.posix), ]
  fix.status$index <- 1:dim(fix.status)[1]
  dat$Status$index <-
    findInterval(dat$Status$datetime.posix, fix.status$datetime.posix)
  
  dat$Status$index %in% fix.status$index
  
  temp <-
    aggregate(battery.volt ~ index + message.interval, data = dat$Status, function(x)
      x[length(x)])
  temp$index %in% fix.status$index
  
  names(temp) <- c("index", "burst.interval", "volt")
  fix.status <- merge(fix.status, temp, all.x = TRUE)
  
  #fix.status$n.fixes[is.na(fix.status$n.fixes)]<-0
  fix.status <- fix.status[order(fix.status$datetime.posix), ]
  fix.status$n.fixes <- 1
  fix.status$n.fixes <- cumsum(fix.status$n.fixes)
  
  table(fix.status$burst.interval)
  
  ylim.prim <-
    c(0, max(fix.status$n.fixes))   # in this example, precipitation
  ylim.sec <- c(3000, 4300)    # in this example, temperature
  
  b <- diff(ylim.prim) / diff(ylim.sec)
  a <- ylim.prim[1] - b * ylim.sec[1] # there was a bug here
  
  
  
  ggplot.fixAndBat <-
    ggplot(fix.status, aes(datetime.posix, n.fixes)) +
    geom_line() +
    geom_line(data = fix.status[!is.na(fix.status$volt), ], aes(y = a + volt *
                                                                  b, color = burst.interval)) +
    aes(group = NA) +
    scale_y_continuous("Position fixes", sec.axis = sec_axis( ~ (. - a) /
                                                                b, name = "Battery (Volt)")) +
    #scale_x_continuous("Date", breaks = 1:12) +
    geom_vline(xintercept = dat$Resets$datetime.posix,
               color = "grey") +
    geom_vline(xintercept = now, color = "blue") +
    geom_hline(yintercept = a + 3750 * b,
               color = "red",
               linetype = "dashed") + # turtle mode threshold
    theme(
      axis.line.y.right = element_line(color = "red"),
      axis.ticks.y.right = element_line(color = "red"),
      axis.text.y.right = element_text(color = "red"),
      axis.title.y.right = element_text(color = "red")
    ) +
    ggtitle("Position fix accumulation")
  
  #----BURSTS
  
  dt <- c(1000, diff(dat$GPS$datetime.posix))
  temp <- ifelse(dt > 60, 1, 0)
  dat$GPS$burst.id <- cumsum(temp)
  
  
  ggplot.bursts <- ggplot() +
    geom_line(data = dat$GPS, aes(x = datetime.posix, y = burst.id)) +
    geom_vline(xintercept = dat$Resets$datetime.posix,
               color = "red") +
    ggtitle("Bursts")
  
  
  
  #----POSITIONS PER BURST
  
  temp <- aggregate(datetime.posix ~ burst.id, dat$GPS, mean)
  temp$n.fixes <-
    aggregate(datetime.posix ~ burst.id, dat$GPS, length)[, 2]
  # ggplot()+
  #   geom_line(data=temp,aes(x=datetime.posix,y=n.fixes))
  
  d <- 60000
  d <- d * 132664.3 + 221225.7
  exp(1.3 - 1.7 * d)
  
  
  
  ########################################
  #       POSITION ERROR (SD OF KERNEL DENSITY)
  ########################################
  
  # #----STEP 1: IDENTIFY POSITION CLUSTERS
  # 
  # #----STEP 2: CALCULATE CENTROID OF EACH CLUSTER (GEOMETRIC MEAN)
  # 
  # #----STEP 3: KERNEL DENSITY RASTER FOR EACH CLUSTER (e.g., to visualize GPS error)
  # 
  # gps.sf.utm <- st_transform(dat$gps.sf, proj4.utm)
  # 
  # 
  # library(MASS)
  # coords <- st_coordinates(gps.sf.utm)
  # krnl <- kde2d(x = coords[, 1], y = coords[, 2], n = 100)
  # 
  # #image(krnl)#, zlim = c(0, 0.05))
  # # persp(krnl, phi = 30, theta = 20, d = 5)
  # 
  # 
  # krnl.r <- raster(krnl)
  # projection(krnl.r) <- proj4.utm
  # 
  # #par(mfrow=c(4,4,1,1))
  # # plot(st_geometry(gps.sf.utm))
  # # plot(krnl.r,alpha=0.5,add=TRUE)
  # # axis(1)
  # # axis(2)
  # 
  # 
  # #plot3d(krnl.r)
  # 
  # 
  # #--calculate standard deviation of the kernel
  # 
  # # gmean.xy<-apply(coords,2,function(x)exp(mean(log(x))))#geometric mean
  # # image(krnl)
  # # plot(gps.sf.utm,add=TRUE)
  # # points(gmean.xy[2]~gmean.xy[1],pch=19,col="red")
  # #
  # # epsilon<-apply(st_coordinates(gps.sf.utm),1,function(x){
  # #
  # #   out<-sqrt((x[1]-gmean.xy[1])^2 + (x[2]-gmean.xy[2])^2)
  # # } )
  # #
  # # hist(c(epsilon,-epsilon))
  # # sd(c(epsilon,-epsilon))
  # 
  # 
  # 
  # ggplot.kernel <- gplot(krnl.r) +
  #   geom_tile(aes(fill = value)) +
  #   scale_fill_gradientn(colours = rev(terrain.colors(225))) +
  #   coord_equal()
  # 
  # 
  # #-----STEP 4: CALCULATE ERROR (SD)
  
  ########################################
  #       ACCELEROMETER DATA
  #####################################
  
  temp <-
    melt(dat$Accelerometer[, c("datetime.posix", "x", "y", "z")], id.vars = c("datetime.posix"))
  
  ggplot.accelerometer <- ggplot() +
    geom_line(data = temp, aes(x = datetime.posix, y = value, color = variable)) +
    geom_vline(xintercept = dat$Resets$datetime.posix,
               color = "black") +
    theme(legend.position = "none") +
    ggtitle("Accelerometer")
  
  ########################################
  #       TIME SERIES OF ALL TYPES OF DATA
  #####################################
  
  # #temp <- melt(dat$All[,c("datetime.posix","message.appId","message","message.msg")], id.vars = c("datetime.posix"))
  #
  #
  # temp<-dat$All[,c("datetime.posix","message.appId","message","message.msg")]
  # temp<-temp[order(temp$datetime.posix),]
  #
  # temp$type<-apply(temp[,c("message.appId","message","message.msg")],1,function(x)paste(x,collapse="_"))
  # temp$fix<-ifelse(temp$message.appId%in%"GPS",1,0)
  # temp$n.fixes<-cumsum(temp$fix)
  #
  #
  # ggplot()+
  #   geom_point(data=temp,aes(x=datetime.posix,y=type,color=type))
  
  ########################################
  #       EXPORT SUMMARY PLOTS
  #####################################
  
  fig.path <- file.path(myVars$WD, myVars$modelName, "GPS TRACKING")
  
  graphics.off()
  pdf(
    file = paste(fig.path, "/", ID, "_Summary.pdf", sep = ""),
    width = 20,
    height = 15,
    pointsize = 12
  )
  
  
  grid.arrange(
    ggplot.osvmap,
    ggplot.fixAndBat,
    ggplot.batteryfixes,
    ggplot.accelerometer,
    nrow = 2
  )
  #,ggplot.battery,ggplot.fixes,,ggplot.bursts
  
  graphics.off()
  
  ########################################
  #       EXPORT PROCESSED NRF DATA
  #####################################
  
  path <-
    file.path(myVars$WD, myVars$modelName, "GPS TRACKING/PROCESSED NRF DATA")
  dir.create(path, recursive = TRUE)
  out.path <- file.path(path, paste("data_", ID, ".RData", sep = ""))
  save(dat, file = out.path)
  
  ########################################
  #       EXPORT PROCESSED GPS DATA
  ########################################
  
    path <-
    file.path(myVars$WD, myVars$modelName, "GPS TRACKING/PROCESSED GPS DATA")
  dir.create(path, recursive = TRUE)
  out.path <- file.path(path, paste("data_", ID, ".RData", sep = ""))
  gps.sf<-dat$gps.sf
  save(gps.sf, file = out.path)
  
  head(gps.sf)
  
}
  



########################################
#       FINDING CENTROID OF DROPPED GPS
########################################

hist(st_coordinates(gps.sf)[,1])

dim(gps.sf)
names(gps.sf)
temp<-gps.sf%>%filter(datetime.posix>=as.POSIXct("2024-03-28",tz="GMT") & st_coordinates(gps.sf)[,1]>5.1 & st_coordinates(gps.sf)[,1]<5.105)
dim(temp)

centroid.sf<-temp%>%st_union()%>%st_centroid

mapview(temp)+mapview(centroid.sf,col.regions="red",alpha.regions=1)

st_coordinates(st_transform(centroid.sf,proj4.latlong))
