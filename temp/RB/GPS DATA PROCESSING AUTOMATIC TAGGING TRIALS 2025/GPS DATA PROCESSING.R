## ------ IMPORT REQUIRED LIBRARIES ------
#library(scales)
rm(list=ls())
library(mapview)
library(crosstalk)
library(tidyverse)
library(leaflet.extras)
library(stringr)
library(httr)
library(rasterVis)
library(gridExtra)
library(reshape2)
#library(OpenStreetMap)
library(osmdata)
library(rjson)
library(rgl)
#library(plotKML)
library(suncalc)
library(ggplot2)
library(ggmap)
#library(googlesheets4)
#library(anglr) #---to convert sp and sf to mesh etc. for rgl plotting
library(sp)#,lib="C:/Users/richbi/OneDrive - Norwegian University of Life Sciences/PROJECTS/LOCAL R LIBRARY")
library(sf)


library(ggmap)
library(mgcv)
#library(maptools)
library(fields)
library(shape)
library(gdistance)
library("ape")#--for minimum spanning tree function (mst)
library(allelematch)
library(abind)
library(PBSmapping)
#library(maptools)#  for lunar and sun schedules
#library(rgdal)
#library(R2jags)
#library(secr)
#library(rgeos)
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
#library(ggsn)
#library(rgdal)
library(raster)
library(sp)
#library(rjags)
library(spdep)
#library(rgeos)
#library(maptools)
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
#library(BBMM)
#library(moveVis)
#library(move)
library(leaflet)
library(move)
library(magick)

### ------ SET REQUIRED WORKING DIRECTORIES ------
#source("C:/My_documents/rovquant/analyses/Rgit/RovQuant/Temp/CM/myWorkingDirectories.R")
#source("C:/My_documents/RovQuant/Temp/PD/myWorkingDirectories.R")
source(
  "C:/Users/richbi/OneDrive - Norwegian University of Life Sciences/PROJECTS/rovquant/Temp/RB/myWorkingDirectories.R"
)


### ------ SOURCE THE REQUIRED FUNCTIONS ------
sourceDirectory(dir.function, modifiedOnly = FALSE)
#sourceDirectory("C:/Users/richbi/OneDrive - Norwegian University of Life Sciences/PROJECTS/RovQuant/Temp/RB/FUNCTIONS", modifiedOnly = FALSE)
#sourceDirectory("C:/Users/richbi/OneDrive - Norwegian University of Life Sciences/PROJECTS/RovQuant/Source_Nimble", modifiedOnly = FALSE)

sourceDirectory(
  "C:/Users/richbi/OneDrive - Norwegian University of Life Sciences/PROJECTS/Reveprosjekt/Source",
  modifiedOnly = FALSE
)

#sourceDirectory("C:/Users/richbi/OneDrive - Norwegian University of Life Sciences/PROJECTS/cattrack/Source/source2021", modifiedOnly = FALSE)



#---LOAD API ACCESS FUNCTION 
source(
  "C:/Users/richbi/OneDrive - Norwegian University of Life Sciences/PROJECTS/GPS DEVELOPMENT 2025/ANALYSIS/nRF nordic rest API download_v4.R"
)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## ------ 0.SET ANALYSIS CHARACTERISTICS -----
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#### ==== 1. GENERAL VARIABLES DECLARATION ====
myVars <- list(WD = "C:\\Users\\richbi\\OneDrive - Norwegian University of Life Sciences\\PROJECTS\\RGIT",
               #---[RB] Need to start using shorter names, otherwise issues with file operations from R.
               modelName = "DeerTrack",
               plot.check = TRUE)


fig.path <- file.path(myVars$WD, myVars$modelName, "FIGURES")
dir.create(fig.path, recursive = TRUE)
dat.path <- file.path(myVars$WD, myVars$modelName, "DATA")
dir.create(dat.path, recursive = TRUE)


#---DEFINE PROJECTIONS
proj4.latlong <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4.utm <- CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## ------ LOADING DEER MARKING DATA-----
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

library(readxl)

path<-file.path(dat.path,"FIRST EXPERIMENT FULL DATA.xlsx")
marking.df <- read_excel(path)

marking.df<-marking.df%>%
  filter(OUTCOME%in%"COLLARED")%>%
  mutate(
  collared.datetime.posix=
    as.POSIXct(paste(
      format(DATE,"%Y-%m-%d"),
      format(TIME,"%H:%M:%S")
    ,sep=""),tz="Europe/Oslo"),
    
   end.datetime.posix=as.POSIXct(ifelse(is.na(COLLAR.DROP.DATETIME) ,as.POSIXct(Sys.time(),tz="UTC"),COLLAR.DROP.DATETIME))
    
    
    )%>%
  arrange(collared.datetime.posix)
    
  


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## ------ DOWNLOADING GPS DATA VIA NORDIC API  -----
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

nrf.path<-file.path(dat.path,"NRF data raw_20250704 15_08_20")
nrf.path<-file.path(dat.path,paste("NRF data raw",format(as.POSIXct(Sys.time(),tz="UTC"),"%Y%m%d %H_%M_%S"),sep="_"))
dir.create(nrf.path, recursive = TRUE)


for (i in 1:dim(marking.df)[1]) {
  print(marking.df$DEER.ID[i])
  
  success<-FALSE
  try({
    
    getNrfData(
      device.id = marking.df$GPS.ID[i]
      ,
      
      start.datetime=marking.df$collared.datetime.posix[i]
      ,
      end.datetime = marking.df$end.datetime.posix[i]#"2025-05-19 05:21"
      ,
      destination = nrf.path
      ,
      token = "b44be00fb92eb6295eb30d7f21b5f683ff3e86ac"
    )
    success<-TRUE
  },silent=TRUE)
  print(success)
}  




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## ------ PROCESSING DEER GPS DATA  -----
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

i<-12
for (i in 1:dim(marking.df)[1]) {
  
  right <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  
  label<-paste("Deer", marking.df$DEER.ID[i],"_GPS",right(marking.df$GPS.ID[i],4), sep = "")
  
  
  print(label)
  now.posix<-marking.df$end.datetime.posix[i]#as.POSIXct(Sys.time(),tz="UTC")
  current.posix <-as.POSIXct(Sys.time(),tz="UTC")
  try({

    ###########################################################
    #       LOAD AND PROCESS NRF DATA
    ###########################################################
    

    path <-
      file.path(nrf.path,
                paste(marking.df$GPS.ID[i], ".json", sep = ""))
    
    #ID <- label
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
    
    
    dat$received.datetime.posix <- as.POSIXct(strptime(temp, "%Y-%m-%d %H:%M:%S")
                                              , tz = "UTC")
    
    attr(dat$received.datetime.posix, "tzone") <- "Europe/Paris"
    
    
    dat$datetime.posix<- as.POSIXct(dat$message.ts/1000,tz = "UTC")
    
    
    
    
      # #------ACCELEROMETER STRING FORMATTING/PARSING
    # 
    # 
    # temp <- strsplit(dat$message.accelerometer_xyz, ", ")
    # accm.data <- as.data.frame(do.call(rbind, temp))
    # accm.data <- data.frame(lapply(accm.data, as.numeric))
    # names(accm.data) <- c("x", "y", "z")
    # dat <- cbind(dat, accm.data)
    
    
    
    full.dat <- dat
    
    full.dat<-full.dat%>%filter(!is.na(message.appId))
    
    #---split into separate data frames, based on data type:
    
    #i <- 1:10
    dat <-
      tapply(1:dim(full.dat)[1], INDEX = list(full.dat$message.appId), function(i) {
        return(full.dat[i, ])
      })
    
    # dat$Accelerometer <-
    #   full.dat[full.dat$message.appId %in% c("Status", "GPS"), ]
    
    dat$All <- full.dat
    
    names(dat)
    
    
    
    ########################################
    #       ALL EVENTS
    ########################################
    
    all.data<-dat$All%>%
      filter(!is.na(message.appId))%>%
      dplyr::select(datetime.posix,received.datetime.posix,message.appId,message.data)
    
    # temp <- gsub("T", " ", all.data$receivedAt)
    # temp <- gsub("Z", "", temp)
    # all.data$received.datetime.posix <- as.POSIXct(strptime(temp, "%Y-%m-%d %H:%M:%S")
    #                                       , tz = "UTC")
    # 
    # attr(all.data$received.datetime.posix, "tzone") <- "Europe/Paris"
    
    
    
    all.data<-all.data%>%rename(activity=message.appId)%>%dplyr::select(datetime.posix,received.datetime.posix,activity)%>%
      arrange(datetime.posix)
    
    
    
    all.data<-all.data%>%mutate(n.events=row_number())
    
    
    temp<-all.data%>%group_by(activity)%>%summarize(n=n())
    
    all.data<-all.data%>%left_join(temp)%>%mutate(activity.n=paste(activity," (",n,")",sep=""))
    
    set.seed(1984)
    
    all.data$activity.n<-factor(all.data$activity.n)
    all.data$activity.n<-factor(all.data$activity.n,levels=rev(sample(levels(all.data$activity.n))))#--to get better color contrasts
    
    #--making dummy n.events value to plot "perls" of activity type
    temp<-all.data
    
    temp$bat<-max(all.data$n.events)-DoScale(as.numeric(temp$activity.n),min(all.data$n.events) + 0.2*max(all.data$n.events) ,max(all.data$n.events)-0.2*max(all.data$n.events))
    
    temp$datetime.posix<-ifelse(is.na(temp$datetime.posix),temp$received.datetime.posix,temp$datetime.posix)
    
    temp$datetime.posix[is.na(temp$datetime.posix)]<- temp$received.datetime.posix[is.na(temp$datetime.posix)]
    
    
    
    ggplot.allActivity<-ggplot(all.data,aes(x=datetime.posix,y=n.events)) + 
      geom_jitter(data=temp,aes(x=datetime.posix,y=bat,color=activity.n),height=max(all.data$n.events)/40,alpha=0.3) +geom_line() +
      geom_vline(aes(xintercept=now.posix),color="red")+
      geom_vline(aes(xintercept=current.posix),color="blue")+
      xlim(time.xlim)+
      
      # theme(legend.position = c(0.2, .7)#,
      #         
      #       #panel.background = element_rect(fill = "black")
      #       ) +
      xlim(range(all.data$datetime.posix,na.rm=TRUE)) + 
      ggtitle("All activity (cumulative)")
    
    
    ########################################
    #       CHECK FREQUENCY OF EVENTS
    ########################################
    
    
    all.data<-all.data%>%mutate(deltaT=as.numeric(datetime.posix-lag(datetime.posix)))
    
    # summary(all.data$deltaT)
    # summary(all.data$deltaT[all.data$deltaT>0 & all.data$deltaT<500])
    # 
    # hist(all.data$deltaT[all.data$deltaT>0 & all.data$deltaT<500])
    

    time.xlim<-range(c(range(all.data$datetime.posix,na.rm=TRUE),current.posix))
    
    ###########################################################
    #       GGA STRING FORMATTING/PARSING
    ###########################################################
    
    has.gnss.data<-FALSE
    
    
    #JGG: HDOP should stay <= 4 for reasonable precision.
    
    
    try({
      
      
      #---see gga string components at: https://www.hemispheregnss.com/technical-resource-manual/Import_Folder/GPGGA_Message.htm
      #----EXPLANATION OF STRING ELEMENTS: http://lefebure.com/articles/nmea-gga/
      
      # EXAMPLE: $GPGGA,001038.00,3334.2313457,N,11211.0576940,W,2,04,5.4,354.682,M,- 26.574,M,7.0,0138*79
      # FIELDS: $GPGGA,HHMMSS.SS,DDMM.MMMMM,K,DDDMM.MMMMM,L,N,QQ,PP.P,AAAA.AA,M,±XX.XX,M, SSS,RRRR*CC<CR><LF>
      
      dat$GNSS<-dat$GNSS%>%filter(!is.na(message.nmea))
      temp <- strsplit(dat$GNSS$message.nmea, ",")
      gga.data <- as.data.frame(do.call(rbind, temp))
      names(gga.data) <- 
        c("GGAtype","HHMMSS.SS","DDMM.MMMMM","K","DDDMM.MMMMM","L","N","QQ","PP.P","AAAA.AA","M","XX.XX","M", "SSS","RRRR")
      # strsplit(
      #   "GGAtype,HHMMSS.SS,DDMM.MMMMM,K,DDDMM.MMMMM,L,N,QQ,PP.P,AAAA.AA,M,±XX.XX,M, SSS,RRRR*CC<CR><LF>",
      #   ","
      # )[[1]]
      dim(gga.data)
      head(gga.data)
      dim(dat$GNSS)
      
      gga.data<-cbind(dat$GNSS,gga.data)
      
     
      
      
      gga.data$lat <-
        as.numeric(substr(gga.data$DDMM.MMMMM, 1, 2)) + as.numeric(substr(gga.data$DDMM.MMMMM, 3, str_length(gga.data$DDMM.MMMMM))) /
        60
      gga.data$lon <-
        as.numeric(substr(gga.data$DDDMM.MMMMM, 1, 3)) + as.numeric(substr(gga.data$DDDMM.MMMMM, 4, str_length(gga.data$DDDMM.MMMMM))) /
        60
      
      gga.data$altitude <- as.numeric(as.character(gga.data$AAAA.AA))
      
      gga.data$lat <- ifelse(gga.data$K %in% "N", gga.data$lat, -gga.data$lat)
      gga.data$lon <- ifelse(gga.data$L %in% "E", gga.data$lon, -gga.data$lon)
      
      gga.data$n.sats<-as.numeric(as.character(gga.data$QQ))
      
      gga.data$HDOP<-as.numeric(as.character(gga.data$PP.P))
      #gga.data$HDOP<-as.numeric(as.character(gga.data$PP.P))
      
      names(gga.data)
      gga.data <- gga.data%>%
        dplyr::select("received.datetime.posix","datetime.posix","lat","lon","altitude","n.sats","HDOP","QQ"  )%>%
        dplyr::arrange(datetime.posix)%>%
        distinct(datetime.posix,.keep_all = TRUE)
      
      
      gps.sf <- st_as_sf(gga.data, coords = c("lon", "lat"))
      gps.sf <- st_set_crs(gps.sf, proj4.latlong)
      #gps.sf<-st_transform(gps.sf,proj4.utm)
      
      mapview(gps.sf)
      
   
      dat$gps.sf <- gps.sf
      dat$gps.df <- gga.data
      
      
      
      
      
      
     
      ###########################################################
      #      SOME INITIAL FILTERING/CLEANING/OUTLIER REMOVAL
      ###########################################################
      
      # dat$gps.sf <-
      #   dat$gps.sf[dat$gps.sf$QQ > 0 &
      #                (dat$gps.sf$altitude > 50 & dat$gps.sf$altitude < 200), ]
      # #dat$gps.sf<-dat$gps.sf[dat$gps.sf$datetime.posix>=fox.capture.data$release.datetime[fox.capture.data$file.name==ID],]
      # 
      # 
      # #temppp<-dat$gps.sf
      # 
      # #dat$GPS
      
      
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
      
      
      
      ggplot.map <-
        ggplot() + geom_point(data = data.frame(st_coordinates(dat$gps.sf)), aes(x = X , y = Y))
      
     
      
      
      ########################################
      #       POSITION ACCUMULATION
      ########################################
      
      gga.data<-gga.data%>%mutate(n.pos=row_number())
      
      gga.received.data<-gga.data%>%mutate(datetime.posix=received.datetime.posix)
      
      ggplot.pos<-ggplot(gga.data,aes(x=datetime.posix,y=n.pos)) + 
        geom_point (data=gga.received.data,mapping=aes(x=datetime.posix,y=n.pos),color="pink",alpha=0.5,size=3) +
        geom_vline(aes(xintercept=now.posix),color="red")+
        geom_vline(aes(xintercept=current.posix),color="blue")+
        xlim(time.xlim)+
        geom_line(color=grey(0.5)) + 
        geom_point () +
        
        #xlim(c(min(all.data$datetime.posix,na.rm=TRUE),now.posix)) +
        ggtitle(paste("GPS position accumulation","-->",gga.data$n.pos[gga.data$datetime.posix==max(gga.data$datetime.posix)]))
      
      
      
      
      
      ########################################
      #       FINAL MAP
      ########################################
      
      
      
      #----- CENTROID FOR LAST X HOURS
      
      
      centroid.xy<-dat$gps.sf%>%
        filter(as.numeric(difftime(now.posix,datetime.posix,units = "hours"))<=48) %>%
        st_transform(crs=4326)%>%
        st_union()%>%st_centroid()%>%st_coordinates()%>%round(5)
      
      
      
      
      # xy.list<-list(
      #   eikeveien=c(59.65370356200299, 10.788878739299015))
      
      library(basemaps)
      
      get_maptypes()
      
      set_defaults(map_service = "esri", map_type =  "world_imagery")#"world_street_map"
      
      
      temp.sf<-st_transform(dat$gps.sf,crs=3857)
      temp.df<-temp.sf%>%st_coordinates()%>%data.frame()
      
      buffered.temp<-temp.sf%>%st_buffer(dist = 250)%>%st_union()
      
      graphics.off()
      ggplot.map<-ggplot()+
        basemap_gglayer(buffered.temp)+#
        scale_fill_identity() + 
        coord_sf()+
        
        ggspatial::geom_spatial_point(data = temp.df, aes(x = X, y = Y), fill = "orange", color = "black",shape=21,size=3)+
        
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              legend.position="none",
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank())+
        ggtitle(paste(label,"\n Centroid last 24hrs: ",paste(centroid.xy,collapse=", "),sep=""))
      
      #print(this.map)
      
      #   out.path<-file.path(fig.path,"FIG zoomed map_neighborhood.png")
      #   png(filename = out.path,width = 9,height = 9,units = "in",res = 500)
      #   print(this.map)
      #   dev.off()
      #   
      #   return(b)
      #   
      # })
      # 
      
      
      ###########################
      ##    NSD PLOT
      ###########################
      
      first.loc<-  dat$gps.sf%>%
        filter(datetime.posix==min(datetime.posix))
      
      mapview(first.loc)
      
      empty <- st_as_sfc("POINT(EMPTY)")
      
      
      dat$gps.sf$nsd<-as.numeric(st_distance(dat$gps.sf,first.loc))
      
      
      
      plot.nsd<-ggplot(data=dat$gps.sf,mapping=aes(x=datetime.posix,y=nsd)) +
        geom_line() +
        geom_point(aes(color=HDOP<=4))+
        geom_vline(aes(xintercept=now.posix),color="red")+
        geom_vline(aes(xintercept=current.posix),color="blue")+
        xlim(time.xlim)

      
      
      has.gnss.data<-TRUE
    },silent=TRUE
    )
    
    
    
    
    
    
    ########################################
    #       GROUND FIX (FOR SIGNAL QUALITY)
    ########################################
    
    #rm(ggplot.rsrp,ggplot.rsrq)
    has.rsrp<-FALSE
    try({
      
      
      rsrp.fields<-grep("rsrp",names(dat$GROUND_FIX),value = TRUE)
      rsrq.fields<-grep("rsrq",names(dat$GROUND_FIX),value = TRUE)
      
      #--TO DO: check with rsrq and rsrp fields we should use (there are multiple of each)
      signal.data<-dat$GROUND_FIX%>%
        filter(!is.na(message.data.lte.rsrp))%>%
        mutate(datetime.posix=received.datetime.posix)# %>% 
      #    dplyr::select(datetime.posix,received.datetime.posix,message.data.lte.rsrp,message.data.lte.nmr.rsrp,message.data.lte.rsrp,message.data.lte.rsrq,message.data.lte.nmr.rsrq)
      # 
      
      rsrp.fields<-grep("rsrp",names(signal.data),value = TRUE)
      rsrq.fields<-grep("rsrq",names(signal.data),value = TRUE)
      
      
      signal.data<-signal.data[,c("datetime.posix","received.datetime.posix",rsrp.fields,rsrq.fields)]
      
      
      temp<-melt(signal.data, id.vars = c("datetime.posix","received.datetime.posix"),
                 measure.vars = rsrp.fields,variable.name = "rsrp.variable",value.name = "rsrp")
      
      
      temp2<-melt(signal.data, id.vars = c("datetime.posix","received.datetime.posix"),
                  measure.vars = rsrq.fields,variable.name = "rsrq.variable",value.name = "rsrq")
      
      head(temp2)
      
      
      #matplot(signal.data[,rsrq.fields])
      
      
      # library(segmented)
      # 
      # fit.glm<-glm(rsrp~datetime.posix, data=temp)
      # fit.seg<-segmented(fit.glm, seg.Z=~datetime.posix,psi=5)
      # 
      # plot(fit.glm)
      # lines(fit.seg)
      # 
      # pred<-expand.grid(datetime.posix=seq(min(temp$datetime.posix),max(temp$datetime.posix),by="hour"))
      # fit<-predict(fit.seg,newdata=pred,se.fit=TRUE)  
      # pred$fit<-fit$fit
      # pred$se.fit<-fit$se.fit
      # pred$ucl<-pred$fit+1.96*pred$se.fit
      # pred$lcl<-pred$fit-1.96*pred$se.fit
      # 
      # 
      # ggplot()+
      # 
      #   geom_point(data=temp,mapping=aes(x=datetime.posix,y=rsrp,color=rsrp.variable))+
      #   
      #   geom_line(data=pred,mapping=aes(y=fit,x=datetime.posix))
      # 
      
      mean.rsrp<-mean(temp$rsrp[temp$rsrp.variable%in%c("message.data.lte.rsrp")],na.rm=TRUE)
      #dBm<- -140+mean(temp$rsrp,na.rm=TRUE)
      
      
      ggplot.rsrp<-ggplot(data=temp,mapping=aes(x=datetime.posix,y=rsrp,color=rsrp.variable))+
        geom_line()+
        geom_hline(yintercept=mean.rsrp,color="black",linetype=2,linewidth=2)+
        geom_vline(aes(xintercept=now.posix),color="red")+
        geom_vline(aes(xintercept=current.posix),color="blue")+
        xlim(time.xlim)+
        ylim(c(-150,-50)) +
        
        theme(legend.position = "none")+
        ggtitle(paste("RSRP     mean RSRP --->",floor(mean.rsrp),sep=""))
      
      
      ggplot.rsrq<-ggplot(data=temp2,mapping=aes(x=datetime.posix,y=rsrq,color=rsrq.variable))+
        geom_line()+
        xlim(c(min(all.data$datetime.posix,na.rm=TRUE),now.posix)) + 
        ylim(c(-50,0)) +
        theme(legend.position = "none",
              legend.title = element_blank())+
        ggtitle("RSRP")
      has.rsrp<-TRUE
    },silent=TRUE)
    
    # #   temp <- gsub("T", " ", bat.data$receivedAt)
    # # temp <- gsub("Z", "", temp)
    # # bat.data$datetime.posix <- as.POSIXct(strptime(temp, "%Y-%m-%d %H:%M:%S")
    # #                                  , tz = "UTC")
    # # 
    # # attr(bat.data$datetime.posix, "tzone") <- "Europe/Paris"
    # 
    # signal.data<-signal.data%>%rename(rsrp=message.data.lte.rsrp,rsrq=message.data.lte.rsrq)%>%dplyr::select(datetime.posix,received.datetime.posix,rsrp,rsrq)%>%
    #   arrange(datetime.posix)
    # 
    # ggplot.rsrp<-ggplot(signal.data,aes(x=datetime.posix,y=rsrp)) +
    #   geom_vline(aes(xintercept=now.posix),color="red")+
    #   #geom_jitter(data=temp,aes(x=datetime.posix,y=bat,color=activity.n),height=(batlim[2]-batlim[1])*0.8/40,alpha=0.5) +
    #   geom_line(color=grey(0.5)) + 
    #   geom_point () +
    #   xlim(c(min(all.data$datetime.posix,na.rm=TRUE),now.posix)) + 
    #   ylim(c(-130,-90)) +
    #   theme(legend.position = "bottom",
    #         legend.title = element_blank())+
    #   ggtitle("RSRP")
    # 
    # ggplot.rsrq<-ggplot(signal.data,aes(x=datetime.posix,y=rsrq)) +
    #   geom_vline(aes(xintercept=now.posix),color="red")+
    #   #geom_jitter(data=temp,aes(x=datetime.posix,y=bat,color=activity.n),height=(batlim[2]-batlim[1])*0.8/40,alpha=0.5) +
    #   geom_line(color=grey(0.5)) + 
    #   geom_point () +
    #   xlim(c(min(all.data$datetime.posix,na.rm=TRUE),now.posix)) + 
    #   ylim(c(-30,0)) +
    #   theme(legend.position = "bottom",
    #         legend.title = element_blank())+
    #   ggtitle("RSRQ")
    
    ########################################
    #       POWER CONSUMPTION
    ########################################
    
    
    
    bat.data<-dat$BATT%>%
      filter(!is.na(message.data))%>%
      dplyr::select(datetime.posix,received.datetime.posix,message.data)
    
    
    #   temp <- gsub("T", " ", bat.data$receivedAt)
    # temp <- gsub("Z", "", temp)
    # bat.data$datetime.posix <- as.POSIXct(strptime(temp, "%Y-%m-%d %H:%M:%S")
    #                                  , tz = "UTC")
    # 
    # attr(bat.data$datetime.posix, "tzone") <- "Europe/Paris"
    
    bat.data<-bat.data%>%rename(bat=message.data)%>%dplyr::select(datetime.posix,received.datetime.posix,bat)%>%
      arrange(datetime.posix)
    
    
    
    
    
    
    #
    
    #---ADD INFO ABOUT DATA TYPE TO MATCH WITH BATTERY STATUS DYNAMICS
    
    #--making dummy battery value to plot "perls" of activity type
    
    batlim<-c(3000,4300)
    
    temp<-all.data
    temp$bat<-batlim[2]-(DoScale(as.numeric(temp$activity.n),batlim[1]- (batlim[2]-batlim[1])*0.2, batlim[2] - (batlim[2]-batlim[1])*0.2)-batlim[1])
    
    
    ggplot.bat<-ggplot(bat.data,aes(x=datetime.posix,y=bat)) +
      geom_jitter(data=temp,aes(x=received.datetime.posix,y=bat,color=activity.n),height=(batlim[2]-batlim[1])*0.8/40,alpha=0.5) +
      geom_line(color=grey(0.5)) + 
      geom_point () +
      geom_vline(aes(xintercept=now.posix),color="red")+
      geom_vline(aes(xintercept=current.posix),color="blue")+
      xlim(time.xlim)+
      ylim(c(3000,4300)) +
      theme(legend.position = "bottom",
            legend.title = element_blank())+
      ggtitle(paste("Battery status","-->",bat.data$bat[bat.data$datetime.posix==max(bat.data$datetime.posix)]))
    
    
    
    ########################################
    #       BATTERY FIX ACCUMULATION
    ########################################
    bat.data<-bat.data%>%mutate(n.pos=row_number())
    
    
    bat.received.data<-bat.data%>%mutate(datetime.posix=received.datetime.posix)
    
    ggplot.bat2<-ggplot(bat.data,aes(x=datetime.posix,y=n.pos)) + 
      
      geom_point (data=bat.received.data,mapping=aes(x=datetime.posix,y=n.pos),color="pink",alpha=0.5,size=3) +
      geom_point (data=bat.received.data,mapping=aes(x=datetime.posix,y=n.pos),color="red",alpha=1,size=0.2) +
      geom_vline(aes(xintercept=now.posix),color="red")+
      geom_vline(aes(xintercept=current.posix),color="blue")+
      xlim(time.xlim)+
      geom_line(color=grey(0.5)) + 
      geom_point () +
      #xlim(c(min(all.data$datetime.posix,na.rm=TRUE),now.posix)) +
      ggtitle(paste("Battery reading accumulation","-->",bat.data$n.pos[bat.data$datetime.posix==max(bat.data$datetime.posix)]))
    
    
    
    
    
    
    ########################################
    #       EXPORT SUMMARY PLOTS
    #####################################
    

    
    graphics.off()
    pdf(
      file = paste(fig.path, "/",label, "_Summary.pdf", sep = ""),
      width = 14,
      height = 10,
      pointsize = 12
    )
    
    
    if(has.gnss.data){
      
      if(has.rsrp){
        grid.arrange(
          ggplot.pos,
          ggplot.map,
          plot.nsd,
          ggplot.bat2,
          ggplot.bat,
          ggplot.rsrp,
          #ggplot.rsrq,
          nrow = 2
        )
        
        
      }else{
        grid.arrange(
          ggplot.pos,
          ggplot.map,
          plot.nsd,
          ggplot.bat2,
          ggplot.bat,
          nrow = 2
        )
        
      }
    }else{
      if(has.rsrp){
        grid.arrange(
          ggplot.bat2,
          ggplot.bat,
          ggplot.rsrp,
          #ggplot.rsrq,
          nrow = 2
        )
        
        
      }else{
        grid.arrange(
          ggplot.bat2,
          ggplot.bat,
          nrow = 2
        )
        
      } 
      
      
      
    }
    
    
    
    
    
    
    
    graphics.off()
    
    ########################################
    #       EXPORT PROCESSED NRF DATA
    #####################################
    
    path <-
      file.path(dat.path, "PROCESSED NRF DATA")
    dir.create(path, recursive = TRUE)
    out.path <- file.path(path, paste("data_", label, ".RData", sep = ""))
    save(dat, file = out.path)
    
    ########################################
    #       EXPORT PROCESSED GPS DATA
    ########################################
    
    path <-
      file.path(dat.path, "PROCESSED GPS DATA")
    dir.create(path, recursive = TRUE)
    out.path <- file.path(path, paste("data_", label, ".RData", sep = ""))
    gps.sf<-dat$gps.sf
    save(gps.sf, file = out.path)
    
    #head(gps.sf)
  },silent=TRUE)
}





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## ------ MAPPING ALL DEER POSITIONS  ----- (USE LEAFLET INSTEAD TO TOGGLE BETWEEN INDIVIDUALS)
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#--for now, only plot
markingJUL2025.df<-marking.df%>%filter(collared.datetime.posix>=as.POSIXct("2025-06-30"))


i<-3
gps.pts.ls<-lapply (1:dim(markingJUL2025.df)[1],function(i){
  print(i)
  gps.sf<-NULL
  try({
    label<-paste("Deer", markingJUL2025.df$DEER.ID[i],"_GPS",right(markingJUL2025.df$GPS.ID[i],4), sep = "")
    
    in.path <- file.path(dat.path, "PROCESSED GPS DATA", paste("data_", label, ".RData", sep = ""))
    
    load(in.path)#gps.sf
    
    
    dim(gps.sf)
    gps.sf<-gps.sf%>%
      filter(QQ > 0 & HDOP<=4)%>%# & n.sats>6)%>%
      st_transform(crsd=3857)
    dim(gps.sf)
    
    #mapview(gps.sf)
    gps.sf$deer.id<-as.character(markingJUL2025.df$DEER.ID[i])
    
  },silent=TRUE
  )
  try(if(dim(gps.sf)[1]<2)gps.sf<-NULL,silent=TRUE)
  return(gps.sf)
  
})

gps.tracks.ls<-lapply(gps.pts.ls,function(x){
  out<-NULL
  try({
  out<-x%>%
    #group_by(deer.id)%>% 
    arrange(datetime.posix) %>% 
    summarize(geometry = st_combine(geometry) ) %>% 
    st_cast("LINESTRING")%>%ungroup()
 
  },silent=TRUE)
  return(out)
})

deer.list<-markingJUL2025.df$DEER.ID

deer.colors<-rainbow(n=length(deer.list))
#deer.colors<-sample(colors(),length(deer.list))
#names(deer.colors)<-deer.list

keep<-unlist(lapply(gps.tracks.ls,function(x)!is.null(x)))

deer.list<-deer.list[keep]
gps.tracks.ls<-gps.tracks.ls[keep]
gps.pts.ls<-gps.pts.ls[keep]

anchor<-c(61.491530956260874, 5.096944740514957)

#make_map <- function() {
  mymap <- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addProviderTiles(
      providers$Esri.WorldImagery,
      options = providerTileOptions(opacity = 0.5)
    ) %>%
    setView(lng = anchor[2], lat = anchor[1], zoom = 14)
  
  
  i<-1
  for(i in 1:length(deer.list)){
    print(i)
    col<-deer.colors[i]
    print(col)
    mymap<-mymap%>%
      addPolylines(lng = st_coordinates(gps.tracks.ls[[i]])[,1], lat= st_coordinates(gps.tracks.ls[[i]])[,2],color =col,weight=1,group=deer.list[i],opacity=0.75)#%>%
    mymap<-mymap%>%
      addCircleMarkers(
        lng = st_coordinates(gps.pts.ls[[i]])[, 1],
        lat = st_coordinates(gps.pts.ls[[i]])[, 2],
        #popup = gps.tracks.ls[[i]],
        group = deer.list[i],
        radius = 1,
        color =col,opacity=0.5
      )
     }
  
  
  
  
  mymap<-mymap %>%
  addLayersControl(
    baseGroups = deer.list, #c(as.character(sort(years, decreasing = TRUE))),
    #overlayGroups = c("Deer"),#, "Management regions", "Counties" ),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
    # hideGroup("Counties") %>%
    # hideGroup("Management regions") %>%
    
    #   htmlwidgets::onRender("
    #         function() {
    #             $('.leaflet-control-layers-overlays').prepend('Administrative boundaries<br>(click on map to show name)');
    #
    #         }
    # ")%>%#$('.leaflet-control-layers-list').prepend('BLA');
    
    addResetMapButton() #%>%
#     # addEasyButton(easyButton(
#     #   icon = "ion-arrow-shrink",
#     #   title = "Reset View",
#     #   onClick = JS(
#     #     "function(btn, map){ map.setView(map._initialCenter, map._initialZoom); }"
#     #   )
#     # )) %>%
#     htmlwidgets::onRender(
#       JS(
#         "
# function(el, x){
#   var map = this;
#   map.whenReady(function(){
#     map._initialCenter = map.getCenter();
#     map._initialZoom = map.getZoom();
# 
#   });
# }"
#         
#         
#       )
#     ) %>%
#     force()
#   
#   }
# make_map()    
  
  
library(htmlwidgets)

out.path<-file.path("C:/Users/richbi/OneDrive - Norwegian University of Life Sciences/PROJECTS/DeerTrack/docs/Leaflet.html")

saveWidget(mymap, file=out.path)
getwd()



# 
# #########################################
# #########################################
# #########################################
# 
# i<-12
# temp<-lapply (1:dim(marking.df)[1],function(i){
#   gps.sf<-NULL
#   try({
#   label<-paste("Deer", marking.df$DEER.ID[i],"_GPS",right(marking.df$GPS.ID[i],4), sep = "")
#   
# in.path <- file.path(dat.path, "PROCESSED GPS DATA", paste("data_", label, ".RData", sep = ""))
# 
# load(in.path)#gps.sf
# 
# 
# dim(gps.sf)
# gps.sf<-gps.sf%>%filter(QQ > 0 & HDOP<=4)
# dim(gps.sf)
# 
# mapview(gps.sf)
# gps.sf$deer.id<-as.character(marking.df$DEER.ID[i])
# 
# },silent=TRUE
#   )
# return(gps.sf)
# 
# })
# all.gps.sf<-do.call(rbind,temp)
# all.gps.track.sf<-all.gps.sf%>%group_by(deer.id)%>% arrange(datetime.posix) %>% summarize(geometry = st_combine(geometry) ) %>% st_cast("LINESTRING")%>%ungroup()
# table(all.gps.sf$deer.id)
# 
# mapview(all.gps.track.sf,zcol="deer.id")
# 
# all.gps.sf%>%st_drop_geometry()%>%summary

