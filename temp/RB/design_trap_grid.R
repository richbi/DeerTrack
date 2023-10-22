
rm(list=ls())
.libPaths(new=c(.libPaths(),"C:\\DIV\\R-4.0.5\\library"))

#
#install.packages(c("mapedit"),lib="C:\\DIV\\R-4.0.5\\library")



#remotes::install_github("hypertidy/anglr",lib="C:\\DIV\\R-4.0.5\\library")

## ------ IMPORT REQUIRED LIBRARIES ------
#library(scales)
library(stringr)
library(httr)
library(rasterVis)
library(gridExtra )
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
library(fasterize)
library(leaflet)


## ------ SET REQUIRED WORKING DIRECTORIES ------
#source("C:/My_documents/rovquant/analyses/Rgit/RovQuant/Temp/CM/myWorkingDirectories.R")
#source("C:/My_documents/RovQuant/Temp/PD/myWorkingDirectories.R")             
source("C:/Users/richbi/OneDrive - Norwegian University of Life Sciences/PROJECTS/rovquant/Temp/RB/myWorkingDirectories.R")   


## ------ SOURCE THE REQUIRED FUNCTIONS ------
sourceDirectory(dir.function, modifiedOnly = FALSE)
#sourceDirectory("C:/Users/richbi/OneDrive - Norwegian University of Life Sciences/PROJECTS/RovQuant/Temp/RB/FUNCTIONS", modifiedOnly = FALSE)
#sourceDirectory("C:/Users/richbi/OneDrive - Norwegian University of Life Sciences/PROJECTS/RovQuant/Source_Nimble", modifiedOnly = FALSE)

sourceDirectory("C:/Users/richbi/OneDrive - Norwegian University of Life Sciences/PROJECTS/Reveprosjekt/Source", modifiedOnly = FALSE)

#sourceDirectory("C:/Users/richbi/OneDrive - Norwegian University of Life Sciences/PROJECTS/cattrack/Source/source2021", modifiedOnly = FALSE)


## ----------------------------------------------------------------------------------------------
## ------ 0.SET ANALYSIS CHARACTERISTICS -----
## ----------------------------------------------------------------------------------------------
### ==== 1. GENERAL VARIABLES DECLARATION ====
myVars <- list( 
  WD = "C:/Users/richbi/OneDrive - Norwegian University of Life Sciences/PROJECTS/Rgit",
  #---[RB] Need to start using shorter names, otherwise issues with file operations from R.
  modelName = "RED DEER ON SVANOYA 2023",
  plot.check = TRUE)

if(is.null(myVars$modelName))stop("YOU SHOULD PROBABLY CHOOSE A NAME FOR THIS ANALYSIS/MODEL")
if(is.null(myVars$WD))stop("YOU SHOULD PROBABLY CHOOSE A WORKING DIRECTORY FOR THIS ANALYSIS/MODEL")
#if(!dir.exists(file.path(myVars$WD,myVars$modelName))){dir.create(file.path(myVars$WD, myVars$modelName))}

fig.path<-file.path(myVars$WD,myVars$modelName,"FIGURES")
dir.create(fig.path,recursive = TRUE)
data.path<-file.path(myVars$WD,myVars$modelName,"DATA")
dir.create(data.path)


#---DEFINE PROJECTIONS
proj4.latlong<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4.utm<-CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")
proj4.NATLgrid<-CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")



###########################################################
#       SVANOY OUTLINE
###########################################################

in.path<-file.path(data.path,"GIS DATA/svanoy_2.gpkg")

svanoy.sf <- st_read(in.path)#, query = "SELECT POLY_ID")

ggplot(svanoy.sf)+geom_sf()


svanoy.sf2<-st_transform(svanoy.sf,proj4.NATLgrid)
###########################################################
#       STUDY AREA
###########################################################


 xy.df<-tibble(lat=c(61.516825,61.464223), lon=c(5.030477,5.178621))
# 
# area.poly.sf <- xy.df %>% 
#   st_as_sf(coords = c("lon", "lat"), 
#            crs = proj4.latlong) %>% 
#   st_bbox() %>% 
#   st_as_sfc()
# 
# ggplot(area.poly.sf) + geom_sf()
# 
# #---need to work on the scale of the national grid (to keep things regular/square)
# #area.poly.sf<-st_as_sf(area.poly.sp)
# area.poly.sf2<-st_transform(area.poly.sf,proj4.NATLgrid)
# 
# area.poly.sf2<-st_sf(area.poly.sf2)
# 
# e<-extent(area.poly.sf2)
# e<-raster::extend(e,250)
# 
# e.sp <- as(e, 'SpatialPolygons')
# e.sf<-st_as_sf(e.sp)
# st_crs(e.sf)<-proj4.NATLgrid
# 
# e.sf0<-e.sf

###########################################################
#       CAMERA TRAPPING GRID
###########################################################

# in.path<-file.path("C:\\Users\\richbi\\OneDrive - Norwegian University of Life Sciences\\PROJECTS\\Rgit\\Reveprosjekt\\RED FOX GPS TRACKING 2023\\DATA\\GIS DATA\\SSB RUTENETT\\rute250m_Norge.shp")
# grid250.sf<-st_read(in.path)
# 
# 
# 
# st_crs(grid250.sf)<-proj4.NATLgrid
# 
# #---need to work on the scale of the national grid (to keep things regular/square)
# #area.poly.sf<-st_as_sf(area.poly.sp)
# area.poly.sf2<-st_transform(area.poly.sf,proj4.NATLgrid)
# 
# area.poly.sf2<-st_sf(area.poly.sf2)
# 
# e<-extent(area.poly.sf2)
# e<-raster::extend(e,250)
# 
# e.sp <- as(e, 'SpatialPolygons') 
# e.sf<-st_as_sf(e.sp)
# st_crs(e.sf)<-proj4.NATLgrid
# 
# grid250.sf<- st_intersection(grid250.sf, e.sf)
# 
# grid250.sf<-grid250.sf[round(as.numeric(st_area(grid250.sf)),0)==round(max(as.numeric(st_area(grid250.sf))),0),]
# 
# 
# out.path<-file.path(myVars$WD,myVars$modelName,"DATA/SSBgrid250.RData")
# save(grid250.sf,file=out.path)

in.path<-file.path(myVars$WD,myVars$modelName,"DATA/SSBgrid250.RData")
load(in.path)#grid250.sf

grid250.sf2<- st_intersection(grid250.sf, svanoy.sf2)


###########################################################
#      LOAD AR5
###########################################################


in.path<-file.path(myVars$WD,myVars$modelName,"DATA/GIS DATA/Basisdata_4602_Kinn_25832_FKB-AR5_FGDB.gdb")

AR5.sf<- read_sf(in.path)
AR5.sf2<-st_transform(AR5.sf,proj4.NATLgrid)


AR5.sf2<- st_intersection(AR5.sf2, svanoy.sf2)

#AR5.sf2
AR5.sf2 <- AR5.sf2 %>% st_collection_extract("POLYGON")


plot1<-ggplot(svanoy.sf2) + 
  geom_sf()+
  #geom_sf(data=area.poly.sf,fill="green",alpha=0.3)+
  geom_sf(data=AR5.sf2,aes(fill=arealtype),color=NA,alpha=0.6)+
  geom_sf(data=grid250.sf2,fill=NA,color="white",alpha=0.3)


graphics.off()
out.path<-file.path(fig.path,"AR5_svanoya.pdf")
pdf(out.path)
plot1
dev.off()



###########################################################
#      MAP EVERYTHING
###########################################################


#e.sf<-st_transform(e.sf,proj4.latlong)

grid250.sf<-st_transform(grid250.sf2,proj4.latlong)
svanoy.sf<-st_transform(svanoy.sf2,proj4.latlong)
AR5.sf<-st_transform(AR5.sf2,proj4.latlong)








library(units)



cell.area<-set_units(250,  m^2)
cell.size<-250


area_honeycomb_grid = st_make_grid(svanoy.sf2, cellsize=cell.size, what = "polygons", square = FALSE)



# To sf and add grid ID
honeycomb_grid_sf2 = st_sf(area_honeycomb_grid) %>%
  # add grid ID
  mutate(grid_id = 1:length(lengths(area_honeycomb_grid)))

ggplot(honeycomb_grid_sf2) + geom_sf() + geom_sf(data=svanoy.sf2,color="red",fill=NA)


honeycomb_grid_sf2<- st_intersection(honeycomb_grid_sf2, svanoy.sf2) 

honeycomb_grid_sf2 <- honeycomb_grid_sf2 %>% st_collection_extract("POLYGON")

honeycomb_grid_sf<-st_transform(honeycomb_grid_sf2,proj4.latlong)


ggplot(honeycomb_grid_sf) + geom_sf() + geom_sf(data=svanoy.sf,fill=NA)

ar5.cols<-rainbow(length(unique(as.numeric(factor(AR5.sf$arealtype)))))[as.numeric(factor(AR5.sf$arealtype))]

deer.leaflet <-  leaflet() %>% #width = "100%"#height=650, width=1000
  # addTiles(group="One")%>%  # Add default OpenStreetMap map tiles
  addProviderTiles(providers$Esri.WorldImagery,
                   group="Two")%>%
  addPolygons(data=AR5.sf,col=ar5.cols,weight = 1,opacity = 1,fillOpacity = 0.5,group="AR5",
              popup = paste("Cell ID ", AR5.sf$arealtype))%>%
  
  addPolygons(data=grid250.sf,col="white",weight = 1,opacity = 0.3,fillOpacity = 0,group="National grid (250m)",
              popup = paste("Cell ID ", grid250.sf$TARGET_FID))%>%
  
  addPolygons(data=honeycomb_grid_sf,col="white",weight = 1,opacity = 0.3,fillOpacity = 0,group="Hexagonal grid (250m)",
              popup = paste("Cell ID ", honeycomb_grid_sf$grid_id))%>%

  addPolygons(data=svanoy.sf,col="red",weight =2,opacity = 1,fillOpacity = 0,group="Svanoya")%>%
  
  setView(lng=mean(xy.df$lon), lat=mean(xy.df$lat), zoom=13)



deer.leaflet%>%  
  addLayersControl(
    #baseGroups = c("Two"),
    overlayGroups  = c("Svanoya","AR5","National grid (250m)","Hexagonal grid (250m)"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>% hideGroup("Hexagonal grid (250m)") %>% hideGroup("AR5")


hist(st_area(honeycomb_grid_sf))


###########################################################
#       EUROPE DEM
###########################################################

in.path <- file.path(myVars$WD,myVars$modelName,"DATA/elevation1x1_new.tif")#=ARTIODACTYLA

dem <- raster(in.path)

dem<-raster::aggregate(dem,5)

library(rayshader)


#And convert it to a matrix:
elmat = raster_to_matrix(dem)

#We use another one of rayshader's built-in textures:
elmat %>%
  sphere_shade(texture = "desert") %>%#('imhof1','imhof2','imhof3','imhof4','desert', 'bw', and 'unicorn').
  add_water(detect_water(elmat), color = "black") %>%
  plot_map()


###########################################################
#       IUCN RANGE MAPS
###########################################################

# path<-file.path(myVars$WD,myVars$modelName,"DATA/IUCN/MAMMALS_TERRESTRIAL_ONLY.shp")
# IUCN.sp <- readOGR(path)
# IUCN_CETARTIODACTYLA.sp<-IUCN.sp[IUCN.sp$order_ %in%"CETARTIODACTYLA",]
# out.path <- file.path(myVars$WD,myVars$modelName,"DATA/IUCN_CETARTIODACTYLA.sp.RData")
# save(IUCN_CETARTIODACTYLA.sp, file=out.path)

in.path <- file.path(myVars$WD,myVars$modelName,"DATA/IUCN_CETARTIODACTYLA.sp.RData")#=ARTIODACTYLA
load(in.path)

#---remove non-extant
length(IUCN_CETARTIODACTYLA.sp)
#IUCN_CETARTIODACTYLA.sp<-IUCN_CETARTIODACTYLA.sp[grep("xtant",IUCN_CETARTIODACTYLA.sp$legend),]
IUCN_CETARTIODACTYLA.sp<-IUCN_CETARTIODACTYLA.sp[IUCN_CETARTIODACTYLA.sp$presence%in%c(1),]
length(IUCN_CETARTIODACTYLA.sp)

table(IUCN_CETARTIODACTYLA.sp$family)
temp<-IUCN_CETARTIODACTYLA.sp[IUCN_CETARTIODACTYLA.sp$family%in%c("CERVIDAE",""),]
table(temp$order_)

#plot(temp[2,])



path<-file.path(myVars$WD,myVars$modelName,"DATA/opendatasoft/world-administrative-boundaries.shp")
world.sp <- readOGR(path)
head(world.sp)


path<-file.path(myVars$WD,myVars$modelName,"DATA/EU/europe_coarse.shp")
europe.sp <- readOGR(path)
head(europe.sp)

europe.countries.sp<-aggregate(europe.sp,by="CNTR_CODE")
#plot(europe.countries.sp)

path<-file.path(myVars$WD,myVars$modelName,"DATA/EU/Europe_coastline.shp")
europe.coast.sp <- readOGR(path)
head(europe.coast.sp)
#plot(europe.coast.sp)

europe.coast.sf<-st_as_sf(europe.coast.sp)

europe.coast.sf<-st_simplify(europe.coast.sf, preserveTopology = FALSE, dTolerance = 20000)


ungulates.sf<-st_as_sf(IUCN_CETARTIODACTYLA.sp)

ungulates.sf<-st_transform(
  ungulates.sf,
  st_crs(europe.coast.sf))




xymin<-apply(st_coordinates(europe.coast.sf)[,c("X","Y")],2,min)
xymax<-apply(st_coordinates(europe.coast.sf)[,c("X","Y")],2,max)

ggplot() + 
  geom_sf(data = ungulates.sf,fill="yellow",color=NA,alpha=0.2)+
  geom_sf(data = europe.coast.sf)+
  ylim(xymin["Y"],xymax["Y"]) +  xlim(xymin["X"],xymax["X"])+
  theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "black",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"))#element_blank())
x.diff<-(xymax["X"]-xymin["X"])*0.05
y.diff<-(xymax["Y"]-xymin["Y"])*0.05


i<-2
poly.list<-lapply(1:6,function(i){
  v<-2
  x<-c(xymin["X"]*v, xymin["X"]*v + x.diff)
  y<-c(xymin["Y"]*v*3 + y.diff*(i-1) , xymin["Y"]*v*3 + y.diff*6)
  
  Poly_Coord_df<-data.frame(x=c(x,rev(x)),y=rep(y,each=2))
  
  p<-Polygon(Poly_Coord_df)
  ps = Polygons(list(p),1)
  SpatialPolygons(list(ps))
  
})

poly.sp<-do.call(raster::bind,poly.list)
poly.sf<-st_as_sf(poly.sp,crs = st_crs(ungulates.sf))
poly.sf<-st_set_crs(poly.sf,value = st_crs(ungulates.sf))

xy<-as.data.frame(do.call(rbind,lapply(1:dim(poly.sf)[1],function(i){
  
  #print(st_coordinates(poly.sf[i,])) 
  c(X=max(st_coordinates(poly.sf[i,])[,1]),Y=min(st_coordinates(poly.sf[i,])[,2]))
})))
xy$label<-1:dim(poly.sf)[1]

ggplot() +
  geom_sf(data = poly.sf,size=5,color=NA,fill="yellow",alpha=0.2)+
  
  geom_text(data=xy,aes(x=X+x.diff/4,y=Y+y.diff/2,label=label),colour="white",size=6)+
  geom_text(mapping = aes(x=min(xy$X),y=max(xy$Y*1.1),label="Number of species"),colour="white",size=6)+
  theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "black",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"))#element_blank())



# ggplot() + 
#   geom_sf(data = ungulates.sf,fill="yellow",color=NA,alpha=0.2)+
#   ylim(xymin["Y"],xymax["Y"]) +  xlim(xymin["X"],xymax["X"])+
# theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#       panel.background = element_rect(fill = "black",
#                                       colour = "black",
#                                       size = 0.5, linetype = "solid"))#element_blank())

ggplot() + 
  geom_sf(data = ungulates.sf,fill="yellow",color=NA,alpha=0.2)+
  #geom_sf(data = europe.coast.sf)+
  geom_sf(data = poly.sf,size=5,color=NA,fill="yellow",alpha=0.2)+
  
  geom_text(data=xy,aes(x=X+x.diff/3,y=Y+y.diff/2,label=label),colour="white",size=4)+
  geom_text(mapping = aes(x=min(xy$X),y=max(xy$Y*1.2),label="Number of species"),colour="white",size=4)+
  
  ylim(xymin["Y"],xymax["Y"]) +  xlim(xymin["X"],xymax["X"])+
  
  theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "black",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"))#element_blank())

# fast row bind
library(tibble)
library(dplyr)
library(mapedit)
sf <- rbindlist(poly.list)
single_sf <- dplyr::bind_rows(poly.list)
library(mapedit)
poly.sf<-mapedit::combine_list_of_sf(poly.list)
# back to st
sf <- st_sf(sf)

pt1 = st_point(c(0,1))
pt2 = st_point(c(1,1))
(sfc = st_sfc(pt1, pt2))
d = st_sf(data.frame(a=1:2, geom=sfc))


# ggplot() + 
#   geom_sf(data = ungulates.sf,aes(fill=binomial),color=NA,alpha=0.2)+
#   #geom_sf(data = europe.coast.sf)+
#   #ylim(xymin["Y"],xymax["Y"]) +  xlim(xymin["X"],xymax["X"])+
#   theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_rect(fill = "black",
#                                         colour = "black",
#                                         size = 0.5, linetype = "solid"))#element_blank())


library(fasterize)

e <- as(extent(europe.coast.sf), 'SpatialPolygons')
crs(e) <- crs(europe.coast.sf)
e.sf<-st_as_sf(e)

ungulates.all.sf<-ungulates.sf

e.sf<-st_transform(e.sf,st_crs(ungulates.all.sf))
ungulates.europe.sf <- st_intersection(ungulates.all.sf, e.sf)
ungulates.europe.sf<-st_cast(ungulates.europe.sf, to="MULTIPOLYGON")



ggplot() + 
  geom_sf(data = ungulates.europe.sf,fill="yellow",color=NA,alpha=0.2)+
  #geom_sf(data = europe.coast.sf)+
  geom_sf(data = poly.sf,size=5,color=NA,fill="yellow",alpha=0.2)+
  
  geom_text(data=xy,aes(x=X+x.diff/3,y=Y+y.diff/2,label=label),colour="white",size=4)+
  geom_text(mapping = aes(x=min(xy$X),y=max(xy$Y*1.2),label="Number of species"),colour="white",size=4)+
  
  #ylim(xymin["Y"],xymax["Y"]) +  xlim(xymin["X"],xymax["X"])+
  
  theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "black",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"))#element_blank())



r.template<-raster(ungulates.europe.sf,resolution=5000)

species.list<-unique(ungulates.europe.sf$binomial)
sf.list<-lapply(species.list,function(x)ungulates.europe.sf[ungulates.europe.sf$binomial%in%x,])

x<-sf.list[[1]]
r.list<-lapply(sf.list,function(x){
  out<-fasterize(x,r.template)
  out[is.na(out)]<-0
  return(out)
})

# e <- as(extent(europe.coast.sf), 'SpatialPolygons')
# crs(e) <- crs(europe.coast.sf)
# 
# europe.r.list<-lapply(r.list,function(x){
#   crop(x, e)
# })
#r <- crop(test_spdf, e)

#ungulates.europe.df <- as.data.frame(r)
#colnames(ungulates.europe.df) <- c("value", "x", "y")



r.brick<-brick(r.list)

sum.r <- calc(r.brick, fun=sum,na.rm=TRUE)

image(sum.r)

test_spdf <- as(sum.r, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")



# e <- as(extent(europe.coast.sf), 'SpatialPolygons')
# crs(e) <- crs(europe.coast.sf)
# r <- crop(test_spdf, e)
# ungulates.europe.df <- as.data.frame(r)
# colnames(ungulates.europe.df) <- c("value", "x", "y")



ggplot() + 
  geom_tile(data = test_df,aes(x=x,y=y,alpha=value),fill="yellow",color=NA)+
  #geom_sf(data = europe.coast.sf)+
  #ylim(xymin["Y"],xymax["Y"]) +  xlim(xymin["X"],xymax["X"])+
  scale_alpha_continuous(range = c(0,1))+#, breaks = c(0,0.25,0.50,1))+
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  theme(legend.position = c(0.2,0.8),
        legend.background = element_rect(fill="transparent"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "black",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"))#element_blank())#legend.position="none", 



range(test_df$y)
range(test_df$x)

#---CRASHES R:
# r<-fasterize(
#    ungulates.sf,r.template,fun = sum,by = "binomial")

# country.sp <- gUnaryUnion(europe.sp, id = europe.sp@data$ CNTR_CODE)
# plot(country.sp)
# 
# #output = st_intersection(input, clip)
# 
# 
# ggplot() + 
#   geom_sf(data = ungulates.sf,aes(fill = binomial,color=binomial),alpha=0.3)+
#   ggtitle("Habitat")+
#   theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank())
# 
# 
# 


###########################################################
#       FLAT MAP OF EUROPE
###########################################################


world.sp

world.sf<-st_as_sf(world.sp)

world.sf<-st_transform(
  world.sf,
  st_crs(europe.coast.sf))



xymin<-apply(st_coordinates(europe.coast.sf)[,c("X","Y")],2,min)
xymax<-apply(st_coordinates(europe.coast.sf)[,c("X","Y")],2,max)

europe.map<-ggplot() + 
  geom_sf(data = world.sf,fill="yellow",color="yellow",alpha=.9,size=0)+
  #geom_sf(data = europe.coast.sf)+
  ylim(xymin["Y"],xymax["Y"]) +  xlim(xymin["X"],xymax["X"])+
  theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "black",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"))#element_blank())


fig.path<-file.path(myVars$WD,myVars$modelName,"FIGURES/FIG_map europe.svg")

graphics.off()
svg(file= fig.path,
    width = 20, height = 15, pointsize = 12, bg="black")

europe.map

graphics.off()