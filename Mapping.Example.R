#Libraries

library(landscapeR) #only need for generating the landscape raster
library(raster)
library(terra)
library(dplyr)
library(leaflet) #needed for masking
library(sf)
library(knitr)
library(ggplot2)


#Set WD & upload Rasters
#setwd("")

#Sungei Puaka Raster
Puaka<-"Polygons_SungeiPuaka_Rast.tif"
Puaka_raster=raster(Puaka)
```

#Plot Puaka
```{r}
Puaka.R <- rast(Puaka)
sources(Puaka.R)
hasValues(Puaka.R)
## [1] TRUE
plot(Puaka.R, main='Sungei Puaka')

# make distance rasters ----

#Puaka
# make raster with NA for non mud (mangrove class 1 into NA)
Puaka.R.mud = classify(Puaka.R, cbind(1,NA))
# make raster with NA for non mangrove (mud class 2 into NA)
Puaka.R.mang = classify(Puaka.R, cbind(2,NA))

# calculate distance with terra::distance
#Puaka
# by making NA values it fills in the NA with distance to cells with values
# *These take a while. Once run they are saves to WD and pulled from there

#Puaka
# make distance from mud 
PuakaR.mud_dist = distance(Puaka.R.mud)
# make distance from mangrove 
PuakaR.mang_dist = distance(Puaka.R.mang)

#Combine Puaka
# combine into 1 with distance from mangrove edge
# positive values are in mangrove, negative are in water
Puaka.R.edge_dist = PuakaR.mud_dist - PuakaR.mang_dist
plot(Puaka.R.edge_dist)
#save file
writeRaster(Puaka.R.edge_dist, "Puaka.R.edge_dist", filetype = "GTiff", overwrite = TRUE)

#Just use this after writing
Puaka.R.edge_dist <-"Puaka.R.edge_dist"
Puaka.R.edge_dist=raster(Puaka.R.edge_dist)
plot(Puaka.R.edge_dist)

#Trim the raster using the shapefile from each location
```{r}
#Upload Shapefile
SP  <- st_read('yourshapefile.shp')

#remove elevation values
SP <- st_zm(SP)

#Reproject the shapefiles to match the raster CRS (Coordinate Reference System)
SP <- st_transform(SP, crs(Puaka.R.edge_dist))

#View them
plot(SP)


#Mask the raster using a polygon (.shp) 
# Crop the raster to the extent of the shapefile
Puaka.cropped_raster <- crop(Puaka.R.edge_dist, SP)
# Mask the cropped raster with the shapefile
Puaka.R.edg_dist.mask <- mask(Puaka.cropped_raster, SP)
#view masked raster
plot(Puaka.R.edg_dist.mask)

#Assign values for Mangrove/Seagrass vs Mudflat/Tidal Flat and then come up with total per each location
```{r}
#Use original values and assign values based on "code"
library(tidyverse)

#Convert regular binary to datafame
Puaka.R.df<- as.data.frame(Puaka.R, xy = TRUE)

#Convert distance based to datafame
Puaka.R.edge_dist.df<-as.data.frame(Puaka.R.edg_dist.mask, xy = TRUE)

#Remove NAs
#load tidyr package
library(tidyr)
#remove all rows with a missing value in any column
Puaka.R.edge_dist.df1 <- Puaka.R.edge_dist.df %>% drop_na()

#Define function for mapping 
fun.MF.PU.Sig<- function(x){yourfunction))
}

#Assign C stock values and put in DFs

#Carbon Stock Values/Functions
#Static kg/m2
PMangC= your value
PMudC=your value


#Functions from sigmoidal regressions
Puaka.C.fun <- fun.MF.PU.Sig

```

```{r}
#For Simple Model add habitat names
Puaka.R.edge_dist.df1$Habitat <- ifelse(Puaka.R.edge_dist.df1$Name >= "0", "Mudflat", "Mangrove")

#Calculate new columns
Puaka.R.edge_dist.df1$SimpleC <- ifelse(Puaka.R.edge_dist.df1$Habitat == "Mangrove", PMangC, PMudC) #If Mangrove then use value one, If not then use value 2

#For Ecotone Model
#Rename Distance for ecotone values
Puaka.R.edge_dist.df1$Puaka.R.edge_dist <-Puaka.R.edge_dist.df1$Name

#Values within model
Puaka.R.edge_dist.df1$EcotoneC <- Puaka.C.fun(Puaka.R.edge_dist.df1$Puaka.R.edge_dist)
#Values outside of model(Need to pull Averages)
Puaka.R.edge_dist.df1$EcotoneC[which(Puaka.R.edge_dist.df1$Puaka.R.edge_dist < -32)] = PMangC
Puaka.R.edge_dist.df1$EcotoneC[which(Puaka.R.edge_dist.df1$Puaka.R.edge_dist > 22)] = PMudC

#Calculate a differences
```{r}
#Difference between Ecotone C compared to Simple Estimate
Puaka.R.edge_dist.df1$CDiff <- Puaka.R.edge_dist.df1$SimpleC-Puaka.R.edge_dist.df1$EcotoneC


#Map All features for habitat, simple C, and C based on distance-Puaka

library(ggplot2)
#Map Puaka
Puaka.Hab<-ggplot()+
  theme_bw() +
  ylab("lat")+
  xlab("long")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+#Removes gray gridline
  #theme(legend.position = "none") + #removes legend
  labs(fill="Type")+
  # theme(legend.position=c(0.1,.7))+
  # theme(legend.position = "none") + #removes legend
  geom_raster(data=Puaka.R.edge_dist.df1, aes(x,y,fill=Habitat))+
  scale_fill_manual(values=c("#009900", "tan4"))+
  geom_sf(data=SP, aes(), fill=NA, color=NA)+
  scale_y_continuous(labels=identity)+
  scale_x_continuous(labels=identity, limits=c(lat,long),
                     breaks=c(latbreaks,longbreaks)) 

print(Puaka.Hab)
#Map Puaka C simple
Puaka.SimpleC<-ggplot()+
  theme_bw() +
  ylab("lat")+
  xlab("long")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+#Removes gray gridline
  labs(fill="kgC/m2")+
  geom_raster(data=Puaka.R.edge_dist.df1, aes(x,y,fill=SimpleC))+
  scale_fill_gradient(low = "chartreuse", high = "darkgreen")+
  geom_sf(data=SP, aes(), fill=NA, color=NA)+
  scale_y_continuous(labels=identity)+
  scale_x_continuous(labels=identity, limits=c(lat,long),
                     breaks=c(latbreaks,longbreaks)) 
print(Puaka.SimpleC)

#Map Puaka C w fun
Puaka.EcotoneC<-ggplot()+
  theme_bw() +
  ylab("lat")+
  xlab("long")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+#Removes gray gridline
  labs(fill="kgC/m2")+
  # theme(legend.position=c(0.1,.7))+
  #theme(legend.position = "none") + #removes legend
  geom_raster(data=Puaka.R.edge_dist.df1, aes(x,y,fill=EcotoneC))+
  scale_fill_gradient(low = "chartreuse", high = "darkgreen")+
  geom_sf(data=SP, aes(), fill=NA, color=NA)+#add .shp file
  scale_y_continuous(labels=identity)+ #Adjust x and y labels and x limits/breaks
  scale_x_continuous(labels=identity, limits=c(lat,long),
                     breaks=c(latbreaks,longbreaks)) 
print(Puaka.EcotoneC)

#rescale values to determine what to put for scale_fill_gradientn values
library(scales)
Puaka.R.edge_dist.df1$CDiff_rescaled <- rescale(Puaka.R.edge_dist.df1$CDiff)

#Outline of SP
OutlineSP<-ggplot(SP)+
  geom_sf(fill=NA, color="black")

#Map of differences
Puaka.CDiff<-ggplot(data=Puaka.R.edge_dist.df1)+
  theme_bw() +
  ylab("lat")+
  xlab("long")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+#Removes gray gridline
  labs(fill="kgC/m2")+ #Add label
  # theme(legend.position = "none") + #removes legend
  geom_raster(data=Puaka.R.edge_dist.df1, aes(x,y,fill=CDiff))+ #Add raster with CDiff fill
  scale_fill_gradientn(colors = c("red3", "white", "blue4"), values= c(0,0.5938571,1))+ #Adjsut color scale with 3 colors and then values with rescaled value previously looked up to see what 0 corresponds to zero value see above with CDiff_Rescaled
  geom_sf(data=SP, aes(), fill=NA, color="black")+ #Add .shp file
  scale_y_continuous(labels=identity)+ #Adjust x and y labels and x limits/breaks
  scale_x_continuous(labels=identity, limits=c(lat,long),
                     breaks=c(latbreaks,longbreaks)) 

print(Puaka.CDiff)


#sum of all mangrove/seagrass for each location
#sum of all mudflat/tidal flat for each location
#sum of entire location

#Assign values based on location and then come up with total per each location
```{r}
#Libraries
library(tidyr)
library(dplyr)
library(data.table)

#sum of all mangrove/seagrass for each location
PuakaSumsSimple<-aggregate(SimpleC ~ Habitat, data=Puaka.R.edge_dist.df1, sum)
PuakaSumsEcotone<-aggregate(EcotoneC ~ Habitat, data=Puaka.R.edge_dist.df1, sum)

#Join dfs and then total
PuakaSumsAll <- merge(x =PuakaSumsSimple, y = PuakaSumsEcotone, by = "Habitat")

#Transpose
PuakaSumsAll1 <- setNames(data.frame(t(PuakaSumsAll[ , - 1])), PuakaSumsAll[ , 1])  # Transpose data

#Total C
PuakaSumsAll1$Total <- rowSums(PuakaSumsAll1, na.rm=TRUE)

#Convert from kg/m2 to Mg/ha
#1000 kg per 1 Mg
#Area is corrected for already
PuakaSumsAll1$MangroveMg = PuakaSumsAll1$Mangrove/1000
PuakaSumsAll1$MudflatMg = PuakaSumsAll1$Mudflat/1000
PuakaSumsAll1$TotalMg = PuakaSumsAll1$Total/1000

#reorganize in excel
```

```{r}
install.packages("readxl")
library(readxl)
#pull totals from .csv
SumsAll <- read_excel("yourexcelfiles.xlsx")
SumsPuaka <- subset(SumsAll, SumsAll$Location == "Sungei Puaka")
```

#graph each
SumsPuakaFig <- ggplot(data=SumsPuaka, aes(x = factor(Type, level =c('Simple', 'Ecotone')), y = MgC, fill = Habitat))+
  ylim(0,5000) + #y-axis values
  xlab("Estimate Type")+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+#Removes gray gridline
  theme(legend.position = "none") + #removes legend
  geom_bar(position='stack', stat = 'identity')+
  scale_fill_manual('Habitat', values = c('green3', 'tan4'))
print(SumsPuakaFig)




