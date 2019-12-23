#Andel myr och vatten runt lyor Vindelfjällen
install.packages("sf")
install.packages("vegan")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("dismo")
install.packages("OpenStreetMap")
install.packages("mapview")


library(dismo) #har bara lekt med de fyra översta paketen
library(vegan)
library(rnaturalearth)
library("OpenStreetMap")
library(mapview)
library(sf)
library(rgeos)
library("sp")
library("rgdal")
library(tidyverse)
library(maptools)
library(raster)
library(writexl)
library(readxl)
library(ggplot2)


# raden nedan gör så att R kan läsa åäö
Sys.setlocale(locale='en_US.UTF-8'); 





Vindellyor <- read_xlsx(path = "lyor/Lyor Vindelfjällen Sweref riktig.xlsx") #läser in filen lybuffer.shp
View(Vindellyor) #åäö funkar
coordinates(Vindellyor) <- c("E", "N")
proj4string(Vindellyor) <- CRS("+init=EPSG:3006")
summary(Vindellyor) #sweref och projected
plot(Vindellyor)

hej


Vindellyor_buffer <- gBuffer(Vindellyor, width = 1500,  byid = TRUE) # gör en 1500 m buffer runt lyor. byid = TRUE gör att det blir en buffer runt varje lya, inte en enda stor

summary(Vindellyor_buffer) #sweref och projected


plot(Vindellyor_buffer, col = "red")
plot(Vindellyor, add = TRUE, pch = 10, cex=0.5 ) # pch = 10 gör symbolen till en rund punkt



myrar <- readOGR(dsn = "./Geodata Vindeln/shaper klara/Myr.shp", layer = "Myr", stringsAsFactors = FALSE) #viktigt med stringsAsFactors = FALSE. Annars blir det fel när man ändrar siffror till numeric.
summary(myrar) # den är Sweref men av någon anledning tror R att det är ett annat koordinatsystem. antagligen för att det inte heter EPSG:3006 när det importeras som shape. När man klipper med intersect nedan blir det en varning om man inte ändrar här.
sweref <- CRS("+init=EPSG:3006")
myrar <- spTransform(myrar, sweref) #sweref
summary(myrar)
plot(myrar)
plot(Vindellyor_buffer, col = "red", add = TRUE)

#clip
myr_runt_lyor <- raster::intersect(Vindellyor_buffer, myrar) #inget varningsmeddelande
summary(myr_runt_lyor)

plot(Vindellyor_buffer, border = "red")
plot(myr_runt_lyor, add = TRUE)
plot(Vindellyor, pch = 10, cex = 0.1, col = "green", add = TRUE)

##### Uträkning myr-area
#' räknar ut arean av myr för varje feature som ligger under respektive
#'  lybuffer-id. 
myr_runt_lyor$Ny_area <- rgeos::gArea(myr_runt_lyor, byid = TRUE) #gArea är bra, räknar bort arean av eventuella hål i en shapefil.

myr_runt_lyor <- as.data.frame(myr_runt_lyor)

View(myr_runt_lyor) # åäö funkar fint

#' jag har bara arean av respektive myr-feature. Nu måste jag
#' gruppera per lya och summera ihop för att få totalarean av myr
#' per lya. Summarise droppar alla kolumner förutom grupperingskolumnen
#' och den nya uträknade kolumnen. Det gör dock inget eftersom jag
#' ska smacka ihop den här med Vindellyor-ramen sen.
myr_area <- myr_runt_lyor %>% 
  group_by(Namn) %>% 
  summarise(area_myr = sum(Ny_area))


head(myr_area) #klart


nrow(myr_area) #alla lyor är inte med här eftersom de som inte har någon area följer med. Endast 79 är med


nrow(Vindellyor) #totalt har jag 91 lyor i Vindeln. Tomma lyor kommer läggas till i slutet när alla dataramar smackas ihop.



###### Samma sak fast med vatten######

#Nu får jag omvandla dataramen Vindellyor till en SpatialPolygonsDataFrame igen


vatten <- readOGR(dsn = "./Geodata Vindeln/shaper klara/Vatten.shp", layer = "Vatten", stringsAsFactors = FALSE) #viktigt med stringsAsFactors = FALSE. Annars blir det fel när man ändrar siffror till numeric.
summary(vatten)
vatten <- spTransform(vatten, sweref)

plot(vatten, border = "royalblue1", col = "royalblue1")
plot(Vindellyor_buffer, border = "red", add = TRUE)


# Clip
vatten_runt_lyor <- raster::intersect(vatten, Vindellyor_buffer)

#' Tittar lite på hur buffrarna ligger


plot.new() # telling R we are starting a new plot
plot(vatten_runt_lyor,  border = "royalblue1", col = "royalblue1", add = TRUE)
plot(Vindellyor_buffer, border = "red", add = TRUE)
pointLabel(coordinates(Vindellyor),labels=Vindellyor$Namn, cex = 0.6) #pointlabels är en funktion i maptools

plot.new() # telling R we are starting a new plot
mapview(Vindellyor_buffer) + vatten_runt_lyor # coolt paket!

#' Många buffrar överlapper.
#' kollar så att klipp (intersect) klippte ut myr under
#' varje buffer och att alla fick den myr som faktiskt faller under
#' bufferten, så att inte den "översta" bufferten får allt 
plot.new() # telling R we are starting a new plot
plot(subset(vatten_runt_lyor, Namn == "FSAC058"), col = "red", border = "red")
plot(subset(vatten_runt_lyor, Namn == "FSAC059"), add = TRUE, col = adjustcolor("yellow", alpha.f = 0.5), border = adjustcolor("yellow", alpha.f = 0.5))
plot(subset(Vindellyor_buffer, Namn == "FSAC058"), add = TRUE, border = "blue")
plot(subset(Vindellyor_buffer, Namn == "FSAC059"), add = TRUE, border = "blue")
#det verkar stämma fint

#Räknar ut area
vatten_runt_lyor$Ny_area <- gArea(vatten_runt_lyor)

vatten_runt_lyor <- as.data.frame(vatten_runt_lyor)

vatten_area <- vatten_runt_lyor %>% 
  group_by(Namn) %>% 
  summarise(area_vatten = sum(Ny_area))






###### Tar och lägger till avstånd till vatten och skog också ####


skog <- readOGR(dsn = "./Geodata Vindeln/shaper klara/Skog.shp", layer = "Skog", stringsAsFactors = FALSE) #viktigt med stringsAsFactors = FALSE. Annars blir det fel när man ändrar siffror till numeric.
summary(skog)
skog <- spTransform(skog, sweref)

plot.new()

plot(skog, col = "green", border = "green")
points(Vindellyor, col = "red", pch =".", cex = 3)

#' kollar lite noggrannare för att inspektera om det finns små trädsamlingar
#' som inte sitter ihop med kontinuerlig skog

mapview(skog) + Vindellyor # öppnas i Safari av någon anledning, men det funkar

#räknar ut avstånd till vatten och skog
dist_vatten<-apply(gDistance(Vindellyor, vatten,byid=TRUE),2,min)
View(dist_vatten) #lynamnen kommer inte med men lyorna verkar ligga i samma ordning som i "lyor"-data frame#####


dist_vatten<-as.data.frame(dist_vatten) #gör om från Spatial till vanlig data frame####
colnames(dist_vatten) <- ("distans_till_vatten") #lägger till kolumnnamn


dist_skog<-apply(gDistance(Vindellyor, skog,byid=TRUE),2,min) #små trädplättar på kalfjället borttagna. Betyder "2" att bara andra värdet sparas? Första värdet är avståndet till sig själv, och det är noll.
View(dist_skog)
dist_skog<-as.data.frame(dist_skog)
colnames(dist_skog) <-("distans_till_skog")

##### Lägger in lämmeldata också ####
#'jag vill ha ut sannolikheten för lämmel under uppgångsår 
#'i varje pixel inom en 1500 meter radie runt lyan.
#'förklaring här: https://www.neonscience.org/extract-values-rasters-r

lemmel_uppgång <- raster("Gnagardata/Lämmelprediktion uppgångsår.tif")
lemmel_uppgång@crs #kollar koordinatsystem. Den är sweref

medelvärde_uppgång <- extract(lemmel_uppgång,             # raster layer
                              Vindellyor,   # SPDF with centroids for buffer
                              buffer = 1500,     # buffer size, units depend on CRS
                              fun=mean,         # what value to extract. 
                              df=TRUE)         # return a dataframe? 


View(medelvärde_uppgång) # Namn försvinner och ersätts av en ID-kolumn, men det är lätt fixat! Enligt tutorial så ska det här gå bra:

medelvärde_uppgång$Namn <- Vindellyor$Namn
colnames(medelvärde_uppgång) # knasiga namn och ordning på kolumner

medelvärde_uppgång <- medelvärde_uppgång %>% 
  dplyr::select(Namn, medelvärde_lämmelprediktion_uppgångsår = La.mmelprediktion_uppga.ngsa.r) # man kan byta namn med select!
  
View(medelvärde_uppgång)
summary(medelvärde_uppgång)

#' Gör ett stickprov för säkerhets skull
#' för att se att det blir rätt medelvärde per lya
x <- subset(Vindellyor, Namn == "FSAC078")
test <- extract(lemmel_uppgång,             # raster layer
                              x,   # SPDF with centroids for buffer
                              buffer = 1500,     # buffer size, units depend on CRS
                              fun=mean,         # what value to extract. 
                              df=TRUE)         # return a dataframe? 
test #stämmer!

# Och slutligen smackar vi ihop alla kolumner
Vindellyor_ram <- as.data.frame(Vindellyor) #gör om från Spatial till vanlig data frame
summary(Vindellyor_ram) #character blir faktor ibland av någon anledning. Dock inte nu


Vindellyor_klar<-Vindellyor_ram %>% 
  bind_cols(dist_vatten, dist_skog) %>%   #lägger till distans till datasetet
  left_join(myr_area, by = "Namn") %>% 
  left_join(vatten_area, by = "Namn") %>% 
  left_join(medelvärde_uppgång, by = "Namn")


View(Vindellyor_klar) #klart!

write_xlsx(Vindellyor_klar, path = "Excelfiler UT/Vindelfjällen lyvariabler utan kulldata.xlsx")






