#Gör ett långt excelark med en lya per år. Lägger även in fas

# raden nedan gör så att R kan läsa åäö
Sys.setlocale(locale='en_US.UTF-8'); 

library(tidyverse)
library(readxl)
library(writexl)
Vindellyor<-read_xlsx(path = "Excelfiler UT/Vindelfjällen lyvariabler utan kulldata.xlsx")
kullar <- read_xls(path = "kulldata/Antal fjällrävskullar 1972-2017_Vindelfjällen_RE.xls", sheet = 2, col_names = FALSE) #xls-fil
gnagarfaser <- read_xlsx(path = "Gnagardata/gnagarfaser.xlsx") # den här har jag klippt ut från ”komplexa kullar Helags - genetiskt och observationer.csv” från Rasmus. Jag har lagt till 2009 som ett lågår. Faserna gäller egentligen Helags så dessa kommer kanske att ändras.

nrow(Vindellyor)
View(kullar)
colnames(kullar) <- c("År", "Namn", "Antal_valpar", "Notering") # lägger till kolumnamn då detta saknades



#Ändrar om till ett system med tre gnagarfaser
gnagarfaser$Fas[gnagarfaser$Fas == "4"| gnagarfaser$Fas == "1"] <- "low"
gnagarfaser$Fas[gnagarfaser$Fas == "2"] <- "increase"
gnagarfaser$Fas[gnagarfaser$Fas == "3"] <- "peak"



View(gnagarfaser)
år_ram <- as.data.frame(2000:2017)

colnames(år_ram) <- "År"

# replikerar lyorna 18 gånger, det vill säga alla år med kull (2000 - 2017)
lyor_lång <- bind_rows(replicate(length(år_ram$År), Vindellyor,simplify = FALSE))
View(lyor_lång)

# replikerar alla år lika många gånger som det finns lyor
År_multi <- as.data.frame(år_ram[rep(seq_len(nrow(år_ram)), each= length(Vindellyor$Namn)), ])
nrow(År_multi) #1638 rader
nrow(lyor_lång)#1638 rader

colnames(År_multi) <- "År"

# lägger ihop med lyorna. Viktigt att de har samma längd eftersom bind_cols går på position
colnames(lyor_lång)

lyor_lång<-lyor_lång %>% 
  bind_cols(År_multi) %>% 
  dplyr::select(Namn, Kommentar, År, N:medelvärde_lämmelprediktion_uppgångsår) #byter ordning


#lägger på faser
lyor_lång_klar <- lyor_lång %>% 
  left_join(gnagarfaser, by = "År") %>%
  dplyr::select(Namn, Kommentar, År, Fas, N:medelvärde_lämmelprediktion_uppgångsår) #byter ordning
  
View(lyor_lång_klar)

#' Så ska vi lägga på kullarna också. Det behövs ett Obs_ID för att kunna 
#' smacka ihop dataramarna på rätt sätt
colnames(kullar)
kullar <- kullar %>% 
  mutate(Kull = replace(Antal_valpar, !is.na(Antal_valpar), 1)) %>%  #om man kör bara mutate(Kull= !is.na(Antal_valpar)) blir det TRUE i kolumnen. Jag vill hellre ha 1 för kull efter
  unite(Obs_ID, Namn, År, sep = "-") %>% 
  dplyr::select(Obs_ID, Kull)


lyor_lång_klar <- lyor_lång_klar %>% 
  unite(Obs_ID, Namn, År, sep = "-", remove = FALSE)

View(lyor_lång_klar) 
View(kullar) # nu har de samma Obs_ID.

#smackar ihop och byter NAs mot 0 samt byter kolumnordning
colnames(lyor_lång_klar)
lyor_lång_klar<- lyor_lång_klar %>%
  left_join(kullar, by = "Obs_ID") %>% 
  mutate(Kull = replace(Kull, is.na(Kull), 0)) %>%  #byter ut NA mot 0
  mutate(area_myr = replace(area_myr, is.na(area_myr), 0)) %>%  #byter ut NA mot 0
  dplyr::select(Obs_ID:E, Kull, distans_till_vatten:medelvärde_lämmelprediktion_uppgångsår)

View(lyor_lång_klar)

length(lyor_lång_klar$Kull[lyor_lång_klar$Kull== 1]) #45
length(kullar$Kull)#48 det är tre kullar till här

kärnkullar <-lyor_lång_klar %>% 
  filter(Kull == 1)
  

kullar[!kullar$Obs_ID %in% kärnkullar$Obs_ID, ] # FSAC145 (en kull) och FSAC143 (två kullar) är inte med

#Var ligger de kullarna?
library(sp)
library(mapview)

alla_lyor<-read_xlsx(path = "lyor/Lyor Sverige Garmin.xlsx")
coordinates(alla_lyor) <- c("X", "Y")
proj4string(alla_lyor) <- CRS("+init=EPSG:3006")

#Världens coolaste paket:
plot.new()
karta<-mapview(subset(alla_lyor, Namn == "FSAC145"| Namn =="FSAC143"), color = "red", label=alla_lyor$Namn, layer.name = "Udda lyor", map.types = "OpenTopoMap") +
  mapview(Vindellyor, label=Vindellyor$Namn) #label gör att lynamn syns när man för musen över featuren i kartan
  

karta
#Printar en interaktiv HTML-karta och en kartbild av de udda lyorna och vindellyorna
mapshot(karta, url = paste0(getwd(), "/map.html"),
        file = paste0(getwd(), "/karta.png"))


write_xlsx(lyor_lång_klar, path = "Excelfiler UT/AIC kullar Vindelfjällen 2000-2017.xlsx")


