# Gör korrelationsmatriser och beräknar vif-värden
library(usdm)
library(corrplot)
library(Hmisc)
library(readxl)
library(tidyverse)

data <- read_xlsx(path="Excelfiler UT/Vindelfjällen lyvariabler utan kulldata.xlsx") #det räcker med den korta datafilen.
colnames(data)
data<-as.data.frame(data)
#plockar ut de numeriska datakolumnerna
variabler <- data %>%
  dplyr::select(distans_till_vatten, distans_till_skog, area_myr, area_vatten, medelvärde_lämmelprediktion_uppgångsår)
vif(variabler) #distans_till_skog och medelvärde_lämmelprediktion_uppgångsår är strax över 2
vifstep(variabler, th =10)# det här testet säger att inga variabler har problem med kollinearitet. Dock är lämmeltäthet och distans till skog ganska kollinerade. Se nedan.

pvars <- c("distans_till_vatten", "distans_till_skog", "area_myr", "area_vatten", 
           "medelvärde_lämmelprediktion_uppgångsår")
#testar skalade värden
variabler_skalad <- variabler
variabler_skalad[pvars] <- lapply(variabler_skalad[pvars],scale)
res<-cor(variabler_skalad, method = c("pearson", "kendall", "spearman"))
round(res, 2)
library("Hmisc") # ger p-värden för korrelationsmatris
res2 <- rcorr(as.matrix(variabler))
res2
res2$r
res2$P

symnum(res, abbr.colnames = FALSE)
library("corrplot")


# Insignificant correlations display p-value (insig = "p-value")
par(mfrow=c(1,1))
corrplot(res2$r, method = "number", type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "pch" , tl.cex = 0.7,
         title = "Variable Correlation matrix, Vindelfjällen", 
         mar = c(1,0,3,0), tl.col = "black",
         tl.srt = 20)
mtext("Insignificant correlations are crossed (p > 0.05)", side=3)

#Jävla drygt. Avstånd till skog och lämmeltäthet är korrelerade.
modell <- lm(medelvärde_lämmelprediktion_uppgångsår~distans_till_skog, 
             data = variabler)
dev.off() # återställer plotzoom och plotfönsterinställningar till default
plot.new()
plot(medelvärde_lämmelprediktion_uppgångsår~distans_till_skog, 
     data = variabler, ylab = "lämmeltäthet", xlab = "distans till skog (m)", 
     main = "Lämmeltäthet ökar med ökat avstånd till skog i Vindelfjällen",
     col = "red")
abline(modell)
anova(modell)
modsum <- summary(modell)
modsum
r2 = modsum$adj.r.squared
p.värde = modsum$coefficients[2,4]
p.värde
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(y = 0.3, x =1000, labels = mylabel) #koordinaterna här för placering. Koordinaterna bestäms av Y och X axeln i plotten och är därför olika varje gång man plottar.

mylabel2 = bquote(italic(P) == .(format(p.värde, digits = 3)))
text(y = 0.25, x = 1150, labels = mylabel2)

# Hur såg Helags ut?
data_helags <- read_xlsx(path=file.choose()) #för krånglig sökväg. Filen är "kärnlyor Helags AIC.xlsx" som finns i Den and territory selection i Masterarbete
colnames(data_helags)
modell_helags <- lm(medelvärde_lämmelprediktion_uppgångsår~distans_till_skog, 
                    data = data_helags)

dev.off()
plot(medelvärde_lämmelprediktion_uppgångsår~distans_till_skog, 
     data = data_helags, xlab = "distans till skog (m)", ylab = "lämmeltäthet", 
     main = "Lämmeltäthet ökar med ökat avstånd till skog i Helags",
     col = "red")
abline(modell_helags)

anova(modell_helags) # Lika signifikant här
modsum_helags <- summary(modell_helags)
modsum_helags

r2_helags = modsum_helags$adj.r.squared
p.värde_helags = modsum_helags$coefficients[2,4]

mylabel_helags = bquote(italic(R)^2 == .(format(r2_helags, digits = 3)))
text(x = 1000, y =0.35, labels = mylabel_helags) #koordinaterna här för placering. Koordinaterna bestäms av Y och X axeln i plotten och är därför olika varje gång man plottar.

mylabel_helags2 = bquote(italic(P) == .(format(p.värde_helags, digits = 2)))
text(x = 1100, y = 0.3, labels = mylabel_helags2)


