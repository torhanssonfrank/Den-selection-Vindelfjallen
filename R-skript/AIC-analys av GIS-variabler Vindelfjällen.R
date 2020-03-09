##### AIC-analys av GIS-variabler Vindelfjällen ######
# raden nedan gör så att R kan läsa åäö
Sys.setlocale(locale='en_US.UTF-8'); 

library(data.table)
library(lme4)
library(lubridate)
library(readxl)
library(writexl)
library(MASS)
library(car)
library(tidyverse)
library(MuMIn) # model averaging
library(glmulti) # model averaging med GLM
library(rJava) # om det inte funkar att installera eller starta, installera Java Development kit. Se den här tråden https://github.com/rstudio/rstudio/issues/2254
library(unmarked) # Används i instruktionerna från Rasmus . gör också model averaging och ger en hel del statistik i outputen. 
library('devtools')
library(visreg)
library(sjPlot) # diagnostic plots for linear models
library(arm)
library(car)

dens<-read_xlsx(path = "Excelfiler UT/AIC kullar Vindelfjällen 2000-2017.xlsx")
dens<-as.data.frame(dens)
summary(dens)
View(dens)

# kollar outliers
hist(dens$medelvärde_lämmelprediktion_uppgångsår, breaks = 100) # flera parametrar verkar inte vara normalfördelade
hist(dens$area_myr, breaks = 100) # outlier
which((dens$area_myr==max(dens$area_myr)))
hist(dens$area_vatten, breaks = 100) # outlier
hist(dens$distans_till_vatten, breaks = 100) #outliers
hist(dens$distans_till_skog, breaks = 100)

par(mfrow=c(1,2))


qqPlot(dens$medelvärde_lämmelprediktion_uppgångsår)
qqPlot(dens$medelvärde_lämmelprediktion_uppgångsår, "lnorm")# inte så mycket bättre
mlu.log <- log(dens$medelvärde_lämmelprediktion_uppgångsår)
qqPlot(mlu.log) # inte så mycket bättre


plot.new()
qqPlot(dens$area_myr)
qqPlot(dens$area_myr, "lnorm") # ingen förbättring
am.log <- log((dens$area_myr+1))
qqPlot(am.log)#ingen förbättring

plot.new()
qqPlot(dens$area_vatten)
qqPlot(dens$area_vatten, "lnorm") # aningen bättre men dålig
av.log <- log(dens$area_vatten)
qqPlot(av.log)# aningen bättre


qqPlot(dens$distans_till_vatten)
qqPlot(dens$distans_till_vatten, "lnorm") #dålig
dtv.log <- log(dens$distans_till_vatten)
qqPlot(dtv.log)  # aningen bättre


qqPlot(dens$distans_till_skog) # hyfsad men dålig fit i lägre och högre kvantiler
qqPlot(dens$distans_till_skog, "lnorm") # sämre
dts.log <- log(dens$distans_till_skog)
qqPlot(dts.log)# plottar inte

#' Min responsvariabel är  binär (antingen Kull eller ingen Kull). Då kan man inte använda
#' linear model eller penalized quasi likelihood (PQL)
#' mina förklarande variabler är inte normalfördelade. 
#' Mina residualer är alltså inte normalfördelade.
#'  Därför är det bäst att använda en generalised linear model med mixed effects. Om
#'  man har 5 eller fler random variabler ska man använda Monte Carlo algorithms (MCMC).
#'  Jag kommer bara köra på lya (Namn) som random. Kommer även testa år.
#'  Det verkar inte som att man anger random variabler på samma sätt i AIC-analyser som i övriga linjära analyser.
#'  Jag tror i alla fall att jag klarar mig med glmer() istället för MCMC (den är baserad på Bayesian likelihood 
#'  så då kanske det inte funkar med AIC? BIC kanske funkar i så fall.)
#'  En bra guide finns här: https://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html

dens.sub <- dens %>% 
  dplyr::select(-N, -E)

#Finns det NAs?
which(is.na(dens.sub), arr.ind = TRUE  ) #Nej

dens.sub$Namn <- as.factor(dens.sub$Namn) #måste vara factor för att kunna analyseras
dens.sub$År <- as.factor(dens.sub$År)
dens.sub$Fas <- as.factor(dens.sub$Fas)
class(dens.sub$Kull)
dens.sub$Kull <- as.numeric(dens.sub$Kull)
names(dens.sub)
rownames(dens.sub) <- NULL

length(dens.sub$Kull[dens.sub$Kull==1]) # Det finns bara 45 kullar totalt

## *************GENERALISED LINEAR MODELS - MULTIMODEL AVERAGING***************** ####


#' får varning att variablerna är på för olika skala. Nä jag kör glmer 1: Some predictor variables are on very different scales: consider rescaling
#' Förklaring till problemet finns här: https://stackoverflow.com/questions/26904580/error-messages-when-running-glmer-in-r


## ***************** ALLA FASER *********************####

# Plockar först ut de variabler jag behöver.


#' Skalar om innan jag lägger på modellen.
pvars <- c("medelvärde_lämmelprediktion_uppgångsår",
           "area_myr","area_vatten", "distans_till_vatten", "distans_till_skog")
datsc <- dens.sub
datsc[pvars] <- lapply(datsc[pvars],scale)

View(datsc)



global.modell <- glmer(Kull ~ Fas + medelvärde_lämmelprediktion_uppgångsår
                       + area_myr + area_vatten + distans_till_vatten
                       + distans_till_skog + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
                       data = datsc) # det ska vara logit eftersom Kull är binär data. När jag lägger till Namn som en random variabel fattar R att Namn är grupper

stdz.model <- standardize(global.modell, standardize.y = FALSE)
summary(stdz.model)
library(sjPlot)
plot_model(stdz.model, type = "slope")
plot_model(stdz.model, type = "diag") # qq-plot for kvantilerna för random effekt mot standard normalkvantiler
plot_model(stdz.model, type = "re")
plot_model(stdz.model, type = "resid" )

library(DHARMa)
# # vignette för DHARMa https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
simulateResiduals(stdz.model, plot = TRUE)
sim.output<-simulateResiduals(stdz.model)
plot(sim.output, quantreg = FALSE)
resid.fitted.plot<-plot(sim.output) #tar längre tid
plotResiduals(datsc$distans_till_vatten , sim.output$scaledResiduals, quantreg = T) # några variabler har enskilt lite dålig fit. Area_vatten och lämmel är ok
resid.fitted.plot # kanske behöver presentera den här
plot.new()
testOverdispersion(sim.output) # med för mycket overdispersion är det mer varians i datat än i modellen. Min overdispersion är inte för hög.
testZeroInflation(sim.output) # tror inte det här är ett problem i binomial data. Kanske i negative binomial.
testUniformity(simulationOutput = sim.output) #Heteroscedasticity: när variansen i en variabel är olika runt responsvariabeln. Till exempel om variansen är lägre vid låga värden av responsvariabeln än vid höga värden. Det ger ofta en konformad scatterplot.
testDispersion(sim.output)# testar både over och underdispersion

#det är bäst att använda de här där man grupperar per lya
sim.group<-recalculateResiduals(sim.output, group = datsc$Namn)
testDispersion(sim.group)
testUniformity(sim.group)
plot(sim.group, quantreg = FALSE)


# Fortsatt analys med omskalning baserat på sample standard deviations
summary(global.modell)
#' Jag fick samma varning, men det funkar: This works, 
#' although we get a warning message about a too-large gradient -- I think 
#' this is actually ignorable (we're still working on getting these error 
#' sensitivity thresholds right)


## increases max gradient -- larger warning
library(MuMIn)
model.set <- dredge(stdz.model)  ## slow, but running ...


par(mfrow=c(1,1))
par(mar = c(3,5,6,4))
plot(model.set, labAsExpr = TRUE)
m.ave<-model.avg(model.set, subset = delta < 2) # skillnaden mellan modellen med lägst AIC och den modellen med högst AIC som väljs
Weights(m.ave)
summary(m.ave)

## Fas 1 ####

fas.1 <- dens.sub %>% 
  filter(Fas == "low")
View(fas.1)
length(fas.1$Kull[fas.1$Kull==1]) # 2 kullar under lågår. Det är på tok för lite för att analysera
length(fas.1$obsID)
View(fas.1)

# Skalar om med sample standard deviations.


pvars <- c("medelvärde_lämmelprediktion_uppgångsår","area_myr",
           "area_vatten", "distans_till_vatten", "distans_till_skog")
datsc.1 <- fas.1
datsc.1[pvars] <- lapply(datsc.1[pvars],scale)
View(datsc.1)

fas.1.modell <- glmer(Kull ~ medelvärde_lämmelprediktion_uppgångsår
                      + area_myr + area_vatten + distans_till_vatten
                      + distans_till_skog + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
                      data = datsc.1)

stdz.model.1 <- standardize(fas.1.modell, standardize.y = FALSE)
car::vif(stdz.model.1)
fas.1.setsc <- dredge(stdz.model.1)

nrow(datsc.1) ## 420
ncol(getME(stdz.model.1,"X")) # 7, om det är mindre än 10 obs per variabel i AIC analysen är det för få. Jag har 420/7 = 60



par(mar = c(3,5,6,4))
plot(fas.1.setsc, labAsExpr = TRUE)
ave.1sc<-model.avg(fas.1.setsc, subset = delta < 2) 
ave.1sc$msTable

## Fas 2 ####
fas.2 <- dens.sub %>% 
  filter(Fas == "increase")

length(fas.2$Kull[fas.2$Kull==1])# 9 kullar. Det är för få för att analysera
length(fas.2$obsID) # om det är mindre än 10 obs per variabel i AIC analysen är det för få. 







#skalar om med sample standard deviations.
pvars <- c("medelvärde_lämmelprediktion_uppgångsår",
           "area_myr","area_vatten", "distans_till_vatten", "distans_till_skog")
datsc.2 <- fas.2
datsc.2[pvars] <- lapply(datsc.2[pvars],scale)
View(datsc.2)

fas.2.modell <- glmer(Kull ~ medelvärde_lämmelprediktion_uppgångsår
                      + area_myr + area_vatten + distans_till_vatten
                      + distans_till_skog + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
                      data = datsc.2) # det ska vara logit eftersom Kull är binär data. När jag lägger till Namn som en random variabel fattar R att Namn är grupper

stdz.model.2 <- standardize(fas.2.modell, standardize.y = FALSE)
car::vif(stdz.model.2)
fas.2.set <- dredge(stdz.model.2)

par(mar = c(3,5,6,4))
plot(fas.2.set, labAsExpr = TRUE)

ave.2sc<-model.avg(fas.2.set, subset = delta < 2) 
summary(ave.2sc)

## Fas 3 ####

fas.3 <- dens.sub %>% 
  filter(Fas == "peak")

length(fas.3$Kull[fas.3$Kull == 1]) # 34 kullar
length(fas.3$Obs_ID) # om det är mindre än 10 obs per variabel i AIC analysen är det för få.





#skalar om med sample standard deviations.
pvars <- c("medelvärde_lämmelprediktion_uppgångsår",
           "area_myr","area_vatten", "distans_till_vatten", "distans_till_skog")
datsc.3 <- fas.3
datsc.3[pvars] <- lapply(datsc.3[pvars],scale)

fas.3.modell <- glmer(Kull ~ medelvärde_lämmelprediktion_uppgångsår
                      + area_myr + area_vatten + distans_till_vatten
                      + distans_till_skog + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
                      data = datsc.3) # det ska vara logit eftersom Kull är binär data. När jag lägger till Namn som en random variabel fattar R att Namn är grupper


stdz.model.3 <- standardize(fas.3.modell, standardize.y = FALSE)
car::vif(stdz.model.3)
summary(stdz.model.3)
fas.3.set <- dredge(stdz.model.3) # stora standard errors betyder att en eller flera modeller inte gick ihop (failed to converge)
ave.3sc<-model.avg(fas.3.set, subset = delta < 2)
summary(ave.3sc)
nrow(ave.3sc$msTable) #9 modeller
confint(ave.3sc, full = TRUE) # conditional är default på alla de här funktionerna. Full= TRUE ger full average. 

logLik(ave.3sc, full = TRUE)
coefTable(ave.3sc, full = TRUE)
vcov(ave.3sc, full = TRUE)
ave.3sc$importance # ger importance baserat på Akaikevikter, inte på antal gånger den är med i de översta modellerna.

# Gör en tabell
sumtable.3<-summary(ave.3sc)
sumtable.3


imp3<-as.data.frame(sumtable.3$importance)
imp3$`sumtable.3$importance` # ska kanske ha med det här med i plotten.
imp3<-stack(imp3) # ger error men det fungerar
imp3
imp3<-round(imp3, digits = 2) #avrundar
imp3$Parameters <- as.character(NA)
imp3$Parameters<-rownames(imp3) # lägger radnamn som kolumn
rownames(imp3) <-NULL #tar bort radnamn
class(imp3$`sumtable.3$importance` ) #sparas som "importance" och "numeric". Vet inte vad importance är för klass
imp3$`sumtable.3$importance` <- as.numeric(imp3$`sumtable.3$importance`)

full.coefs.3<-as.data.frame(round(sumtable.3$coefmat.full, digits = 3)) # tar ut koefficienterna för full average
full.coefs.3$Parameters <- as.character(NA)
full.coefs.3$Parameters<-rownames(full.coefs.3)
rownames(full.coefs.3)<- NULL

coefs.table.3 <- full.coefs.3 %>% 
  dplyr::select(Parameters, Estimate, `Std. Error`) %>% 
  dplyr::rename(`Unconditional SE` = `Std. Error`)

coefs.table.3
conf.int.3<-as.data.frame(round(confint(sumtable.3, full = TRUE), digits = 3)) #konfidensintervall för full average
conf.int.3$Parameters <- rownames(conf.int.3)
rownames(conf.int.3) <- NULL

conf.int.3 <- conf.int.3 %>% 
  unite(`Confidence interval`,`2.5 %`, `97.5 %`, sep = ", ")
conf.int.3

coefs.table.3 <-coefs.table.3 %>% 
  left_join(conf.int.3, by = "Parameters")

coefs.table.3

colnames(imp3)[1] <- "Relative importance"


coefs.table.3 <- coefs.table.3 %>% 
  left_join(imp3, by = "Parameters")

coefs.table.3



coefs.table.3$Parameters <- c("Intercept", "distance to forest", "lemming density","area bogs",
                              "area water", "distance to water")
coefs.table.3 <- coefs.table.3 %>% 
  arrange(desc(`Relative importance`))
nrow(coefs.table.3) #6
coefs.table.3

coefs.table.3 <- coefs.table.3[c(6,1:5),] #flyttar interceptet till toppen. Hamnade i botten
coefs.table.3
sumtable.3$coefmat.full
sumtable.3$importance
confint(sumtable.3, full = TRUE)

write_xlsx(coefs.table.3, path = "Excelfiler UT/Tabell_toppfas_GISdata_Vindelfjällen.xlsx")


## figur fas 3 ####
plot.df3<-coefs.table.3 %>% 
  slice(-1) # tar bort interceptet
plot.df3
plot.df3$Estimate <- paste0('Est. = ', plot.df3$Estimate) # lägger till "Est. =" i början på alla rader
plot.df3$`Unconditional SE` <- paste0('U.SE = ', plot.df3$`Unconditional SE`)
plot.df3$`Confidence interval` <- paste0('C.I = ', plot.df3$`Confidence interval`)

plot.df3

?fill
g3<-ggplot(data=plot.df3, aes(x=Parameters, y=`Relative importance`, fill=`Relative importance`)) +
  scale_x_discrete(limits = plot.df3$Parameters) + #säger år ggplot att hålla samma ordning på kolumnerna som i dataramen
  geom_bar(stat="identity", width=0.9)+
  geom_text(aes(label= plot.df3$Estimate), vjust=-6, color="black", size = 5.5)+
  geom_text(aes(label= plot.df3$`Unconditional SE`), vjust=-4, color="black", size = 5.5)+
  geom_text(aes(label= plot.df3$`Confidence interval`), vjust=-2, color="black", size = 5.5)+
  labs(x = "Parameters in selected models, lemming peak phase")+
  labs(y = "Relative importance")+
  theme_minimal()

g3<-g3+theme(axis.text=element_text(size=17, color = "black"), # ändrar stapeltextens storlek
             axis.title=element_text(size=17,face="bold"))+ # ändrar axeltitlarnas storlek
  annotate(geom = "label", x = 4, y = 0.85,
           label = "Intercept\nEst. = -4.837\nU.SE =  0.976\nC.I =  -6.756, -2.918", # geom = "label" gör en ruta runt texten. geom = "text" ger bara text utan ruta. \n betyder ny rad.
           hjust = 0, size = 5.5) + # vänsterjusterar texten
  coord_cartesian(ylim=c(0, 1.25))+ # sätter plottens zoom. Jag vill ha utrymme ovanför staplarna så att texten får plats
  scale_y_continuous(breaks=seq(0, 1, 0.2)) +  # sätter min och maxvärdena för vad som ska visas på y-axeln (inte datat, bara axeln). 0.2 är intervallet. 
  guides(fill=FALSE) # tar bort färgstapeln till höger
g3
coefs.table.3

ggsave("importance.plot.fas3.Vindelfjällen.png", width = 35, height = 20, units = "cm") # sparar plotten i working directory
