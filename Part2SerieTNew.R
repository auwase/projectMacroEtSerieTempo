# PARTIE SERIE_TEMPORELLE
install.packages("tseries")
library(tseries)
library(readxl)
library(astsa)
library(ggfortify)
library(forecast)
library(fpp2)
library(seasonal)
library(dplyr)
library(urca)
library(xts)
# MODELE ALLEMAGNE
data_All <- read_excel("data_All.xlsx")

data_All$Date <-as.Date(data_All$Date)

Date_covid <- as.Date ("2020-01-01")

data_All <- data_All %>%
  rename(PIB_mond1 = PIB_mond)

data_All<-mutate(data_All,PIB_mond1 =lag(PIB_mond1,default=0))

# si la date est superieur ou egal a la date , elle atrribue la valeur 1
data_All<- mutate(data_All,covid_dummy =ifelse(Date>=Date_covid , 1,0))

model_All<- lm(BC~Croissance_PIB  +PIB_mond1 + covid_dummy , data= data_All)

# MODELE USA
data_USA <- read_excel("data_USA.xlsx")

data_USA$Date <-as.Date(data_USA$Date)

Date_covid <- as.Date ("2020-01-01")

data_USA <- data_USA %>%
  rename(PIB_mond2 = PIB_mond)

data_USA<-mutate(data_USA,PIB_mond2 =lag(PIB_mond2,default=0))

# si la date est superieur ou egal a la date , elle atrribue la valeur 1
data_USA<- mutate(data_USA,covid_dummy =ifelse(Date>=Date_covid , 1,0))

model_USA<- lm(BC~Croissance_PIB  +PIB_mond2 + covid_dummy , data= data_USA)
#################################### USA ################################################
## SEASONNLITE ET RUPTURE
head(data_USA)
acf(data_USA)

## VALEURS ABBERANTES 
boxplot(data_USA[c("Croissance_PIB", "PIB_mond2")], 
        main = "Boxplot pour Croissance PIB et PIB mond2",
        names = c("Croissance PIB", "PIB mond2"),
        las = 2, # Oriente les étiquettes perpendiculairement à l'axe
        col = c("lightblue", "lightgreen"))

## RACINE UNITAIRE (STATIONNALITE OU NON STATIONNALITE)
##Création d'objet xts en excluant la colonne de date du data frame et en utilisant cette colonne comme index
data_USA <- xts(data_USA[, -which(names(data_USA) == "Date")], order.by = data_USA$Date)
##data_USA_log <- log(data_USA)
head(data_USA)

# Application du test ADF
adf_test_USA <- lapply(data_USA[, c("BC", "Croissance_PIB", "PIB_mond2")], function(x) adf.test(x, alternative = "stationary"))

# Application du test PP
pp_test_USA <- lapply(data_USA[, c("BC", "Croissance_PIB", "PIB_mond2")], function(x) pp.test(x))

# Application du test KPSS
kpss_test_USA <- lapply(data_USA[, c("BC", "Croissance_PIB", "PIB_mond2")], function(x) kpss.test(x))






#################################### ALLEMAGNE ################################################
## SEASONNLITE ET RUPTURE
head(data_All)
acf(data_All)

## VALEURS ABBERANTES 
boxplot(data_All[c("Croissance_PIB", "PIB_mond1")], 
        main = "Boxplot pour Croissance PIB et PIB mond1",
        names = c("Croissance PIB", "PIB mond2"),
        las = 2, # Oriente les étiquettes perpendiculairement à l'axe
        col = c("lightblue", "lightgreen"))

## RACINE UNITAIRE (STATIONNALITE OU NON STATIONNALITE)
##Création d'objet xts en excluant la colonne de date du data frame et en utilisant cette colonne comme index
data_All <- xts(data_All[, -which(names(data_All) == "Date")], order.by = data_All$Date)
##data_USA_log <- log(data_USA)
head(data_All)

# Application du test ADF
adf_test_All <- lapply(data_All[, c("BC", "Croissance_PIB", "PIB_mond1")], function(x) adf.test(x, alternative = "stationary"))

# Application du test PP
pp_test_All <- lapply(data_All[, c("BC", "Croissance_PIB", "PIB_mond1")], function(x) pp.test(x))

# Application du test KPSS

kpss_test_All <- lapply(data_USA[, c("BC", "Croissance_PIB", "PIB_mond1")], function(x) kpss.test(x))
getwd()
