

################################PARTIE SERIE TEMPORELLE############################"
getwd()

##1. YRAITEMENT DE DONNEES 
### Saisonalité, Valeurs Abberantes, Rupture 
library(xts)
library(readxl)
library(astsa)
library(ggfortify)
library(forecast)
library(fpp2)
library(seasonal)
library(readxl)
library(dplyr)
data_All <- read_excel("data_All.xlsx")
data_USA<- read_excel("data_USA.xlsx")
Pib_mondiale <- read_excel("Pib_mondiale .xlsx")

head(Pib_mondiale)

# Convertir la colonne timestamp au format Date
Pib_mondiale$timestamp <- as.Date(Pib_mondiale$timestamp)

# Renommer la colonne "WDI/NY.GDP.MKTP.KD.ZG/WLD" en "pibmondiale"
names(Pib_mondiale)[2] <- "pibmondiale"

# Filtrer les données pour la période de temps souhaitée
Pib_filtered <- Pib_mondiale %>%
  filter(timestamp >= "2010-01-01" & timestamp <= "2021-10-01")

# Convertir en objet xts
Pib_xts <- xts(Pib_filtered$pibmondiale, order.by = Pib_filtered$timestamp)

# Renommer les colonnes
colnames(Pib_xts) <- "Tx_cr_PIB_mond"

# Afficher les premières lignes de l'objet xts
print(head(Pib_xts))


# Convertir Pib_xts en data frame
TxCr_Pib_monde <- data.frame(Date = index(Pib_xts), Tx_cr_PIB_mond = coredata(Pib_xts))

colnames(TxCr_Pib_monde)
colnames(data_All)

# Renommer le nouveau dataframe fusionné en d_Allemagne
d_Allemagne <- merge(data_All, TxCr_Pib_monde, by = "Date")
d_USA <- merge(data_USA, TxCr_Pib_monde, by = "Date")
# Afficher les premières lignes de d_Allemagne pour vérifier
print(head(d_Allemagne))
library(openxlsx)
write.xlsx(d_Allemagne,"d_Allemagne.xlsx", rownames = FALSE)
write.xlsx(d_USA,"d_USA.xlsx", rownames = FALSE)

#############################  à regarde? le données utilise(d_Allemagne,d_USA?)
# MODELE ALLEMAGNE
data_All <- read_excel("data_All.xlsx")
data_All$Date <-as.Date(data_All$Date)
Date_covid <- as.Date ("2020-01-01")
data_All<-mutate(data_All,PIB_mond1 =lag(PIB_mond,default=0))
# si la date est superieur ou egal a la date , elle atrribue la valeur 1
data_All<- mutate(data_All,covid_dummy =ifelse(Date>=Date_covid , 1,0))

model_All<- lm(BC~Croissance_PIB  +PIB_mond1 + covid_dummy , data= data_All)

# MODELE USA
data_USA <- read_excel("data_USA.xlsx")
data_USA$Date <-as.Date(data_USA$Date)
Date_covid <- as.Date ("2020-01-01")
data_USA<-mutate(data_USA,PIB_mond2 =lag(PIB_mond,default=0))
# si la date est superieur ou egal a la date , elle atrribue la valeur 1
data_USA<- mutate(data_USA,covid_dummy =ifelse(Date>=Date_covid , 1,0))

model_USA<- lm(BC~Croissance_PIB  +PIB_mond2 + covid_dummy , data= data_USA)

########################################################################################################
# Tracer l'objet xts
#plot(as.xts(data_All))

# Convertir la colonne "Date" au format date-heure
#data_USA$Date <- as.Date(data_USA$Date)
#str(data_USA$Date)
#t# Convertir le data frame en un objet xts et définir "Date" comme noms de lignes
#data_USA <- data_USA |> 
 # tibble::column_to_rownames(var = "Date")

# Convertir le data frame en un objet xts
# Tracer l'objet xts
#plot(as.xts(data_USA))


# Calculer les premières différences du logarithme naturel du taux de croissance du PIB
# Cette transformation est réalisée pour stabiliser la variance et éliminer les tendances des données :
# - Prendre le logarithme stabilise la variance, la rendant plus constante dans le temps
# - Calculer les différences supprime les tendances, rendant les données stationnaires et adaptées à l'analyse

########### POUR USA
#Crois_Pib_USA <- diff(log(data_USA$Croissance_PIB))

#Crois_Pib_USA
#Bal_Com_USA <- diff(log(data_USA$BC))
#Bal_Com_USA
################# USA donne NA pour toute la colonne de BC
#### POUR Allemagne 

#Crois_Pib_All <- diff(log(data_All$Croissance_PIB))

#Crois_Pib_All
#Bal_Com_All <- diff(log(data_All$BC))
#Bal_Com_All
### Suprimer les NAs
#donne_BC_CrPib <- na.trim(merge(Bal_Com_All, Crois_Pib_All))

######################################################################################################################