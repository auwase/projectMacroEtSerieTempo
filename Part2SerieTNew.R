# PARTIE SERIE_TEMPORELLE

library(dplyr)
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
