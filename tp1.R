###########################################################################
# Auteur : Anthony Carmichael
# UQTR ECL1017 - Écoinformatique
# TP1 : Aquarium de Québec
# Github: 
# https://github.com/AnthonyCarmichael/TP1-Aquarium-Quebec-UQTR-ECL1017      
##########################################################################

# included libs
library(dplyr)
library(readxl)
library(skimr)
library(tidyverse)
library(janitor)
library(writexl)
library(lubridate)

# load data
dat = read_xlsx("./data/aquarium.xlsx")
taxo_poisson = read_xlsx("./data/taxo_poissons.xlsx")

# Permet de voir les valeurs uniques de col station
# unique(dat$nomStation) 

# Avec arr dat |> group_by avec la col nomStation |> Summarize le nombre entrées
dat |> group_by(nomStation) |> summarize(observations = n())

q2_nb_obs_total = dat |> summarize(observations = n())

# Nouvelle variable avec seulement les observations de la station AQ (Aquarium du Québec)
dataAQ <- dat |> filter(nomStation == "Pêche de l'Aquarium du Québec")

# Q3 : d’observations après filter
q3_nb_obs <- dataAQ |> summarize(observations = n())

unique(dataAQ$scientificName)

# Q4 : nb Micropterus dolomieu recensés le 1992-08-14T02:05:00 
q4_nb_achigans_timestamp  <- dataAQ |> filter(scientificName == "Micropterus dolomieu" & eventdate == "1992-08-14T02:05:00") |> summarise("nombre d’achigans" = organismQuantity)

dataAQ |> group_by(scientificName) |> summarise("Nombre d'espèces capturées" = sum(organismQuantity))

# Q5 : Combien d'anguilles ("Anguilla rostrata") depuis le début
q5_nb_anguilles <- dataAQ |> filter(scientificName == "Anguilla rostrata") |> summarise("Nombre d'anguilles" = sum(organismQuantity))

# arrange desc arr species observed AQ
arrSpeciesObservedAQ <- dataAQ |> group_by(scientificName) |> summarise(organismQuantity = sum(organismQuantity)) |> arrange(desc(organismQuantity))

# Q6: Quelle espèce plus présente?
q6_espece_most_AQ <- arrSpeciesObservedAQ |> filter(organismQuantity == max(organismQuantity))

# Déterminer l'évolution de l'espèce dominante au courant du temps
# Ajout d'une col annee da
dataAQ$annee <- year(dataAQ$eventdate)

# filter only data from 
dataAQ2020 <- dataAQ |> filter(annee == 2020)


# Tableau desc espece selon nombre individus avant 1980
arrSpeciesObserved1980AQ <- dataAQ |>
  filter(annee < 1980) |>
  group_by(scientificName) |>
  summarise(organismQuantity = sum(organismQuantity)) |>
  arrange(desc(organismQuantity))


# Q7 : Quelle espèce était la plus abondante avant 1980
q7_espece_most_1980_AQ <- dataAQ |>
  filter(annee < 1980) |>
  group_by(scientificName) |>
  summarise(organismQuantity = sum(organismQuantity)) |> 
  filter(organismQuantity == max(organismQuantity)) 


# Tableau desc espèce selon nombre individus après 2010
arrSpeciesObserved2010AQ <- dataAQ |>
  filter(annee > 2010) |>
  group_by(scientificName) |>
  summarise(organismQuantity = sum(organismQuantity)) |>
  arrange(desc(organismQuantity))


# Q8 : Quelle espèce était la plus abondante après 2010
q8_espece_most_2010_AQ <- dataAQ |>
  filter(annee > 2010) |>
  group_by(scientificName) |>
  summarise(organismQuantity = sum(organismQuantity)) |> 
  filter(organismQuantity == max(organismQuantity)) 



                                                                                                                               