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
unique(dat$nomStation) 

# Avec arr dat |> group_by avec la col nomStation |> Summarize le nombre entrées
dat |> group_by(nomStation) |> summarize(observations = n())

q02_nb_obs_total = dat |> summarize(observations = n())

# Nouvelle variable avec seulement les observations de la station AQ (Aquarium du Québec)
dataAQ <- dat |> filter(nomStation == "Pêche de l'Aquarium du Québec")

# Q3 : d’observations après filter
q03_nb_obs_AQ <- dataAQ |> summarize(observations = n())

unique(dataAQ$scientificName)

# Q4 : nb Micropterus dolomieu recensés le 1992-08-14T02:05:00 
q04_nb_achigans_timestamp  <- dataAQ |> filter(scientificName == "Micropterus dolomieu" & eventdate == "1992-08-14T02:05:00") |> summarise("nombre d’achigans" = organismQuantity)

dataAQ |> group_by(scientificName) |> summarise("Nombre d'espèces capturées" = sum(organismQuantity))

# Q5 : Combien d'anguilles ("Anguilla rostrata") depuis le début
q05_nb_anguilles <- dataAQ |> filter(scientificName == "Anguilla rostrata") |> summarise("Nombre d'anguilles" = sum(organismQuantity))

# arrange desc arr species observed AQ
arrSpeciesObservedAQ <- dataAQ |> group_by(scientificName) |> summarise(organismQuantity = sum(organismQuantity)) |> arrange(desc(organismQuantity))

# Q6: Quelle espèce plus présente?
q06_espece_most_AQ <- arrSpeciesObservedAQ |> filter(organismQuantity == max(organismQuantity))

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
q07_espece_most_1980_AQ <- dataAQ |>
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
q08_espece_most_2010_AQ <- dataAQ |>
  filter(annee > 2010) |>
  group_by(scientificName) |>
  summarise(organismQuantity = sum(organismQuantity)) |> 
  filter(organismQuantity == max(organismQuantity)) 

# left_join les données famille dans la table dataAQ
dataAQ <- left_join(dataAQ, taxo_poisson, by = 'scientificName')

# Q9 : Combien d’esturgeons (famille "Acipenseridae")
q09_nb_esturg_AQ <- dataAQ |>
  filter(famille == "Acipenseridae")|> 
  summarise(organismQuantity = sum(organismQuantity))

# Tableau arranger individu par famille pour valider la reponses
arrIndividualByFam <- dataAQ |>
  group_by(famille) |>
  summarise(organismQuantity = sum(organismQuantity)) |>
  arrange(desc(organismQuantity))

# 10 : Quelle famille a le plus grand nombre d’individus capturée par jours
q10_most_individus_fam <- dataAQ |>
  group_by(famille) |>
  summarise(mean = mean(organismQuantity)) |>
  filter(mean == max(mean))
  