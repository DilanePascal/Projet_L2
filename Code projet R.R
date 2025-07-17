##chargement des bases de données et libraries

setwd("C:/Users/Emmaüs Connect/Documents/TD R/Projet R")

delinquance_regionale = read.table("donnee-reg-data.gouv-2024-geographie2024-produit-le2025-01-26.csv"
                                   ,header=TRUE,sep=";",dec=",",skip=0)
autres_variables_interessantes = read.table("data.csv", sep=";",header=TRUE, dec=".", skip=2, quote="", fill=TRUE)

library(datasets)
library(tidyverse)
library(tidyr)
library(knitr)
library(ggplot2)
library(dplyr)
library(sf)


##fusion des deux bases de données

names(autres_variables_interessantes)[1]="Code_region"
delinquance_fusionnée=inner_join(delinquance_regionale,autres_variables_interessantes, by ="Code_region")
?ggplot

##filtrage des données selon les indicateurs:

###Niveau des homicides

nouveau = select(delinquance_fusionnée,Code_region,Libellé,indicateur,nombre,annee)
filtrage_homicides = filter(nouveau,indicateur=="Homicides")
homicides = filtrage_homicides %>%
  pivot_wider(names_from = "annee", values_from = "nombre")

##nombre total des homicides sur 8 ans (2016-2024)

homicides_totalsur8ans = sum(sum(homicides$'2016'),
                             sum(homicides$'2017'),
                             sum(homicides$'2018'),
                             sum(homicides$'2019'),
                             sum(homicides$'2020'),
                             sum(homicides$'2021'),
                             sum(homicides$'2022'),
                             sum(homicides$'2023'),
                             sum(homicides$'2024'))

###Niveau des trafics de stupéfiants (indidcateur)

filtrage_trafic_stup = filter(nouveau,indicateur=="Trafic de stupéfiants")
trafic_stup = filtrage_trafic_stup %>%
  pivot_wider(names_from = "annee", values_from = "nombre")


## nombre total des trafics de stupéfiants sur 8 ans

trafic_stup_totalsur8ans = sum(sum(trafic_stup$'2016'),
                               sum(trafic_stup$'2017'),
                               sum(trafic_stup$'2018'),
                               sum(trafic_stup$'2019'),
                               sum(trafic_stup$'2020'),
                               sum(trafic_stup$'2021'),
                               sum(trafic_stup$'2022'),
                               sum(trafic_stup$'2023'),
                               sum(trafic_stup$'2024'))

###Niveau des tentatives d'homicides (indicateur)

filtrage_tentatives_homicides = filter(nouveau,indicateur=="Tentatives d'homicides")
tentatives_homicides = filtrage_tentatives_homicides %>%
  pivot_wider(names_from = "annee", values_from = "nombre")

##nombre total des tentatives d'homicides sur 8 ans

tentatives_homicides_totalsur8ans = sum(sum(tentatives_homicides$'2016'),
                                        sum(tentatives_homicides$'2017'),
                                        sum(tentatives_homicides$'2018'),
                                        sum(tentatives_homicides$'2019'),
                                        sum(tentatives_homicides$'2020'),
                                        sum(tentatives_homicides$'2021'),
                                        sum(tentatives_homicides$'2022'),
                                        sum(tentatives_homicides$'2023'),
                                        sum(tentatives_homicides$'2024'))

classementinfraction_2024=delinquance_fusionnée %>%
  group_by(indicateur) %>%
  summarise(Nombre = max(nombre[which(annee==2024)], na.rm = TRUE))%>%
  arrange(desc(Nombre))

#Tableau synthétique
table_res = data.frame(
  Indicateur = c("Homicides", "Tentatives d'homicides", "Trafic de stupéfiants", "Vols sans violences"),
  Total_2016_2024 = c(homicides_totalsur8ans, tentatives_homicides_totalsur8ans, trafic_stup_totalsur8ans)
)
kable(table_res, caption = "Synthèse des infractions sur 8 ans")

##Filtrage des données suivant les indicateurs etudiés

filtrage_selonindicateurs = subset(delinquance_fusionnée,indicateur %in% 
                                     c("Homicides", "Tentatives d'homicides","Trafic de stupéfiants"))

##representation de l'evolution des 3 indicateurs sur la période de 8 ans

groupage_selonindicateurs = filtrage_selonindicateurs %>%
  group_by(annee, indicateur) %>%
  summarise(nombre = sum(as.numeric(nombre), na.rm = TRUE))

Année = as.factor(groupage_selonindicateurs$annee)
Infractions = groupage_selonindicateurs$nombre

ggplot(groupage_selonindicateurs, aes(x = Année, y = nombre, fill = indicateur)) +
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values = c("Homicides" = "red", 
                               "Tentatives d'homicides" = "green", 
                               "Trafic de stupéfiants" = "blue")) +
  theme_minimal()

##ou encore
ggplot(groupage_selonindicateurs)+
  geom_line(aes(x = Année, y = Infractions, color = indicateur, group = indicateur), size =1.5)

##correlation des indicateurs avec les autres variables

# Vérification de la corrélation avec le taux de chômage
cor_chomage_infraction = cor(delinquance_fusionnée$Taux.de.chômage.des.15.64.ans..RP..2021[which(delinquance_fusionnée$annee<=2021)], 
                              delinquance_fusionnée$nombre[which(delinquance_fusionnée$annee<=2021)], use="complete.obs")

# Vérification de la corrélation avec la densité de la population (si disponible)
cor_densitépop_infraction = cor(delinquance_fusionnée$Densité.de.population.2021[which(delinquance_fusionnée$annee<=2021)], 
                             delinquance_fusionnée$nombre[which(delinquance_fusionnée$annee<=2021)], use="complete.obs")

print(paste("Corrélation taux de chômage - infractions :", cor_chomage_infraction))
print(paste("Corrélation densité de la population - infractions :", cor_densitépopulation_infraction))

# Graphique de la corrélation entre le taux de chômage et les infractions
plot(delinquance_fusionnée$Taux.de.chômage.des.15.64.ans..RP..2021, delinquance_fusionnée$nombre,
     ylab = "Nombre d'infractions", xlab = "Taux de chômage (15-64 ans)",
     col="blue")

## Evolution des homicides sur quelques regions

plot(2016:2024,homicides[1,4:12],type="l",ylim=c(0,200),
     xlab="Année",
     ylab="Nombre d'infractions")
lines(2016:2024,homicides[2,4:12], col="red")
lines(2016:2024,homicides[3,4:12],col="blue")
lines(2016:2024,homicides[4,4:12],col="green")
lines(2016:2024,homicides[5,4:12],col="pink")
lines(2016:2024,homicides[6,4:12],col="black")


## Evolution des tentatives d'homicides sur quelques regions

plot(2016:2024,tentatives_homicides[1,4:12],type="l",ylim=c(0,1000),
     xlab="Année",
     ylab="Nombre d'infractions")
lines(2016:2024,tentatives_homicides[2,4:12], col="red")
lines(2016:2024,tentatives_homicides[3,4:12],col="blue")
lines(2016:2024,tentatives_homicides[4,4:12],col="green")
lines(2016:2024,tentatives_homicides[5,4:12],col="pink")
lines(2016:2024,tentatives_homicides[6,4:12],col="black")

## Evolution des trafics de stupéfiants sur quelques regions

plot(2016:2024,trafic_stup[1,4:12],type="l",ylim=c(100,16000),
     xlab="Année",
     ylab="Nombre d'infractions")
lines(2016:2024,trafic_stup[2,4:12], col="red")
lines(2016:2024,trafic_stup[3,4:12],col="blue")
lines(2016:2024,trafic_stup[4,4:12],col="green")
lines(2016:2024,trafic_stup[5,4:12],col="pink")
lines(2016:2024,trafic_stup[6,4:12],col="black")

##part que représente l'Île de France dans les niveaux d'infractions commis

part_homicides_iledefrance = sum(homicides[6,4:12])/homicides_totalsur8ans
paste(print(part_trafic_stup_iledefrance))
part_trafic_stup_iledefrance = sum(trafic_stup[6,4:12])/trafic_stup_totalsur8ans
part_tentative_homicides_iledefrance = sum(tentatives_homicides[6,4:12])/tentatives_homicides_totalsur8ans


##Représentation cartographiée

#niveau du trafic des stupefiants en 2024
carte = read_sf("REGION.shp")
names(carte)[6]="Code_region"
carte$Code_region = as.numeric(carte$INSEE_REG)
fusion_traficstup_carte = left_join(carte, trafic_stup, by ="Code_region")

plot(fusion_traficstup_carte["2024"])

#niveau des homicides en 2024
fusion_homicides_carte = left_join(carte, homicides, by ="Code_region")

plot(fusion_homicides_carte["2024"])