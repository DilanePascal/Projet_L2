##chargement des bases de données et des librairies

setwd("C:/Users/Emmaüs Connect/Documents/TD R/Projet R")

table_regional=read.table("donnee-reg-data.gouv-2024-geographie2024-produit-le2025-01-26.csv"
                          ,header=TRUE,sep=";",dec=",",skip=0)

table_caract_region=read.table("data.csv", sep=";",header=TRUE, dec=".", skip=2, quote="", fill=TRUE)

library(datasets)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(sf)



##FUSION TABLES

names(table_caract_region)[1]="Code_region"
synthese=inner_join(table_regional,table_caract_region, by ="Code_region")


##nombre total de crimes et delits par region

crime_total=synthese %>%
  group_by(Code_region,Libellé,annee,indicateur) %>%
  summarise(Total_Crimes = sum(nombre, na.rm = TRUE))
crime_total=arrange(crime_total,Code_region)
glimpse(crime_total)

##moyenne des crimes et delits commis par année de 2016 à 2021

Moyenne_crimes=summarise(crime_total,Nbr_moyen_crimes_délits= mean(Total_Crimes))
Moyenne_generale=Moyenne_crimes %>%
  pivot_wider(names_from = "annee", values_from = "Nbr_moyen_crimes_délits")

##nombre total de crimes et délits commis par année de 2016 à 2021

Total_crimes_par_année_region=count(crime_total,annee, wt = Total_Crimes)


##nombre total de crimes et délits commis par année et par région
 
total_general= Total_crimes_par_année_region%>%
  pivot_wider(names_from = "annee", values_from = "n")

##Carte

carte=read_sf("REGION.shp")
names(carte)[4]="Code_region"
carte$Code_region=as.numeric(carte$Code_region)
fusion_carte=left_join(carte,neww_homicides2[6:18,], by ="Code_region")
carte$Code_region=as.numeric()

plot(fusion_carte["2017"], main="Niveau des homicides en 2017")


##representation evolution nominale
##Il faut des légendes
plot.new()
plot(2016:2024,Moyenne_generale[1,3:11],type="l",ylim=c(500,2000),
     xlab="Année",
     ylab="Moyenne des crimes")
lines(2016:2024,Moyenne_generale[2,3:11], col="red")
lines(2016:2024,Moyenne_generale[3,3:11],col="blue")
lines(2016:2024,Moyenne_generale[4,3:11],col="green")


##boite à moustache  taux pour mille, indicateur V
indicateur=factor(synthese$indicateur)
boxplot(taux_pour_mille ~ indicateur, data = synthese,
        col = rainbow(length(levels(indicateur))),
        xlab = "Indicateur", 
        ylab = "Taux pour mille",
        las = 2,
        cex.axis = 0.6)


synthese %>% group_by(Libellé) %>% summarise(moyenne=mean(Total_til_2021$n),
    ecart_type=sd(Total_til_2021$n), mediane=median(Total_til_2021$n))

synthese$taux_pour_mille=as.numeric(gsub(",", ".", synthese$taux_pour_mille))


# representation de la relation entre taux de chômage et taux pour mille
 
plot(synthese$nombre,synthese$insee_pop,ylim=c(100000,1000000),
     pch=1,col="red",
     ylab="Nombre d'infractions",
     xlab="Année")
?points
?cor
correlation=cor(synthese$insee_pop,synthese$nombre, use="complete.obs")
correlation2=cor(synthese$nombre,synthese$insee_log)
correlation3=cor(synthese$insee_log,synthese$insee_pop,use="complete.obs")

##histogramme evolution du niveau des homicides

new=select(synthese,Code_region,Libellé,indicateur,nombre,annee)
new_homicides=filter(new,indicateur=="Homicides")
neww_homicides2=new_homicides %>%
  pivot_wider(names_from = "annee", values_from = "nombre")

new_trafic_stup=filter(new,indicateur=="Trafic de stupéfiants")
neww_trafic_stup2=new_trafic_stup %>%
  pivot_wider(names_from = "annee", values_from = "nombre")

new_tentatives_homicides=filter(new,indicateur=="Vols sans violence contre des personnes")
neww_tentatives_homicides2=new_tentatives_homicides %>%
  pivot_wider(names_from = "annee", values_from = "nombre")

hist(neww_homicides2$`2016`,
     breaks=20,main="Evolution du nombre d'homicides",
     xlab='Année', ylab="nombre d'homicides", col='lightgreen')

# Visualiser l'evolution de qUelques indicateurs sur les annees

Annee=2016:2024
total_homicides=c(sum(neww_homicides2$`2016`),
                  sum(neww_homicides2$`2017`),
                  sum(neww_homicides2$`2018`),
                  sum(neww_homicides2$'2019'),
                  sum(neww_homicides2$'2020'),
                  sum(neww_homicides2$'2021'),
                  sum(neww_homicides2$'2022'),
                  sum(neww_homicides2$'2023'),
                  sum(neww_homicides2$'2024'))

homicides_8ans=sum(sum(neww_homicides2$`2016`),
                   sum(neww_homicides2$`2017`),
                   sum(neww_homicides2$`2018`),
                   sum(neww_homicides2$'2019'),
                   sum(neww_homicides2$'2020'),
                   sum(neww_homicides2$'2021'),
                   sum(neww_homicides2$'2022'),
                   sum(neww_homicides2$'2023'),
                   sum(neww_homicides2$'2024'))

total_trafic_stup=c(sum(neww_trafic_stup2$'2016'),
                         sum(neww_trafic_stup2$'2017'),
                         sum(neww_trafic_stup2$'2018'),
                         sum(neww_trafic_stup2$'2019'),
                         sum(neww_trafic_stup2$'2020'),
                         sum(neww_trafic_stup2$'2021'),
                         sum(neww_trafic_stup2$'2022'),
                         sum(neww_trafic_stup2$'2023'),
                         sum(neww_trafic_stup2$'2024'))

total_vols_sans_violences=c(sum(neww_vols_sans_violence2$'2016'),
                    sum(neww_vols_sans_violence2$'2017'),
                    sum(neww_vols_sans_violence2$'2018'),
                    sum(neww_vols_sans_violence2$'2019'),
                    sum(neww_vols_sans_violence2$'2020'),
                    sum(neww_vols_sans_violence2$'2021'),
                    sum(neww_vols_sans_violence2$'2022'),
                    sum(neww_vols_sans_violence2$'2023'),
                    sum(neww_vols_sans_violence2$'2024'))

trafic_stup_totalsur8ans=sum(sum(neww_trafic_stup2$'2016'),
                             sum(neww_trafic_stup2$'2017'),
                             sum(neww_trafic_stup2$'2018'),
                             sum(neww_trafic_stup2$'2019'),
                             sum(neww_trafic_stup2$'2020'),
                             sum(neww_trafic_stup2$'2021'),
                             sum(neww_trafic_stup2$'2022'),
                             sum(neww_trafic_stup2$'2023'),
                             sum(neww_trafic_stup2$'2024'))
filtrage_tentatives_homicides = filter(new,indicateur=="Tentatives d'homicides")
neww_tentatives_homicides2 = filtrage_tentatives_homicides %>%
  pivot_wider(names_from = "annee", values_from = "nombre")

total_tentatives_homicides=c(sum(neww_tentatives_homicides2$'2016'),
                             sum(neww_tentatives_homicides2$'2017'),
                             sum(neww_tentatives_homicides2$'2018'),
                             sum(neww_tentatives_homicides2$'2019'),
                             sum(neww_tentatives_homicides2$'2020'),
                             sum(neww_tentatives_homicides2$'2021'),
                             sum(neww_tentatives_homicides2$'2022'),
                             sum(neww_tentatives_homicides2$'2023'),
                             sum(neww_tentatives_homicides2$'2024'))
tentatives_homicides_totalsur8ans=sum(sum(neww_tentatives_homicides2$'2016'),sum(neww_tentatives_homicides2$'2017'),
                             sum(neww_tentatives_homicides2$'2018'),
                             sum(neww_tentatives_homicides2$'2019'),
                             sum(neww_tentatives_homicides2$'2020'),
                             sum(neww_tentatives_homicides2$'2021'),
                             sum(neww_tentatives_homicides2$'2022'),
                             sum(neww_tentatives_homicides2$'2023'),
                             sum(neww_tentatives_homicides2$'2024'))

data_totaux=data.frame(Annee,
           total_homicides,
           total_trafic_stup,
           total_tentatives_homicides,
           total_vols_sans_violences)

##Evolution nominale sur un diagramme en pointillés
           
plot(data_totaux$Annee,data_totaux$total_homicides,type="l",ylim=c(200,50000),
     pch=1,col="red",
     xlab="Années",
     ylab="Nombre d'incidents")
lines(2016:2024,data_totaux$total_tentatives_homicides, col="gray")
lines(2016:2024,data_totaux$total_trafic_stup, col="green")

plot(2016:2024,neww_homicides2[1,4:12],type="l",ylim=c(0,200),
     xlab="Année",
     ylab="Nombre d'infractions")
lines(2016:2024,neww_homicides2[2,4:12], col="red")
lines(2016:2024,neww_homicides2[3,4:12],col="blue")
lines(2016:2024,neww_homicides2[4,4:12],col="green")
lines(2016:2024,neww_homicides2[5,4:12],col="pink")
lines(2016:2024,neww_homicides2[6,4:12],col="black")
lines(2016:2024,neww_homicides2[7,4:12],col="gray")
lines(2016:2024,neww_homicides2[8,4:12],col="purple")
lines(2016:2024,neww_homicides2[9,4:12],col="brown")
lines(2016:2024,neww_homicides2[10,4:12],col="yellow")
lines(2016:2024,neww_homicides2[11,4:12],col="orange")
lines(2016:2024,neww_homicides2[12,4:12],col="aquamarine")
lines(2016:2024,neww_homicides2[13,4:12],col="navy")
lines(2016:2024,neww_homicides2[14,4:12],col="maroon")
lines(2016:2024,neww_homicides2[15,4:12],col="magenta")
lines(2016:2024,neww_homicides2[16,4:12],col="gold")
lines(2016:2024,neww_homicides2[17,4:12],col="#00abff")
lines(2016:2024,neww_homicides2[18,4:12],col="lavender")

plot(2016:2024,neww_trafic_stup2[1,4:12],type="l",ylim=c(100,16000),
     xlab="Année",
     ylab="")
lines(2016:2024,neww_trafic_stup2[2,4:12], col="red")
lines(2016:2024,neww_trafic_stup2[3,4:12],col="blue")
lines(2016:2024,neww_trafic_stup2[4,4:12],col="green")
lines(2016:2024,neww_trafic_stup2[5,4:12],col="pink")
lines(2016:2024,neww_trafic_stup2[6,4:12],col="black")
lines(2016:2024,neww_trafic_stup2[7,4:12],col="gray")
lines(2016:2024,neww_trafic_stup2[8,4:12],col="purple")
lines(2016:2024,neww_trafic_stup2[9,4:12],col="brown")
lines(2016:2024,neww_trafic_stup2[10,4:12],col="yellow")
lines(2016:2024,neww_trafic_stup2[11,4:12],col="orange")
lines(2016:2024,neww_trafic_stup2[12,4:12],col="aquamarine")
lines(2016:2024,neww_trafic_stup2[13,4:12],col="navy")
lines(2016:2024,neww_trafic_stup2[14,4:12],col="maroon")
lines(2016:2024,neww_trafic_stup2[15,4:12],col="magenta")
lines(2016:2024,neww_trafic_stup2[16,4:12],col="gold")
lines(2016:2024,neww_trafic_stup2[17,4:12],col="#00abff")
lines(2016:2024,neww_trafic_stup2[18,4:12],col="lavender")

part_homicides_iledefrance=sum(neww_homicides2[6,4:12])/homicides_8ans
##faire une phrase avec le pourcentage d'homicides en ile de france
part_trafic_stup_iledefrance=sum(neww_trafic_stup2[6,4:12])/trafic_stup_totalsur8ans
part_tentatives_homicides_iledefrance=sum(neww_tentatives_homicides2[6,4:12])/tentatives_homicides_totalsur8ans

max(neww_trafic_stup2[,4:12])
 
plot(2016:2024,neww_tentatives_homicides2[1,4:12],type="l",ylim=c(0,1000),
     xlab="Année",
     ylab="")
lines(2016:2024,neww_tentatives_homicides2[2,4:12], col="red")
lines(2016:2024,neww_tentatives_homicides2[3,4:12],col="blue")
lines(2016:2024,neww_tentatives_homicides2[4,4:12],col="green")
lines(2016:2024,neww_tentatives_homicides2[5,4:12],col="pink")
lines(2016:2024,neww_tentatives_homicides2[6,4:12],col="black")
lines(2016:2024,neww_tentatives_homicides2[7,4:12],col="gray")
lines(2016:2024,neww_tentatives_homicides2[8,4:12],col="purple")
lines(2016:2024,neww_tentatives_homicides2[9,4:12],col="brown")
lines(2016:2024,neww_tentatives_homicides2[10,4:12],col="yellow")
lines(2016:2024,neww_tentatives_homicides2[11,4:12],col="orange")
lines(2016:2024,neww_tentatives_homicides2[12,4:12],col="aquamarine")
lines(2016:2024,neww_tentatives_homicides2[13,4:12],col="navy")
lines(2016:2024,neww_tentatives_homicides2[14,4:12],col="maroon")
lines(2016:2024,neww_tentatives_homicides2[15,4:12],col="magenta")
lines(2016:2024,neww_tentatives_homicides2[16,4:12],col="gold")
lines(2016:2024,neww_tentatives_homicides2[17,4:12],col="#00abff")
lines(2016:2024,neww_tentatives_homicides2[18,4:12],col="lavender")

legend('topright',legend=paste('Région =',c("Guadeloupe","Martinique","Guyane","La Réunion","Mayotte","Île de France")),
       col=c("red","blue","green","pink","black"),lwd=c(0.5,0.5,0.5,0.5,0.5,0.5))
freq=t(as.matrix(data_totaux[,-1]))
barplot(freq,
        beside=TRUE,
        names.arg = data_totaux$Annee,
        legend.text=row.names(freq),
        xlab = "Année",
        ylab = "Nombre d'incidents",
        args.legend=list(title='Totaux'))

##Evolution homicides, Tentatives d'homicides et trafic de stupefiants V

data_filtered=subset(synthese,indicateur %in% 
                       c("Homicides", "Tentatives d'homicides","Trafic de stupéfiants"))

data_grouped=data_filtered %>%
  group_by(annee, indicateur) %>%
  summarise(nombre = sum(as.numeric(nombre), na.rm = TRUE))
?geom_bar
ggplot(data_grouped, aes(x = as.factor(annee), y = nombre, fill = indicateur)) +
  geom_histogram(stat="identity", position="dodge")
  labs(title="Évolution des homicides, tentatives d'homicides et du trafic de stupéfiants",
       x="Année", y="Nombre de cas") +
  theme_minimal()
