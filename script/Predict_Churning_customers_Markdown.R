---
title: "Predict Churning customers"
author: "Louis Hiel, Rodrique Bernard & Christine Fouque"
date: "26 février 2021"
output: html_document
---
---
    output:
    word_document:reference_docx: "template.docx"
---
library(knitr)

??knitr

```

```

# Objectif: Prédire la résiliation d’un client bancaires
# Kaggle: https://www.kaggle.com/sakshigoyal7/credit-card-customers
# Jeux d’apprentissage : 10 K lignes / 23 colonnes

# TODO : https://www.kaggle.com/josephchan524/bankchurnersclassifier-recall-97-accuracy-95

##################################################################################-
# 1 - Chargement des librairies ####
##################################################################################-

# Enhanced data.frame
if ("data.table" %in% rownames(installed.packages()) == FALSE) {
    install.packages("data.table")
}
library(data.table)
# gestion des dates
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {
    install.packages("lubridate")
}
library(lubridate)
# R Companion to Applied Regression (Functions to Accompany J. Fox and S. Weisberg, AThird Edition, Sage, 2019.)
if ("car" %in% rownames(installed.packages()) == FALSE) {
    install.packages("car")
}
library(car)
# Several functions are available for calculating the most widely used effect sizes (ES),
# along with their variances, confidence intervals and p-values
if ("compute.es" %in% rownames(installed.packages()) == FALSE) {
    install.packages("compute.es")
}
library(compute.es)
# Graphical and tabular effect displays, e.g., of interactions, for various statistical models with linear predictors.
if ("effects" %in% rownames(installed.packages()) == FALSE) {
    install.packages("effects")
}
library(effects)
# Create Elegant Data Visualisations Using the Grammar of Graphics
if ("ggplot2" %in% rownames(installed.packages()) == FALSE) {
    install.packages("ggplot2")
}
library(ggplot2)
# Simultaneous Inference in General Parametric Models
if ("multcomp" %in% rownames(installed.packages()) == FALSE) {
    install.packages("multcomp")
}
library(multcomp)
# Package for Analysis of Space-Time Ecological Series
if ("pastecs" %in% rownames(installed.packages()) == FALSE) {
    install.packages("pastecs")
}
library(pastecs)
# A Collection of Robust Statistical Methods
if ("WRS2" %in% rownames(installed.packages()) == FALSE) {
    install.packages("WRS2")
}
library(WRS2)
# set of packages that work in harmony because they share common data representations and 'API' design.
if ("tidyverse" %in% rownames(installed.packages()) == FALSE) {
    install.packages("tidyverse")
}
library(tidyverse)
if ("dplyr" %in% rownames(installed.packages()) == FALSE) {
    install.packages("dplyr")
}
library(dplyr)
# Regression subset selection, including exhaustive search.
if ("leaps" %in% rownames(installed.packages()) == FALSE) {
    install.packages("leaps")
}
library(leaps)
# Graphes de corrélation
if ("corrplot" %in% rownames(installed.packages()) == FALSE) {
    install.packages("corrplot")
}
library(corrplot)
# lib de graphes avancés
if ("highcharter" %in% rownames(installed.packages()) == FALSE) {
    install.packages("highcharter")
}
library(highcharter)

# pour ajouter une nouvelle librairie
# if("" %in% rownames(installed.packages()) == FALSE) {install.packages("")};library()

# les librairies installées
# library()

##################################################################################-
# 2 - Import des données  ####
##################################################################################-

data <- read.csv("data/BankChurners.csv", sep = ",")
# Vérification des types de champs et des valeurs nulles

summary(data)
# pas de NA

length(unique(data$CLIENTNUM))
# 10127 lignes pour 10127 numeros de compte => pas de doublons

# les types
#str(data)

# Retrait des colonnes inutiles
#   - CLIENTNUM
#   - Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1
#   - Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2
data <- data[, -c(1, 22, 23)]

# modification Attrition_Flag : 0 Existing Customer, 1 Attrited Customer ---
data <-
    data %>% mutate(Attrition_Flag = recode(
        Attrition_Flag,
        "Attrited Customer" = "1",
        "Existing Customer" = "0"
    ))
ls.str(data)

# ---la variable a expliquer :Attrition_Flag ( Factor ) => regression logistique


summary(data)

# 20 colonnes restantes
length (colnames(data))

##################################################################################-
# 3 - Analyse exploratoire des données (EDA)  ####
##################################################################################-

######################## Creation de deux dataframe ##############################
# Séparation des clients :  ceux qui ont quitté la banque de ceux qui sont restés
# Modification de l' Attrition_Flag : 0 Existing Customer, 1 Attrited Customer ---
data <-
    data %>% mutate(Attrition_Flag = recode(
        Attrition_Flag,
        "Attrited Customer" = "1",
        "Existing Customer" = "0"
    ))
# les deux datasets
data_quit <- data[(data$Attrition_Flag) == 1, ] # 1627
data_stay <- data[(data$Attrition_Flag) == 0, ] # 8500

######################## Analyses des variables qualitatives  ##############################
attach(data)
#### Gender ####


# tableaux de frequences ---
unique (Gender)
frequence_Gender <- data.frame(table(Gender, Attrition_Flag))
frequence_Gender
ratio_F <- c("0", "1")
# frequence des femmes qui partent parmi les clients qui partent

ratio_F[1] <-
    round((frequence_Gender$Freq[frequence_Gender$Gender == "F" &
                                     frequence_Gender$Attrition_Flag == "0"]) / (sum(frequence_Gender$Freq[frequence_Gender$Attrition_Flag ==
                                                                                                               "0"])), 3)
#   -parmi les clients qui partent
ratio_F[2]  <-
    round((frequence_Gender$Freq[frequence_Gender$Gender == "F" &
                                     frequence_Gender$Attrition_Flag == "1"]) / (sum(frequence_Gender$Freq[frequence_Gender$Attrition_Flag ==
                                                                                                               "1"])), 3)
(ratio_F)
#  es femmes sont surrepresentées parmi les clients qui résilient : 57 % contre 52 %
#  peut etre parce que ce sont svt les femmes dans un couples sui s'ocuoent de l'administratifs?
# Plot attrition par genre
data_quit %>%
    select(Attrition_Flag, Gender) %>%
    mutate(Gender = ifelse(Gender == "F", "Female", "Male")) %>%
    ggplot(aes(x = Attrition_Flag, fill = Gender)) +
    geom_bar(position = "dodge2") +
    geom_text(
        aes(
            y = (..count..) / sum(..count..),
            label = paste0(round(prop.table(..count..) * 100), '%')
        ),
        stat = 'count',
        size = 3,
        vjust = -4,
        position = position_dodge(.9)
    ) +
    labs(title = "Distribution par genre",
         x = "Attrited Customers", y = "NB")
data_stay %>%
    select(Attrition_Flag, Gender) %>%
    mutate(Gender = ifelse(Gender == "F", "Female", "Male")) %>%
    ggplot(aes(x = Attrition_Flag, fill = Gender)) +
    geom_bar(position = "dodge2") +
    geom_text(
        aes(
            y = (..count..) / sum(..count..),
            label = paste0(round(prop.table(..count..) * 100), '%')
        ),
        stat = 'count',
        size = 3,
        vjust = -4,
        position = position_dodge(.9)
    ) +
    labs(title = "Distribution par genre",
         x = "Existing Customer", y = "NB")
# Marital_Status ----
# unique (Marital_Status)
# Married  Single   Unknown  Divorced

# ratio_Divorced<- c("0","1")
# ratio_Unknown<- c("0","1")
# ratio_Single<- c("0","1")
# ratio_Married <- c("0","1")

frequence_Marital_Status <-
    data.frame(table(Attrition_Flag, Marital_Status))
barplot((table(Attrition_Flag, Marital_Status)))
ratio_Married[1] <-
    round((frequence_Marital_Status$Freq[frequence_Marital_Status$Marital_Status ==
                                             "Married" &
                                             frequence_Marital_Status$Attrition_Flag == "0"]) / (sum(frequence_Marital_Status$Freq[frequence_Marital_Status$Attrition_Flag ==
                                                                                                                                       "0"])), 3)
ratio_Married[2] <-
    round((frequence_Marital_Status$Freq[frequence_Marital_Status$Marital_Status ==
                                             "Married" &
                                             frequence_Marital_Status$Attrition_Flag == "1"]) / (sum(frequence_Marital_Status$Freq[frequence_Marital_Status$Attrition_Flag ==
                                                                                                                                       "1"])), 3)
# > ratio_Married
# "0.468" "0.436" => parmi les mariés on trouve legerement plus de personnes qui restent abonnés ( -3%)


ratio_Single[1] <-
    round((frequence_Marital_Status$Freq[frequence_Marital_Status$Marital_Status ==
                                             "Single" &
                                             frequence_Marital_Status$Attrition_Flag == "0"]) / (sum(frequence_Marital_Status$Freq[frequence_Marital_Status$Attrition_Flag ==
                                                                                                                                       "0"])), 3)
ratio_Single[2] <-
    round((frequence_Marital_Status$Freq[frequence_Marital_Status$Marital_Status ==
                                             "Single" &
                                             frequence_Marital_Status$Attrition_Flag == "1"]) / (sum(frequence_Marital_Status$Freq[frequence_Marital_Status$Attrition_Flag ==
                                                                                                                                       "1"])), 3)
# > ratio_Single
# "0.385" "0.411" => parmi les celibataires, on trouve legerement plus de personnes qui se desabonnent (+2.6 %)


ratio_Unknown[1] <-
    round((frequence_Marital_Status$Freq[frequence_Marital_Status$Marital_Status ==
                                             "Unknown" &
                                             frequence_Marital_Status$Attrition_Flag == "0"]) / (sum(frequence_Marital_Status$Freq[frequence_Marital_Status$Attrition_Flag ==
                                                                                                                                       "0"])), 3)
ratio_Unknown[2] <-
    round((frequence_Marital_Status$Freq[frequence_Marital_Status$Marital_Status ==
                                             "Unknown" &
                                             frequence_Marital_Status$Attrition_Flag == "1"]) / (sum(frequence_Marital_Status$Freq[frequence_Marital_Status$Attrition_Flag ==
                                                                                                                                       "1"])), 3)
# > ratio_Unknown
# "0.073" "0.079" => parmi les statut inconnus on retrouve une proportion tres legerement plus elevée de clients qui partent  (+0.6%)

ratio_Divorced[1] <-
    round((frequence_Marital_Status$Freq[frequence_Marital_Status$Marital_Status ==
                                             "Divorced" &
                                             frequence_Marital_Status$Attrition_Flag == "0"]) / (sum(frequence_Marital_Status$Freq[frequence_Marital_Status$Attrition_Flag ==
                                                                                                                                       "0"])), 3)
ratio_Divorced[2] <-
    round((frequence_Marital_Status$Freq[frequence_Marital_Status$Marital_Status ==
                                             "Divorced" &
                                             frequence_Marital_Status$Attrition_Flag == "1"]) / (sum(frequence_Marital_Status$Freq[frequence_Marital_Status$Attrition_Flag ==
                                                                                                                                       "1"])), 3)
# > ratio_Divorced
# "0.074" "0.074" => parmi les divorcés on retrouve la meme proportion de client s qui partent que ceux qui restent.

#  = > il peut etre interessant de regrouper les classes divorcés et Unknown qui semblent avoir des proportions identiques



# Card_Category ----
# unique (Card_Category)
# Blue Gold Platinum Silver

frequence_Card_Category <-
    data.frame(table(Attrition_Flag, Card_Category))

ratio_Blue <- c("0", "1")
ratio_Gold <- c("0", "1")
ratio_Platinum <- c("0", "1")
ratio_Silver <- c("0", "1")

ratio_Gold[1] <-
    round((frequence_Card_Category$Freq[frequence_Card_Category$Card_Category ==
                                            "Gold" &
                                            frequence_Card_Category$Attrition_Flag == "0"]) / (sum(frequence_Card_Category$Freq[frequence_Card_Category$Attrition_Flag ==
                                                                                                                                    "0"])), 3)
ratio_Gold[2] <-
    round((frequence_Card_Category$Freq[frequence_Card_Category$Card_Category ==
                                            "Gold" &
                                            frequence_Card_Category$Attrition_Flag == "1"]) / (sum(frequence_Card_Category$Freq[frequence_Card_Category$Attrition_Flag ==
                                                                                                                                    "1"])), 3)
# > ratio_Gold
#  "0.011" "0.013" => parmi les clients "carte Gold" peu de différence entre cleints qui partent et qui restent (+0.02%)

ratio_Blue[1] <-
    round((frequence_Card_Category$Freq[frequence_Card_Category$Card_Category ==
                                            "Blue" &
                                            frequence_Card_Category$Attrition_Flag == "0"]) / (sum(frequence_Card_Category$Freq[frequence_Card_Category$Attrition_Flag ==
                                                                                                                                    "0"])), 3)
ratio_Blue[2] <-
    round((frequence_Card_Category$Freq[frequence_Card_Category$Card_Category ==
                                            "Blue" &
                                            frequence_Card_Category$Attrition_Flag == "1"]) / (sum(frequence_Card_Category$Freq[frequence_Card_Category$Attrition_Flag ==
                                                                                                                                    "1"])), 3)
# > ratio_Blue
#  "0.931" "0.934" => parmi les clients "carte Blue" peu de différence entre clients qui partent et qui restent (+0.03%)

ratio_Platinum[1] <-
    round((frequence_Card_Category$Freq[frequence_Card_Category$Card_Category ==
                                            "Platinum" &
                                            frequence_Card_Category$Attrition_Flag == "0"]) / (sum(frequence_Card_Category$Freq[frequence_Card_Category$Attrition_Flag ==
                                                                                                                                    "0"])), 3)
ratio_Platinum[2] <-
    round((frequence_Card_Category$Freq[frequence_Card_Category$Card_Category ==
                                            "Platinum" &
                                            frequence_Card_Category$Attrition_Flag == "1"]) / (sum(frequence_Card_Category$Freq[frequence_Card_Category$Attrition_Flag ==
                                                                                                                                    "1"])), 3)
# > ratio_Platinum
# "0.002" "0.003" => parmi les clients "carte Platinum" peu de différence entre clients qui partent et qui restent (+0.1%)

ratio_Silver[1] <-
    round((frequence_Card_Category$Freq[frequence_Card_Category$Card_Category ==
                                            "Silver" &
                                            frequence_Card_Category$Attrition_Flag == "0"]) / (sum(frequence_Card_Category$Freq[frequence_Card_Category$Attrition_Flag ==
                                                                                                                                    "0"])), 3)
ratio_Silver[2] <-
    round((frequence_Card_Category$Freq[frequence_Card_Category$Card_Category ==
                                            "Silver" &
                                            frequence_Card_Category$Attrition_Flag == "1"]) / (sum(frequence_Card_Category$Freq[frequence_Card_Category$Attrition_Flag ==
                                                                                                                                    "1"])), 3)
# > ratio_Silver
#  "0.056" "0.050"  => parmi les clients "carte Silver" peu de différence entre clients qui partent et qui restent (-0.6 %)

#  la proportion de catégorie de carte ne semble pas etre tres différente entre abonnés et desabonnés

# Income_Category ----
# unique (Income_Category)
# 6 classes : $120K +, $40K - $60K, $60K - $80K, $80K - $120K, Less than $40K, Unknown
frequence_Income_Category <-
    data.frame(table(Attrition_Flag, Income_Category))

ratio_moins40 <- c("0", "1")
ratio_40_60 <- c("0", "1")
ratio_60_80 <- c("0", "1")
ratio_80_120 <- c("0", "1")
ratio_un <- c("0", "1")
ratio_plus120 <- c("0", "1")

ratio_moins40[1] <-
    round((frequence_Income_Category$Freq[frequence_Income_Category$Income_Category ==
                                              "Less than $40K" &
                                              frequence_Income_Category$Attrition_Flag == "0"]) / (sum(frequence_Income_Category$Freq[frequence_Income_Category$Attrition_Flag ==
                                                                                                                                          "0"])), 2)
ratio_moins40[2] <-
    round((frequence_Income_Category$Freq[frequence_Income_Category$Income_Category ==
                                              "Less than $40K" &
                                              frequence_Income_Category$Attrition_Flag == "1"]) / (sum(frequence_Income_Category$Freq[frequence_Income_Category$Attrition_Flag ==
                                                                                                                                          "1"])), 2)
# > ratio_moins40   #  "0.347" "0.376"  parmi les "Less than $40K" la proportion est  plus fortes pour les desabonnés  (+3 %)

ratio_40_60[1] <-
    round((frequence_Income_Category$Freq[frequence_Income_Category$Income_Category ==
                                              "$40K - $60K" &
                                              frequence_Income_Category$Attrition_Flag == "0"]) / (sum(frequence_Income_Category$Freq[frequence_Income_Category$Attrition_Flag ==
                                                                                                                                          "0"])), 3)
ratio_40_60[2] <-
    round((frequence_Income_Category$Freq[frequence_Income_Category$Income_Category ==
                                              "$40K - $60K" &
                                              frequence_Income_Category$Attrition_Flag == "1"]) / (sum(frequence_Income_Category$Freq[frequence_Income_Category$Attrition_Flag ==
                                                                                                                                          "1"])), 3)
# > ratio_40_60     #  "0.179" "0.167" parmi les "$40K - $60K" on observe une proportion legerement plus faible chez les desabonnés (-1.2%)

ratio_60_80[1] <-
    round((frequence_Income_Category$Freq[frequence_Income_Category$Income_Category ==
                                              "$60K - $80K" &
                                              frequence_Income_Category$Attrition_Flag == "0"]) / (sum(frequence_Income_Category$Freq[frequence_Income_Category$Attrition_Flag ==
                                                                                                                                          "0"])), 3)
ratio_60_80[2] <-
    round((frequence_Income_Category$Freq[frequence_Income_Category$Income_Category ==
                                              "$60K - $80K" &
                                              frequence_Income_Category$Attrition_Flag == "1"]) / (sum(frequence_Income_Category$Freq[frequence_Income_Category$Attrition_Flag ==
                                                                                                                                          "1"])), 3)
# > ratio_60_80     #  "0.143" "0.116" parmi les "$60K - $80K" on observe une proportion  plus faible chez les desabonnés (-2.7 %)

ratio_80_120[1] <-
    round((frequence_Income_Category$Freq[frequence_Income_Category$Income_Category ==
                                              "$80K - $120K" &
                                              frequence_Income_Category$Attrition_Flag == "0"]) / (sum(frequence_Income_Category$Freq[frequence_Income_Category$Attrition_Flag ==
                                                                                                                                          "0"])), 3)
ratio_80_120[2] <-
    round((frequence_Income_Category$Freq[frequence_Income_Category$Income_Category ==
                                              "$80K - $120K" &
                                              frequence_Income_Category$Attrition_Flag == "1"]) / (sum(frequence_Income_Category$Freq[frequence_Income_Category$Attrition_Flag ==
                                                                                                                                          "1"])), 3)
# > ratio_80_120    #  "0.152" "0.149" parmi les "$80K - $120K" on observe une proportion legerement plus faible chez les desabonnés (-0.3%)

ratio_plus120[1] <-
    round((frequence_Income_Category$Freq[frequence_Income_Category$Income_Category ==
                                              "$120K +" &
                                              frequence_Income_Category$Attrition_Flag == "0"]) / (sum(frequence_Income_Category$Freq[frequence_Income_Category$Attrition_Flag ==
                                                                                                                                          "0"])), 3)
ratio_plus120[2] <-
    round((frequence_Income_Category$Freq[frequence_Income_Category$Income_Category ==
                                              "$120K +" &
                                              frequence_Income_Category$Attrition_Flag == "1"]) / (sum(frequence_Income_Category$Freq[frequence_Income_Category$Attrition_Flag ==
                                                                                                                                          "1"])), 3)
# > ratio_plus120   #  "0.071" "0.077" parmi les "$120K +", on observe une proportion legerement plus forte chez les desabonnés (+0.6%)

ratio_un[1] <-
    round((frequence_Income_Category$Freq[frequence_Income_Category$Income_Category ==
                                              "Unknown" &
                                              frequence_Income_Category$Attrition_Flag == "0"]) / (sum(frequence_Income_Category$Freq[frequence_Income_Category$Attrition_Flag ==
                                                                                                                                          "0"])), 3)
ratio_un[2] <-
    round((frequence_Income_Category$Freq[frequence_Income_Category$Income_Category ==
                                              "Unknown" &
                                              frequence_Income_Category$Attrition_Flag == "1"]) / (sum(frequence_Income_Category$Freq[frequence_Income_Category$Attrition_Flag ==
                                                                                                                                          "1"])), 3)
# > ratio_un        # "0.109" "0.115" parmi les "Unknown", on observe une proportion legerement plus forte chez les desabonnés (+0.6%)

# => les proportions les plus notables  :
#   - sont chez les plus bas revenus (- 40 K$):  3 % de différence dans la proportion de desabonnés parmi les desabonnés.
# - les revenus moyens ("40-80 K$") :  on observe une différence de 1.2 % et 2.7 % des abonnés parmi l'ensemble des abonnés


# Education_Level ----
# unique (Education_Level)
# College Doctorate Graduate High School Post-Graduate Uneducated Unknown

# Unknown
# Uneducated
# High School
# College
# Graduate
# Post-Graduate
# Doctorate

frequence_Education_Level <-
    data.frame(table(Attrition_Flag, Education_Level))


ratio_College <- c("0", "1")
ratio_Doctorate <- c("0", "1")
ratio_Graduate <- c("0", "1")
ratio_HighSchool <- c("0", "1")
ratio_Post_Graduate <- c("0", "1")
ratio_Uneducated <- c("0", "1")
ratio_Unknown <- c("0", "1")

ratio_Uneducated[1] <-
    round((frequence_Education_Level$Freq[frequence_Education_Level$Education_Level ==
                                              "Uneducated" &
                                              frequence_Education_Level$Attrition_Flag == "0"]) / (sum(frequence_Education_Level$Freq[frequence_Education_Level$Attrition_Flag ==
                                                                                                                                          "0"])), 3)
ratio_Uneducated[2] <-
    round((frequence_Education_Level$Freq[frequence_Education_Level$Education_Level ==
                                              "Uneducated" &
                                              frequence_Education_Level$Attrition_Flag == "1"]) / (sum(frequence_Education_Level$Freq[frequence_Education_Level$Attrition_Flag ==
                                                                                                                                          "1"])), 3)
# > ratio_Uneducated
# "0.147" "0.146" tres légere différence de ratio parmi les "Uneducated", un peu moins de desabonnés (-0.1%)

ratio_HighSchool[1] <-
    round((frequence_Education_Level$Freq[frequence_Education_Level$Education_Level ==
                                              "High School" &
                                              frequence_Education_Level$Attrition_Flag == "0"]) / (sum(frequence_Education_Level$Freq[frequence_Education_Level$Attrition_Flag ==
                                                                                                                                          "0"])), 3)
ratio_HighSchool[2] <-
    round((frequence_Education_Level$Freq[frequence_Education_Level$Education_Level ==
                                              "High School" &
                                              frequence_Education_Level$Attrition_Flag == "1"]) / (sum(frequence_Education_Level$Freq[frequence_Education_Level$Attrition_Flag ==
                                                                                                                                          "1"])), 3)
# > ratio_HighSchool
# "0.201" "0.188" (-1.3) légere différence de ratio parmi les "High School", un peu moins de desabonnés(-1.3%)

ratio_College[1] <-
    round((frequence_Education_Level$Freq[frequence_Education_Level$Education_Level ==
                                              "College" &
                                              frequence_Education_Level$Attrition_Flag == "0"]) / (sum(frequence_Education_Level$Freq[frequence_Education_Level$Attrition_Flag ==
                                                                                                                                          "0"])), 3)
ratio_College[2] <-
    round((frequence_Education_Level$Freq[frequence_Education_Level$Education_Level ==
                                              "College" &
                                              frequence_Education_Level$Attrition_Flag == "1"]) / (sum(frequence_Education_Level$Freq[frequence_Education_Level$Attrition_Flag ==
                                                                                                                                          "1"])), 3)
# > ratio_College
# "0.101" "0.095" (-0.6) tres légere différence de ratio parmi les ""College", un peu moins de desabonnés (-0.6%)

ratio_Graduate[1] <-
    round((frequence_Education_Level$Freq[frequence_Education_Level$Education_Level ==
                                              "Graduate" &
                                              frequence_Education_Level$Attrition_Flag == "0"]) / (sum(frequence_Education_Level$Freq[frequence_Education_Level$Attrition_Flag ==
                                                                                                                                          "0"])), 3)
ratio_Graduate[2] <-
    round((frequence_Education_Level$Freq[frequence_Education_Level$Education_Level ==
                                              "Graduate" &
                                              frequence_Education_Level$Attrition_Flag == "1"]) / (sum(frequence_Education_Level$Freq[frequence_Education_Level$Attrition_Flag ==
                                                                                                                                          "1"])), 3)
# > ratio_Graduate
# "0.311" "0.299" (-1.2) légere différence de ratio parmi les "Graduate", un peu moins de desabonnés (-1.2 %)

ratio_Post_Graduate[1] <-
    round((frequence_Education_Level$Freq[frequence_Education_Level$Education_Level ==
                                              "Post-Graduate" &
                                              frequence_Education_Level$Attrition_Flag == "0"]) / (sum(frequence_Education_Level$Freq[frequence_Education_Level$Attrition_Flag ==
                                                                                                                                          "0"])), 3)
ratio_Post_Graduate[2] <-
    round((frequence_Education_Level$Freq[frequence_Education_Level$Education_Level ==
                                              "Post-Graduate" &
                                              frequence_Education_Level$Attrition_Flag == "1"]) / (sum(frequence_Education_Level$Freq[frequence_Education_Level$Attrition_Flag ==
                                                                                                                                          "1"])), 3)
# > ratio_Post_Graduate
# "0.050"  "0.057" (+0.7) légere différence de ratio parmi les "Post Graduate", un peu plus de desabonnés (+0.7)

ratio_Doctorate[1] <-
    round((frequence_Education_Level$Freq[frequence_Education_Level$Education_Level ==
                                              "Doctorate" &
                                              frequence_Education_Level$Attrition_Flag == "0"]) / (sum(frequence_Education_Level$Freq[frequence_Education_Level$Attrition_Flag ==
                                                                                                                                          "0"])), 3)
ratio_Doctorate[2] <-
    round((frequence_Education_Level$Freq[frequence_Education_Level$Education_Level ==
                                              "Doctorate" &
                                              frequence_Education_Level$Attrition_Flag == "1"]) / (sum(frequence_Education_Level$Freq[frequence_Education_Level$Attrition_Flag ==
                                                                                                                                          "1"])), 3)
# > ratio_Doctorate (+1.6) légere différence de ratio parmi les "Doctorate", un peu plus de desabonnés
# "0.042" "0.058"

ratio_Unknown[1] <-
    round((frequence_Education_Level$Freq[frequence_Education_Level$Education_Level ==
                                              "Unknown" &
                                              frequence_Education_Level$Attrition_Flag == "0"]) / (sum(frequence_Education_Level$Freq[frequence_Education_Level$Attrition_Flag ==
                                                                                                                                          "0"])), 3)
ratio_Unknown[2] <-
    round((frequence_Education_Level$Freq[frequence_Education_Level$Education_Level ==
                                              "Unknown" &
                                              frequence_Education_Level$Attrition_Flag == "1"]) / (sum(frequence_Education_Level$Freq[frequence_Education_Level$Attrition_Flag ==
                                                                                                                                          "1"])), 3)
# > ratio_Unknown
# "0.149" "0.157" (+0.8) légere différence de ratio parmi les "Unknown", un peu plus de desabonnés

#  en fonction du niveau d'etude on voit une legere différence de proportion parmi les abonnées ou les desabonnés.
# Il semble que le niveau d'etude relativement élevé (graduate, post graduate et doctorate) facilite de depart de la banque.
# parmi les " unknox on trouve une tres legere surrepresentation des desabonnés

# --- les histogrammes des varibles qualitatives en fonction Attrition_Flag


boxplot(data$Attrition_Flag ~ data$Gender)
# ### sur le dataset complet (data)
# categ <- data %>%
#     select(Attrition_Flag,Gender, Marital_Status, Card_Category, Income_Category, Education_Level)
#
# # Plot attrition par genre
#
# categ %>%
#     select(Attrition_Flag, Gender) %>%
#     mutate(Gender = ifelse(Gender == "F","Female","Male"))%>%
#     ggplot(aes(x=Attrition_Flag,fill=Gender)) +
#     geom_bar(position="dodge2") +
#     geom_text(aes(y = (..count..)/sum(..count..),
#                   label = paste0(round(prop.table(..count..) * 100), '%')),
#               stat = 'count',
#               size = 3,
#               vjust=-4,
#               position = position_dodge(.9))+
#     labs(title="Distribution par genre",
#          x="Attrition_Flag",y="NB")
#
#
# # Plot par Statut marital
# categ %>%
#     select(Attrition_Flag, Marital_Status) %>%
#     ggplot(aes(x=Attrition_Flag,fill=Marital_Status)) +
#     geom_bar(position="dodge2") +
#     geom_text(aes(y = (..count..)/sum(..count..),
#                   label = paste0(round(prop.table(..count..) * 100), '%')),
#               stat = 'count',
#               size = 3,
#               vjust=-4,
#               position = position_dodge(.9))+
#     labs(title="Distribution par statut marital",
#          x="Attrition_Flag",y="NB")
#
# # Plot par carte bancaire
# categ %>%
#     select(Attrition_Flag, Card_Category) %>%
#     ggplot(aes(x=Attrition_Flag,fill=Card_Category))+
#     geom_bar(position="dodge2") +
#     geom_text(aes(y = (..count..)/sum(..count..),
#                   label = paste0(round(prop.table(..count..) * 100,2), '%')),
#               stat = 'count',
#               size = 3,
#               vjust=-4,
#               position = position_dodge(.9))+
#     labs(title="Distribution par type de CB",
#          x="Attrition_Flag", y="NB")
#
# # Plot par rentrée financiere
# categ %>%
#     select(Attrition_Flag, Income_Category) %>%
#     ggplot(aes(x=Attrition_Flag,fill=Income_Category)) +
#     geom_bar(position="dodge2") +
#     geom_text(aes(y = (..count..)/sum(..count..),
#                   label = paste0(round(prop.table(..count..) * 100,2), '%')),
#               stat = 'count',
#               size = 3,
#               vjust=-4,
#               position = position_dodge(.9))+
#     labs(title="Distribution par rentrée financière",
#          x="Attrition_Flag",y="NB")
#
# # Plot par niveau d'étude
# categ %>%
#     select(Attrition_Flag, Education_Level) %>%
#     ggplot(aes(x=Attrition_Flag,fill=Education_Level)) +
#     geom_bar(position="dodge2") +
#     geom_text(aes(y = (..count..)/sum(..count..),
#                   label = paste0(round(prop.table(..count..) * 100,1), '%')),
#               stat = 'count',
#               size = 3,
#               vjust=-4,
#               position = position_dodge(.9))+
#     labs(title="Distribution par niveau d'étude",
#          x="Attrition_Flag",y="NB")


###  les personnes qui ont resiliées (data_quit)

# Analyse de chaque variable de manière séparée


# Plot par Statut marital
categ %>%
    select(Attrition_Flag, Marital_Status) %>%
    ggplot(aes(x = Attrition_Flag, fill = Marital_Status)) +
    geom_bar(position = "dodge2") +
    geom_text(
        aes(
            y = (..count..) / sum(..count..),
            label = paste0(round(prop.table(..count..) * 100), '%')
        ),
        stat = 'count',
        size = 3,
        vjust = -4,
        position = position_dodge(.9)
    ) +
    labs(title = "Distribution par statut marital",
         x = "Attrition_Flag", y = "NB")

# Plot par carte bancaire
categ %>%
    select(Attrition_Flag, Card_Category) %>%
    ggplot(aes(x = Attrition_Flag, fill = Card_Category)) +
    geom_bar(position = "dodge2") +
    geom_text(
        aes(
            y = (..count..) / sum(..count..),
            label = paste0(round(prop.table(..count..) * 100, 2), '%')
        ),
        stat = 'count',
        size = 3,
        vjust = -4,
        position = position_dodge(.9)
    ) +
    labs(title = "Distribution par type de CB",
         x = "Attrition_Flag", y = "NB")

# Plot par rentrée financiere
categ %>%
    select(Attrition_Flag, Income_Category) %>%
    ggplot(aes(x = Attrition_Flag, fill = Income_Category)) +
    geom_bar(position = "dodge2") +
    geom_text(
        aes(
            y = (..count..) / sum(..count..),
            label = paste0(round(prop.table(..count..) * 100, 2), '%')
        ),
        stat = 'count',
        size = 3,
        vjust = -4,
        position = position_dodge(.9)
    ) +
    labs(title = "Distribution par rentrée financière",
         x = "Attrition_Flag", y = "NB")

# Plot par niveau d'étude
categ %>%
    select(Attrition_Flag, Education_Level) %>%
    ggplot(aes(x = Attrition_Flag, fill = Education_Level)) +
    geom_bar(position = "dodge2") +
    geom_text(
        aes(
            y = (..count..) / sum(..count..),
            label = paste0(round(prop.table(..count..) * 100, 2), '%')
        ),
        stat = 'count',
        size = 3,
        vjust = -4,
        position = position_dodge(.9)
    ) +
    labs(title = "Distribution par niveau d'étude",
         x = "Attrition_Flag", y = "NB")

### Les clients qui restent (data_stay)
##################################################################################

# Analyse de chaque variable de manière séparée
categ <- data_stay %>%
    select(
        Attrition_Flag,
        Gender,
        Marital_Status,
        Card_Category,
        Income_Category,
        Education_Level
    )

# Plot attrition par genre
categ %>%
    select(Attrition_Flag, Gender) %>%
    mutate(Gender = ifelse(Gender == "F", "Female", "Male")) %>%
    ggplot(aes(x = Attrition_Flag, fill = Gender)) +
    geom_bar(position = "dodge2") +
    geom_text(
        aes(
            y = (..count..) / sum(..count..),
            label = paste0(round(prop.table(..count..) * 100), '%')
        ),
        stat = 'count',
        size = 3,
        vjust = -4,
        position = position_dodge(.9)
    ) +
    labs(title = "Distribution par genre",
         x = "Attrition_Flag", y = "NB")

# Plot par Statut marital
categ %>%
    select(Attrition_Flag, Marital_Status) %>%
    ggplot(aes(x = Attrition_Flag, fill = Marital_Status)) +
    geom_bar(position = "dodge2") +
    geom_text(
        aes(
            y = (..count..) / sum(..count..),
            label = paste0(round(prop.table(..count..) * 100), '%')
        ),
        stat = 'count',
        size = 3,
        vjust = -4,
        position = position_dodge(.9)
    ) +
    labs(title = "Distribution par statut marital",
         x = "Attrition_Flag", y = "NB")

# Plot par carte bancaire
categ %>%
    select(Attrition_Flag, Card_Category) %>%
    ggplot(aes(x = Attrition_Flag, fill = Card_Category)) +
    geom_bar(position = "dodge2") +
    geom_text(
        aes(
            y = (..count..) / sum(..count..),
            label = paste0(round(prop.table(..count..) * 100, 2), '%')
        ),
        stat = 'count',
        size = 3,
        vjust = -4,
        position = position_dodge(.9)
    ) +
    labs(title = "Distribution par type de CB",
         x = "Attrition_Flag", y = "NB")

# Plot par rentrée financiere
categ %>%
    select(Attrition_Flag, Income_Category) %>%
    ggplot(aes(x = Attrition_Flag, fill = Income_Category)) +
    geom_bar(position = "dodge2") +
    geom_text(
        aes(
            y = (..count..) / sum(..count..),
            label = paste0(round(prop.table(..count..) * 100, 2), '%')
        ),
        stat = 'count',
        size = 3,
        vjust = -4,
        position = position_dodge(.9)
    ) +
    labs(title = "Distribution par rentrée financière",
         x = "Attrition_Flag", y = "NB")

# Plot par niveau d'étude
categ %>%
    select(Attrition_Flag, Education_Level) %>%
    ggplot(aes(x = Attrition_Flag, fill = Education_Level)) +
    geom_bar(position = "dodge2") +
    geom_text(
        aes(
            y = (..count..) / sum(..count..),
            label = paste0(round(prop.table(..count..) * 100, 2), '%')
        ),
        stat = 'count',
        size = 3,
        vjust = -4,
        position = position_dodge(.9)
    ) +
    labs(title = "Distribution par niveau d'étude",
         x = "Attrition_Flag", y = "NB")

# VARIABLES QUANTITATIVES
########################

# ???? TEST DE STUDENT ????
# pour comparer des groupes dont les effectifs sont différentS : comparaison de moyenne
# => test de student : http://www.sthda.com/french/wiki/test-de-student-est-il-toujours-correct-de-comparer-des-moyennes
# source('http://www.sthda.com/upload/rquery_t_test.r')
# rquery.t.test(x, y = NULL, paired = FALSE, graph = TRUE, ...)
# exemple : rquery.t.test(data_quit$Avg_Open_To_Buy, data_stay$Avg_Open_To_Buy)
# Error in shapiro.test(x) : sample size must be between 3 and 5000
# ??????????????????????????????????????????????????????????????????????????????????


#--- les 14 variables quantitatives
# 1- Months_on_book          : int
hist(Months_on_book)
boxplot(Months_on_book ~ Attrition_Flag)


barplot(table(data$Months_on_book[data$Attrition_Flag == 0]))
barplot(table(data$Months_on_book[data$Attrition_Flag == 1]))

# 2- Total_Relationship_Count: int
hist(Months_on_book)
barplot(Contacts_Count_12_mon)
boxplot(Months_on_book ~ Attrition_Flag)
# 3- Months_Inactive_12_mon  : int



# 4- Contacts_Count_12_mon   : int
hist(Contacts_Count_12_mon)
boxplot(Contacts_Count_12_mon ~ Attrition_Flag)

# 5- Credit_Limit            : num
hist(Credit_Limit)
boxplot(Credit_Limit ~ Attrition_Flag)
# 6- Total_Revolving_Bal     : int
hist(Total_Revolving_Bal)
boxplot(Total_Revolving_Bal ~ Attrition_Flag)
# 7- Avg_Open_To_Buy         : num
hist(Avg_Open_To_Buy)
boxplot(Avg_Open_To_Buy ~ Attrition_Flag)
# 8- Total_Amt_Chng_Q4_Q1    : num
hist(Total_Amt_Chng_Q4_Q1)
boxplot(Total_Amt_Chng_Q4_Q1 ~ Attrition_Flag)
# 9- Total_Trans_Amt         : int
hist(Total_Trans_Ct)
boxplot(Total_Trans_Ct ~ Attrition_Flag)
# 10- Total_Trans_Ct          : int
hist(Total_Trans_Ct)
boxplotTotal_Trans_Ct( ~ Attrition_Flag)
# 11- Total_Ct_Chng_Q4_Q1     : num
hist(Total_Ct_Chng_Q4_Q1)
boxplot(Total_Ct_Chng_Q4_Q1 ~ Attrition_Flag)
# 12- Avg_Utilization_Ratio   : num
hist(Avg_Utilization_Ratio)
boxplot(Avg_Utilization_Ratio ~ Attrition_Flag)
# 13- Customer_Age            : int
hist(Customer_Age)
boxplot(Customer_Age ~ Attrition_Flag)
# 14- Dependent_count         : int
hist(Dependent_count)
boxplot(Dependent_count ~ Attrition_Flag)

# ---- les graphiques
# Transaction Count
pTotal_Trans_Ct <-
    data %>% ggplot(aes(x = Total_Trans_Ct, fill = Attrition_Flag)) +
    geom_density(alpha = 0.7) +
    scale_fill_manual(values = c('#3C3838', '#338076'), name = '') +
    theme_classic() +
    labs(title = 'Total Transaction Count (Last 12 months) by flag') +
    theme(legend.position = 'bottom')
pTotal_Trans_Ct

pContacts_Count_12_mon <-
    data %>% ggplot(aes(x = Contacts_Count_12_mon, fill = Attrition_Flag)) +
    geom_density(alpha = 0.7) +
    scale_fill_manual(values = c('#3C3838', '#338076'), name = '') +
    theme_classic() +
    labs(title = 'Total Transaction Count (Last 12 months) by flag') +
    theme(legend.position = 'bottom')
pContacts_Count_12_mon





# ---- modification des classes lorsqu'il y a des trop fort desequilibres

#CLASSE
data_select <- data
#classe_marital_class
data_select[which(data_select$Marital_Status %in% c("Divorced", "Single")), "Marital_Status"] <-
    "Single"
#Card_category_class
data_select[which(data_select$Card_Category %in% c("Gold", "platinum", "Silver")), "Card_Category"] <-
    "Others"
#Income_category_class
data_select[which(data_select$Income_Category %in% c("Less than $40K", "$40K - $60K")), "Income_Category"] <-
    "Less than $60K"
data_select[which(data_select$Income_Category %in% c("$60K - $80K", "$80K - $120K", "$120K +")), "Income_Category"] <-
    "More than $60K"
# Warning message:   invalid factor level, NA generated


# ---- Etude de la corelation


# Graphique I des corrélations entre chacune des variables
# data %>% select(where(is.numeric)) %>%
#     as.matrix() %>%
#     cor() %>%
#     corrplot(method = "number", type="lower")

# Graphique II des corrélations entre chacune des variables avec la méthode de spearman
cor_spearman <-
    cor(data[, sapply(data, is.numeric)], method = 'spearman')

# Visualizing with a heatmap the correlation matrix with the pearson method
as.matrix(data.frame(cor_spearman)) %>%
    round(3) %>% #round
    hchart() %>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_title(text = "Spearman's correlation coefficients", align = "center") %>%
    hc_legend(align = "center") %>%
    hc_colorAxis(stops = color_stops(colors = viridis::inferno(10))) %>%
    hc_plotOptions(series = list(boderWidth = 0,
                                 dataLabels = list(enabled = TRUE)))


#----Faire des clusters (Kmeans /clusters de clients sui partent vs qui restent)
# https://www.datanovia.com/en/fr/blog/visualisation-du-clustering-k-means-dans-r-guide-etape-par-etape/

#----faire une AFC
# http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/74-afc-analyse-factorielle-des-correspondances-avec-r-l-essentiel/

#  pour nous aider a trouver les perimetres de nos modeles (il y en aura surement pls)







# variable dependante Y = départ ou non : attrition_flag
# variable qualitative donc on fera une régression logistique





# variable explicatives
# age, genre, niveau education, statut marital, personnes à charge, revenu, type de carte, période de relation avec la banque...


# model_quali<-glm(Attrition_Flag~Customer_Age, data=data, family= binomial(logit))
#
# # Interprétation
# model_quali
# summary(model_quali)
# exp(coef(model_quali))
#
# # Matrice de confusion
# appren.p <- cbind(data_reg, predict(model_quali, newdata = data_reg, type = "link",
#                                     se = TRUE))
# appren.p <- within(appren.p, {
#     PredictedProb <- plogis(fit)
#     LL <- plogis(fit - (1.96 * se.fit))
#     UL <- plogis(fit + (1.96 * se.fit))
# })
# appren.p <- cbind(appren.p, pred.chd = factor(ifelse(appren.p$PredictedProb > 0.5, 1, 0)))
# colnames(appren.p)
# appren.p<-appren.p[,c("Winner","diff_dist_att","diff_dist_att_class","fit","PredictedProb","pred.chd")]
# (m.confusion <- as.matrix(table(appren.p$pred.chd, appren.p$Winner)))
#
# # Taux de bien classé
# (m.confusion[1,1]+m.confusion[2,2]) / sum(m.confusion)
#
# # Sensibilité
# (m.confusion[2,2]) / (m.confusion[2,2]+m.confusion[1,2])
#
# # Sensibilité
# (m.confusion[2,2]) / (m.confusion[2,2]+m.confusion[1,2])
#
# # Spécificité
# (m.confusion[1,1]) / (m.confusion[1,1]+m.confusion[2,1])
#
#
# # ODs ratio
# exp(cbind(coef(model_quali), confint(model_quali)))
# library(questionr)
# odds.ratio(model_quali)
# install.packages("GGally")
# library(GGally)
# library(broom.helpers)
# ggcoef_model(model_quali, exponentiate = TRUE)


# Graphique des corrélations entre chacune des variables
# mutates
# data_corr <- data_clean %>% mutate_if(is.factor, as.numeric)
# data_colnames <- colnames(data_clean)
# data_colnames
# # compute correlation
# correlation= cor(data_corr)
# # correlation as data.frame
# target_corr= as.data.frame(correlation[,1])
# rownames(target_corr)<-data_colnames
# # correlation column name
# colnames(target_corr) <-'Correlation'
# # sort dataframe
# target_corr <- target_corr %>% arrange(desc(Correlation))
# # exclude target
# target_corr <- target_corr %>% filter(Correlation<1)
# # round
# target_corr <- round(target_corr,2)
# target_corr %>% arrange(desc(Correlation)) %>%
#     ggplot(aes(x=Correlation,
#                y=reorder(rownames(target_corr), Correlation),
#                fill=Correlation)) +
#     geom_col(color='black') + labs(title='Target Correlation', y='Variables') +
#     theme_classic() +
#     theme(legend.position = 'none')
