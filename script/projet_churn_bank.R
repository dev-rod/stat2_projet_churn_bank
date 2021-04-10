# Objectif: Prédire la résiliation d’un client bancaire
# Kaggle: https://www.kaggle.com/sakshigoyal7/credit-card-customers
# Jeux d’apprentissage : 10 K lignes / 23 colonnes

# TODO : https://www.kaggle.com/josephchan524/bankchurnersclassifier-recall-97-accuracy-95

##################################################################################
# 1 - Chargement des librairies ####
##################################################################################-
# ```{r}
# data.frame amélioré
if ("data.table" %in% rownames(installed.packages()) == FALSE) {
install.packages("data.table", dependencies=TRUE)
}
library(data.table)
# gestion des dates
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {
    install.packages("lubridate", dependencies=TRUE)
}
library(lubridate)
# R Companion to Applied Regression (Functions to Accompany J. Fox and S. Weisberg, AThird Edition, Sage, 2019.)
if ("car" %in% rownames(installed.packages()) == FALSE) {
    install.packages("car", dependencies=TRUE)
}
library(car)
# Several functions are available for calculating the most widely used effect sizes (ES),
# along with their variances, confidence intervals and p-values
if ("compute.es" %in% rownames(installed.packages()) == FALSE) {
    install.packages("compute.es", dependencies=TRUE)
}
library(compute.es)
# Graphical and tabular effect displays, e.g., of interactions, for various statistical models with linear predictors.
if ("effects" %in% rownames(installed.packages()) == FALSE) {
    install.packages("effects", dependencies=TRUE)
}
library(effects)
# Create Elegant Data Visualisations Using the Grammar of Graphics
if ("ggplot2" %in% rownames(installed.packages()) == FALSE) {
    install.packages("ggplot2", dependencies=TRUE)
}
library(ggplot2)
# Simultaneous Inference in General Parametric Models
if ("multcomp" %in% rownames(installed.packages()) == FALSE) {
    install.packages("multcomp" ,dependencies=TRUE)
}
library(multcomp)
# Package for Analysis of Space-Time Ecological Series
if ("pastecs" %in% rownames(installed.packages()) == FALSE) {
    install.packages("pastecs", dependencies=TRUE)
}
library(pastecs)
# A Collection of Robust Statistical Methods
if ("WRS2" %in% rownames(installed.packages()) == FALSE) {
    install.packages("WRS2", dependencies=TRUE)
}
library(WRS2)
# set of packages that work in harmony because they share common data representations and 'API' design.
if ("tidyverse" %in% rownames(installed.packages()) == FALSE) {
    install.packages("tidyverse", dependencies=TRUE)
}
library(tidyverse)

if ("dplyr" %in% rownames(installed.packages()) == FALSE) {
     install.packages("dplyr", dependencies=TRUE)
 }

library(dplyr)
# Regression subset selection, including exhaustive search.
if ("leaps" %in% rownames(installed.packages()) == FALSE) {
    install.packages("leaps", dependencies=TRUE)
}
library(leaps)
# Graphes de corrélation
if ("corrplot" %in% rownames(installed.packages()) == FALSE) {
    install.packages("corrplot",dependencies=TRUE)
}
library(corrplot)
# lib de graphes avancés
if ("highcharter" %in% rownames(installed.packages()) == FALSE) {
    install.packages("highcharter",dependencies=TRUE)
}
library(highcharter)

# Tools for reordering and modifying factor levels  with Categorical Variables (Factors)
if("forcats" %in% rownames(installed.packages()) == FALSE) {install.packages("forcats",dependencies=TRUE)};library(forcats)
# 
if("skimr" %in% rownames(installed.packages()) == FALSE) {install.packages("skimr",dependencies=TRUE)};library(skimr)
# stats

# créer des graphiques prêts à être publiés
if("ggpubr" %in% rownames(installed.packages()) == FALSE) {install.packages("ggpubr",dependencies=TRUE)};library(ggpubr)
# Extraire et visualiser les résultats d’analyses de données multivariées
if("factoextra" %in% rownames(installed.packages()) == FALSE) {install.packages("factoextra",dependencies=TRUE)};library(factoextra)
# Surrogate Residuals for Ordinal and General Regression Models
if("sure" %in% rownames(installed.packages()) == FALSE) {install.packages("sure",dependencies=TRUE)};library(sure)
# Fonctions diverses pour les graphiques "Grid"(grilles)
if("gridExtra" %in% rownames(installed.packages()) == FALSE) {install.packages("gridExtra",dependencies=TRUE)};library(gridExtra)

# pour installer un nouveau package
# install.packages("package_name", repos=c("http://rstudio.org/_packages",
                                         # "http://cran.rstudio.com",dependencies=TRUE))

# pour le probleme de CONFLIT de select() entre les deux librairies MASS et dplyr
require(MASS)
require(dplyr)
# puis utiliser dplyr::select() pour utiliser le select() de la librairie dplyr. Par defaut c'est le select de la  la librairie MASS

# pour ajouter une nouvelle librairie 
# if("" %in% rownames(installed.packages()) == FALSE) {install.packages("")};library()

# Pour voir les librairies installées
# library()

##################################################################################-
# 2 - Import des données  ####
##################################################################################-
data <- read.csv("data/BankChurners.csv", sep = ",")

#####  Retrait des colonnes inutiles pour notre étude
#   - CLIENTNUM
#   - Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1
#   - Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2
data <- data[, -c(1, 22, 23)]

##### 20 colonnes restantes
length (colnames(data))

##### Vérification des valeurs nulles
summary(data)
# pas de valeurs nulles (NA)

##### Vérification des doublons
length(unique(data$CLIENTNUM))
# 10127 lignes pour 10127 numeros de compte => pas de doublons

##### Vérification des types de champs 
str(data)

##### la variable explicative : Attrition_Flag ( Factor )
# C'est une vartiable qualitative, le modele a utiliser est donc une regression logistique=> regression logistique
summary(data)
print (skim(data))

# ---les  variables qualitatives
# 1 Card_Category           : Factor
# 2 Gender                  : Factor
# 3 Education_Level         : Factor
# 4 Marital_Status          : Factor
# 5 Income_Category         : Factor

#--- les 14 variables quantitatives
# 1- Months_on_book          : int
# 2- Total_Relationship_Count: int
# 3- Months_Inactive_12_mon  : int
# 4- Contacts_Count_12_mon   : int
# 5- Credit_Limit            : num
# 6- Total_Revolving_Bal     : int
# 7- Avg_Open_To_Buy         : num
# 8- Total_Amt_Chng_Q4_Q1    : num
# 9- Total_Trans_Amt         : int
# 10- Total_Trans_Ct         : int
# 11- Total_Ct_Chng_Q4_Q1    : num
# 12- Avg_Utilization_Ratio  : num
# 13- Customer_Age           : int
# 14- Dependent_count        : int

##################################################################################-
# 3 - Analyse exploratoire des données (EDA)  ####
##################################################################################-

### 3.1 - creation de deux dataframe ##############################
### Séparation des clients :  ceux qui ont quitté la banque de ceux qui sont restés

####  modification Attrition_Flag : 0 Existing Customer, 1 Attrited Customer ----
data$Attrition_Flag<-as.character(data$Attrition_Flag)
data$Attrition_Flag[data$Attrition_Flag=="Existing Customer"]<-0
data$Attrition_Flag[data$Attrition_Flag=="Attrited Customer"]<-1
data$Attrition_Flag <- as_factor(data$Attrition_Flag)

#### Conversion des variables qualitatives facteurs et réordonnancement des niveaux de facon plus coherente ----

# Attrition_Flag
data$Attrition_Flag <- as_factor(data$Attrition_Flag)
# Gender
data$Gender <- as_factor(data$Gender)
# Education_Level
data$Education_Level <- as_factor(data$Education_Level)
# réordonnancement par niveaux de diplome croissant
data$Education_Level <- fct_relevel(data$Education_Level, "Unknown", "Uneducated", "High School", "College", "Graduate", "Post-Graduate", "Doctorate")
# Marital_Status
data$Marital_Status <- as_factor(data$Marital_Status)
# réordonnancement par statut marital
data$Marital_Status <- fct_relevel(data$Marital_Status, "Unknown", "Single", "Married", "Divorced")
# Income_Category 
data$Income_Category <- as_factor(data$Income_Category)
# réordonnancement par niveaux de revenu croissant
data$Income_Category <- fct_relevel(data$Income_Category, "Unknown", "Less than $40K", "$40K - $60K", "$60K - $80K", "$80K - $120K", "$120K +")
# Card_Category
data$Card_Category <- as_factor(data$Card_Category)
# réordonnancement par categrie de carte de credit croissant
data$Card_Category <- fct_relevel(data$Card_Category, "Blue", "Silver", "Gold", "Platinum")

# verif :
# str(data)
# skim(data)

#### les deux datasets ----
data_quit <- data[(data$Attrition_Flag) == 1, ]
skim(data_quit)
str(data_quit)
# 1627 obs.

data_stay <- data[(data$Attrition_Flag) == 0, ]
skim(data_stay)
str(data_stay)
# 8500 obs.

### 3.1 - Analyses des 5 variables qualitatives ### ----

#  attacher "data" c'est a dire eviter de retaper data$ au debut de chaque variable  
attach(data)

#### Gender ####

##### tableaux de frequences par Attriction_Flag----

#  les modalités
unique (Gender)

#  tableau de contingences
frequence_Gender <-
    data.frame(table(Attrition_Flag, Gender))

# les vecteurs contenant les proportions selon l'AttrictionFlag
ratio_F <- c("0", "1")
ratio_F[1] <-
    round((frequence_Gender$Freq[frequence_Gender$Gender == "F" &
                                     frequence_Gender$Attrition_Flag == "0"]) / (sum(frequence_Gender$Freq[frequence_Gender$Attrition_Flag ==
                                                                                                              "0"])), 3)
ratio_F[2]  <-
    round((frequence_Gender$Freq[frequence_Gender$Gender == "F" &
                                     frequence_Gender$Attrition_Flag == "1"]) / (sum(frequence_Gender$Freq[frequence_Gender$Attrition_Flag ==
                                                                                                               "1"])), 3)
##### Plot attrition par genre ----

# pour les desabonnements(Attrited Customers )
require(MASS)
require(dplyr)
# mtcars %>%
#     dplyr::select(mpg)

data_quit %>%
    dplyr::select(Attrition_Flag, Gender) %>%
    mutate (Gender = ifelse(Gender == "F", "Female", "Male")) %>%
    ggplot(aes(x = Attrition_Flag, fill = Gender)) +
    geom_bar(position = "dodge2")+
    geom_text(
        aes(
            y = (..count..) / sum(..count..),
            label = paste0(round(prop.table(..count..) * 100), '%')
        ),
        stat = 'count',
        size = 3,
        vjust = -4,
        position = position_dodge(.9))+
    labs(title = "Distribution par genre",
         x = "Attrited Customers", y = "NB")

# pour les clients (Existing Customer)
data_stay %>%
    dplyr::select(Attrition_Flag, Gender) %>%
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

##### Resultats variable Gender ----
    # parmi Existing Customer "0.521" soit 52.1 % 
    # Attrited Customers : "0.572" soit 57.2 %
    # 57 % des personnes aant fermées leur compte sont des femmes tandis que 52 % des clients de la 
    # sont des femmes. Les femmes sont donc en sur representation parmi les desabonnés avec une différence de proportion de 5%.


#### Marital_Status #### 
##### frequences des modalités en fonction de l'Attrition_Flag ----

# Les modalités
# unique (Marital_Status)
# Married  Single   Unknown  Divorced

# tableau de contingences
frequence_Marital_Status <-
    data.frame(table(Attrition_Flag, Marital_Status))

# les vecteurs pour les ratio par modalités
ratio_Divorced<- c("0","1")
ratio_Unknown<- c("0","1")
ratio_Single<- c("0","1")
ratio_Married <- c("0","1")

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

# "0.468" "0.436" 

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
# "0.385" "0.411" 

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
# "0.073" "0.079"

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
# "0.074" "0.074" 
 
##### Plot attrition par statut marital ----

# pour les desabonnements(Attrited Customers )
data_quit %>%
    dplyr::select(Attrition_Flag, Marital_Status) %>%
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
         x = "Attrited Customers", y = "NB")

#  pour les clients
data_stay %>%
    dplyr::select(Attrition_Flag, Marital_Status) %>%
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
         x = "Existing Custumers", y = "NB")

##### Les resultats Marital_Status----
# > ratio_Divorced
# "0.074" "0.074" => parmi les divorcés on retrouve la meme proportion de client s qui partent que ceux qui restent.
# > ratio_Unknown
# "0.073" "0.079" => parmi les statut inconnus on retrouve une proportion tres legerement plus elevée de clients qui partent  (+0.6%)
# => peu de différence de proportion pour les divorcés et les statuts inconnus.
# > ratio_Single
# "0.385" "0.411" => parmi les celibataires, on trouve legerement plus de personnes qui se desabonnent (+2.6 %)
# > ratio_Married
# "0.468" "0.436" => parmi les mariés on trouve legerement plus de personnes qui restent abonnés ( -3%)
#  = > il peut etre interessant de regrouper les classes divorcés et Unknown qui semblent avoir des proportions identiques

#### Card_Category  ----

#####frequences des modalités en fonction de l'Attrition_Flag 

# Les modalités
# unique (Card_Category)
# Blue Gold Platinum Silver

# tableau de contingences
frequence_Card_Category <-
    data.frame(table(Attrition_Flag, Card_Category))

# vecteurs pour les ratio en fonction du types de carte de credit 

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

##### Plot attrition par types de cartes de crédits ----

# pour les desabonnements(Attrited Customers )

data_quit %>%
    dplyr::select(Attrition_Flag, Card_Category) %>%
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
         x = "Attrited Customers", y = "NB")
#  pour les clients 
data_stay %>%
    dplyr::select(Attrition_Flag, Card_Category) %>%
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
         x = "Existing custumes", y = "NB")

##### Les resultats par Card_Category---- 
#  la proportion de catégorie de carte ne semble pas etre tres différente entre abonnés et desabonnés
#  On observe pour ratio_Silver la plus grosse différence de pourcantage dans les deux groupes est de 0.6 %.
# > ratio_Gold
#  "0.011" "0.013" => parmi les clients "carte Gold" peu de différence entre cleints qui partent et qui restent (+0.02%)
# > ratio_Blue
#  "0.931" "0.934" => parmi les clients "carte Blue" peu de différence entre clients qui partent et qui restent (+0.03%)
# > ratio_Platinum
# "0.002" "0.003" => parmi les clients "carte Platinum" peu de différence entre clients qui partent et qui restent (+0.1%)
# > ratio_Silver
#  "0.056" "0.050"  => parmi les clients "carte Silver" peu de différence entre clients qui partent et qui restent (-0.6 %)

#### Income_Category ----

##### les frequences de chaque classe en fonction de l'Attrition_Flag

#  les modalités
# unique (Income_Category)
# 6 classes : $120K +, $40K - $60K, $60K - $80K, $80K - $120K, Less than $40K, Unknown

#  tableau de contingeences
frequence_Income_Category <-
    data.frame(table(Attrition_Flag, Income_Category))

#  vecteurs des ratios

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




##### Plot attrition par classe de revenus ----

data %>% ggplot(aes(x=income_category, y=pct, color= attrition_flag))

# pour les desabonnements(Attrited Customers )
data_quit %>%
    dplyr::select(Attrition_Flag, Income_Category) %>%
    ggplot(aes(x=Attrition_Flag,fill=Income_Category)) +
    geom_bar(position="dodge2") +
    geom_text(aes(y = (..count..)/sum(..count..),
                  label = paste0(round(prop.table(..count..) * 100,2), '%')),
              stat = 'count',
              size = 3,
              vjust=-4,
              position = position_dodge(.9))+
    labs(title="Distribution par rentrée financière",
         x="Attrited Customers",y="NB")

#  pour les clients 
data_stay %>%
    dplyr::select(Attrition_Flag, Income_Category) %>%
    ggplot(aes(x=Attrition_Flag,fill=Income_Category)) +
    geom_bar(position="dodge2") +
    geom_text(aes(y = (..count..)/sum(..count..),
                  label = paste0(round(prop.table(..count..) * 100,2), '%')),
              stat = 'count',
              size = 3,
              vjust=-4,
              position = position_dodge(.9))+
    labs(title="Distribution par rentrée financière",
         x="Existing custumers",y="NB")

##### Les resultats par classe de revenus  ----
# => les proportions les plus notables  :
#   - sont chez les plus bas revenus (- 40 K$):  3 % de différence dans la proportion de desabonnés parmi les desabonnés.
# - les revenus moyens ("40-80 K$") :  on observe une différence de 1.2 % et 2.7 % des abonnés parmi l'ensemble des abonnés


#### Education_Level ----
#####frequences des modalités en fonction de l'Attrition_Flag 
#  les modalités
# unique (Education_Level)
# College Doctorate Graduate High School Post-Graduate Uneducated Unknown

# Dans l'ordre du niveau d'etude
    # Unknown
    # Uneducated
    # High School
    # College
    # Graduate
    # Post-Graduate
    # Doctorate

#  tableau de contingences
frequence_Education_Level <-
    data.frame(table(Attrition_Flag, Education_Level))

#  les vecteurs pour les ratios par modalités

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

##### Plot attrition par niveau d'education ----

# pour les desabonnements(Attrited Customers )
par(mfrow=c(1,2))
data_quit %>%
        dplyr::select(Attrition_Flag, Education_Level) %>%
        ggplot(aes(x=Attrition_Flag,fill=Education_Level)) +
        geom_bar(position="dodge2") +
        geom_text(aes(y = (..count..)/sum(..count..),
                      label = paste0(round(prop.table(..count..) * 100,1), '%')),
                  stat = 'count',
                  size = 3,
                  vjust=-4,
                  position = position_dodge(.9))+
        labs(title="Distribution par niveau d'étude",
             x="Attrited Customers",y="NB")

#  pour les clients 
data_stay %>%
        dplyr::select(Attrition_Flag, Education_Level) %>%
        ggplot(aes(x=Attrition_Flag,fill=Education_Level)) +
        geom_bar(position="dodge2") +
        geom_text(aes(y = (..count..)/sum(..count..),
                      label = paste0(round(prop.table(..count..) * 100,1), '%')),
                  stat = 'count',
                  size = 3,
                  vjust=-4,
                  position = position_dodge(.9))+
        labs(title="Distribution par niveau d'étude",
             x="Existing Custumers",y="NB")

#####Les resultats par niveau d'education (Education_Level) ----
#  en fonction du niveau d'etude on voit une legere différence de proportion parmi les abonnées ou les desabonnés.
# Il semble que le niveau d'etude relativement élevé (graduate, post graduate et doctorate) facilite de depart de la banque.
# parmi les " unknow on trouve une tres legere surrepresentation des desabonnés

# ---------------------------------------------------
### 3.3 LES 14 VARIABLES QUANTITATIVES ----

#### Echantillonnage avec 1000 obs de chaque population (1000 restant et 1000 partant) ----
sample_quit<-sample(1:dim(data_quit)[1],1000)
sample_Stay<-sample(1:dim(data_stay)[1],1000)
data_reg<-rbind(data_quit[sample_quit,],data_stay[sample_Stay,])
skim(data_reg)

#### les deux datasets avec 1000 obs ----
# data_reg_quit
data_reg_quit <- data_reg[(data_reg$Attrition_Flag) == 1, ]
skim(data_reg_quit)
str(data_reg_quit)
# data_reg_stay 
data_reg_stay <- data_reg[(data_reg$Attrition_Flag) == 0, ]
skim(data_reg_stay)
str(data_reg_stay)

#### COMPARAISON DES DEUX POPULATIONS
# les effectifs sont différentS on procede par comparaison de moyenne => Test de Student ou de Wilcoxon
# La fonction rquery_t_test.r permet de verifier si toutes les conditions sont remplies pour le test de Student
# si les deux populations ne suivent pas la distribution normales alors il est indiqué qu 'il faut faire le test de wilcoxon
# source('http://www.sthda.com/upload/rquery_t_test.r')
# rquery.t.test(x, y = NULL, paired = FALSE, graph = TRUE, ...)
# STUDENT  : Comparaison des moyennes des deux populations via la fonction rquery_t_test 
source('http://www.sthda.com/upload/rquery_t_test.r')


### Pour chaque variables quantitatives  :----
# + analyse visuelle : 
    # histogramme de chaque population
    # boxplot variable en fonction de Attrition_Flag
    # plot de la variable par attrtion_flag
# + statistiques descriptives des deux echantillons: Min.,1st Qu.,Median,Mean,3rd Qu.,Max.
# + rquery_t_test  => qui donne le resultat du test si l ensemble des critères est respecté
# + test wilcoxon si l un des deux echantillons ne respecte pas la loi normale
 
#### 01 Customer_Age ----

# analyse visuelle :
    # histogramme de chaque population
hist(data_reg_stay$Customer_Age)
hist(data_reg_quit$Customer_Age)
    #boxplot  Customer_Age selon l Attrition_Flag
boxplot(Customer_Age ~ Attrition_Flag)
    # plot de Customer_Age par attrition_flag
plot_Customer_Age <-
    data %>% ggplot(aes(x = Customer_Age, fill = Attrition_Flag)) +
    geom_density(alpha = 0.7) +
    scale_fill_manual(values = c('black', 'white'), name ='') +
    theme_classic() +
    labs(title = 'Customer_Age by Attrition_Flag') +
    theme(legend.position = 'bottom')
plot_Custumer_Age

# stat descriptives
summary(data_stay$Customer_Age)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    # 26.00   41.00   46.00   46.26   52.00   73.00 
summary(data_quit$Customer_Age)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    # 26.00   41.00   47.00   46.66   52.00   68.00 
#  variances
var(data_stay$Customer_Age)
    # 65.30509
var(data_quit$Customer_Age)
    # 58.76221
# STUDENT
rquery.t.test(data_reg_quit$Customer_Age, data_reg_stay$Customer_Age)
# => Use a non parametric test like Wilcoxon test.
# WILCOXON
wilcox.test(data_reg_quit$Customer_Age, data_reg_stay$Customer_Age)
# W = 511132, p-value = 0.3884 => Les deux échantillons  ne sont pas significativement différents.

#### 02 Dependent_count ----
# analyse visuelle :
# histogramme de chaque population
hist(data_reg_stay$Dependent_count)
hist(data_reg_quit$Dependent_count)
#boxplot  Dependent_count selon l Attrition_Flag
boxplot(Dependent_count ~ Attrition_Flag)
# plot de Dependent_count par attrition_flag
plot_Dependent_count <-
    data %>% ggplot(aes(x = Dependent_count, fill = Attrition_Flag)) +
    geom_density(alpha = 0.7) +
    scale_fill_manual(values = c('black', 'white'), name ='') +
    theme_classic() +
    labs(title = 'Dependent_count by Attrition_Flag') +
    theme(legend.position = 'bottom')
plot_Dependent_count

# stat descriptives
summary(data_stay$Dependent_count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   1.000   2.000   2.335   3.000   5.000 
summary(data_quit$Dependent_count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   2.000   2.000   2.403   3.000   5.000 
#  variances
var(data_stay$Dependent_count)
# [1] 1.698405
var(data_quit$Dependent_count)
# [1] 1.625651

# STUDENT
rquery.t.test(data_reg_quit$Dependent_count, data_reg_stay$Dependent_count)
# => Use a non parametric test like Wilcoxon test.
# WILCOXON
wilcox.test(data_reg_quit$Dependent_count, data_reg_stay$Dependent_count)
# W = 513114, p-value = 0.2975 => Les deux échantillons  ne sont pas significativement différents.


#### 03 Months_on_book ----
# analyse visuelle : 
# histogramme de chaque population
hist(data_reg_stay$Months_on_book)
hist(data_reg_quit$Months_on_book)
#boxplot  Months_on_book selon l Attrition_Flag
boxplot(Months_on_book ~ Attrition_Flag)
# plot de Months_on_book par attrition_flag
plot_Months_on_book <-
    data %>% ggplot(aes(x = Months_on_book, fill = Attrition_Flag)) +
    geom_density(alpha = 0.7) +
    scale_fill_manual(values = c('black', 'white'), name ='') +
    theme_classic() +
    labs(title = 'Months_on_book by Attrition_Flag') +
    theme(legend.position = 'bottom')
plot_Months_on_book

# stat descriptives
summary(data_stay$Months_on_book)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 13.00   31.00   36.00   35.88   40.00   56.00 
summary(data_quit$Months_on_book)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 13.00   32.00   36.00   36.18   40.00   56.00 
#  variances
var(data_stay$Months_on_book)
# [1] 64.34943
var(data_quit$Months_on_book)
# [1] 60.78617

# STUDENT
rquery.t.test(data_reg_quit$Months_on_book, data_reg_stay$Months_on_book)
# => Use a non parametric test like Wilcoxon test.
# WILCOXON
wilcox.test(data_reg_quit$Months_on_book, data_reg_stay$Months_on_book)
# W = 509041, p-value = 0.4808 => Les deux échantillons ne sont pas significativement différents.

#### 04 Total_Relationship_Count ----
# analyse visuelle : 
# histogramme de chaque population
hist(data_reg_stay$Total_Relationship_Count)
hist(data_reg_quit$Total_Relationship_Count)
#boxplot  Total_Relationship_Count selon l Attrition_Flag
boxplot(Total_Relationship_Count ~ Attrition_Flag)
# plot de Total_Relationship_Count par attrition_flag
plot_Total_Relationship_Count <-
    data %>% ggplot(aes(x = Total_Relationship_Count, fill = Attrition_Flag)) +
    geom_density(alpha = 0.7) +
    scale_fill_manual(values = c('black', 'white'), name ='') +
    theme_classic() +
    labs(title = 'Total_Relationship_Count by Attrition_Flag') +
    theme(legend.position = 'bottom')
plot_Total_Relationship_Count

# stat descriptives
summary(data_stay$Total_Relationship_Count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   3.000   4.000   3.915   5.000   6.000 
summary(data_quit$Total_Relationship_Count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    2.00    3.00    3.28    5.00    6.00 
var(data_stay$Total_Relationship_Count)
# 1] 2.337686
var(data_quit$Total_Relationship_Count)
# [1] 2.489395

# STUDENT
rquery.t.test(data_reg_quit$Total_Relationship_Count, data_reg_stay$Total_Relationship_Count)
# => Use a non parametric test like Wilcoxon test.
# WILCOXON
wilcox.test(data_reg_quit$Total_Relationship_Count, data_reg_stay$Total_Relationship_Count)
# W = 374438, p-value < 2.2e-16 => Les deux échantillons sont significativement différents.

#### 05 Months_Inactive_12_mon ----
# analyse visuelle : 
# histogramme de chaque population
hist(data_reg_stay$Months_Inactive_12_mon)
hist(data_reg_quit$Months_Inactive_12_mon)
#boxplot  Months_Inactive_12_mon selon l Attrition_Flag
boxplot(Months_Inactive_12_mon ~ Attrition_Flag)
# plot de Months_Inactive_12_mon par attrition_flag
plot_Months_Inactive_12_mon <-
    data %>% ggplot(aes(x = Months_Inactive_12_mon, fill = Attrition_Flag)) +
    geom_density(alpha = 0.7) +
    scale_fill_manual(values = c('black', 'white'), name ='') +
    theme_classic() +
    labs(title = 'Months_Inactive_12_mon by Attrition_Flag') +
    theme(legend.position = 'bottom')
plot_Months_Inactive_12_mon
# stat descriptives
summary(data_stay$Months_Inactive_12_mon)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.000   1.000   2.000   2.274   3.000   6.000 
summary(data_quit$Months_Inactive_12_mon)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   2.000   3.000   2.693   3.000   6.000 
var(data_stay$Months_Inactive_12_mon)
# [1] 1.033763
var(data_quit$Months_Inactive_12_mon)
# 1] 0.8093216

# STUDENT
rquery.t.test(data_reg_quit$Months_Inactive_12_mon, data_reg_stay$Months_Inactive_12_mon)
# => Use a non parametric test like Wilcoxon test.
# WILCOXON
wilcox.test(data_reg_quit$Months_Inactive_12_mon, data_reg_stay$Months_Inactive_12_mon)
# W = 634140, p-value < 2.2e-16 => Les deux échantillons sont significativement différents.


#### 06 Contacts_Count_12_mon ---- 
# analyse visuelle : 
# histogramme de chaque population
hist(data_reg_stay$Contacts_Count_12_mon)
hist(data_reg_quit$Contacts_Count_12_mon)
#boxplot  Contacts_Count_12_mon selon l Attrition_Flag
boxplot(Contacts_Count_12_mon ~ Attrition_Flag)
# plot de Contacts_Count_12_mon par attrition_flag
plot_Contacts_Count_12_mon <-
    data %>% ggplot(aes(x = Contacts_Count_12_mon, fill = Attrition_Flag)) +
    geom_density(alpha = 0.7) +
    scale_fill_manual(values = c('black', 'white'), name ='') +
    theme_classic() +
    labs(title = 'Contacts_Count_12_mon by Attrition_Flag') +
    theme(legend.position = 'bottom')
plot_Contacts_Count_12_mon
# stat descriptives
summary(data_stay$Contacts_Count_12_mon)
summary(data_quit$Contacts_Count_12_mon)
var(data_stay$Contacts_Count_12_mon)
var(data_quit$Contacts_Count_12_mon)
# summary(data_stay$Contacts_Count_12_mon)
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# # 0.000   2.000   2.000   2.356   3.000   5.000
# summary(data_quit$Contacts_Count_12_mon)
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# # 0.000   2.000   3.000   2.972   4.000   6.000
# var(data_stay$Contacts_Count_12_mon)
# 1.169503
# var(data_quit$Contacts_Count_12_mon)
# 1.189271

# STUDENT
rquery.t.test(data_reg_quit$Contacts_Count_12_mon, data_reg_stay$Contacts_Count_12_mon)
# => Use a non parametric test like Wilcoxon test.
wilcox.test(data_reg_quit$Contacts_Count_12_mon, data_reg_stay$Contacts_Count_12_mon)
# W = 662803, p-value < 2.2e-16=> Les deux échantillons sont significativement différents.

#### 07 Credit_Limit ---- 
# analyse visuelle : 
# histogramme de chaque population
hist(data_reg_stay$Credit_Limit)
hist(data_reg_quit$Credit_Limit)
#boxplot  Credit_Limit selon l Attrition_Flag
boxplot(Credit_Limit ~ Attrition_Flag)
# plot de Credit_Limit par attrition_flag
plot_Credit_Limit <-
    data %>% ggplot(aes(x = Credit_Limit, fill = Attrition_Flag)) +
    geom_density(alpha = 0.7) +
    scale_fill_manual(values = c('black', 'white'), name ='') +
    theme_classic() +
    labs(title = 'Credit_Limit by Attrition_Flag') +
    theme(legend.position = 'bottom')
plot_Credit_Limit
# stat descriptives
summary(data_stay$Credit_Limit)
summary(data_quit$Credit_Limit)
var(data_stay$Credit_Limit)
var(data_quit$Credit_Limit)

# summary(data_stay$Credit_Limit)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1438    2602    4644    8727   11253   34516 
# > summary(data_quit$Credit_Limit)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1438    2114    4178    8136    9934   34516 
# var(data_stay$Credit_Limit)
# [1] 82536676
# var(data_quit$Credit_Limit)
# [1] 82725102

# STUDENT
rquery.t.test(data_reg_quit$Credit_Limit, data_reg_stay$Credit_Limit)
# => Use a non parametric test like Wilcoxon test.
# WILCOXON
wilcox.test(data_reg_quit$Credit_Limit, data_reg_stay$Credit_Limit)
# W = 465106, p-value = 0.006881=> Les deux échantillons  ne sont pas significativement différents.

#### 08 Total_Revolving_Bal ----
# analyse visuelle : 
# histogramme de chaque population
hist(data_reg_stay$Total_Revolving_Bal)
hist(data_reg_quit$Total_Revolving_Bal)
#boxplot  Total_Revolving_Bal selon l Attrition_Flag
boxplot(Total_Revolving_Bal ~ Attrition_Flag)
# plot de Total_Revolving_Bal par attrition_flag
plot_Total_Revolving_Bal <-
    data %>% ggplot(aes(x = Total_Revolving_Bal, fill = Attrition_Flag)) +
    geom_density(alpha = 0.7) +
    scale_fill_manual(values = c('black', 'white'), name ='') +
    theme_classic() +
    labs(title = 'Total_Revolving_Bal by Attrition_Flag') +
    theme(legend.position = 'bottom')
plot_Total_Revolving_Bal
# stat descriptives
summary(data_stay$Total_Revolving_Bal)
summary(data_quit$Total_Revolving_Bal)
var(data_stay$Total_Revolving_Bal)
var(data_quit$Total_Revolving_Bal)
# summary(data_stay$Total_Revolving_Bal)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0     800    1364    1257    1807    2517 
# summary(data_quit$Total_Revolving_Bal)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0     0.0     0.0   672.8  1303.5  2517.0 
# var(data_stay$Total_Revolving_Bal)
# [1] 574178
# var(data_quit$Total_Revolving_Bal)
# [1] 848951.4

# STUDENT
rquery.t.test(data_reg_quit$Total_Revolving_Bal, data_reg_stay$Total_Revolving_Bal)
# => Use a non parametric test like Wilcoxon test.
# WILCOXON
wilcox.test(data_reg_quit$Total_Revolving_Bal, data_reg_stay$Total_Revolving_Bal)
# W = 324850, p-value < 2.2e-16=> Les deux échantillons  sont  significativement différents.

#### 09 Avg_Open_To_Buy   ----  
# analyse visuelle : 
# histogramme de chaque population
hist(data_reg_stay$Avg_Open_To_Buy)
hist(data_reg_quit$Avg_Open_To_Buy)
#boxplot  Avg_Open_To_Buy selon l Attrition_Flag
boxplot(Avg_Open_To_Buy ~ Attrition_Flag)
# plot de Avg_Open_To_Buy par attrition_flag
plot_Avg_Open_To_Buy <-
    data %>% ggplot(aes(x = Avg_Open_To_Buy, fill = Attrition_Flag)) +
    geom_density(alpha = 0.7) +
    scale_fill_manual(values = c('black', 'white'), name ='') +
    theme_classic() +
    labs(title = 'Avg_Open_To_Buy by Attrition_Flag') +
    theme(legend.position = 'bottom')
plot_Avg_Open_To_Buy
# stat descriptives
summary(data_stay$Avg_Open_To_Buy)
summary(data_quit$Avg_Open_To_Buy)
var(data_stay$Avg_Open_To_Buy)
var(data_quit$Avg_Open_To_Buy)

# > summary(data_stay$Avg_Open_To_Buy)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 15    1184    3470    7470    9978   34516 
# > summary(data_quit$Avg_Open_To_Buy)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3    1587    3488    7463    9258   34516 
# > var(data_stay$Avg_Open_To_Buy)
# [1] 82585780
# > var(data_quit$Avg_Open_To_Buy)
# [1] 82977673

# STUDENT
rquery.t.test(data_reg_quit$Avg_Open_To_Buy, data_reg_stay$Avg_Open_To_Buy)
# => Use a non parametric test like Wilcoxon test.
# WILCOXON
wilcox.test(data_reg_quit$Avg_Open_To_Buy, data_reg_stay$Avg_Open_To_Buy)
# W = 521333, p-value = 0.09853 => Les deux échantillons  ne sont pas significativement différents.


#### 10 Total_Amt_Chng_Q4_Q1 ----

# analyse visuelle : 
# histogramme de chaque population
hist(data_reg_stay$Total_Amt_Chng_Q4_Q1)
hist(data_reg_quit$Total_Amt_Chng_Q4_Q1)
#boxplot  Total_Amt_Chng_Q4_Q1 selon l Attrition_Flag
boxplot(Total_Amt_Chng_Q4_Q1 ~ Attrition_Flag)
# plot de Total_Amt_Chng_Q4_Q1 par attrition_flag
plot_Total_Amt_Chng_Q4_Q1 <-
    data %>% ggplot(aes(x = Total_Amt_Chng_Q4_Q1, fill = Attrition_Flag)) +
    geom_density(alpha = 0.7) +
    scale_fill_manual(values = c('black', 'white'), name ='') +
    theme_classic() +
    labs(title = 'Total_Amt_Chng_Q4_Q1 by Attrition_Flag') +
    theme(legend.position = 'bottom')
plot_Total_Amt_Chng_Q4_Q1
# stat descriptives
summary(data_stay$Total_Amt_Chng_Q4_Q1)
summary(data_quit$Total_Amt_Chng_Q4_Q1)
var(data_stay$Total_Amt_Chng_Q4_Q1)
var(data_quit$Total_Amt_Chng_Q4_Q1)

# > # stat descriptives
#     > summary(data_stay$Total_Amt_Chng_Q4_Q1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2560  0.6430  0.7430  0.7725  0.8600  3.3970 
# > summary(data_quit$Total_Amt_Chng_Q4_Q1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.5445  0.7010  0.6943  0.8560  1.4920 
# > var(data_stay$Total_Amt_Chng_Q4_Q1)
# [1] 0.04742953
# > var(data_quit$Total_Amt_Chng_Q4_Q1)
# # [1] 0.04619247

# STUDENT
rquery.t.test(data_reg_quit$Total_Amt_Chng_Q4_Q1, data_reg_stay$Total_Amt_Chng_Q4_Q1)
# => Use a non parametric test like Wilcoxon test.
# WILCOXON  
wilcox.test(data_reg_quit$Total_Amt_Chng_Q4_Q1, data_reg_stay$Total_Amt_Chng_Q4_Q1)
# W = 414090, p-value = 2.874e-11=> Les deux échantillons sont significativement différents.

#### 11 Total_Trans_Amt ----

# analyse visuelle : 
# histogramme de chaque population
hist(data_reg_stay$Total_Trans_Amt)
hist(data_reg_quit$Total_Trans_Amt)
#boxplot  Total_Trans_Amt selon l Attrition_Flag
boxplot(Total_Trans_Amt ~ Attrition_Flag)
# plot de Total_Trans_Amt par attrition_flag
plot_Total_Trans_Amt<-
    data %>% ggplot(aes(x = Total_Trans_Amt, fill = Attrition_Flag)) +
    geom_density(alpha = 0.7) +
    scale_fill_manual(values = c('black', 'white'), name ='') +
    theme_classic() +
    labs(title = 'Total_Trans_Amt by Attrition_Flag') +
    theme(legend.position = 'bottom')
plot_Total_Trans_Amt
# stat descriptives
summary(data_stay$Total_Trans_Amt)
summary(data_quit$Total_Trans_Amt)
var(data_stay$Total_Trans_Amt)
var(data_quit$Total_Trans_Amt)

# STUDENT
rquery.t.test(data_reg_quit$Total_Trans_Amt, data_reg_stay$Total_Trans_Amt)
# => Use a non parametric test like Wilcoxon test.
# WILCOXON  
wilcox.test(data_reg_quit$Total_Trans_Amt, data_reg_stay$Total_Trans_Amt)
# W = 345364, p-value < 2.2e-16=> Les deux échantillons sont significativement différents.

#### 12 Total_Trans_Ct ----

# analyse visuelle : 
# histogramme de chaque population
hist(data_reg_stay$Total_Trans_Ct)
hist(data_reg_quit$Total_Trans_Ct)
#boxplot  Total_Trans_Ct selon l Attrition_Flag
boxplot(Total_Trans_Ct ~ Attrition_Flag)
# plot de Total_Trans_Ct par attrition_flag
plot_Total_Trans_Ct<-
    data %>% ggplot(aes(x = Total_Trans_Ct, fill = Attrition_Flag)) +
    geom_density(alpha = 0.7) +
    scale_fill_manual(values = c('black', 'white'), name ='') +
    theme_classic() +
    labs(title = 'Total_Trans_Ct by Attrition_Flag') +
    theme(legend.position = 'bottom')
plot_Total_Trans_Ct
# stat descriptives
summary(data_stay$Total_Trans_Ct)
summary(data_quit$Total_Trans_Ct)
var(data_stay$Total_Trans_Ct)
var(data_quit$Total_Trans_Ct)
# stat descriptives
#     > summary(data_stay$Total_Trans_Ct)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 11.00   54.00   71.00   68.67   82.00  139.00 
# > summary(data_quit$Total_Trans_Ct)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 10.00   37.00   43.00   44.93   51.00   94.00 
# > var(data_stay$Total_Trans_Ct)
# [1] 525.2811
# > var(data_quit$Total_Trans_Ct)
# [1] 212.2391

# STUDENT
rquery.t.test(data_reg_quit$Total_Trans_Ct, data_reg_stay$Total_Trans_Ct)
# => Use a non parametric test like Wilcoxon test.
# WILCOXON
wilcox.test(data_reg_quit$Total_Trans_Ct, data_reg_stay$Total_Trans_Ct)
# W = 225446, p-value < 2.2e-16=> Les deux échantillons sont significativement différents.

#### 13 Total_Ct_Chng_Q4_Q1 ---- 
# analyse visuelle : 
# histogramme de chaque population
hist(data_reg_stay$Total_Ct_Chng_Q4_Q1)
hist(data_reg_quit$Total_Ct_Chng_Q4_Q1)
#boxplot  Total_Ct_Chng_Q4_Q1 selon l Attrition_Flag
boxplot(Total_Ct_Chng_Q4_Q1 ~ Attrition_Flag)
# plot de Total_Ct_Chng_Q4_Q1 par attrition_flag
plot_Total_Ct_Chng_Q4_Q1<-
    data %>% ggplot(aes(x = Total_Ct_Chng_Q4_Q1, fill = Attrition_Flag)) +
    geom_density(alpha = 0.7) +
    scale_fill_manual(values = c('black', 'white'), name ='') +
    theme_classic() +
    labs(title = 'Total_Ct_Chng_Q4_Q1 by Attrition_Flag') +
    theme(legend.position = 'bottom')
plot_Total_Ct_Chng_Q4_Q1
# stat descriptives
summary(data_stay$Total_Ct_Chng_Q4_Q1)
summary(data_quit$Total_Ct_Chng_Q4_Q1)
var(data_stay$Total_Ct_Chng_Q4_Q1)
var(data_quit$Total_Ct_Chng_Q4_Q1)

# > # stat descriptives
#     > summary(data_stay$Total_Ct_Chng_Q4_Q1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0280  0.6170  0.7210  0.7424  0.8330  3.7140 
# > summary(data_quit$Total_Ct_Chng_Q4_Q1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.4000  0.5310  0.5544  0.6920  2.5000 
# > var(data_stay$Total_Ct_Chng_Q4_Q1)
# [1] 0.05200885
# > var(data_quit$Total_Ct_Chng_Q4_Q1)
# [1] 0.05146261


# STUDENT
rquery.t.test(data_reg_quit$Total_Ct_Chng_Q4_Q1, data_reg_stay$Total_Ct_Chng_Q4_Q1)
# => Use a non parametric test like Wilcoxon test.
# WILCOXON  
wilcox.test(data_reg_quit$Total_Ct_Chng_Q4_Q1, data_reg_stay$Total_Ct_Chng_Q4_Q1)
# W = 263315, p-value < 2.2e-16=> Les deux échantillons sont significativement différents.
#### 14 Avg_Utilization_Ratio ----
# analyse visuelle : 
# histogramme de chaque population
hist(data_reg_stay$Avg_Utilization_Ratio)
hist(data_reg_quit$Avg_Utilization_Ratio)
#boxplot  Avg_Utilization_Ratio selon l Attrition_Flag
boxplot(Avg_Utilization_Ratio ~ Attrition_Flag)
# plot de Avg_Utilization_Ratio par attrition_flag
plot_Avg_Utilization_Ratio<-
    data %>% ggplot(aes(x = Avg_Utilization_Ratio, fill = Attrition_Flag)) +
    geom_density(alpha = 0.7) +
    scale_fill_manual(values = c('black', 'white'), name ='') +
    theme_classic() +
    labs(title = 'Avg_Utilization_Ratio by Attrition_Flag') +
    theme(legend.position = 'bottom')
plot_Avg_Utilization_Ratio
# stat descriptives
summary(data_stay$Avg_Utilization_Ratio)
summary(data_quit$Avg_Utilization_Ratio)
var(data_stay$Avg_Utilization_Ratio)
var(data_quit$Avg_Utilization_Ratio)
# > summary(data_stay$Avg_Utilization_Ratio)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0550  0.2110  0.2964  0.5292  0.9940 
# > summary(data_quit$Avg_Utilization_Ratio)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  0.0000  0.1625  0.2310  0.9990 
# > var(data_stay$Avg_Utilization_Ratio)
# [1] 0.07429321
# > var(data_quit$Avg_Utilization_Ratio)
# [1] 0.06993783
# STUDENT
rquery.t.test(data_reg_quit$Avg_Utilization_Ratio, data_reg_stay$Avg_Utilization_Ratio)
# => Use a non parametric test like Wilcoxon test.
# WILCOXON
wilcox.test(data_reg_quit$Avg_Utilization_Ratio, data_reg_stay$Avg_Utilization_Ratio)
# W = 325727, p-value < 2.2e-16=> Les deux échantillons sont significativement différents.

# test de 2 graphes sur une seule feuille avec grid.arrange()
p1 <- data %>%
    select(Total_Trans_Ct,Attrition_Flag) %>%
    ggplot(aes(x=Total_Trans_Ct,fill=Attrition_Flag)) +
    geom_bar(alpha=0.4,position="dodge") +
    labs(title="Distribution of Total Transaction Count by Customer type",
         x="Total Transaction Count",y="Count")
p2 <- data %>%
    select(Total_Trans_Amt,Attrition_Flag) %>%
    ggplot(aes(x=Total_Trans_Amt,fill=Attrition_Flag)) +
    geom_density(alpha=0.4) +
    labs(title="Distribution of Total Transaction Amount by Customer type",
         x="Total Transaction Amount",y="Density")
grid.arrange(p1, p2, nrow = 2)

# Les 9 variables pour lequels les populations sont significativement différent :
# Avg_Utilization_Ratio
# Total_Ct_Chng_Q4_Q1
# Total_Trans_Ct   
# Total_Trans_Amt
# Total_Amt_Chng_Q4_Q1
# Total_Revolving_Bal
# Months_Inactive_12_mon
# Contacts_Count_12_mon
# Total_Relationship_Count
# ==> Ce sont probablement les variables les plus explicatives qur le depart ou non des clients.


### 3.4 CORRELATION ENTRE VARIABLE ----

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
    round(3) %>% 
    hchart() %>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_title(text = "Spearman's correlation coefficients", align = "center") %>%
    hc_legend(align = "center") %>%
    hc_colorAxis(stops = color_stops(colors = viridis::inferno(10))) %>%
    hc_plotOptions(series = list(boderWidth = 0,
                                 dataLabels = list(enabled = TRUE)))

#  Visualisation des 3 plus fortes corrélations ressortant de la matrice de corrélation :
# Custumer age and months of book are highly correlated (0.79)
ggplot(data, aes(x=Customer_Age, y=Months_on_book)) + geom_point(color = "black",size= 0.3) + theme_classic() + ggtitle("Months on book vs Customer Age")
# Total Transaction Count and Total Transaction Amount are highly correlated (0.81)
ggplot(data, aes(x=Total_Trans_Amt, y= Total_Trans_Ct)) + geom_point(color = "red",size = 0.3) + theme_classic() + ggtitle("Total Trans Amt vs Total Trans Ct")
# Total Revovlving Balance and Average Utilization Ratio are correlated (0.62)
ggplot(data, aes(x=Total_Revolving_Bal, y= Avg_Utilization_Ratio)) + geom_point(color = "blue",size= 0.3) + theme_classic() + ggtitle("Total Revolving Bal vs Avg Utilization Ratio")


#----Faire des clusters (Kmeans /clusters de clients sui partent vs qui restent) => RODRIGUE
# https://www.datanovia.com/en/fr/blog/visualisation-du-clustering-k-means-dans-r-guide-etape-par-etape/

data_k <- data %>% mutate_if(is.factor,as.numeric)
data_k
# seed
set.seed(123)
# Retrait du taux d'attrition & CLUSTERING en 6 groupes avec KMEANS
res.km <- kmeans(scale(data_k[, -1]), 6, nstart = 25)
# RÉDUCTION DE DIMENSION À L'AIDE DU PCA
# Réduction de chacune des 23 variables quantitatives à 23 groupe/dimension en utilisant l'ACP(prcomp)
res.pca <- prcomp(data_k[, -1], scale = TRUE)

# Coordonnées des individus
ind.coord <- as.data.frame(factoextra::get_pca_ind(res.pca)$coord)

# Ajouter des clusters obtenus à l'aide de l'algorithme K-means
ind.coord$cluster <- factor(res.km$cluster)

# Ajout de la variable cible à partir de l'ensemble de données d'origine
ind.coord$target <- data$Attrition_Flag

pca_cluster <- ind.coord %>%
    group_by(target, cluster) %>%
    count() %>%
    as.data.frame()

percentage_total <- pca_cluster %>%
    group_by(target) %>%
    summarise(per_tot=n/sum(n)*100)

pca_cluster <- cbind(pca_cluster,'%'=round(percentage_total$per_tot,1))

c2 <- tableGrob(pca_cluster)

c3 <- ggscatter(
    ind.coord, x = "Dim.1", y = "Dim.2",
    color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
    size = 1.5,  legend = "right", ggtheme = theme_bw(),
    xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
    ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
) +
    stat_mean(aes(color = cluster), size = 4) +
    theme_classic() +
    theme(legend.position='top')

# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)

variance.percent <- eigenvalue$variance.percent

c3 <- ggscatter(
    ind.coord, x = "Dim.1", y = "Dim.2",
    color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
    size = 1.5,  legend = "right", ggtheme = theme_bw(),
    xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
    ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
) +
    stat_mean(aes(color = cluster), size = 4) +
    theme_classic() +
    theme(legend.position='top')

my_gp <- grid::gpar(fontsize=18, font=1)
my_top <- grid::textGrob("Kmeans cluster and PCA", gp=my_gp)
gridExtra::grid.arrange(c2, c3, ncol=2, top=my_top)

# CONCLUSION

# THe 5 top features of determing a customer's attrition:
# Total Transaction Count
# Total Revolving Balance
# Total Transaction Amount
# Total Relationship Count
# Total Count Change

#----faire une AFC
# http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/74-afc-analyse-factorielle-des-correspondances-avec-r-l-essentiel/

#  pour nous aider a trouver les perimetres de nos modeles (il y en aura surement pls)

# variable dependante Y = départ ou non : attrition_flag
# variable qualitative donc on fera une régression logistique

# variable explicatives
# age, genre, niveau education, statut marital, personnes à charge, revenu, type de carte, période de relation avec la banque...


#partage du dataset en 70/30 

intrain<-createDataPartition(data$Attrition_Flag,p=0.7,
                             list = F,
                             times = 1)
# creation des datasets: testing (30%) & training (70%) pour minimiser le ridque de surentraienemnt 
training <-data[intrain,]
testing <-data[-intrain,]

# ---- modification des classes lorsqu'il y a des trop fort desequilibres


# # CLASSE => message d'erreur 

# data_select<-data
# #classe_marital_class
# data_select[which(data_select$Marital_Status %in% c("Divorced","Single")),"Marital_Status"]<-"Single"
# #Card_category_class
# data_select[which(data_select$Card_Category %in% c("Gold","platinum","Silver")),"Card_Category"]<-"Others"
# #Income_category_class
# data_select[which(data_select$Income_Category %in% c("Less than $40K","$40K - $60K")),"Income_Category"]<-"Less than $60K"
# data_select[which(data_select$Income_Category %in% c("$60K - $80K","$80K - $120K","$120K +")),"Income_Category"]<-"More than $60K"



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


#Model complet test
data_select<-data
data_select$Attrition_Flag <- as.factor(data_select$Attrition_Flag)
data_select<-c("Attrition_Flag","Customer_Age","Dependent_count","Months_on_book","Total_Relationship_Count","Months_Inactive_12_mon","Contacts_Count_12_mon","Credit_Limit","Total_Revolving_Bal","Avg_Open_To_Buy","Total_Amt_Chng_Q4","Total_Trans_Amt","Total_Trans_Ct","Total_Ct_Chng_Q4_Q1","Avg_Utilization_Ratio")
data_select<-as.factor(data_select)
simple.model <- glm(Attrition_Flag ~1, data = data_select, family = binomial)
summary(simple.model)
