# Objectif: Prédire la résiliation d’un client bancaire
# Kaggle: https://www.kaggle.com/sakshigoyal7/credit-card-customers
# Jeux d’apprentissage : 10 K lignes / 23 colonnes

# TODO : https://www.kaggle.com/josephchan524/bankchurnersclassifier-recall-97-accuracy-95

##################################################################################-
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

# ```{r}
data <- read.csv("data/BankChurners.csv", sep = ",")
# ```

#####  Retrait des colonnes inutiles pour notre étude
#   - CLIENTNUM
#   - Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1
#   - Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2
data <- data[, -c(1, 22, 23)]

##### Vérification des valeurs nulles
summary(data)
# pas de valeurs nulles (NA)

##### Vérification des doublons
length(unique(data$CLIENTNUM))
# 10127 lignes pour 10127 numeros de compte => pas de doublons

##### Vérification des types de champs 
str(data)


##### la variable explicative : Attrition_Flag ( Factor )
est une vartiable qualitative, le modele a utiliser est donc une regression logistique=> regression logistique

summary(data)
skim(data)
# ---les  variables qualitatives
# 1 Card_Category           : Factor
# 3 Gender                  : Factor
# 4 Education_Level         : Factor
# 5 Marital_Status          : Factor
# 6 Income_Category         : Factor

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
# 10- Total_Trans_Ct          : int
# 11- Total_Ct_Chng_Q4_Q1     : num
# 12- Avg_Utilization_Ratio   : num
# 13- Customer_Age            : int
# 14- Dependent_count         : int



# 20 colonnes restantes
length (colnames(data))

##################################################################################-
# 3 - Analyse exploratoire des données (EDA)  ####
##################################################################################-

### Creation de deux dataframe ##############################
#### Séparation des clients :  ceux qui ont quitté la banque de ceux qui sont restés

# modification Attrition_Flag : 0 Existing Customer, 1 Attrited Customer ---

data$Attrition_Flag<-as.character(data$Attrition_Flag)
data$Attrition_Flag[data$Attrition_Flag=="Existing Customer"]<-0
data$Attrition_Flag[data$Attrition_Flag=="Attrited Customer"]<-1
data$Attrition_Flag <- as_factor(data$Attrition_Flag)

#### les deux datasets
data_quit <- data[(data$Attrition_Flag) == 1, ] # 1627
data_stay <- data[(data$Attrition_Flag) == 0, ] # 8500
str(data_quit)

#### Conversion des facteurs et réordonnancement des niveaux 
data$Attrition_Flag <- as_factor(data$Attrition_Flag)
data$Gender <- as_factor(data$Gender)
data$Education_Level <- as_factor(data$Education_Level)
data$Education_Level <- fct_relevel(data$Education_Level, "Unknown", "Uneducated", "High School", "College", "Graduate", "Post-Graduate", "Doctorate")
data$Marital_Status <- as_factor(data$Marital_Status)
data$Marital_Status <- fct_relevel(data$Marital_Status, "Unknown", "Single", "Married", "Divorced")
data$Income_Category <- as_factor(data$Income_Category)
data$Income_Category <- fct_relevel(data$Income_Category, "Unknown", "Less than $40K", "$40K - $60K", "$60K - $80K", "$80K - $120K", "$120K +")
data$Card_Category <- as_factor(data$Card_Category)
data$Card_Category <- fct_relevel(data$Card_Category, "Blue", "Silver", "Gold", "Platinum")

str(data)

### Analyses des variables qualitatives ### ----

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

# Resultats variable Gender ----
    # parmi Existing Customer "0.521" soit 52.1 % 
    # Attrited Customers : "0.572" soit 57.2 %
    # 57 % des personnes aant fermées leur compte sont des femmes tandis que 52 % des clients de la 
    # sont des femmes. Les femmes sont donc en sur representation parmi les desabonnés avec une différence de proportion de 5%.


#### Marital_Status #### 
#####frequences des modalités en fonction de l'Attrition_Flag ----

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

##### Les resultats ----
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
         x = "Existing custumes", y = "NB")

#####Les resultats ---- 
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

#####Les resultats  
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

#####Les resultats 
#  en fonction du niveau d'etude on voit une legere différence de proportion parmi les abonnées ou les desabonnés.
# Il semble que le niveau d'etude relativement élevé (graduate, post graduate et doctorate) facilite de depart de la banque.
# parmi les " unknox on trouve une tres legere surrepresentation des desabonnés

# ---------------------------------------------------
### 3.3 VARIABLES QUANTITATIVES ----

#  14 varibles quanti

# ???? TEST DE STUDENT ????
# pour comparer des groupes dont les effectifs sont différentS : comparaison de moyenne
# => test de student : http://www.sthda.com/french/wiki/test-de-student-est-il-toujours-correct-de-comparer-des-moyennes
# source('http://www.sthda.com/upload/rquery_t_test.r')
# rquery.t.test(x, y = NULL, paired = FALSE, graph = TRUE, ...)
# exemple : rquery.t.test(data_quit$Avg_Open_To_Buy, data_stay$Avg_Open_To_Buy)
# Error in shapiro.test(x) : sample size must be between 3 and 5000
# ??????????????????????????????????????????????????????????????????????????????????



## Analyses visuelles 

#### Customer_Age #### 

#  histogramme
hist(data_stay$Customer_Age)
hist(data_quit$Customer_Age)

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
# var(data_quit$Customer_Age)
# 58.76221

#  Customer_Age selon l' Attrition_Flag
boxplot(Customer_Age ~ Attrition_Flag)
plot_Custumer_Age <-
    data %>% ggplot(aes(x = Customer_Age, fill = Attrition_Flag)) +
    geom_density(alpha = 0.7) +
    scale_fill_manual(values = c('#3C3838', '#338076'), name = '') +
    theme_classic() +
    labs(title = 'Customer_Age by flag') +
    theme(legend.position = 'bottom')
plot_Custumer_Age

# test de normalité 
# les histogramme montre une distribution qui suit la loi normale

#  test d'egalités des variances
summary(aov(data_quit$Customer_Age~data_stay$Customer_Age))

#  test de student
t.test(data_stay$Customer_Age~data_quit$Customer_Age,var.equal=TRUE)


####   Months_on_book #### 
hist(Months_on_book)
boxplot(Months_on_book ~ Attrition_Flag)


barplot(table(data$Months_on_book[data$Attrition_Flag == 0]))
barplot(table(data$Months_on_book[data$Attrition_Flag == 1]))

#### Total_Relationship_Count #### 
hist(Months_on_book)
barplot(Contacts_Count_12_mon)
boxplot(Months_on_book ~ Attrition_Flag)
####  Months_Inactive_12_mon #### 



####  Contacts_Count_12_mon#### 
hist(Contacts_Count_12_mon)
boxplot(Contacts_Count_12_mon ~ Attrition_Flag)

plot_Contacts_Count_12_mon <-
    data %>% ggplot(aes(x = Contacts_Count_12_mon, fill = Attrition_Flag)) +
    geom_density(alpha = 0.1) +
    scale_fill_manual(values = c('#3C3338', '#338076'), name = '') +
    theme_classic() +
    labs(title = 'Total Transaction Count (Last 12 months) by flag') +
    theme(legend.position = 'bottom')
plot_Contacts_Count_12_mon
#### Credit_Limit #### 
hist(Credit_Limit)
boxplot(Credit_Limit ~ Attrition_Flag)
####  Total_Revolving_Bal #### 
hist(Total_Revolving_Bal)
boxplot(Total_Revolving_Bal ~ Attrition_Flag)
#### Avg_Open_To_Buy #### 
hist(Avg_Open_To_Buy)
boxplot(Avg_Open_To_Buy ~ Attrition_Flag)
#### Total_Amt_Chng_Q4_Q1 #### 
hist(Total_Amt_Chng_Q4_Q1)
boxplot(Total_Amt_Chng_Q4_Q1 ~ Attrition_Flag)
#### Total_Trans_Amt #### 
hist(Total_Trans_Ct)
boxplot(Total_Trans_Ct ~ Attrition_Flag)
#### Total_Trans_Ct #### 

hist(Total_Trans_Ct)
boxplotTotal_Trans_Ct( ~ Attrition_Flag)

plot_Total_Trans_Ct <-
    data %>% ggplot(aes(x = Total_Trans_Ct, fill = Attrition_Flag)) +
    geom_density(alpha = 0.7) +
    scale_fill_manual(values = c('#3C3838', '#338076'), name = '') +
    theme_classic() +
    labs(title = 'Total Transaction Count (Last 12 months) by flag') +
    theme(legend.position = 'bottom')
plot_Total_Trans_Ct
#### Total_Ct_Chng_Q4_Q1 #### 
hist(Total_Ct_Chng_Q4_Q1)
boxplot(Total_Ct_Chng_Q4_Q1 ~ Attrition_Flag)
#### Avg_Utilization_Ratio #### 
hist(Avg_Utilization_Ratio)
boxplot(Avg_Utilization_Ratio ~ Attrition_Flag)

####  Dependent_count  #### 
hist(Dependent_count)
boxplot(Dependent_count ~ Attrition_Flag)





### 3.4 CORRELATION ENTRE VARIABLE ----

#Model complet test
data_select<-data
data_select$Attrition_Flag <- as.factor(data_select$Attrition_Flag)
data_select<-c("Attrition_Flag","Customer_Age","Dependent_count","Months_on_book","Total_Relationship_Count","Months_Inactive_12_mon","Contacts_Count_12_mon","Credit_Limit","Total_Revolving_Bal","Avg_Open_To_Buy","Total_Amt_Chng_Q4","Total_Trans_Amt","Total_Trans_Ct","Total_Ct_Chng_Q4_Q1","Avg_Utilization_Ratio")
data_select<-as.factor(data_select)
simple.model <- glm(Attrition_Flag ~1, data = data_select, family = binomial)
summary(simple.model)




#Echantillonnage pour plus tard
data_quit<-data[(data$Attrition_Flag)=="Quit",]
data_stay<-data[(data$Attrition_Flag)=="Stay",]
sample_quit<-sample(1:dim(data_quit)[1],1000)
sample_Stay<-sample(1:dim(data_stay)[1],1000)
data_reg<-rbind(data_quit[sample_quit,],data_stay[sample_Stay,])
table(data_reg$Attrition_Flag)



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


#----Faire des clusters (Kmeans /clusters de clients sui partent vs qui restent)
# https://www.datanovia.com/en/fr/blog/visualisation-du-clustering-k-means-dans-r-guide-etape-par-etape/




# PCA KMEANS  trouvé sur Kaggle => en doc "who'sgonna churn?"
# => pb de library pour les fonction (tableGrob et "get_eigenvalue")
data_k <- data %>% mutate_if(is.factor,as.numeric)
# seed
set.seed(2)
# CLUSTERING USING KMEANS
res.km <- kmeans(scale(data_k[, -1]), 6, nstart = 25)
# DIMENSION REDUCTION USING PCA
res.pca <- prcomp(data_k[, -1],  scale = TRUE)

# Coordinates
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)

# Add clusters obtained using the K-means algorithm
ind.coord$cluster <- factor(res.km$cluster)

# Add target from original dataset
ind.coord$target <- data$Attrition_Flag
pca_cluster <- ind.coord %>% group_by(target,cluster) %>% count() %>% as.data.frame()
percentage_total= pca_cluster %>% group_by(target) %>% summarise(per_tot=n/sum(n)*100)
pca_cluster <- cbind(pca_cluster,'%'=round(percentage_total$per_tot,1))
c2 <- table(pca_cluster)
View(c2)


# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)

# factoextra : Extract and Visualize the Results of Multivariate Data Analyses
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


grid.arrange(c2,c3,ncol=2,
             top=textGrob("Kmeans cluster and PCA",
                          gp=gpar(fontsize=18,font=1)))











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
