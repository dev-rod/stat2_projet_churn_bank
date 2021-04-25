# Objectif: Prédire la résiliation d’un client bancaire
# Kaggle: https://www.kaggle.com/sakshigoyal7/credit-card-customers
# Jeux d’apprentissage : 10 K lignes / 23 colonnes

# TODO : https://www.kaggle.com/josephchan524/bankchurnersclassifier-recall-97-accuracy-95

# dernier commit intégré hors moi : analyses visuels + comparaison de moyenne des variables quantis - 29e6d29
# Krysyna committed 11 days ago - 31 mars 2021 - 00h20

# Pour voir les librairies installées
# library()

##################################################################################-
# 1 - Chargement des librairies ----
##################################################################################-
# ```{r}
# fonctions du projet medas
source('script/functions.R')
load_libraries()
# install.packages("C:/Users/CFOUQUE/Desktop/questionr_0.7.4.zip", repos = NULL, type = "win.binary")

# si besoin de supprimer un package
# remove.packages(pkgs, lib)
#     Arguments 
#     pkgs    a character vector with the names of the packages to be removed.
#     lib	    a character vector giving the library directories to remove the packages from. 
#              If missing, defaults to the first element in .libPaths().

##################################################################################-
# 2 - Import des données ----
##################################################################################-
data <- read.csv("data/BankChurners.csv", sep = ",")

##################################################################################-
# 2.1 - Préparation des données ----
##################################################################################-

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
# C'est une vartiable qualitative, le modele a utiliser est donc une regression logistique
summary(data)
# print (View(skim(data)))

# ---les 5 variables qualitatives
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
# 3 - Analyse exploratoire des données ----
##################################################################################-

### 3.1 - Séparation des clients en 2 dataframe : partis/restés ----
data$Attrition_Flag<-as.character(data$Attrition_Flag)
data$Attrition_Flag[data$Attrition_Flag=="Existing Customer"]<-0
data$Attrition_Flag[data$Attrition_Flag=="Attrited Customer"]<-1

#### 3.2 - Classification et réordonnancement des variables qualitatives ----

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
# réordonnancement par categorie de carte de credit croissant
data$Card_Category <- fct_relevel(data$Card_Category, "Blue", "Silver", "Gold", "Platinum")

# verif :
# str(data)
# View(skim(data)

#### 3.3 - stats des 2 populations (clients, anciens clients) ----
data_quit <- data[(data$Attrition_Flag) == 1, ]
# View(skim(data_quit))
# str(data_quit)
# 1627 partis

data_stay <- data[(data$Attrition_Flag) == 0, ]
# View(View(skim(data_stay)))
# str(data_stay)
# 8500 restés

#### - Echantillonnage de 1000 restants et 1000 partants pour avoir deux populations de meme effectif ----

sample_quit<-sample(1:dim(data_quit)[1],1000)
sample_stay<-sample(1:dim(data_stay)[1],1000)

data_reg<-rbind(data_quit[sample_quit,],data_stay[sample_stay,])
# View(skim(data_reg))

# data_reg_quit
data_reg_quit <- data_reg[(data_reg$Attrition_Flag) == 1, ]
# View(skim(data_reg_quit))
# str(data_reg_quit)

# data_reg_stay 
data_reg_stay <- data_reg[(data_reg$Attrition_Flag) == 0, ]
# View(skim(data_reg_stay))
# str(data_reg_stay)

### 3.4 - 5 variables qualitatives : Analyse des ratios selon la valeur de l'Attriction_Flag----
# Tableaux de frequences par Attrition_Flag

#==> On cherche a trouver les relations entre 2 variables, à trouver les valeurs aberrantes (outliers)
# et exclure les variables sans intérêt,c'est a dire qui n'influencent pas le depart des clients

#### 3.4.1 - Gender ----
# search_cors(data, data_stay, data_quit, "Gender")
search_cors(data, data_reg_stay, data_reg_quit, "Gender")
# parmi Existing Customer "0.521" soit 52.1 % 
# Attrited Customers : "0.572" soit 57.2 %
# 57 % des personnes qui ont fermé leur compte sont des femmes tandis que 52 % des clients actifs sont des femmes.
# Les femmes sont donc en sur representation parmi les desabonnés avec une différence de proportion de 5%.

#### 3.4.2 - Marital_Status ----
# search_cors(data, data_stay, data_quit, "Marital_Status")
search_cors(data, data_reg_stay, data_reg_quit, "Marital_Status")
# "0.074" "0.074" => parmi les divorcés on retrouve la meme proportion de clients qui partent que ceux qui restent.
# "0.073" "0.079" => parmi les statut inconnus on retrouve une proportion tres legerement plus elevée de clients qui partent  (+0.6%)
# => peu de différence de proportion pour les divorcés et les statuts inconnus.
# "0.385" "0.411" => parmi les celibataires, on trouve legerement plus de personnes qui se desabonnent (+2.6 %)
# "0.468" "0.436" => parmi les mariés on trouve legerement plus de personnes qui restent abonnés ( -3%)
#  = > ? il peut etre interessant de regrouper les classes divorcés et Unknown qui semblent avoir des proportions identiques?

#### 3.4.3 - Card_Category  ----
# search_cors(data, data_stay, data_quit, "Card_Category")
search_cors(data, data_reg_stay, data_reg_quit, "Card_Category")
#  la proportion de catégorie de carte ne semble pas être tres différente entre abonnés et desabonnés
#  On observe pour ratio_Silver que la plus grosse différence de pourcentage dans les deux groupes est de 0.6 %.
#  "0.931" "0.934" => parmi les clients "carte Blue" peu de différence entre clients qui partent et qui restent (+0.03%)
#  "0.056" "0.050"  => parmi les clients "carte Silver" peu de différence entre clients qui partent et qui restent (-0.6 %)
#  "0.011" "0.013" => parmi les clients "carte Gold" peu de différence entre cleints qui partent et qui restent (+0.02%)
# "0.002" "0.003" => parmi les clients "carte Platinum" peu de différence entre clients qui partent et qui restent (+0.1%)
# ==> cette variable semble peu explicative

#### 3.4.4 - Income_Category ----
# search_cors(data, data_stay, data_quit, "Income_Category")
search_cors(data, data_reg_stay, data_reg_quit, "Income_Category")
# 6 classes : $120K +, $40K - $60K, $60K - $80K, $80K - $120K, Less than $40K, Unknown
#  "0.347" "0.376"  parmi les "Less than $40K" la proportion est  plus fortes pour les desabonnés  (+3 %)
#  "0.179" "0.167" parmi les "$40K - $60K" on observe une proportion legerement plus faible chez les desabonnés (-1.2%)
#  "0.143" "0.116" parmi les "$60K - $80K" on observe une proportion  plus faible chez les desabonnés (-2.7 %)
#  "0.152" "0.149" parmi les "$80K - $120K" on observe une proportion legerement plus faible chez les desabonnés (-0.3%)
#  "0.071" "0.077" parmi les "$120K +", on observe une proportion legerement plus forte chez les desabonnés (+0.6%)
# "0.109" "0.115" parmi les "Unknown", on observe une proportion legerement plus forte chez les desabonnés (+0.6%)
# => les proportions les plus notables  :
#      - les plus bas revenus (- 40 K$):  3 % de différence dans la proportion de desabonnés parmi les desabonnés.
#      - les revenus moyens ("40-80 K$") : on observe une différence de 1.2 % et 2.7 % des abonnés parmi l'ensemble
#               ==> regrouper les deux classes $40/60K et $60/80K  ==> $40K - $80K 

#### 3.4.5 - Education_Level ----
# search_cors(data, data_stay, data_quit, "Education_Level")
search_cors(data, data_reg_stay, data_reg_quit, "Education_Level")
# Unknown, Uneducated, High School, College, Graduate, Post-Graduate, Doctorate
# "0.147" "0.146" tres légère différence de ratio parmi les "Uneducated", un peu moins de desabonnés (-0.1%)
# "0.201" "0.188" (-1.3) légere différence de ratio parmi les "High School", un peu moins de desabonnés(-1.3%)
# "0.101" "0.095" (-0.6) tres légere différence de ratio parmi les ""College", un peu moins de desabonnés (-0.6%)
# "0.311" "0.299" (-1.2) légere différence de ratio parmi les "Graduate", un peu moins de desabonnés (-1.2 %)
# "0.050"  "0.057" (+0.7) légere différence de ratio parmi les "Post Graduate", un peu plus de desabonnés (+0.7)
# (+1.6) légere différence de ratio parmi les "Doctorate", un peu plus de desabonnés
# "0.042" "0.058"
# "0.149" "0.157" (+0.8) légere différence de ratio parmi les "Unknown", un peu plus de desabonnés
# en fonction du niveau d'etude on voit une legere différence de proportion parmi les abonnées ou les desabonnés.
# Il semble que le niveau d'etude relativement élevé (graduate, post graduate et doctorate) facilite de depart de la banque.
# parmi les " unknow on trouve une tres legere surrepresentation des desabonnés

# POUR CONCLURE SUR LES VARIABLES QUALITATIVES : ----
# qui partira le + : une femme célibataire à bas revenu (<40 K$) avec un niveau d'éducation très élevé
# qui restera le + : un homme marié à revenu moyen (40-80K$) avec un niveau d'éducation intermédiaire
#  on ne s'interessera pas  au type de carte de credit qui semble tres peu explicative de la variable d'interet
# ==> On se focalisera sur les variables explicatives :
        # - Gender
        # - Marital_Status
        # - Income_Category
        # - Education_Level


### 3.5 - 14 variables quantitatives : Analyse ----

#### COMPARAISON DES 2 POPULATIONS
# les effectifs sont différentS on procede par comparaison de moyenne => Test de Student ou de Wilcoxon
# La fonction rquery_t_test.r permet de verifier si toutes les conditions sont remplies pour le test de Student
# si les deux populations ne suivent pas la distribution normale alors il faut faire le test de wilcoxon
# rquery.t.test(x, y = NULL, paired = FALSE, graph = TRUE, ...)
# STUDENT  : Comparaison des moyennes des deux populations via la fonction rquery_t_test 

### Pour chaque variable quantitative :
# + analyse visuelle : 
    # histogramme de chaque population
    # boxplot variable en fonction de Attrition_Flag
    # plot de la variable par Attrition_Flag
# + statistiques descriptives des deux echantillons: Min.,1st Qu.,Median,Mean,3rd Qu.,Max.
# + rquery_t_test => qui donne le resultat du test si l'ensemble des critères est respecté (meme variance et distribution selon une loi normale)
# + test de wilcoxon si l'un des deux échantillons ne respecte pas la loi normale

#### 3.5.2 - Customer_Age ----
# desc_stat(data, data_stay, data_quit, "Customer_Age", "Attrition_Flag")
desc_stat(data, data_reg_stay, data_reg_quit, "Customer_Age", "Attrition_Flag")
test_stat(data_reg_stay, data_reg_quit, "Customer_Age", c("stay", "quit"))
# summary(data_stay$Customer_Age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 26.00   41.00   46.00   46.26   52.00   73.00 
# summary(data_quit$Customer_Age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
# 26.00   41.00   47.00   46.66   52.00   68.00 
# variance data_stay$Customer_Age => 65.30509
# variance data_quit$Customer_Age => 58.76221
# STUDENT => Use a non parametric test like Wilcoxon test.
# WILCOXON => W = 511132, p-value = 0.3884 => Les deux échantillons ne sont pas significativement différents.
# la variable n'explique pas les difference de valeur de l'Attrition_Flag => elle ne sera pas conserver pour la recherche du meuilleur modele

#### 3.5.3 - Dependent_count ----
# desc_stat(data, data_stay, data_quit, "Dependent_count", "Attrition_Flag")
desc_stat(data, data_reg_stay, data_reg_quit, "Dependent_count", "Attrition_Flag")
test_stat(data_reg_stay, data_reg_quit, "Dependent_count", c("stay", "quit"))
# summary(data_stay$Dependent_count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   1.000   2.000   2.335   3.000   5.000 
# summary(data_quit$Dependent_count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   2.000   2.000   2.403   3.000   5.000 
# variance data_stay$Dependent_count=> 1.698405
# variance data_quit$Dependent_count => 1.625651
# STUDENT => Use a non parametric test like Wilcoxon test.
# WILCOXON => W = 513114, p-value = 0.2975 => Les deux échantillons ne sont pas significativement différents.


#### 3.5.4 - Months_on_book ----
# desc_stat(data, data_stay, data_quit, "Months_on_book", "Attrition_Flag")
desc_stat(data, data_reg_stay, data_reg_quit, "Months_on_book", "Attrition_Flag")
test_stat(data_reg_stay, data_reg_quit, "Months_on_book", c("stay", "quit"))
# summary(data_stay$Months_on_book)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 13.00   31.00   36.00   35.88   40.00   56.00 
# summary(data_quit$Months_on_book)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 13.00   32.00   36.00   36.18   40.00   56.00 
# var(data_stay$Months_on_book)
# [1] 64.34943
# var(data_quit$Months_on_book)
# [1] 60.78617
# STUDENT => Use a non parametric test like Wilcoxon test.
# WILCOXON => W = 509041, p-value = 0.4808 => Les deux échantillons ne sont pas significativement différents.

#### 3.5.5 - Total_Relationship_Count ----
# desc_stat(data, data_stay, data_quit, "Total_Relationship_Count", "Attrition_Flag")
desc_stat(data, data_reg_stay, data_reg_quit, "Total_Relationship_Count", "Attrition_Flag")
test_stat(data_reg_stay, data_reg_quit, "Total_Relationship_Count", c("stay", "quit"))
# summary(data_stay$Total_Relationship_Count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   3.000   4.000   3.915   5.000   6.000 
# summary(data_quit$Total_Relationship_Count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    2.00    3.00    3.28    5.00    6.00 
# var(data_stay$Total_Relationship_Count)
# 1] 2.337686
# var(data_quit$Total_Relationship_Count)
# [1] 2.489395
# STUDENT => Use a non parametric test like Wilcoxon test.
# WILCOXON => W = 374438, p-value < 2.2e-16 => Les deux échantillons sont significativement différents.

#### 3.5.6 - Months_Inactive_12_mon ----
# desc_stat(data, data_stay, data_quit, "Months_Inactive_12_mon", "Attrition_Flag")
desc_stat(data, data_reg_stay, data_reg_quit, "Months_Inactive_12_mon", "Attrition_Flag")
test_stat(data_reg_stay, data_reg_quit, "Months_Inactive_12_mon", c("stay", "quit"))
# > summary(data_stay$Months_Inactive_12_mon)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  0.000   1.000   2.000   2.274   3.000   6.000
# > summary(data_quit$Months_Inactive_12_mon)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.000   2.000   3.000   2.693   3.000   6.000
# > var(data_stay$Months_Inactive_12_mon)
# [1] 1.033763
# > var(data_quit$Months_Inactive_12_mon)
# 1] 0.8093216
# STUDENT => Use a non parametric test like Wilcoxon test.
# WILCOXON => W = 634140, p-value < 2.2e-16 => Les deux échantillons sont significativement différents.

#### 3.5.7 - Contacts_Count_12_mon ----
# desc_stat(data, data_stay, data_quit, "Contacts_Count_12_mon", "Attrition_Flag")
desc_stat(data, data_reg_stay, data_reg_quit, "Contacts_Count_12_mon", "Attrition_Flag")
test_stat(data_reg_stay, data_reg_quit, "Contacts_Count_12_mon", c("stay", "quit"))
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
# STUDENT => Use a non parametric test like Wilcoxon test.
# WILCOXON => w = 662803, p-value < 2.2e-16=> Les deux échantillons sont significativement différents.

#### 3.5.8 - Credit_Limit ----
# desc_stat(data, data_stay, data_quit, "Credit_Limit", "Attrition_Flag")
desc_stat(data, data_reg_stay, data_reg_quit, "Credit_Limit", "Attrition_Flag")
test_stat(data_reg_stay, data_reg_quit, "Credit_Limit", c("stay", "quit"))
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
# STUDENT => Use a non parametric test like Wilcoxon test.
# WILCOXON => W = 465106, p-value = 0.006881=> Les deux échantillons ne sont pas significativement différents.

#### 3.5.9 - Total_Revolving_Bal ----
# desc_stat(data, data_stay, data_quit, "Total_Revolving_Bal", "Attrition_Flag")
desc_stat(data, data_reg_stay, data_reg_quit, "Total_Revolving_Bal", "Attrition_Flag")
test_stat(data_reg_stay, data_reg_quit, "Total_Revolving_Bal", c("stay", "quit"))
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
# STUDENT => Use a non parametric test like Wilcoxon test.
# WILCOXON
# W = 324850, p-value < 2.2e-16=> Les deux échantillons sont significativement différents.

#### 3.5.10 - Avg_Open_To_Buy ----
# desc_stat(data, data_stay, data_quit, "Avg_Open_To_Buy", "Attrition_Flag")
desc_stat(data, data_reg_stay, data_reg_quit, "Avg_Open_To_Buy", "Attrition_Flag")
test_stat(data_reg_stay, data_reg_quit, "Avg_Open_To_Buy", c("stay", "quit"))
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
# STUDENT => Use a non parametric test like Wilcoxon test.
# WILCOXON => W = 521333, p-value = 0.09853 => Les deux échantillons  ne sont pas significativement différents.

#### 3.5.11 - Total_Amt_Chng_Q4_Q1 ----
# desc_stat(data, data_stay, data_quit, "Total_Amt_Chng_Q4_Q1", "Attrition_Flag")
desc_stat(data, data_reg_stay, data_reg_quit, "Total_Amt_Chng_Q4_Q1", "Attrition_Flag")
test_stat(data_reg_stay, data_reg_quit, "Total_Amt_Chng_Q4_Q1", c("stay", "quit"))
# > summary(data_stay$Total_Amt_Chng_Q4_Q1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2560  0.6430  0.7430  0.7725  0.8600  3.3970 
# > summary(data_quit$Total_Amt_Chng_Q4_Q1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.5445  0.7010  0.6943  0.8560  1.4920 
# > var(data_stay$Total_Amt_Chng_Q4_Q1)
# [1] 0.04742953
# > var(data_quit$Total_Amt_Chng_Q4_Q1)
# # [1] 0.04619247
# STUDENT => Use a non parametric test like Wilcoxon test.
# WILCOXON => W = 414090, p-value = 2.874e-11=> Les deux échantillons sont significativement différents.

#### 3.5.12 - Total_Trans_Amt ----
# desc_stat(data, data_stay, data_quit, "Total_Trans_Amt", "Attrition_Flag")
desc_stat(data, data_reg_stay, data_reg_quit, "", "Attrition_Flag")
test_stat(data_reg_stay, data_reg_quit, "Total_Trans_Amt", c("stay", "quit"))
# STUDENT => Use a non parametric test like Wilcoxon test.
# WILCOXON => W = 345364, p-value < 2.2e-16=> Les deux échantillons sont significativement différents.

#### 3.5.13 - Total_Trans_Ct ----
# desc_stat(data, data_stay, data_quit, "Total_Trans_Ct", "Attrition_Flag")
desc_stat(data, data_reg_stay, data_reg_quit, "", "Attrition_Flag")
test_stat(data_reg_stay, data_reg_quit, "Total_Trans_Ct", c("stay", "quit"))
# > summary(data_stay$Total_Trans_Ct)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 11.00   54.00   71.00   68.67   82.00  139.00 
# > summary(data_quit$Total_Trans_Ct)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 10.00   37.00   43.00   44.93   51.00   94.00 
# > var(data_stay$Total_Trans_Ct)
# [1] 525.2811
# > var(data_quit$Total_Trans_Ct)
# [1] 212.2391
# STUDENT => Use a non parametric test like Wilcoxon test.
# WILCOXON => W = 225446, p-value < 2.2e-16=> Les deux échantillons sont significativement différents.

#### 3.5.13.1 -  Total_Trans_Ct & Total_Trans_Amt ----
# require(MASS)
# require(dplyr)
#Test de graphe avec grid  
# p1 <- data %>%
#     dplyr::select(Total_Trans_Ct,Attrition_Flag) %>%
#     ggplot(aes(x=Total_Trans_Ct,fill=Attrition_Flag)) +
#     geom_bar(alpha=0.4,position="dodge") +
#     labs(title="Distribution of Total Transaction Count by Customer type", x="Total Transaction Count", y="Count")
# p2 <- data %>%
#     dplyr::select(Total_Trans_Amt,Attrition_Flag) %>%
#     ggplot(aes(x=Total_Trans_Amt,fill=Attrition_Flag)) +
#     geom_density(alpha=0.4) +
#     labs(title="Distribution of Total Transaction Amount by Customer type", x="Total Transaction Amount", y="Density")
# grid.arrange(p1, p2, nrow = 2)


#### 3.5.14 - Total_Ct_Chng_Q4_Q1 ---- 
# desc_stat(data,data_stay, data_quit, "Total_Ct_Chng_Q4_Q1", "Attrition_Flag")
desc_stat(data, data_reg_stay, data_reg_quit, "Total_Ct_Chng_Q4_Q1", "Attrition_Flag")
test_stat(data_reg_stay, data_reg_quit,"Total_Ct_Chng_Q4_Q1",c("stay", "quit"))
# > summary(data_stay$Total_Ct_Chng_Q4_Q1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0280  0.6170  0.7210  0.7424  0.8330  3.7140 
# > summary(data_quit$Total_Ct_Chng_Q4_Q1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.4000  0.5310  0.5544  0.6920  2.5000 
# > var(data_stay$Total_Ct_Chng_Q4_Q1)
# [1] 0.05200885
# > var(data_quit$Total_Ct_Chng_Q4_Q1)
# [1] 0.05146261
# STUDENT => Use a non parametric test like Wilcoxon test.
# WILCOXON => W = 263315, p-value < 2.2e-16=> Les deux échantillons sont significativement différents.

#### 3.5.15 - Avg_Utilization_Ratio ----
# desc_stat(data, data_reg_stay, data_reg_quit, data_stay, data_quit, "Avg_Utilization_Ratio", "Attrition_Flag")
desc_stat(data, data_reg_stay, data_reg_quit, "Avg_Utilization_Ratio", "Attrition_Flag")
test_stat(data_reg_stay, data_reg_quit, "Avg_Utilization_Ratio")
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
# STUDENT => Use a non parametric test like Wilcoxon test.
# WILCOXON => W = 325727, p-value < 2.2e-16=> Les deux échantillons sont significativement différents.

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
# ==> Ce sont probablement les variables les plus explicatives sur le départ ou non des clients.

### 4 - Matrice de correlation entre variables ----

### 4.1 - Graphique des corrélations entre chacune des variables avec la méthode de spearman  ----

# Graphique des corrélations entre chacune des variables avec la méthode de spearman
cor_spearman <-
    cor(data[, sapply(data, is.numeric)], method = 'spearman')

# Visualisation avec une carte de chaleur de la matrice de corrélation de pearson
    round(3) %>% 
    hchart() %>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_title(text = "Spearman's correlation coefficients", align = "center") %>%
    hc_legend(align = "center") %>%
    hc_colorAxis(stops = color_stops(colors = viridis::inferno(10))) %>%
    hc_plotOptions(series = list(boderWidth = 0,
                                 dataLabels = list(enabled = TRUE)))

#  Visualisation des  plus fortes corrélations ressortant de la matrice de corrélation :

    # Custumer age  ~  months of book 
ggplot(data, aes(x=Customer_Age, y=Months_on_book)) + geom_point(color = "black",size= 0.3) + theme_classic() + ggtitle("Months on book vs Customer Age")

    # Total Transaction Count  ~  Total Transaction Amount (0.81)
ggplot(data, aes(x=Total_Trans_Amt, y= Total_Trans_Ct)) + geom_point(color = "red",size = 0.3) + theme_classic() + ggtitle("Total Trans Amt vs Total Trans Ct")

    # Total Revovlving Balance  ~ Average Utilization Ratio (0.62)
ggplot(data, aes(x=Total_Revolving_Bal, y= Avg_Utilization_Ratio)) + geom_point(color = "blue",size= 0.3) + theme_classic() + ggtitle("Total Revolving Bal vs Avg Utilization Ratio")

    # Avg_Open_To_Buy ~ Avg_Utilization_Ratio (-0.686)
ggplot(data, aes(x=Avg_Open_To_Buy, y=Avg_Utilization_Ratio)) + geom_point(color = "green",size= 0.3) + theme_classic() + ggtitle("Avg_Open_To_Buy ~ Avg_Utilisation_Ratio")

    # Avg_Utilization_Ratio ~ Total_Revolving_Bal (0.709)
ggplot(data, aes(x=Avg_Utilization_Ratio, y=Total_Revolving_Bal)) + geom_point(color = "yellow",size= 0.3) + theme_classic() + ggtitle("Avg_Utilisation_Ratio ~ Total_Revolving_Bal")

# ==> Cela nous donne des piste de simplification du modele de regression logistique pour le rendre plus robuste

### 4.2 - Graphique des corrélations entre l'Attrition_Flag et les autres variables ----
# correlation des variables avec Attriction_Flag  dans l'ordre decroissant
# mutates
data_corr <- data %>% mutate_if(is.factor,as.numeric)
# compute correlation 
correlation= cor(data_corr)
correlation
# correlation as data.frame d'une colonne : Attriction_Flag
target_corr= as.data.frame(correlation[,1])
# correlation column name
colnames(target_corr) <-'Correlation'
# sort dataframe
target_corr <- abs(target_corr )%>% arrange(desc(Correlation))
# exclude target
 target_corr <- target_corr %>% filter(Correlation<1) 
# round
target_corr <- round(target_corr,2)
# LITE DES VARIABLES DANS L'ORDRE DE LEUR CORRELATION
target_corr

# RESULTATS :
    # Total_Trans_Ct                  0.37
    # Total_Ct_Chng_Q4_Q1             0.29
    # Total_Revolving_Bal             0.26
    # Contacts_Count_12_mon           0.20
    # Avg_Utilization_Ratio           0.18
    # Total_Trans_Amt                 0.17
    # Months_Inactive_12_mon          0.15
    # Total_Relationship_Count        0.15
    # Total_Amt_Chng_Q4_Q1            0.13
    # Gender                          0.04
    # Credit_Limit                    0.02
    # Dependent_count                 0.02
    # Marital_Status                  0.02
    # Customer_Age                    0.02
    # Income_Category                 0.02
    # Months_on_book                  0.01
    # Card_Category                   0.01
    # Education_Level                 0.01
    # Avg_Open_To_Buy                 0.00

# PLOT CORRELATION
target_corr %>% arrange(desc(Correlation)) %>%
    ggplot(aes(x=Correlation,
               y=reorder(rownames(target_corr),Correlation),
               fill=Correlation)) +
    geom_col(color='black') + labs(title='Corrélation avec Attriction_Flag (en valeur absolue)',y='') +
    theme_classic() +
    theme(legend.position = 'none')

# si on veut connaitre le signe et le voir
 
# mutates
data_corr <- data %>% mutate_if(is.factor,as.numeric)
# compute correlation
correlation= cor(data_corr)
correlation
# correlation as data.frame d'une colonne : Attriction_Flag
target_corr= as.data.frame(correlation[,1])
# correlation column name
colnames(target_corr) <-'Correlation'
# sort dataframe
target_corr <- (target_corr )%>% arrange(desc(Correlation))
# exclude target
target_corr <- target_corr %>% filter(Correlation<1)
# round
target_corr <- round(target_corr,2)
target_corr
# PLOT CORRELATION
target_corr %>% arrange(desc(Correlation)) %>%
    ggplot(aes(x=Correlation,
               y=reorder(rownames(target_corr),Correlation),
               fill=Correlation)) +
    geom_col(color='black') + labs(title='Corrélation avec Attriction_Flag, en tenant compte du signe',y='') +
    theme_classic() +
    theme(legend.position = 'none')
# on obtient :
# Correlation
# Contacts_Count_12_mon           0.20
# Months_Inactive_12_mon          0.15
# Dependent_count                 0.02
# Marital_Status                  0.02
# Customer_Age                    0.02
# Income_Category                 0.02
# Months_on_book                  0.01
# Education_Level                 0.01
# Avg_Open_To_Buy                 0.00
# Card_Category                  -0.01
# Credit_Limit                   -0.02
# Gender                         -0.04
# Total_Amt_Chng_Q4_Q1           -0.13
# Total_Relationship_Count       -0.15
# Total_Trans_Amt                -0.17
# Avg_Utilization_Ratio          -0.18
# Total_Revolving_Bal            -0.26
# Total_Ct_Chng_Q4_Q1            -0.29
# Total_Trans_Ct                 -0.37

### 5 - Réaliser des ACM sur Attrition_flag ----
# http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/74-afc-analyse-factorielle-des-correspondances-avec-r-l-essentiel/

# sur quelles variables ? faire une CAH en complément

#  pour nous aider a trouver les perimetres de nos modeles (il y en aura surement pls)

# variable dependante Y = départ ou non : attrition_flag
# variable qualitative donc on fera une régression logistique

# variable explicatives
# age, genre, niveau education, statut marital, personnes à charge, revenu, type de carte, période de relation avec la banque...


### 6 - Classifications par clustering avec K-means ----
# A réaliser à partir des résultats de l'ACM réalisée precedemment
# https://www.datanovia.com/en/fr/blog/visualisation-du-clustering-k-means-dans-r-guide-etape-par-etape/

# "Il existe clairement des différences entre les deux types de clients.
# La principale différence concerne le montant des transactions.
# La principale différence concerne le nombre des transactions.
# La principale différence concerne le montant des impayés.
# Pouvons-nous maintenant diviser davantage les différences ?
# Nous pouvons essayer de regrouper les données et voir s'il existe des modèles identifiables dans l'ensemble de données
# qui peuvent nous aider à séparer une meilleure rotation des clients:
# Ensemble de données de clustering en 6 dimensions, nous pouvons voir que la 5ème dimension couvre
# environ 96% du total des clients en rotation, cela pourrait signifier que la 5ème dimension du clustering
# contient la plupart des fonctionnalités que nous devons connaître à leur sujet.
# Au lieu de cela, les deux dimensions de l'analyse en composantes principales expliquent
# environ 31% de la variance totale des données"

# TODO réduire les 23 variables à 13 plus significatives (4 quali et 9 quanti à priori),
# qual : Gender, Marital_Status, Income_Category, Education_Level
# quant: Avg_Utilization_Ratio, Total_Ct_Chng_Q4_Q1, Total_Trans_Ct, Total_Trans_Amt, Total_Amt_Chng_Q4_Q1, Total_Revolving_Bal,
# Months_Inactive_12_mon, Contacts_Count_12_mon, Total_Relationship_Count
# faire une cAH et tatonner parmi les 13 pour trouver des clusters

data_k <- data %>% mutate_if(is.factor, as.numeric)
data_kmeans <- scale(data_k[, c(-1, -2, -4, -8, -9, -13, -15)])
# seed
set.seed(123)

# determining nb clusters with elbow method (very low)
fviz_nbclust(data_kmeans, kmeans, method = "wss") +
    geom_vline(xintercept = 4, linetype = 2)+
    labs(subtitle = "Elbow method")
# ==> 4 clusters

# determining nb clusters with Silhouette method (quickly)
fviz_nbclust(data_kmeans, kmeans, method = "silhouette")+
    labs(subtitle = "Silhouette method")
# ==> 2 clusters

# determining nb clusters with gap statistics method (very very low)
fviz_nbclust(data_kmeans, kmeans, nstart = 25, method = "gap_stat", nboot = 50)+
    labs(subtitle = "Gap statistic method")
# ==> 1 cluster

# if ("NbClust" %in% rownames(installed.packages()) == FALSE) {install.packages("NbClust", dependencies=TRUE)};library(NbClust)
# nb <- NbClust(data_kmeans, distance = "euclidean", min.nc = 2,
#               max.nc = 10, method = "kmeans")

# Retrait du taux d'attrition & CLUSTERING en 4 groupes avec KMEANS
res.km <- kmeans(data_kmeans, 4, nstart = 25)
# RÉDUCTION DE DIMENSION À L'AIDE DU PCA
# Réduction de chacune des 13 variables quantitatives à 13 groupe/dimension en utilisant l'ACP(prcomp)
res.pca <- prcomp(data_kmeans, scale = FALSE)

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

# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)

variance.percent <- eigenvalue$variance.percent

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

my_gp <- grid::gpar(fontsize=18, font=1)
my_top <- grid::textGrob("Kmeans cluster and PCA", gp=my_gp)
gridExtra::grid.arrange(c2, c3, ncol=2, top=my_top)

fviz_pca_var(res.pca,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)

#  "#494368", "#033f63"
# factoextra::fviz_cluster(res.km, data = data_kmeans,
#                          palette = c("#ceec97", "#f4b393", "#fc60a8", "#7a28cb"),
#                          geom = "point",
#                          ellipse.type = "convex",
#                          ggtheme = theme_bw()
# )

# CONCLUSION

# THe 5 top features of determing a customer's attrition:
# Total Transaction Count
# Total Revolving Balance
# Total Transaction Amount
# Total Relationship Count
# Total Count Change


### 7 - Régression logistique et AIC ----

# partage du dataset en 70/30 
intrain <- createDataPartition(data$Attrition_Flag, p=0.7, list = F, times = 1)
# creation des datasets: testing (30%) & training (70%) pour minimiser le ridque de surentraienemnt 
training <- data[intrain,]
testing <- data[-intrain,]

# ---- modification des classes lorsqu'il y a des trop fort desequilibres

# CLASSE => message d'erreur 

data_select <- data[, c(-2, -4, -8, -9, -13, -15)]
# classe_marital_class
data_select[which(data_select$Marital_Status %in% c("Divorced","Single")),"Marital_Status"] <- "Single"
# Card_category_class
#data_select[which(data_select$Card_Category %in% c("Gold","platinum","Silver")),"Card_Category"]<-"Others"
# Income_category_class

levels(data_select$Income_Category) = c(levels(data_select$Income_Category), "Less than $60K", "More than $60K")
data_select[which(data_select$Income_Category %in% c("Less than $40K", "$40K - $60K")),"Income_Category"] <- "Less than $60K"
data_select[which(data_select$Income_Category %in% c("$60K - $80K","$80K - $120K","$120K +")),"Income_Category"] <- "More than $60K"

full.model <- lm(Attrition_Flag ~., data = data_select)
summary(full.model)




model_quali<-glm(Attrition_Flag~Income_Category, data=data_select, family= binomial(logit))

# Interprétation
# model_quali
summary(model_quali)
exp(coef(model_quali))

# Matrice de confusion
appren.p <- cbind(data_reg, predict(model_quali, newdata = data_reg, type = "link", se = TRUE))
appren.p <- within(appren.p, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
})
appren.p <- cbind(appren.p, pred.chd = factor(ifelse(appren.p$PredictedProb > 0.5, 1, 0)))
colnames(appren.p)
appren.p<-appren.p[,c("binattrition","Customer_Age","Card_Category","fit","PredictedProb","pred.chd")]
(m.confusion <- as.matrix(table(appren.p$pred.chd, appren.p$binattrition)))

# Taux de bien classé
(m.confusion[1,1]+m.confusion[2,2]) / sum(m.confusion)

# Sensibilité
(m.confusion[2,2]) / (m.confusion[2,2]+m.confusion[1,2])

# Sensibilité
(m.confusion[2,2]) / (m.confusion[2,2]+m.confusion[1,2])

# Spécificité
(m.confusion[1,1]) / (m.confusion[1,1]+m.confusion[2,1])


# ODs ratio
exp(cbind(coef(model_quali), confint(model_quali)))
library(questionr)
odds.ratio(model_quali)
install.packages("GGally")
library(GGally)
library(broom.helpers)
ggcoef_model(model_quali, exponentiate = TRUE)


#Model complet test
#data_select<-data
#data_select$Attrition_Flag <- as.factor(data_select$Attrition_Flag)
#data_select<-c("Attrition_Flag","Customer_Age","Dependent_count","Months_on_book","Total_Relationship_Count","Months_Inactive_12_mon","Contacts_Count_12_mon","Credit_Limit","Total_Revolving_Bal","Avg_Open_To_Buy","Total_Amt_Chng_Q4","Total_Trans_Amt","Total_Trans_Ct","Total_Ct_Chng_Q4_Q1","Avg_Utilization_Ratio")
#data_select<-as.factor(data_select)
#simple.model <- glm(Attrition_Flag ~1, data = data_select, family = binomial)
#summary(simple.model)


#Nouveau test regression
data$Attrition_Flag<-as.factor(data$Attrition_Flag)
class(data$Attrition_Flag)


#Prendre des randoms pour que les résultats puissent être reproductibles
set.seed(2000)


#création de  notre partitionnement par 70/30
part<-createDataPartition(data$Attrition_Flag,p=0.7,
                          list = F,
                          times = 1)



#Nouveau test regression
data$Attrition_Flag<-as.character(data$Attrition_Flag)
data$Attrition_Flag[data$Attrition_Flag=="Existing Customer"]<-"Stay"
data$Attrition_Flag[data$Attrition_Flag=="Attrited Customer"]<-"Quit"
data$Attrition_Flag<-as.factor(data$Attrition_Flag)
class(data$Attrition_Flag)

#Formation d'une matrice de confusion
Attrition_step <-ifelse(predictions>=d[[1]],"Stay","Quit")

#création d'ensembles de données de tests
test1 <-data[part,]
test2<-data[-part,]
set.seed(2000)

# REGRESSION LOGISTIQUE STEP- 10 000 STEPS
Model1 <-glm(as.factor(Attrition_Flag)~.,data=data,
             family=binomial(logit))
Model_step <-step(Model1,
                  direction = "both",
                  steps = 10000,
                  trace = F)
predictions <-predict(Model_step,test2,
                      type = "response")
roc_step<-roc(response=test2$Attrition_Flag,predictor=predictions)
plot(roc_step)







#point de coupure
pred_step <-prediction(predictions,test2$Attrition_Flag)
plot(performance(pred_step,"tpr","fpr"),colorize=T)
auc_step<-performance(pred_step,"auc")
auc_step
roc_step<-roc(response=test2$Attrition_Flag,predictor=predictions)
plot(roc_step)
d<-coords(roc_step,"best","threshold",transpose=T)
d
roc_step
#Formation d'une matrice de confusion
Attrition_step <-ifelse(predictions>=d[[1]],"Stay","Quit")
test2$Attrition_Flag<-as.factor(test2$Attrition_Flag)
Attrition_step<-as.factor(Attrition_step)


#matrice de confusion
cm_1 <-confusionMatrix(test2$Attrition_Flag,Attrition_step)
cm_1


set.seed(2000)
#MODEL 2
#regression logiqtique -k-fold 
#
#k-folds(folds=10) 

ctrl_specs <-trainControl(method = "cv",
                          savePredictions = "all",
                          number = 10,
                          classProbs = T)
dim(training)

Model2<- train(Attrition_Flag~.,data = training,
               method="glm",
               family=binomial,
               trControl=ctrl_specs)
Model2

ctrl_specs <-trainControl(method = "cv",
                          savePredictions = "all",
                          number = 10,
                          classProbs = T)
dim(test1)
Model2<- train(Attrition_Flag~.,data = test1,
               method="glm",
               family=binomial,
               trControl=ctrl_specs)
Model2



#model 3 méthode lasso regression
lambda_vector <-10^seq(-5,5,length=500)
set.seed(2000)
Model3 <-train(Attrition_Flag~.,data =test1,
               method="glmnet",
               tuneGrid=expand.grid(alpha=1,lambda=lambda_vector),
               trControl=ctrl_specs,
               preProcess=c("center","scale"),
               na.action = na.omit)
Model3
Model3$bestTune$lambda
#méthode LASSO regression coefficients(paramètres estimés)
round(coef(Model3$finalModel,Model3$bestTune$lambda),3)
varImp(Model3)

#importance des variables
ggplot(varImp(Model3))+
    labs(title = "Rang importance des vars")



##autres modèles
log.model <- glm(Attrition_Flag ~ ., data=data, family=binomial(link='logit'))
summary(log.model)
step(log.model, direction="backward", trace=FALSE)

modele_final<-glm(formula = Attrition_Flag ~ Customer_Age + Gender + Dependent_count + 
                      Marital_Status + Income_Category + Card_Category + Total_Relationship_Count + 
                      Months_Inactive_12_mon + Contacts_Count_12_mon + Credit_Limit + 
                      Total_Revolving_Bal + Total_Amt_Chng_Q4_Q1 + Total_Trans_Amt + 
                      Total_Trans_Ct + Total_Ct_Chng_Q4_Q1, family = binomial(link = "logit"), 
                  data = data)
pred<- predict(modele_final, test2, type='response')
plot(modele_final)

hist(pred)

