# Objectif: Prédire la résiliation d’un client bancaire
# Kaggle: https://www.kaggle.com/sakshigoyal7/credit-card-customers
# Jeux d’apprentissage : 10 K lignes / 23 colonnes

# https://www.kaggle.com/josephchan524/bankchurnersclassifier-recall-97-accuracy-95

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
print (skim(data))


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
# 3 - Analyse exploratoire des données (EDA) ----
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
# skim(data)

#### 3.3 - stats des partis et restés ----
data_quit <- data[(data$Attrition_Flag) == 1, ]
skim(data_quit)
str(data_quit)
# 1627 partis

data_stay <- data[(data$Attrition_Flag) == 0, ]
skim(data_stay)
str(data_stay)
# 8500 restés

### 3.4 - 5 variables qualitatives : Analyse ----
# Tableaux de frequences par Attrition_Flag
# On cherche a trouver les relations entre 2 variables, à trouver les valeurs aberrantes (outliers)
# et exclure les variables sans intérêt

#### 3.4.1 - Gender ----
search_cors(data, data_stay, data_quit, "Gender")
# parmi Existing Customer "0.521" soit 52.1 % 
# Attrited Customers : "0.572" soit 57.2 %
# 57 % des personnes qui ont fermé leur compte sont des femmes tandis que 52 % des clients actifs sont des femmes.
# Les femmes sont donc en sur representation parmi les desabonnés avec une différence de proportion de 5%.

#### 3.4.2 - Marital_Status ----
search_cors(data, data_stay, data_quit, "Marital_Status")
# "0.074" "0.074" => parmi les divorcés on retrouve la meme proportion de clients qui partent que ceux qui restent.
# "0.073" "0.079" => parmi les statut inconnus on retrouve une proportion tres legerement plus elevée de clients qui partent  (+0.6%)
# => peu de différence de proportion pour les divorcés et les statuts inconnus.
# "0.385" "0.411" => parmi les celibataires, on trouve legerement plus de personnes qui se desabonnent (+2.6 %)
# "0.468" "0.436" => parmi les mariés on trouve legerement plus de personnes qui restent abonnés ( -3%)
#  = > il peut etre interessant de regrouper les classes divorcés et Unknown qui semblent avoir des proportions identiques

#### 3.4.3 - Card_Category  ----
search_cors(data, data_stay, data_quit, "Card_Category")
#  la proportion de catégorie de carte ne semble pas être tres différente entre abonnés et desabonnés
#  On observe pour ratio_Silver que la plus grosse différence de pourcentage dans les deux groupes est de 0.6 %.
#  "0.931" "0.934" => parmi les clients "carte Blue" peu de différence entre clients qui partent et qui restent (+0.03%)
#  "0.056" "0.050"  => parmi les clients "carte Silver" peu de différence entre clients qui partent et qui restent (-0.6 %)
#  "0.011" "0.013" => parmi les clients "carte Gold" peu de différence entre cleints qui partent et qui restent (+0.02%)
# "0.002" "0.003" => parmi les clients "carte Platinum" peu de différence entre clients qui partent et qui restent (+0.1%)

#### 3.4.4 - Income_Category ----
search_cors(data, data_stay, data_quit, "Income_Category")
# 6 classes : $120K +, $40K - $60K, $60K - $80K, $80K - $120K, Less than $40K, Unknown
#  "0.347" "0.376"  parmi les "Less than $40K" la proportion est  plus fortes pour les desabonnés  (+3 %)
#  "0.179" "0.167" parmi les "$40K - $60K" on observe une proportion legerement plus faible chez les desabonnés (-1.2%)
#  "0.143" "0.116" parmi les "$60K - $80K" on observe une proportion  plus faible chez les desabonnés (-2.7 %)
#  "0.152" "0.149" parmi les "$80K - $120K" on observe une proportion legerement plus faible chez les desabonnés (-0.3%)
#  "0.071" "0.077" parmi les "$120K +", on observe une proportion legerement plus forte chez les desabonnés (+0.6%)
# "0.109" "0.115" parmi les "Unknown", on observe une proportion legerement plus forte chez les desabonnés (+0.6%)
# => les proportions les plus notables  :
# sont chez les plus bas revenus (- 40 K$):  3 % de différence dans la proportion de desabonnés parmi les desabonnés.
# les revenus moyens ("40-80 K$") : on observe une différence de 1.2 % et 2.7 % des abonnés parmi l'ensemble des abonnés

#### 3.4.5 - Education_Level ----
search_cors(data, data_stay, data_quit, "Education_Level")
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

# en résumé
# qui partira le + : une femme célibataire à bas revenu (<40 K$) avec un niveau d'éducation très élevé
# qui restera le + : un homme marié à revenu moyen (40-80K$) avec un niveau d'éducation intermédiaire
# On retiendra Gender, Marital_Status, Income_Category, Education_Level

### 3.5 - 14 variables quantitatives : Analyse ----

#### 3.5.1 - Echantillonnage de 1000 restant et 1000 partant ----
sample_quit<-sample(1:dim(data_quit)[1],1000)
sample_stay<-sample(1:dim(data_stay)[1],1000)

# On utilise l'échantillonage pour les test de student et de wilcoxon (cet échantillonage )
data_reg <- rbind(data_quit[sample_quit,],data_stay[sample_stay,])
skim(data_reg)

# data_reg_quit
data_reg_quit <- data_reg[(data_reg$Attrition_Flag) == 1, ]
skim(data_reg_quit)
str(data_reg_quit)
# data_reg_stay 
data_reg_stay <- data_reg[(data_reg$Attrition_Flag) == 0, ]
skim(data_reg_stay)
str(data_reg_stay)

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
# + rquery_t_test => qui donne le resultat du test si l'ensemble des critères est respecté
# + test de wilcoxon si l'un des deux échantillons ne respecte pas la loi normale

#### 3.5.2 - Customer_Age ----
desc_stat(data, data_stay, data_quit, "Customer_Age", "Attrition_Flag")
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

#### 3.5.3 - Dependent_count ----
desc_stat(data, data_stay, data_quit, "Dependent_count", "Attrition_Flag")
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
# TODO on a des valeurs aberrantes on dirait sur ceux qui sont restés
# p value > 5%, on ne peut rejeter l'hypothèse nulle, les 2 echantillons ne sont pas forcément différents

#### 3.5.4 - Months_on_book ----
desc_stat(data, data_stay, data_quit, "Months_on_book", "Attrition_Flag")
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
desc_stat(data, data_stay, data_quit, "Total_Relationship_Count", "Attrition_Flag")
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
# p value < 5%, on rejette l'hypothèse nulle, les 2 echantillons sont différents

#### 3.5.6 - Months_Inactive_12_mon ----
desc_stat(data, data_stay, data_quit, "Months_Inactive_12_mon", "Attrition_Flag")
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
# WILCOXON => W = 634140, p-value < 2.2e-16 => Les 2 échantillons sont significativement différents.

#### 3.5.7 - Contacts_Count_12_mon ----
desc_stat(data, data_stay, data_quit, "Contacts_Count_12_mon", "Attrition_Flag")
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
# WILCOXON => w = 662803, p-value < 2.2e-16=> Les 2 échantillons sont significativement différents.

#### 3.5.8 - Credit_Limit ----
desc_stat(data, data_stay, data_quit, "Credit_Limit", "Attrition_Flag")
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
desc_stat(data, data_stay, data_quit, "Total_Revolving_Bal", "Attrition_Flag")
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
desc_stat(data, data_stay, data_quit, "Avg_Open_To_Buy", "Attrition_Flag")
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
# WILCOXON => W = 521333, p-value = 0.09853 => Les deux échantillons ne sont pas significativement différents.

#### 3.5.11 - Total_Amt_Chng_Q4_Q1 ----
desc_stat(data, data_stay, data_quit, "Total_Amt_Chng_Q4_Q1", "Attrition_Flag")
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
desc_stat(data, data_stay, data_quit, "Total_Trans_Amt", "Attrition_Flag")
test_stat(data_reg_stay, data_reg_quit, "Total_Trans_Amt", c("stay", "quit"))
# STUDENT => Use a non parametric test like Wilcoxon test.
# WILCOXON => W = 345364, p-value < 2.2e-16=> Les deux échantillons sont significativement différents.

#### 3.5.13 - Total_Trans_Ct ----
desc_stat(data, data_stay, data_quit, "Total_Trans_Ct", "Attrition_Flag")
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

#### 3.5.13.1 - Comparaison Total_Trans_Ct & Total_Trans_Amt ----
# Tentative comparaison distributions montant et volume de transactions sur partis et restés, mais axes pas clairs
p1 <- data %>%
    select(Total_Trans_Ct,Attrition_Flag) %>%
    ggplot(aes(x=Total_Trans_Ct,fill=Attrition_Flag)) +
    geom_bar(alpha=0.4,position="dodge") +
    labs(title="Distribution of Total Transaction Count by Customer type", x="Total Transaction Count", y="Count")
p2 <- data %>%
    select(Total_Trans_Amt,Attrition_Flag) %>%
    ggplot(aes(x=Total_Trans_Amt,fill=Attrition_Flag)) +
    geom_density(alpha=0.4) +
    labs(title="Distribution of Total Transaction Amount by Customer type", x="Total Transaction Amount", y="Density")
grid.arrange(p1, p2, nrow = 2)


# juste un test de graph gridding a priori, on garde ou pas ? peu pertinent à priori, les axes ne sont pas clairs

#### 3.5.14 - Total_Ct_Chng_Q4_Q1 ---- 
desc_stat(data, data_stay, data_quit, "Total_Ct_Chng_Q4_Q1", "Attrition_Flag")
test_stat(data_reg_stay, data_reg_quit, "Total_Ct_Chng_Q4_Q1", c("stay", "quit"))
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
desc_stat(data, data_stay, data_quit, "Avg_Utilization_Ratio", "Attrition_Flag")
test_stat(data_reg_stay, data_reg_quit, "Avg_Utilization_Ratio", c("stay", "quit"))
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

# Les 9 variables pour lequels les populations sont significativement différentes :
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

# Graphique des corrélations entre chacune des variables avec la méthode de spearman
spearman_graph_correlation(data)
# Visualisation des 5 plus fortes corrélations ressortant de la matrice de corrélation :

# Customer age and months of book are highly correlated (0.77)
# plus un client est d'âge avancé plus on peut deviner que le client est installé dans cette banque
# depuis un certain temps mais ce n'est pas forcément intuitif
# on ignorera cette relation car ses 2 variables n'ont pas été retenu
ggplot(data, aes(x=Customer_Age, y=Months_on_book)) + geom_point(color = "black", size= 0.3) + theme_classic() + ggtitle("Months on book vs Customer Age")

# Total Transaction Count and Total Transaction Amount are highly correlated (0.88)
# oui en effet cela peut paraitre logique
ggplot(data, aes(x=Total_Trans_Amt, y=Total_Trans_Ct)) + geom_point(color = "red", size = 0.3) + theme_classic() + ggtitle("Total Trans Amt vs Total Trans Ct")

# Total Revolving Balance and Average Utilization Ratio are correlated (0.71)
# oui en effet le taux d'utilisation de la carte est forcément lié au crédit renouvelable restant en fin de mois
ggplot(data, aes(x=Total_Revolving_Bal, y=Avg_Utilization_Ratio)) + geom_point(color = "blue", size= 0.3) + theme_classic() + ggtitle("Total Revolving Bal vs Avg Utilization Ratio")

# Credit_Limit and Avg_open_to_buy are correlated (0.93)
# oui en effet la limite de crédit conditionne le montant des achats et in extenso leur moyenne
ggplot(data, aes(x=Credit_Limit, y=Avg_Open_To_Buy)) + geom_point(color = "blue", size= 0.3) + theme_classic() + ggtitle("Credit_Limit vs Avg_open_to_buy")

# Avg_Utilization_Ratio and Avg_Open_To_Buy are negatively correlated (-0.69)
# oui en effet le taux d'utilisation conditionne le montant des achats et in extenso leur moyenne
ggplot(data, aes(x=Avg_Utilization_Ratio, y=Avg_Open_To_Buy)) + geom_point(color = "blue", size= 0.3) + theme_classic() + ggtitle("Avg_Utilization_Ratio vs Avg_Open_To_Buy")

# qu'en retire-t-on ??
# on peut retirer à priori de notre jeu de 13 variables avg_utilization_ratio et Total_Trans_Amt

### 5 - Réaliser des ACM sur Attrition_flag ? ----
# http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/74-afc-analyse-factorielle-des-correspondances-avec-r-l-essentiel/

# sur quelles variables ? faire une CAH en complément ?
# pour nous aider a trouver les perimetres de nos modeles (il y en aura surement plusieurs)

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

# Réduire les 23 variables à 13 plus significatives (4 quali et 9 quanti à priori),
# qual : Gender, Marital_Status, Income_Category, Education_Level
# quant: Avg_Utilization_Ratio, Total_Ct_Chng_Q4_Q1, Total_Trans_Ct, Total_Trans_Amt, Total_Amt_Chng_Q4_Q1, Total_Revolving_Bal,
# Months_Inactive_12_mon, Contacts_Count_12_mon, Total_Relationship_Count
# même à 11 ? en retirant aussi avg_utilization_ratio et Total_Trans_Amt en cl de la corrélation de spearman ?

# conversion quali ordinal en quanti
data_numerized <- data %>% mutate_if(is.factor, as.numeric)
data_numerized$Attrition_Flag[data_numerized$Attrition_Flag==1]<-0
data_numerized$Attrition_Flag[data_numerized$Attrition_Flag==2]<-1

# graphique de corrélation avec la variable cible Attrition_Flag seulement
graph_target_correlation(data_numerized, "Attrition_Flag")

# graphique de corrélations avec la méthode de spearman
spearman_graph_correlation(data_numerized)

# Retrait des colonnes qu'on considère inutiles
# (variables dépendantes entre elles et du coup aussi dépendantes de la variable cible)
# (variables indépendantes de la variable cible)
# customer_age, dependent_count, card_category, months_on_book,
# credit_limit, avg_open_to_buy, total_trans_amt, avg_utilization_ratio
data_k <- data_numerized[, c(-2, -4, -8, -9, -13, -15, -17, -20)]
# refaisons un graphique des corrélations entre chacune des 10 variables restantes
spearman_graph_correlation(data_k)
# ==> comme on a converti en numérique les variables quali,
# on observe curieusement une forte corrélation entre le revenu entrant et le genre (0,79)
# on notera une légère relation inverse entre Total_Relationship_Count et Total_Trans_Ct (-0,23)

# attention, standardisation (centrage réduction) nécessaire des données au préalable
data_kmeans <- scale(data.frame(data_k))

# seed
set.seed(123)

# determining nb clusters with elbow method (warning very low)
# observation de l'inertie intra-classe et recherche du "coude"
# où l'adjonction d'une classe ne correspond à rien
# (WARNING : LOW about 5 minute)
estimate_nb_cluster(data_kmeans)
# ==> donc 4 clusters devraient suffire

# KMEANS CLUSTERING en 4 groupes
res.km <- kmeans(data_kmeans, centers = 4, nstart = 25)

# nuage de points des 4 clusters
graphe_nuage_clusters(res.km, data_k)

# graphe des nuage de points par paires de variables
# (WARNING : LOW about 2 minute)
pairs(data_kmeans, col=c(1:2)[res.km$cluster])
# le income_category est central il est influé par 
# Gender, Education_Level, Marital_Status
# moins on est riche et plus on influe sur les autres variables quanti

# Réduction de dimension à l'aide d'une ACP
# Réduction de chacune des 11 variables quantitatives à 2 dimension en utilisant l'ACP(prcomp)

res.pca <- prcomp(data_k[, c(-1)], scale = TRUE)

# Analyse étendue des dimensions des 4 clusters avec ACP
graphe_acp_nuage_clusters(res.pca, res.km, data_numerized$Attrition_Flag)
# eigenvalue variance.percent cumulative.variance.percent
# Dim.1        1.8             16.4                        16.4
# Dim.2        1.5             13.5                        29.9
# Dim.3        1.3             11.8                        41.8
# Dim.4        1.0              9.3                        51.1
# Dim.5        1.0              9.1                        60.2
# Dim.6        1.0              8.9                        69.1

# cercle de corrélation
fviz_pca_var(res.pca,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)

# Que retire-t-on de ces 4 populations et réussir à trouver une interprétation
# (rappel : l'ACP permet une projection des 11 variables/dimensions retenues ici sur 2 dimensions dans un nuage de point,
# le kmeans n'est pas là que pour découper cette projection en n cluster ici 4 )
# les 6 premières dimensions ne parviennent pas à expliquer plus de 70% de la variance ou masse totale d'inertie
# de même leurs valeurs propres sont toutes supérieures ou égales à 1 (critère de Kaiser) ce qui ne permet pas vraiment d'en sélectionner de pertinentes)
# Les 2 premières dimensions ne représentent que 30% de la variance
# sur le kmeans de ces 2 premières, le cluster n°3 (sur 4) représente 98% de ceux qui sont partis (1595 / 1627 individus) mais du coup cela ne signifie encore moins de chose
# CL : les cumuls de variance ne nous permettent pas ici d'obtenir pour le moment une qualité globale explicative de l’analyse via ACP/kmeans.

# graphique de liaison entre Gender, Education_Level et Income_Category
custom_theme <- theme_bw() +
    theme(plot.title = element_text(face = "bold", color = "black", size=14),
          plot.subtitle = element_text(face = "italic", color = "black", size=12),
          axis.text = element_text(color = "black"), legend.text = element_text(size=10),
          legend.title = element_text(size = 12), legend.position = "none",
          strip.background =element_rect(fill="#666666"), strip.text = element_text(color="white", face="bold"),
          plot.caption = element_text(face = "italic"))

sample_data <- data %>%
    select(Attrition_Flag, Education_Level, Gender, Income_Category) %>%
    filter(Attrition_Flag == 1) %>%
    count(Attrition_Flag, Education_Level, Gender, Income_Category) %>%
    mutate(
        interesting_group = ifelse(Gender == "F" & Education_Level == "Graduate" &
                                       Income_Category == "Less than $40K",
                                   "Interesting", "Not Interesting")
    )

sample_data %>%
    ggplot(aes(y = n,
               axis1 = Gender, axis2 = Education_Level, axis3 = Income_Category)) +
    ggalluvial::geom_alluvium(aes(fill = interesting_group), alpha = 0.4,  absolute = FALSE) +
    ggalluvial::geom_stratum(absolute = FALSE, width = 0.45) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), absolute = FALSE) +
    scale_x_discrete(limits = c("Gender", "Education Level","Income Category"), expand = c(.1, .05)) +
    custom_theme + ggsci::scale_fill_nejm() +
    labs(
        title = "Différents Groupes avec le churn",
        y = "Nb",
        caption = "Les femmes diplômées qui sont bas dans la catégorie revenus montrent une bonne portion du churn"
    )

# extension avec le marital_status
sample_data <- data %>%
    select(Attrition_Flag, Education_Level, Gender, Income_Category, Marital_Status) %>%
    filter(Attrition_Flag == 1) %>%
    count(Attrition_Flag, Education_Level, Gender, Income_Category, Marital_Status) %>%
    mutate(
        interesting_group = ifelse(Gender == "F" & Education_Level == "Graduate" &
                                   Income_Category == "Less than $40K" & Marital_Status %in% c("Single", "Married"),
                                   "Interesting", "Not Interesting")
    )

sample_data %>%
    ggplot(aes(y = n,
               axis1 = Gender, axis2 = Marital_Status, axis3 = Education_Level, axis4 = Income_Category)) +
    ggalluvial::geom_alluvium(aes(fill = interesting_group), alpha = 0.4,  absolute = FALSE) +
    ggalluvial::geom_stratum(absolute = FALSE, width = 0.45) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), absolute = FALSE) +
    scale_x_discrete(limits = c("Gender", "Marital_Status", "Education Level","Income Category"), expand = c(.1, .05)) +
    custom_theme + ggsci::scale_fill_nejm() +
    labs(
        title = "Différents Groupes avec le churn",
        y = "Nb",
        caption = "Les femmes diplômées célibataires qui sont bas dans la catégorie revenus montrent une bonne portion du churn"
    )


# CONCLUSION

# Retenons pour la réalisation des modèles que le top 5 des variables
# déterminant le départ d'un client :
# Total Transaction Count (Total_Trans_Ct)
# Total Count Change (Total_Ct_Chng_Q4_Q1)
# Total Revolving Balance (Total_Revolving_Bal)
# Total Transaction Amount (Total_Trans_Amt)
# Total Relationship Count (Total_Relationship_Count)

# Nous pouvons tenter 3 modèles sur 
# Gender
# Income_Category
# Total_Trans_Ct (discretisé) ?



### 7 - Régression logistique et AIC ----

# Sélection du meilleur modèle

data_model <- data_reg

# Création des variables explicatives
data_model <- data_model %>%
    mutate(
        ratio_Trans_Amt_Ct = Total_Trans_Amt / Total_Trans_Ct
    ) %>%
    mutate(
        ratio_Age_Month_On_Book = Customer_Age / Months_on_book
    )

# reclassify ?

# Months_on_book(<30, 31<40, +40)
data_model[data_model$Months_on_book <= 30, "Months_on_book"] <- "< 30"
data_model[data_model$Months_on_book > 30 & data_model$Months_on_book <= 40 , "Months_on_book"] <- "31 > 40"
data_model[data_model$Months_on_book > 40, "Months_on_book"] <- "+40"

# Total_Relationship_Count (<3, >=3)
data_model[data_model$Total_Relationship_Count < 3, "Total_Relationship_Count"] <- "< 3"
data_model[data_model$Total_Relationship_Count >= 3,  "Total_Relationship_Count"] <- " >=3"

# Months_Inactive_12_mon(1, 2, >2)
data_model[data_model$Months_Inactive_12_mon == 1, "Months_Inactive_12_mon"] <- "1"
data_model[data_model$Months_Inactive_12_mon == 2,  "Months_Inactive_12_mon"] <- "2"
data_model[data_model$Months_Inactive_12_mon > 2, "Months_Inactive_12_mon"] <- "+2"

# Contacts_Count_12_mon(<3, >3)
data_model[data_model$Contacts_Count_12_mon < 3, "Contacts_Count_12_mon"] <- "< 3"
data_model[data_model$Contacts_Count_12_mon >= 3,  "Contacts_Count_12_mon"] <- " >=3"

# Avg_Utilization_Ratio(0, other)
data_model[data_model$Avg_Utilization_Ratio > 0, "Avg_Utilization_Ratio"] <- 2
data_model[data_model$Avg_Utilization_Ratio == 0, "Avg_Utilization_Ratio"] <- 1
data_model[data_model$Avg_Utilization_Ratio == 1, "Avg_Utilization_Ratio"] <- "zero"
data_model[data_model$Avg_Utilization_Ratio == 2, "Avg_Utilization_Ratio"] <- "over zero"

# Total_Ct_Chng_Q4_Q1(0.6)
data_model[data_model$Total_Ct_Chng_Q4_Q1 > 0.6, "Total_Ct_Chng_Q4_Q1"] <- 2
data_model[data_model$Total_Ct_Chng_Q4_Q1 <= 0.6, "Total_Ct_Chng_Q4_Q1"] <- 1
data_model[data_model$Total_Ct_Chng_Q4_Q1 == 2, "Total_Ct_Chng_Q4_Q1"] <- "over0.6"
data_model[data_model$Total_Ct_Chng_Q4_Q1 == 1, "Total_Ct_Chng_Q4_Q1"] <- "below0.6"

# Total_Trans_Ct(60)
data_model[data_model$Total_Trans_Ct <= 60, "Total_Trans_Ct"] <- 1
data_model[data_model$Total_Trans_Ct > 60, "Total_Trans_Ct"] <- 2
data_model[data_model$Total_Trans_Ct == 2, "Total_Trans_Ct"] <- "over60"
data_model[data_model$Total_Trans_Ct == 1, "Total_Trans_Ct"] <- "below60"

# Total_Trans_Amt(0, 3000, 7000, 11000)
data_model[data_model$Total_Trans_Amt <= 3000, "Total_Trans_Amt"] <- 1
data_model[data_model$Total_Trans_Amt > 3000 & data_model$Total_Trans_Amt <= 7000, "Total_Trans_Amt"] <- 2
data_model[data_model$Total_Trans_Amt > 7000 & data_model$Total_Trans_Amt <= 11000, "Total_Trans_Amt"] <- 3
data_model[data_model$Total_Trans_Amt > 11000, "Total_Trans_Amt"] <- 4
data_model[data_model$Total_Trans_Amt == 1, "Total_Trans_Amt"] <- "very_few"
data_model[data_model$Total_Trans_Amt == 2, "Total_Trans_Amt"] <- "few"
data_model[data_model$Total_Trans_Amt == 3, "Total_Trans_Amt"] <- "medium"
data_model[data_model$Total_Trans_Amt == 4, "Total_Trans_Amt"] <- "lot"

# Total_Amt_Chng_Q4_Q1(<0.5, >=0.5<1, >=1)
data_model[data_model$Total_Amt_Chng_Q4_Q1 < 0.5, "Total_Amt_Chng_Q4_Q1"] <- 0.1
data_model[data_model$Total_Amt_Chng_Q4_Q1 >= 0.5 & data_model$Total_Amt_Chng_Q4_Q1 < 1, "Total_Amt_Chng_Q4_Q1"] <- 0.2
data_model[data_model$Total_Amt_Chng_Q4_Q1 >= 1, "Total_Amt_Chng_Q4_Q1"] <- 0.3
data_model[data_model$Total_Amt_Chng_Q4_Q1 == 0.1, "Total_Amt_Chng_Q4_Q1"] <- "very_few"
data_model[data_model$Total_Amt_Chng_Q4_Q1 == 0.2, "Total_Amt_Chng_Q4_Q1"] <- "few"
data_model[data_model$Total_Amt_Chng_Q4_Q1 == 0.3, "Total_Amt_Chng_Q4_Q1"] <- "lot"

# Avg_Open_To_Buy(<5000, 5000-30000, >30000)
data_model[data_model$Avg_Open_To_Buy < 5000, "Avg_Open_To_Buy"] <- 0.1
data_model[data_model$Avg_Open_To_Buy >= 5000 & data_model$Avg_Open_To_Buy < 30000, "Avg_Open_To_Buy"] <- 0.2
data_model[data_model$Avg_Open_To_Buy >= 30000, "Avg_Open_To_Buy"] <- 0.3
data_model[data_model$Avg_Open_To_Buy == 0.1, "Avg_Open_To_Buy"] <- "very_few"
data_model[data_model$Avg_Open_To_Buy == 0.2, "Avg_Open_To_Buy"] <- "few"
data_model[data_model$Avg_Open_To_Buy == 0.3, "Avg_Open_To_Buy"] <- "lot"

# Total_Revolving_Bal(0-500, 500-2200, >2200)
data_model[data_model$Total_Revolving_Bal < 500, "Total_Revolving_Bal"] <- 0.1
data_model[data_model$Total_Revolving_Bal >= 500 & data_model$Total_Revolving_Bal < 2200, "Total_Revolving_Bal"] <- 0.2
data_model[data_model$Total_Revolving_Bal >= 2200, "Total_Revolving_Bal"] <- 0.3
data_model[data_model$Total_Revolving_Bal == 0.1, "Total_Revolving_Bal"] <- "very_few"
data_model[data_model$Total_Revolving_Bal == 0.2, "Total_Revolving_Bal"] <- "few"
data_model[data_model$Total_Revolving_Bal == 0.3, "Total_Revolving_Bal"] <- "lot"

# Credit_Limit(<5000, 5000-30000, >30000)
data_model[data_model$Credit_Limit < 5000, "Credit_Limit"] <- 0.1
data_model[data_model$Credit_Limit >= 5000 & data_model$Credit_Limit < 30000, "Credit_Limit"] <- 0.2
data_model[data_model$Credit_Limit >= 30000, "Credit_Limit"] <- 0.3
data_model[data_model$Credit_Limit == 0.1, "Credit_Limit"] <- "very_few"
data_model[data_model$Credit_Limit == 0.2, "Credit_Limit"] <- "few"
data_model[data_model$Credit_Limit == 0.3, "Credit_Limit"] <- "lot"

summary(data_model)

# régression linéaire logistique:  - départ de la banque en fonction du profil (Gender, Education_Level, Income_Category) du match en fonction du style


# data_model_quit <- data_model[(data_model$Attrition_Flag) == 1, ]
# skim(data_model_quit)
# data_model_stay <- data_model[(data_model$Attrition_Flag) == 0, ]
# str(data_model_stay)
# 
# desc_stat(data_model, data_model_stay, data_model_quit, "ratio_Age_Month_On_Book", "Attrition_Flag")
# test_stat(data_model_stay, data_model_quit, "ratio_Age_Month_On_Book", c("stay", "quit"))


set.seed(2000)
# partage du dataset en 70/30
intrain <- createDataPartition(data_model$Attrition_Flag, p=0.7, list = F, times = 1)
# creation des datasets: testing (30%) & training (70%) pour minimiser le risque de surentrainement
training <- data_model[intrain,]
testing <- data_model[-intrain,]

# source('script/functions.R')
# best_model <- select_best_model(training)


# Construction du modèle
full.model <- glm(Attrition_Flag~., data=training, family=binomial(logit))
simple.model <- glm(Attrition_Flag~1, data=training, family=binomial(logit))

# backward <- stepAIC(full.model, direction = "backward")
# 
# forward <- stepAIC(simple.model, direction="forward", scope=list(lower=simple.model, upper=full.model))

# challenge itératif avec ajout d'une nouvelle variable
stepwise_aic <- stepAIC(simple.model, direction="both", scope=list(lower=simple.model, upper=full.model))


# AIC: 928.26
# best model <- glm(formula = Attrition_Flag ~ Total_Trans_Ct + ratio_Trans_Amt_Ct +
#         Total_Revolving_Bal + Total_Ct_Chng_Q4_Q1 + Contacts_Count_12_mon +
#         Total_Relationship_Count + Total_Amt_Chng_Q4_Q1 + Gender +
#         Months_Inactive_12_mon, family = binomial(logit), data = training)

# AIC: 727.37
# best.model <- glm(Attrition_Flag ~ Total_Trans_Amt + ratio_Trans_Amt_Ct + Total_Revolving_Bal + 
#                       Total_Relationship_Count + Total_Trans_Ct + Total_Amt_Chng_Q4_Q1 + 
#                       Months_Inactive_12_mon + Gender + Contacts_Count_12_mon + 
#                       Total_Ct_Chng_Q4_Q1 + Avg_Utilization_Ratio + Credit_Limit + 
#                       Avg_Open_To_Buy + Income_Category + Marital_Status + Dependent_count, family = binomial(logit), data = training)

# classification BIS
data_model_bis <- data_model

# AIC 745.79 en le comentant et laissant les 2 autres
# AIC=876.87 en le laissant et en commentant les 2 autres
#data_model_bis[data_model_bis$Total_Trans_Amt %in% c("few", "lot", "medium"), "Total_Trans_Amt"] <- "few_medium_lot"

# a voir bof : Months_Inactive_12_mon0/(Months_Inactive_12_mon1, Months_Inactive_12_mon2)

# a voir bof : (Credit_Limitfew, Credit_Limitlot)/Credit_Limitvery few


# AIC=926.12 en le comentant et en laissant les 2 autres
# AIC=745.1 en le laissant et en commentant les 2 autres
# data_model_bis$Income_Category = as.character(data_model_bis$Income_Category)
# data_model_bis[data_model_bis$Income_Category %in% c("Unknown", "Less than $40K", "$40K - $60K", "$60K - $80K"), "Income_Category"] <- "Less than $80K"
# data_model_bis[data_model_bis$Income_Category %in% c("$80K - $120K", "$120K +"), "Income_Category"] <- "More than $80K"
# data_model_bis$Income_Category = as.factor(data_model_bis$Income_Category)

# AIC=844.31 en le comentant et en laissant les 2 autres
# AIC=743.7 en le laissant et en commentant les 2 autres
# retrait Marital_Status & Dependent_count
data_model_bis <- data_model_bis[,c(-4, -6)]

intrain <- createDataPartition(data_model_bis$Attrition_Flag, p=0.7, list = F, times = 1)
# creation des datasets: testing (30%) & training (70%) pour minimiser le risque de surentrainement
training <- data_model_bis[intrain,]
testing <- data_model_bis[-intrain,]

# Construction du modèle
full.model <- glm(Attrition_Flag~., data=training, family=binomial(logit))
simple.model <- glm(Attrition_Flag~1, data=training, family=binomial(logit))

# challenge itératif avec ajout d'une nouvelle variable
stepwise_aic <- stepAIC(simple.model, direction="both", scope=list(lower=simple.model, upper=full.model))

best.model <- glm(Attrition_Flag ~ Total_Trans_Amt + ratio_Trans_Amt_Ct + Total_Revolving_Bal + 
                      Total_Relationship_Count + Total_Trans_Ct + Total_Amt_Chng_Q4_Q1 + 
                      Months_Inactive_12_mon + Gender + Contacts_Count_12_mon + 
                      Total_Ct_Chng_Q4_Q1 + Avg_Utilization_Ratio + Credit_Limit + 
                      Avg_Open_To_Buy + Income_Category + Marital_Status + Dependent_count, family = binomial(logit), data = training)


# Interprétation
best.model
summary(best.model)
exp(coef(best.model))

# Matrice de confusion
appren.p <- cbind(testing, predict(best.model, newdata = testing, type = "link", se = TRUE))
appren.p <- within(appren.p, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
})
appren.p <- cbind(appren.p, pred.chd = factor(ifelse(appren.p$PredictedProb > 0.5, 1, 0)))

(m.confusion <- as.matrix(table(appren.p$pred.chd, appren.p$Attrition_Flag)))
# 266 vrai négatif, 268 vrai positif, 34 faux négatif, 32 faux positif

# Taux de bien classé
taux_bien_classe <- (m.confusion[1,1]+m.confusion[2,2]) / sum(m.confusion)
taux_bien_classe

# Sensibilité
sensibilite <- (m.confusion[2,2]) / (m.confusion[2,2]+m.confusion[1,2])
sensibilite

# Spécificité
specificite <- (m.confusion[1,1]) / (m.confusion[1,1]+m.confusion[2,1])
specificite

# ODs ratio
exp(cbind(coef(best.model), confint(best.model)))
odds.ratio(best.model)

ggcoef_model(best.model, exponentiate = TRUE)


