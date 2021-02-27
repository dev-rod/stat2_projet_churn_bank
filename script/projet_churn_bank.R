# Objectif: Prédire la résiliation d’un client bancaires
# Kaggle: https://www.kaggle.com/sakshigoyal7/credit-card-customers
# Jeux d’apprentissage : 10 K lignes / 23 colonnes

# TODO : https://www.kaggle.com/josephchan524/bankchurnersclassifier-recall-97-accuracy-95

##################################################################################
# 1 - Chargement des librairies
##################################################################################

# Enhanced data.frame
if("data.table" %in% rownames(installed.packages()) == FALSE) {install.packages("data.table")};library(data.table)
# gestion des dates
if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")};library(lubridate)
# Functions to Accompany J. Fox and S. Weisberg, An R Companion to Applied Regression, Third Edition, Sage, 2019.
if("car" %in% rownames(installed.packages()) == FALSE) {install.packages("car")};library(car)
# Several functions are available for calculating the most widely used effect sizes (ES),

# along with their variances, confidence intervals and p-values
if("compute.es" %in% rownames(installed.packages()) == FALSE) {install.packages("compute.es")};library(compute.es)
# Graphical and tabular effect displays, e.g., of interactions, for various statistical models with linear predictors.
if("effects" %in% rownames(installed.packages()) == FALSE) {install.packages("effects")};library(effects)
# Create Elegant Data Visualisations Using the Grammar of Graphics
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")};library(ggplot2)
# Simultaneous Inference in General Parametric Models
if("multcomp" %in% rownames(installed.packages()) == FALSE) {install.packages("multcomp")};library(multcomp)
# Package for Analysis of Space-Time Ecological Series
if("pastecs" %in% rownames(installed.packages()) == FALSE) {install.packages("pastecs")};library(pastecs)
# A Collection of Robust Statistical Methods
if("WRS2" %in% rownames(installed.packages()) == FALSE) {install.packages("WRS2")};library(WRS2)
# set of packages that work in harmony because they share common data representations and 'API' design.
if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")};library(tidyverse)
#if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")};library(dplyr)
# Regression subset selection, including exhaustive search.
if("leaps" %in% rownames(installed.packages()) == FALSE) {install.packages("leaps")};library(leaps)
# Graphes de corrélation
if("corrplot" %in% rownames(installed.packages()) == FALSE) {install.packages("corrplot")};library(corrplot)
# lib de graphes avancés
if("highcharter" %in% rownames(installed.packages()) == FALSE) {install.packages("highcharter")};library(highcharter)
# reshape2
# if("reshape2" %in% rownames(installed.packages()) == FALSE) {install.packages("reshape2")};library(reshape2)


# pour ajouter une nouvelle librairie
# if("" %in% rownames(installed.packages()) == FALSE) {install.packages("")};library()

# les librairies installées
# library()

##################################################################################
# 2 - Import des données 
##################################################################################

data <- read.csv("data/BankChurners.csv", sep = ",")
# Vérification des types de champs et des valeurs nulles
# pas de NA
summary(data)
# pas de doublons
length(unique(data$CLIENTNUM)) # 10127 lignes pour 10127 numeros de compte 
# les types 
#str(data)
# Retrait des colonnes inutiles 
#   - CLIENTNUM
#   - Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1
#   - Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2
data <- data[,-c(1,22,23)]

# --- Attrition_Flag encoding: 0 Existing Customer, 1 Attrited Customer ---
data<- data %>% mutate(Attrition_Flag =recode(Attrition_Flag, "Attrited Customer" = "1", "Existing Customer" = "0"))
str(data)

# les  variables qualitatives
# 1 Card_Category           : Factor  
# 2 Attrition_Flag          : Factor 
# 3 Gender                  : Factor 
# 4 Education_Level         : Factor 
# 5 Marital_Status          : Factor 
# 6 Income_Category         : Factor 

# les 14 variables quantitatives
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

summary(data)

# 20 colonnes restantes
length (colnames(data))

##################################################################################
# 2 - Analyses visuelles
##################################################################################

# Séparation des clients :  ceux qui ont quitté la banque de ceux qui sont restés

# --- Attrition_Flag encoding: 0 Existing Customer, 1 Attrited Customer ---
data<- data %>% mutate(Attrition_Flag = recode(Attrition_Flag, "Attrited Customer" = "1", "Existing Customer" = "0"))

data_quit <- data[(data$Attrition_Flag)==1,] # 1627
data_stay <- data[(data$Attrition_Flag)==0,] # 8500 

    # ???? TEST DE STUDENT ????
    # pour comparer des groupes dont les effectifs sont différentS : comparaison de moyenne
    # => test de student : http://www.sthda.com/french/wiki/test-de-student-est-il-toujours-correct-de-comparer-des-moyennes
    # source('http://www.sthda.com/upload/rquery_t_test.r')
    # rquery.t.test(x, y = NULL, paired = FALSE, graph = TRUE, ...)
    # exemple : rquery.t.test(data_quit$Avg_Open_To_Buy, data_stay$Avg_Open_To_Buy)
    # Error in shapiro.test(x) : sample size must be between 3 and 5000 
    # ??????????????????????????????????????????????????????????????????????????????????

# 2.1 - Analyses visuelles du dataset complet (data)
##################################################################################

# Analyse Attrition_Flag en fonction de chaque variable dependant du client
categ <- data %>%
    select(Attrition_Flag,Gender, Marital_Status, Card_Category, Income_Category, Education_Level)

# tableau de frequences en fonction de Attrition_Flag
# Gender
frequence_Gender <- data.frame(table(Gender,Attrition_Flag))
# frequence des femmes qui partent parmi les clients qui partent 
ratio_FO <- (frequence_Gender$Freq[frequence_Gender$Gender== "F"& frequence_Gender$Attrition_Flag=="0" ])/(sum(frequence_Gender$Freq[frequence_Gender$Attrition_Flag=="0"]))
ratio_F1  <- (frequence_Gender$Freq[frequence_Gender$Gender== "F"& frequence_Gender$Attrition_Flag=="1" ])/(sum(frequence_Gender$Freq[frequence_Gender$Attrition_Flag=="1"]))
# calcul des frequence relative femme /femme+homme 

# => il semble y avoir legerement plus de femme que d'homes parmi les personnes qui résilient'
# Marital_Status
    # unique (Marital_Status)
    # Married  Single   Unknown  Divorced
frequence_Marital_Status <- data.frame(table(Attrition_Flag,Marital_Status))
frequence_Marital_Status
ratio_FO <- (frequence_Marital_Status$Freq[frequence_Marital_Status$Gender== "F"& frequence_Gender$Attrition_Flag=="0" ])/(sum(frequence_Gender$Freq[frequence_Gender$Attrition_Flag=="0"]))

# Card_Category
frequence_Card_Category <- data.frame(table(Attrition_Flag,Card_Category))
frequence_Card_Category
# Income_Category
frequence_Income_Category <- data.frame(table(Attrition_Flag,Income_Category))
frequence_Income_Category
# Education_Level
frequence_Education_Level <- data.frame(table(Attrition_Flag,Education_Level))
frequence_Education_Level
# Months_on_book
frequence_Months_on_book <- data.frame(table(Attrition_Flag,Months_on_book))
frequence_Months_on_book
# Total_Relationship_Count
frequence_Total_Relationship_Count <- data.frame(table(Attrition_Flag,Total_Relationship_Count))
frequence_Total_Relationship_Count
# Contacts_Count_12_mon
frequence_Total_Relationship_Count <- data.frame(table(Attrition_Flag,Total_Relationship_Count))
frequence_Total_Relationship_Count
# Credit_Limit
frequence_Credit_Limit <- data.frame(table(Attrition_Flag,Credit_Limit))
frequence_Credit_Limit
# Total_Revolving_Bal
frequence_Total_Revolving_Bal <- data.frame(table(Attrition_Flag,Total_Revolving_Bal))
frequence_Total_Revolving_Bal
# Avg_Open_To_Buy
frequence_Avg_Open_To_Buy <- data.frame(table(Attrition_Flag,Avg_Open_To_Buy))
frequence_Avg_Open_To_Buy
# Total_Amt_Chng_Q4_Q1
frequence_Total_Amt_Chng_Q4_Q1 <- data.frame(table(Attrition_Flag,Total_Amt_Chng_Q4_Q1))
frequence_Total_Amt_Chng_Q4_Q1
# Avg_Open_To_Buy
frequence_Avg_Open_To_Buy <- data.frame(table(Attrition_Flag,Avg_Open_To_Buy))
frequence_Avg_Open_To_Buy
# Total_Amt_Chng_Q4_Q1
frequence_Avg_Open_To_Buy <- data.frame(table(Attrition_Flag,Avg_Open_To_Buy))
frequence_Avg_Open_To_Buy
# Total_Trans_Amt
frequence_Total_Trans_Amt <- data.frame(table(Attrition_Flag,Total_Trans_Amt))
frequence_Total_Trans_Amt
# Dependent_count
frequence_Dependent_count <- data.frame(table(Attrition_Flag,Dependent_count))
frequence_Dependent_count
# Customer_Age
# frequence_Customer_Age <- data.frame(table(Attrition_Flag,Customer_Age))
# frequence_Customer_Age
# # Avg_Utilization_Ratio
# frequence_Avg_Utilization_Ratio <- data.frame(table(Attrition_Flag,Avg_Utilization_Ratio))
# frequence_Avg_Utilization_Ratio
# # Total_Ct_Chng_Q4_Q1 
# frequence_Total_Ct_Chng_Q4_Q1 <- data.frame(table(Attrition_Flag,Total_Ct_Chng_Q4_Q1))
# frequence_Total_Ct_Chng_Q4_Q1
# # Total_Trans_Ct
# frequence_Total_Trans_Ct <- data.frame(table(Attrition_Flag,Total_Trans_Ct))
# frequence_Total_Trans_Ct



    

# Plot attrition par genre
categ %>%
    select(Attrition_Flag, Gender) %>%
    mutate(Gender = ifelse(Gender == "F","Female","Male"))
    ggplot(aes(x=Attrition_Flag,fill=Gender)) +
    geom_bar(position="dodge2") +
    geom_text(aes(y = (..count..)/sum(..count..),
                  label = paste0(round(prop.table(..count..) * 100), '%')),
              stat = 'count',
              size = 3,
              vjust=-4,
              position = position_dodge(.9))+
    labs(title="Distribution par genre",
         x="Attrition_Flag",y="NB")

# Plot par Statut marital
categ %>%
    select(Attrition_Flag, Marital_Status) %>%
    ggplot(aes(x=Attrition_Flag,fill=Marital_Status)) +
    geom_bar(position="dodge2") +
    geom_text(aes(y = (..count..)/sum(..count..), 
                  label = paste0(round(prop.table(..count..) * 100), '%')), 
              stat = 'count', 
              size = 3,
              vjust=-4,
              position = position_dodge(.9))+
    labs(title="Distribution par statut marital",
         x="Attrition_Flag",y="NB")

# Plot par carte bancaire
categ %>%
    select(Attrition_Flag, Card_Category) %>%
    ggplot(aes(x=Attrition_Flag,fill=Card_Category))+
    geom_bar(position="dodge2") +
    geom_text(aes(y = (..count..)/sum(..count..), 
                  label = paste0(round(prop.table(..count..) * 100,2), '%')), 
              stat = 'count', 
              size = 3,
              vjust=-4,
              position = position_dodge(.9))+
    labs(title="Distribution par type de CB",
         x="Attrition_Flag", y="NB")

# Plot par rentrée financiere
categ %>%
    select(Attrition_Flag, Income_Category) %>%
    ggplot(aes(x=Attrition_Flag,fill=Income_Category)) +
    geom_bar(position="dodge2") +
    geom_text(aes(y = (..count..)/sum(..count..), 
                  label = paste0(round(prop.table(..count..) * 100,2), '%')), 
              stat = 'count', 
              size = 3,
              vjust=-4,
              position = position_dodge(.9))+
    labs(title="Distribution par rentrée financière",
         x="Attrition_Flag",y="NB")

# Plot par niveau d'étude
categ %>%
    select(Attrition_Flag, Education_Level) %>%
    ggplot(aes(x=Attrition_Flag,fill=Education_Level)) +
    geom_bar(position="dodge2") +
    geom_text(aes(y = (..count..)/sum(..count..), 
                  label = paste0(round(prop.table(..count..) * 100,1), '%')), 
              stat = 'count', 
              size = 3,
              vjust=-4,
              position = position_dodge(.9))+
    labs(title="Distribution par niveau d'étude",
         x="Attrition_Flag",y="NB")




# 2.2 - Analyses visuelles du dataset des personnes qui ont résiliées  (data_quit)
##################################################################################

# Analyse de chaque variable de manière séparée
categ <- data_quit %>%
    select(Attrition_Flag,Gender, Marital_Status, Card_Category, Income_Category, Education_Level)

# Plot attrition par genre
categ %>%
    select(Attrition_Flag, Gender) %>%
    mutate(Gender = ifelse(Gender == "F","Female","Male")) %>%
    ggplot(aes(x=Attrition_Flag,fill=Gender)) +
    geom_bar(position="dodge2") +
    geom_text(aes(y = (..count..)/sum(..count..), 
                  label = paste0(round(prop.table(..count..) * 100), '%')), 
              stat = 'count', 
              size = 3,
              vjust=-4,
              position = position_dodge(.9))+
    labs(title="Distribution par genre",
         x="Attrition_Flag",y="NB")

# Plot par Statut marital
categ %>%
    select(Attrition_Flag, Marital_Status) %>%
    ggplot(aes(x=Attrition_Flag,fill=Marital_Status)) +
    geom_bar(position="dodge2") +
    geom_text(aes(y = (..count..)/sum(..count..), 
                  label = paste0(round(prop.table(..count..) * 100), '%')), 
              stat = 'count', 
              size = 3,
              vjust=-4,
              position = position_dodge(.9))+
    labs(title="Distribution par statut marital",
         x="Attrition_Flag",y="NB")

# Plot par carte bancaire
categ %>%
    select(Attrition_Flag, Card_Category) %>%
    ggplot(aes(x=Attrition_Flag,fill=Card_Category))+
    geom_bar(position="dodge2") +
    geom_text(aes(y = (..count..)/sum(..count..), 
                  label = paste0(round(prop.table(..count..) * 100,2), '%')), 
              stat = 'count', 
              size = 3,
              vjust=-4,
              position = position_dodge(.9))+
    labs(title="Distribution par type de CB",
         x="Attrition_Flag", y="NB")

# Plot par rentrée financiere
categ %>%
    select(Attrition_Flag, Income_Category) %>%
    ggplot(aes(x=Attrition_Flag,fill=Income_Category)) +
    geom_bar(position="dodge2") +
    geom_text(aes(y = (..count..)/sum(..count..), 
                  label = paste0(round(prop.table(..count..) * 100,2), '%')), 
              stat = 'count', 
              size = 3,
              vjust=-4,
              position = position_dodge(.9))+
    labs(title="Distribution par rentrée financière",
         x="Attrition_Flag",y="NB")

# Plot par niveau d'étude
categ %>%
    select(Attrition_Flag, Education_Level) %>%
    ggplot(aes(x=Attrition_Flag,fill=Education_Level)) +
    geom_bar(position="dodge2") +
    geom_text(aes(y = (..count..)/sum(..count..), 
                  label = paste0(round(prop.table(..count..) * 100,2), '%')), 
              stat = 'count', 
              size = 3,
              vjust=-4,
              position = position_dodge(.9))+
    labs(title="Distribution par niveau d'étude",
         x="Attrition_Flag",y="NB")

# 2.2 - Analyses visuelles du dataset des personnes qui ont résiliées  (data_quit)
##################################################################################

# Analyse de chaque variable de manière séparée
categ <- data_quit %>%
    select(Attrition_Flag,Gender, Marital_Status, Card_Category, Income_Category, Education_Level)

# Plot attrition par genre
categ %>%
    select(Attrition_Flag, Gender) %>%
    mutate(Gender = ifelse(Gender == "F","Female","Male")) %>%
    ggplot(aes(x=Attrition_Flag,fill=Gender)) +
    geom_bar(position="dodge2") +
    geom_text(aes(y = (..count..)/sum(..count..), 
                  label = paste0(round(prop.table(..count..) * 100), '%')), 
              stat = 'count', 
              size = 3,
              vjust=-4,
              position = position_dodge(.9))+
    labs(title="Distribution par genre",
         x="Attrition_Flag",y="NB")

# Plot par Statut marital
categ %>%
    select(Attrition_Flag, Marital_Status) %>%
    ggplot(aes(x=Attrition_Flag,fill=Marital_Status)) +
    geom_bar(position="dodge2") +
    geom_text(aes(y = (..count..)/sum(..count..), 
                  label = paste0(round(prop.table(..count..) * 100), '%')), 
              stat = 'count', 
              size = 3,
              vjust=-4,
              position = position_dodge(.9))+
    labs(title="Distribution par statut marital",
         x="Attrition_Flag",y="NB")

# Plot par carte bancaire
categ %>%
    select(Attrition_Flag, Card_Category) %>%
    ggplot(aes(x=Attrition_Flag,fill=Card_Category))+
    geom_bar(position="dodge2") +
    geom_text(aes(y = (..count..)/sum(..count..), 
                  label = paste0(round(prop.table(..count..) * 100,2), '%')), 
              stat = 'count', 
              size = 3,
              vjust=-4,
              position = position_dodge(.9))+
    labs(title="Distribution par type de CB",
         x="Attrition_Flag", y="NB")

# Plot par rentrée financiere
categ %>%
    select(Attrition_Flag, Income_Category) %>%
    ggplot(aes(x=Attrition_Flag,fill=Income_Category)) +
    geom_bar(position="dodge2") +
    geom_text(aes(y = (..count..)/sum(..count..), 
                  label = paste0(round(prop.table(..count..) * 100,2), '%')), 
              stat = 'count', 
              size = 3,
              vjust=-4,
              position = position_dodge(.9))+
    labs(title="Distribution par rentrée financière",
         x="Attrition_Flag",y="NB")

# Plot par niveau d'étude
categ %>%
    select(Attrition_Flag, Education_Level) %>%
    ggplot(aes(x=Attrition_Flag,fill=Education_Level)) +
    geom_bar(position="dodge2") +
    geom_text(aes(y = (..count..)/sum(..count..), 
                  label = paste0(round(prop.table(..count..) * 100,2), '%')), 
              stat = 'count', 
              size = 3,
              vjust=-4,
              position = position_dodge(.9))+
    labs(title="Distribution par niveau d'étude",
         x="Attrition_Flag",y="NB")












#CLASSE
data_select<-data
#classe_marital_class
data_select[which(data_select$Marital_Status %in% c("Divorced","Single")),"Marital_Status"]<-"Single"
#Card_category_class
data_select[which(data_select$Card_Category %in% c("Gold","platinum","Silver")),"Card_Category"]<-"Others"
#Income_category_class
data_select[which(data_select$Income_Category %in% c("Less than $40K","$40K - $60K")),"Income_Category"]<-"Less than $60K"
data_select[which(data_select$Income_Category %in% c("$60K - $80K","$80K - $120K","$120K +")),"Income_Category"]<-"More than $60K"


# Graphique I des corrélations entre chacune des variables
# data %>% select(where(is.numeric)) %>%
#     as.matrix() %>%
#     cor() %>%
#     corrplot(method = "number", type="lower")

# Graphique II des corrélations entre chacune des variables avec la méthode de spearman
cor_spearman <- cor(data[, sapply(data, is.numeric)], method = 'spearman')

# Visualizing with a heatmap the correlation matrix with the pearson method
as.matrix(data.frame(cor_spearman)) %>% 
    round(3) %>% #round
    hchart() %>% 
    hc_add_theme(hc_theme_smpl()) %>%
    hc_title(text = "Spearman's correlation coefficients", align = "center") %>% 
    hc_legend(align = "center") %>% 
    hc_colorAxis(stops = color_stops(colors = viridis::inferno(10))) %>%
    hc_plotOptions(
        series = list(
            boderWidth = 0,
            dataLabels = list(enabled = TRUE)))


# Transaction Count
p2 <- data %>% ggplot(aes(x=Total_Trans_Ct, fill=Attrition_Flag)) + 
    geom_density(alpha=0.7) +
    scale_fill_manual(values= c('#3C3838','#338076'),name='') +
    theme_classic() +
    labs(title='Total Transaction Count (Last 12 months) by flag') +
    theme(legend.position='bottom')
p2

# variable dependante Y = départ ou non : attrition_flag
# variable qualitative donc on fera une régression logistique


# faire une AFC

# stat descriptive


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

