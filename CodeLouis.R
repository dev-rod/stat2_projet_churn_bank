# Objectif: Pr?dire la r?siliation d'un client bancaires
# Kaggle: https://www.kaggle.com/sakshigoyal7/credit-card-customers
# Jeux d'apprentissage : 10 K lignes / 23 colonnes
???
# TODO : https://www.kaggle.com/josephchan524/bankchurnersclassifier-recall-97-accuracy-95
???
##################################################################################
# 1 - Chargement des librairies 
##################################################################################
library(data.table)
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
# Regression subset selection, including exhaustive search.
if("leaps" %in% rownames(installed.packages()) == FALSE) {install.packages("leaps")};library(leaps)
???
##################################################################################
# 2 - Import des donn?es 
##################################################################################

# (10127 matchs)
data <- read.csv("data/BankChurners.csv", sep = ",")

# V?rification des types de champs et des valeurs nulles
summary(data)
str(data)
???
# mutates
data_corr <- data %>% mutate_if(is.factor,as.numeric)
# compute correlation 
correlation= cor(data_corr)
# correlation as data.frame
target_corr= as.data.frame(correlation[,1])
# correlation column name
colnames(target_corr) <-'Correlation'
# sort dataframe
target_corr <- target_corr %>% arrange(desc(Correlation))
# exclude target
target_corr <- target_corr %>% filter(Correlation<1) 
# round
target_corr <- round(target_corr,2)
???
rownames(target_corr)
???
# PLOT CORRELATION
target_corr %>% arrange(desc(Correlation)) %>%
  ggplot(aes(x=Correlation,
             y=reorder(rownames(target_corr),Correlation),
             fill=Correlation)) +
  geom_col(color='black') + labs(title='Target Correlation',y='') +
  theme_classic() +
  theme(legend.position = 'none')
#Second modele
data = data[,-c(1,22,23)]
data$Attrition_Flag<-as.character(data$Attrition_Flag)
data$Attrition_Flag[data$Attrition_Flag=="Existing Customer"]<-"Stay"
data$Attrition_Flag[data$Attrition_Flag=="Attrited Customer"]<-"Quit"
data_quit<-data[(data$Attrition_Flag)=="Quit",]

data_quit %>% select(where(is.numeric)) %>% as.matrix() %>%
  cor() %>% corrplot(method = "number", type="lower")

data %>% select(where(is.numeric)) %>% as.matrix() %>%
  cor() %>% corrplot(method = "number", type="lower")

#Echantillonnage ??? 50/50 sur la variable ??? pr???dire
data_quit<-data[(data$Attrition_Flag)=="Quit",]
data_stay<-data[(data$Attrition_Flag)=="Stay",]
sample_quit<-sample(1:dim(data_quit)[1],1000)
sample_Stay<-sample(1:dim(data_stay)[1],1000)
data_reg<-rbind(data_quit[sample_quit,],data_stay[sample_Stay,])
table(data_reg$Attrition_Flag)



# factorisation des varibales
data_select<-na.omit(joueur_unique[(joueur_unique$wins + joueur_unique$losses>3) ,
                                   c(
                                     "Attrition_Flag","Customer_Age","Dependent_count","Months_on_book","Total_Relationship_Count","Months_Inactive_12_mon","Contacts_Count_12_mon","Credit_Limit","Total_Revolving_Bal","Avg_Open_To_Buy","Total_Amt_Chng_Q4","Total_Trans_Amt","Total_Trans_Ct","Total_Ct_Chng_Q4_Q1","Avg_Utilization_Ratio"
                                    )])

???
# supprimer  IGNORER LES 2 DERNI?RES COLONNES (NAIVE BAYES CLAS.)
???
# variable dependante Y = d?part ou non : attrition_flag
# variable qualitative donc on fera une r?gression logistique
???
???
# faire une AFC
???
# stat descriptive avec les variables characters
#Choix des variables
???categ <- data %>% select(Attrition_Flag,Gender,Marital_Status,Card_Category,Income_Category,Education_Level)
#Plot par genre
categ %>%
  select(Attrition_Flag,Gender) %>%
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
#Plot par Statut marital
categ %>%
  select(Attrition_Flag,Marital_Status) %>%
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

#Plot par carte bancaire
categ %>%
  select(Attrition_Flag,Card_Category) %>%
  ggplot(aes(x=Attrition_Flag,fill=Card_Category))+
  geom_bar(position="dodge2") +
  geom_text(aes(y = (..count..)/sum(..count..), 
                label = paste0(round(prop.table(..count..) * 100,2), '%')), 
            stat = 'count', 
            size = 3,
            vjust=-4,
            position = position_dodge(.9))+
  labs(title="Distribution par type de CB",
       
       x="Attrition_Flag",y="NB")
#Plot par rentr?e financiere
categ %>%
  select(Attrition_Flag,Income_Category) %>%
  ggplot(aes(x=Attrition_Flag,fill=Income_Category)) +
  geom_bar(position="dodge2") +
  geom_text(aes(y = (..count..)/sum(..count..), 
                label = paste0(round(prop.table(..count..) * 100,2), '%')), 
            stat = 'count', 
            size = 3,
            vjust=-4,
            position = position_dodge(.9))+
  labs(title="Distribution par rentr?e financi?re",
       
       x="Attrition_Flag",y="NB")

#Plot par niveau d'?tude
  categ %>%
    select(Attrition_Flag,Education_Level) %>%
    ggplot(aes(x=Attrition_Flag,fill=Education_Level)) +
  geom_bar(position="dodge2") +
  geom_text(aes(y = (..count..)/sum(..count..), 
                label = paste0(round(prop.table(..count..) * 100,2), '%')), 
            stat = 'count', 
            size = 3,
            vjust=-4,
            position = position_dodge(.9))+
  labs(title="Distribution par niveau d'?tude",
       
       x="Attrition_Flag",y="NB")
  

???
???
???
???
???
???
???
# variable explicatives
# age, genre, niveau education, statut marital, personnes ? charge, revenu, type de carte, p?riode de relation avec la banque...
???
???
model_quali<-glm(Attrition_Flag~Customer_Age, data=data, family= binomial(logit))
???
# Interpr?tation
model_quali
summary(model_quali)
exp(coef(model_quali))
???
# Matrice de confusion
appren.p <- cbind(data_reg, predict(model_quali, newdata = data_reg, type = "link", 
                                    se = TRUE))
appren.p <- within(appren.p, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
appren.p <- cbind(appren.p, pred.chd = factor(ifelse(appren.p$PredictedProb > 0.5, 1, 0)))
colnames(appren.p)
appren.p<-appren.p[,c("Winner","diff_dist_att","diff_dist_att_class","fit","PredictedProb","pred.chd")]
(m.confusion <- as.matrix(table(appren.p$pred.chd, appren.p$Winner)))
???
# Taux de bien class?
(m.confusion[1,1]+m.confusion[2,2]) / sum(m.confusion)
???
# Sensibilit?
(m.confusion[2,2]) / (m.confusion[2,2]+m.confusion[1,2])
???
# Sensibilit?
(m.confusion[2,2]) / (m.confusion[2,2]+m.confusion[1,2])
???
# Sp?cificit? 
(m.confusion[1,1]) / (m.confusion[1,1]+m.confusion[2,1])
???
???
# ODs ratio
exp(cbind(coef(model_quali), confint(model_quali)))
library(questionr)
odds.ratio(model_quali)
install.packages("GGally")
library(GGally)
library(broom.helpers)
ggcoef_model(model_quali, exponentiate = TRUE)
