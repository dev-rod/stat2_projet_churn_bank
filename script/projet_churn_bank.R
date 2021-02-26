# Objectif: Prédire la résiliation d’un client bancaires
# Kaggle: https://www.kaggle.com/sakshigoyal7/credit-card-customers
# Jeux d’apprentissage : 10 K lignes / 23 colonnes

# TODO : https://www.kaggle.com/josephchan524/bankchurnersclassifier-recall-97-accuracy-95

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

##################################################################################
# 2 - Import des données 
##################################################################################

# (10127 matchs)
data <- read.csv("data/BankChurners.csv", sep = ",")

# Vérification des types de champs et des valeurs nulles
summary(data)
str(data)

# Retrait des colonnes 
# - CLIENTNUM
# - Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1
# - Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2
data <- data[,-c(1,22,23)]

# Séparation des clients qui ont quitté la banque de ceux qui sont resté
data$Attrition_Flag<-as.character(data$Attrition_Flag)
data$Attrition_Flag[data$Attrition_Flag=="Existing Customer"]<-"Stay"
data$Attrition_Flag[data$Attrition_Flag=="Attrited Customer"]<-"Quit"
data_quit <- data[(data$Attrition_Flag)=="Quit",]

# Analyse de chaque variable de manière séparée
categ <- data_quit %>%
    select(Attrition_Flag,Gender, Marital_Status, Card_Category, Income_Category, Education_Level)

categ %>%
    select(Attrition_Flag,Gender) %>%
    mutate(Gender = ifelse(Gender == "F", "Female", "Male")) %>%
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

# Graphique des corrélations entre chacune des variables
data_quit %>%
    select(where(is.numeric)) %>%
    as.matrix() %>%
    cor() %>%
    corrplot(method = "number", type="lower")

# Obtaining the correlation matrix with the Spearman method
cor_spearman <- cor(dados_cor[, sapply(dados_cor, is.numeric)], method = 'spearman')
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
p2 <- data_clean %>% ggplot(aes(x=Total_Trans_Ct, fill=Attrition_Flag)) + 
    geom_density(alpha=0.7) +
    scale_fill_manual(values= c('#3C3838','#338076'),name='') +
    theme_classic() +
    labs(title='Total Transaction Count (Last 12 months) by flag') +
    theme(legend.position='bottom')


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

