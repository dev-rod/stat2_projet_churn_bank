

# utf8::
# deeplayer?
# tidyverse?


##################################################################################
# 1 - Import des donn�es
##################################################################################
setwd("C:/Users/CFOUQUE/!!perso/!!MEDAS/STAT2")

# les librairies
library(data.table)
library(lubridate)
library(car)
library(compute.es)
library(effects)
library(ggplot2)
library(multcomp)
library(pastecs)
library(WRS)
library(tidyverse)
library(leaps)
library(prettyR)

# Importer la base de donn�es
data <-
  read.csv("C:/Users/CFOUQUE/!!perso/!!MEDAS/STAT2/Data/data.csv",
           sep = ",")

#type
class(data)
# observer la base de données :
# => faire des graphique (plot,... )
## resumé
summary(data)

## structure : v�rifier le format de vos champs !!! 
str(data)

# pensez aux dates! souvent des probleme de formats => Lubridate
data$date <- as.Date(data$date)

# librairie lubridate On ne conserve que les valeurs où year > 2000
data <- data[year(data$date) > 2000,]

##################################################################################
# 2 - Cr�ation d'une table de combattant unique
##################################################################################

# Etape 1 et 2: faire un dataframe avec toutes les variables concernant le joueur bleu  (et rouge)
# ------------- # + Date + Gagnant + Catéorie de poids + Nombre de round

# le nom des colonnes du df
colnames(data)

# creation d'un vecteur qui contien le nom des colonnes => futur entete de la table combattant unique
nom_colonne <- colnames(data)
nom_colonne

# joueur rouge : on filtre les nom de colonne contenu dans nom_colonne qui commence par R_
# REGEX %like%"^R_"
col_rouge <- nom_colonne[nom_colonne %like% "^R_"]
col_rouge 

# joueur bleu : on ajoute à nom_colonne les variables dons le nom commence par par B_
# REGEX %like%"^B_"
col_bleu <- nom_colonne[nom_colonne %like% "^B_"]
col_bleu

# on ajoute le contenu des colonnes dont les champs sont demandés dansl'énoncé 
#"date","Winner","title_bout","weight_class","no_of_rounds" et le contenu des chmps de col_rouge
 # df2=df1[,c("coln")] # creation de df2 avec le contenu de la coln de df1

#creation d'un dataframe avec les colonne pour les combattant rouge
data_rouge <- data[, c("date","Winner","title_bout","weight_class","no_of_rounds",
                col_rouge)]
colnames(data_rouge)
# 74 colonnes

# Creation d'un dataframe avec les colonne pour les combattant Bleu
data_bleu <- data[, c("date","Winner","title_bout","weight_class","no_of_rounds",
            col_bleu)]
colnames(data_bleu)
# 74 colonnes

# Etape 2  : Renommer les variables pour que les noms de colonnes soit identiques entre les deux dataframe cr��s ci-dessus
# ---------
colnames(data_rouge) <-
  c(
    "date",
    "Winner",
    "title_bout",
    "weight_class",
    "no_of_rounds",
    substr(col_rouge, 3, 1000)) # on extrait la chaine de caracteres de 3 à 1000 
                                # = on supprime les 2 premiers caractères de chaque colonne de col_rouge
 
colnames(data_bleu) <-
  c(
    "date",
    "Winner",
    "title_bout",
    "weight_class",
    "no_of_rounds",
    substr(col_bleu, 3, 1000)) # on extrait la chaine de caracteres de 3 à 1000 
    # = on supprime les 2 premiers caractères de chaque colonne de col_rouge
 
# explication de substr : Extract or replace substrings in a character vector
# help(substr)
# substr(x, start, stop) (remplace ou extrait la chaine de caractere x de debut à stop)

# Etape 4 : Concatener les deux dataframes en un seul
# ---------

# bind : combine des objets par lignes (rbind) et par colonnes (cbind)
base_joueur_match <- rbind(data_rouge, data_bleu)
View(base_joueur_match)

# Etape 5 : S�lectionner seulement la ligne correspondant au dernier combat par combattant
# ---------
# sorte de group by des colonnes date et fighter du dataset data , max
# ici cela correspond a la plus grande date soit la plus recente

# creation d'un df qui contient la date du dernier combat de chaque joueur
date_max <-
  aggregate(date ~ fighter , data = base_joueur_match , max)
help(aggregate)
#aggregate(vecteur1~vecteur2, data=df, agregateur (min, max, count, mean...))

# jointure des deux datasets date et base_joueur_match
joueur_unique <- merge(
  base_joueur_match,
  date_max,
  by.x = c("date", "fighter") ,
  by.y = c("date", "fighter"),
  all.x = F ,
  all.y = F
) # les deux a false pour jointure interne?

# help(merge) => data.table
# merge(x, y, by = NULL, by.x = NULL, by.y = NULL, all = FALSE,
#       all.x = all, all.y = all, sort = TRUE, suffixes = c(".x", ".y"), no.dups = TRUE,
#       allow.cartesian=getOption("datatable.allow.cartesian"),  # default FALSE
#       ...)


# Arguments
#
# x, y	: data tables. y is coerced to a data.table if it isn't one already.
# by  :# A vector of shared column names in x and y to merge on. This defaults to the shared key columns between the two tables. If y has no key columns, this defaults to the key of x.
# by.x, by.y	:# Vectors of column names in x and y to merge on.
# all
# logical; all = TRUE is shorthand to save setting both all.x = TRUE and all.y = TRUE.
# all.x
# logical; if TRUE, then extra rows will be added to the output, one for each row in x that has no matching row in y. These rows will have 'NA's in those columns that are usually filled with values from y. The default is FALSE, so that only rows with data from both x and y are included in the output.
# all.y
# logical; analogous to all.x above.

# on enleve les doublons, on recupere les nom des combattants de joueur unique :
# la presence des doublons est du a des joueurs qui combattaient pls fois par jours (avant 2000)

#tableau de freqence des combattant
table(joueur_unique$fighter)
# transformer en df pour etre plus lisible (2 colonnes : Var1 et freq)
doublon <- data.frame(table(joueur_unique$fighter))
doublon

# on filtre doublon tel que la colonne doublon$Freq soit >1 c'est a dire present au moins 2 fois
doublon[doublon$Freq > 1, ]
doublon
# pas de doublon si on filtre au depart year(date)>2000


##################################################################################
# 3 - Calculer la r�gression lin�aire simple entre le poids  (variable quanti)t tla taille
##################################################################################

#RQ : attach() permet d'éviter de mettre "nomdu dataframe$ " devant les variables
attach(joueur_unique)

# selection des colonne d'interet et suppr des NA avec na.omit() 
data_taille_poids <-
  na.omit(joueur_unique[, c("Weight_lbs", "Height_cms")])

# observation de ce nouveau dataframe
summary(data_taille_poids)

# A - Analyse graphique :
#------------------------

# pour voir les valeur abbérentes et si il semble y avoir un lien entre poids et tailles

#plot de la taille en fonction du poids(variable quanti- quanti) pour voir si on dictingue une tendance => oui
plot(data_taille_poids$Weight_lbs, data_taille_poids$Height_cms)

# observation de la taille
  # tableau de frequence 
table(data_taille_poids$Height_cms)
  # histogramme
hist(data_taille_poids$Height_cms) # loi normale 

# observation du poids
  # histogramme
hist(data_taille_poids$Weight_lbs) # pas tout a fait loi normale, grande traine 
  #table de frequence du poids
table(data_taille_poids$Weight_lbs)

# B - Construction du mod�le
#--------------------------

# type des variables
str(data_taille_poids)
# 'data.frame':	1739 obs. of  2 variables:
#   $ Weight_lbs: num  265 170 155 155 265 183 155 245 205 227 ...
# $ Height_cms: num  183 157 180 170 208 ...

#la regression lineaire
res.lm<-lm( Weight_lbs~Height_cms  , data = data_taille_poids)
?lm
summary(res.lm)

# ****** Hypothèse testées: H0 Y mal expliquée par X
#                           H1 Y bien expliqué par Y
# Coefficients: y = ax+b a et b
#               Estimate    std. Error  t value    Pr(>|t|)
# (Intercept) -386.01655(b)  10.81181     -35.70     <2e-16 ***
#   Height_cms  3.10431 (a)   0.06046       51.35     <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 21.8 on 1737 degrees of freedom
# Multiple R-squared:  0.6028,	Adjusted R-squared:  0.6026 => 60 % 
# ici 60 % des resultats sont expliqué par notre modele
# F-statistic:  2637 on 1 and 1737 DF,  p-value: < 2.2e-16 => 
# <0.05 donc on est dans l'interfalle de confiance de 95 % (erreur de mùoins de 5 %)
# < 0.05 => H0 est rejetée , taille expliquée par poids a 60%


# B1 - Estimation des param�tres (m�thode des moindres carr�s)
# B2 - Test global du mod�le (test F) / tests de nullit� descoefficients
# B3 - Qualit� du mod�le (coefficient R�)
rstud = rstudent(res.lm)
ecart<-cbind(data_taille_poids,res.lm$fitted.values,res.lm$residuals,rstud)


# plot de la taille en fonction de la hauteur
plot(Weight_lbs ~ Height_cms , data = data_taille_poids , pch = 16)
abline(res.lm, col = "red", lwd = 2)

# C - V�rification des hypoth�ses
#--------------------------
# C1 - Valeurs ajust�es / r�sidus studentis�s (ind�pendance, structure de variance, points aberrants)
rstud = rstudent(res.lm)
plot(rstud,
     pch = 20,
     ylab = "R�sidus studentis�s",
     ylim = c(-3, 3))
abline(
  h = c(0),
  col = "grey",
  lty = 1,
  lwd = 2
)
abline(
  h = c(-2, 2),
  col = "grey",
  lty = 2,
  lwd = 2
)
length(rstud[rstud > 1.95 | rstud < -1.95]) / length(rstud)
# [1] 0.06843013 => 6.8% un peu au dessus des 5 %

# C2 - Distance de Cook (points influents)
res.cook = cooks.distance(model = res.lm)
plot(res.cook,
     type = "h",
     ylab = "Distances de Cook",
     ylim = c(0, 0.6))
abline(h = 0.5, col = "gray", lty = 2)
# pas de points abbérants

# C3 - Droite de Henry (normalit�des residus)

res.qq = qqnorm(rstud,
                pch = 20,
                ylim = c(-3, 7),
                xlim = c(-3, 3))
qqline(rstud,
       lty = 2,
       lwd = 2,
       col = 2)

ecart <-
  cbind(data_taille_poids,
        res.lm$fitted.values,
        res.lm$residuals,
        rstud)
ecart

# D - Pr�diction
# Quel serait la taille d'une personne pesant 135 lbs
hist(data_taille_poids$Height_cms)
new <- data.frame(Height_cms = seq(160, 200, 5))

yy <- cbind(new, predict(res.lm, new, interval = "prediction"))
yy
#   Height_cms fit       lwr      upr
# 1        160 110.6731  67.84057 153.5056
# 2        165 126.1946  83.38860 169.0006
# 3        170 141.7162  98.92842 184.5039
# 4        175 157.2377 114.46003 200.0154
# 5        180 172.7593 129.98343 215.5351
# 6        185 188.2808 145.49861 231.0630
# 7        190 203.8024 161.00558 246.5992
# 8        195 219.3239 176.50434 262.1435
# 9        200 234.8455 191.99491 277.6960


##################################################################################
# 4- Calculer la r�gression lin�aire multiple entre le ratio de victoire et
# Le nombre de coup � la t�te /  "avg_HEAD_att
# le nombre de coup au corp / avg_BODY_att
# le nombre de coup au sol / avg_GROUND_att

##################################################################################

# A - Analyse graphique
#--------------------
str(joueur_unique)
joueur_unique$ratio_victoire<-joueur_unique$wins/(joueur_unique$wins + joueur_unique$losses)
hist(joueur_unique$ratio_victoire)

# victoire/matchs totaux
joueur_unique$ratio_victoire <-
  joueur_unique$wins / (joueur_unique$wins + joueur_unique$losses)

# unique weight_class :modularité de la classe qualitative explicative
unique(joueur_unique$weight_class)

# on ne garde que les joureurs ayant fait au moins 3 matchs, et 
base_reg <-
  joueur_unique[(joueur_unique$wins + joueur_unique$losses)
                > 3, c("avg_HEAD_att",
                       "avg_BODY_att",
                       "avg_GROUND_att",
                       "ratio_victoire")]

hist(base_reg$ratio_victoire)
table(joueur_unique$wins + joueur_unique$losses)
summary(base_reg)
str(base_reg)


hist(base_reg$ratio_victoire) # verfi la normalite"
summary(base_reg) # observe les coups a la tete, au body, au sol.
str(base_reg) 

# pour la classe Welterweight
base_reg_Welterweight <-
  joueur_unique[(joueur_unique$wins + joueur_unique$losses) > 3 &
                  joueur_unique$weight_class == 'Welterweight' , c("avg_HEAD_att",
                                                                   "avg_BODY_att",
                                                                   "avg_GROUND_att",
                                                                   "ratio_victoire")]
hist(base_reg_Welterweight$ratio_victoire)
summary(base_reg_Welterweight)
str(base_reg_Welterweight)


# voir NAs, datatype, etc.

# analyse graphique
plot(base_reg)
plot(base_reg_Welterweight)
# les correlations
cor(base_reg$avg_HEAD_att, base_reg$avg_BODY_att)
#[1] 0.6061485 => bcp de corelation
cor(base_reg$avg_HEAD_att, base_reg$avg_GROUND_att)
# 0.06576005=>pas trop de correlation
cor(base_reg)
# avg_HEAD_att avg_BODY_att avg_GROUND_att ratio_victoire
# avg_HEAD_att     1.00000000  0.606148500    0.065760048      0.1233457
# avg_BODY_att     0.60614850  1.000000000   -0.002469216      0.1145523
# avg_GROUND_att   0.06576005 -0.002469216    1.000000000      0.2685333
# ratio_victoire   0.12334572  0.114552324    0.268533261      1.0000000



# B - Construction du mod�le
#---------------------------
# Voir exemple :
# http://www.sthda.com/english/articles/40-regression-analysis/168-multiple-linear-regression-in-r/

res.lm1 <-
  lm(ratio_victoire ~ avg_BODY_att + avg_GROUND_att + base_reg$avg_HEAD_att, data = base_reg)
summary(res.lm1)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           0.4703129  0.0157990  29.769  < 2e-16 ***
#   avg_BODY_att          0.0022627  0.0012451   1.817   0.0696 .  => peu important, a supprimer?
# avg_GROUND_att        0.0070525  0.0009412   7.493 1.94e-13 *** => important
#   base_reg$avg_HEAD_att 0.0003201  0.0002495   1.283   0.1999    => eu important
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1662 on 735 degrees of freedom
# Multiple R-squared:  0.08743,	Adjusted R-squared:  0.0837 => 8.37 % d'expliqué par le modele
# F-statistic: 23.47 on 3 and 735 DF,  p-value: 1.623e-14 => ok

#on eneleve un variable qui semble peu explicative 
res.lm <-
  lm(ratio_victoire ~ avg_BODY_att + avg_GROUND_att, data = base_reg)
summary(res.lm)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    0.4799246  0.0139163  34.487  < 2e-16 ***
#   avg_BODY_att   0.0032334  0.0009893   3.268  0.00113 **  => mieux qu'avac l'autre variables
#   avg_GROUND_att 0.0071547  0.0009382   7.626 7.51e-14 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1663 on 736 degrees of freedom
# Multiple R-squared:  0.08538,	Adjusted R-squared:  0.0829  => 8.29 % d'explicatibilité le meme qu 'avec 2 variables
# F-statistic: 34.35 on 2 and 736 DF,  p-value: 5.442e-15 => ok 

#=> ce modele est mieux, cor(base_reg$avg_HEAD_att, base_reg$avg_BODY_att)
#[1] 0.6061485 => bcp de corelation => une seule variable suffit pour expliquer
# il vaut meix ne pas avoir trop de variables
# deplus visuellement pas vraiment de corélation lineaire
plot(ratio_victoire ~ avg_HEAD_att , data = base_reg , pch = 16)
abline(res.lm, col = "red", lwd = 2)

# B1 - Estimation des param�tres (m�thode des moindres carr�s)
# B2 - Test global du mod�le (test F) / tests de nullit� descoefficients
# B3 - Qualit� du mod�le (coefficient R�)

# C - V�rification des hypoth�ses => idem regression lineaire simple
# C1 - Valeurs ajust�es / r�sidus studentis�s (ind�pendance, structure de variance, points aberrants)
rstud = rstudent(res.lm)
plot(rstud,
     pch = 20,
     ylab = "R�sidus studentis�s",
     ylim = c(-3, 3))
abline(
  h = c(0),
  col = "grey",
  lty = 1,
  lwd = 2
)
abline(
  h = c(-2, 2),
  col = "grey",
  lty = 2,
  lwd = 2
)
#=> pas de structure dans les residus =OK, les valeurs se trouvent surtout entre -2 et+2 (1.95)
length(rstud[rstud > 1.95 | rstud < -1.95]) / length(rstud)
# 95 % des residis sont dans l'intervalle de confiance de 95ù=> OK
# C2 - Distance de Cook (points influents)
res.cook = cooks.distance(model = res.lm)
plot(res.cook,
     type = "h",
     ylab = "Distances de Cook",
     ylim = c(0, 0.6))
abline(h = 0.5, col = "gray", lty = 2)
# pas de points qui tire le modele à lui

# C3 - Droite de Henry (normalit�)
res.qq = qqnorm(rstud,
                pch = 20,
                ylim = c(-3, 7),
                xlim = c(-3, 3))
qqline(rstud,
       lty = 2,
       lwd = 2,
       col = 2)

# ecart <-cbind(data_taille_poids,
#         res.lm$fitted.values,
#         res.lm$residuals,
#         rstud)
# 
# # ne marche pas ??
# ecart <-
#   cbind(data_taille_poids,
#         res.lm$fitted.values,
#         res.lm$residuals,
#         rstud)
# ecart

##################################################################################
# 4- Calculer une ANOVA

#Analyse stance
table(joueur_unique$Stance)
aggregate(avg_TOTAL_STR_att~Stance, data = joueur_unique, mean)
boxplot(formula=avg_TOTAL_STR_att~Stance, data=joueur_unique, boxwex=0.3, col="lightblue", pch=10, xlab="Cat�gorie de poids")
#Analyse weight_class
aggregate(avg_TOTAL_STR_att~weight_class, data = joueur_unique, mean)
hist(joueur_unique$avg_TOTAL_STR_att)
boxplot(formula=avg_TOTAL_STR_att~weight_class, data=joueur_unique, boxwex=0.3, col="lightblue", pch=10, xlab="Cat�gorie de poids")

# Prédire le nombre de coup tenté (avg_TOTAL_STR_att)en fonction de la catégorie
# de poids.(weight_class)

# Prédire le nombre de coup à la tête reçu en fonction de la
# catégorie de poids. Que concluez-vous ?
##################################################################################

# landed = coup tenté, Att = coup réussi

# table de frequence dans un dataframe => les classe de poids
data.frame(table(joueur_unique$weight_class))

# group by la moyenne 
agg <- aggregate(avg_TOTAL_STR_att ~ weight_class, data = joueur_unique, mean)
agg
#help(aggregate) moyenne du nombre de coups par classe de poids => plus les gens sont legers plus il y a de coups?

str(joueur_unique)

base_anova <-
  na.omit(joueur_unique[, c("avg_HEAD_landed", "avg_TOTAL_STR_att", "weight_class")])

# FACTORISATION
# base_anova$weight_factor <- as.factor(base_anova$weight_class)

# analyse graphique
boxplot(
  formula = avg_TOTAL_STR_att ~ weight_class,
  data = base_anova,
  boxwex = 0.3,
  col = 'lightblue'
)

hist(base_anova$avg_TOTAL_STR_att)

plot(base_anova)

# correlations
cor(base_anova$avg_TOTAL_STR_att, base_anova$weight_class)
help(cor)

# le modele
mod.lm <- lm(avg_TOTAL_STR_att ~ weight_class, data = base_anova)
summary(mod.lm)

# Coefficients:
#   Estimate  Std. Error t value                                      Pr(>|t|)    
  # (Intercept)                        105.826      4.056  26.090     < 2e-16 ***
#   weight_classCatch Weight           -39.014     19.940  -1.957   0.050591 .  
# weight_classFeatherweight           -3.774      5.736  -0.658     0.510701    
# weight_classFlyweight              -10.633      7.224  -1.472     0.141271    
# weight_classHeavyweight            -30.979      5.959  -5.199     2.29e-07 ***
#   weight_classLight Heavyweight      -28.250      5.790  -4.880   1.18e-06 ***
#   weight_classLightweight            -16.935      5.008  -3.381   0.000740 ***
#   weight_classMiddleweight           -27.005      5.337  -5.059   4.74e-07 ***
#   weight_classWelterweight           -17.716      4.956  -3.575   0.000362 ***
#   weight_classWomen's Bantamweight    10.691      9.376   1.140   0.254412    
# weight_classWomen's Featherweight   -2.376     21.768  -0.109     0.913102    
# weight_classWomen's Flyweight       37.720      8.847   4.264     2.14e-05 ***
# weight_classWomen's Strawweight     28.666      7.886   3.635     0.000288 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 47.82 on 1450 degrees of freedom
# Multiple R-squared:  0.09708,	Adjusted R-squared:  0.08961 => 8.9 % => pas terrblie 
# F-statistic: 12.99 on 12 and 1450 DF,  p-value: < 2.2e-16 => ok
anova(mod.lm)


#=> il semlble y avoir une incoherence des resultats : nb de coups eleves mais peu d'incidence
# sur certaines classe => si on regarde les effectifs des classes on voit qu il y a des classes
# moins frequentes
#=> la solution serait de regrouper les classes et de passer de 4 classes a 8

# C - V�rification des hypoth�ses
# C1 - Valeurs ajust�es / r�sidus studentis�s (ind�pendance, structure de variance, points aberrants)
rstud = rstudent(mod.lm)
plot(rstud,
     pch = 20,
     ylab = "R�sidus studentis�s",
     ylim = c(-3, 3))
abline(
  h = c(0),
  col = "grey",
  lty = 1,
  lwd = 2
)
abline(
  h = c(-2, 2),
  col = "grey",
  lty = 2,
  lwd = 2
)
length(rstud[rstud > 1.95 | rstud < -1.95]) / length(rstud)
#[1] 0.050581
# C2 - Distance de Cook (points influents)
res.cook = cooks.distance(model = mod.lm)
plot(res.cook,
     type = "h",
     ylab = "Distances de Cook",
     ylim = c(0, 0.6))
abline(h = 0.5, col = "gray", lty = 2)

# C3 - Droite de Henry (normalit�)
res.qq = qqnorm(rstud,
                pch = 20,
                ylim = c(-3, 7),
                xlim = c(-3, 3))
qqline(rstud,
       lty = 2,
       lwd = 2,
       col = 2)

ecart <-
  cbind(data_taille_poids,
        res.lm$fitted.values,
        res.lm$residuals,
        rstud)

### Factor conversion, which keeps value attributes ####
unique(df_maxdate$weight_class)

df_regresion <- df_maxdate


############################################################################
#############ANOVA entre nombre de coup tent� et cat�gorie de poids retravailler
############################################################################

#Analyse de la variable weight_class
data.frame(table(joueur_unique$weight_class))
aggregate(avg_TOTAL_STR_att~weight_class, data = joueur_unique, mean)
hist(joueur_unique$avg_TOTAL_STR_att)
boxplot(formula=avg_TOTAL_STR_att~weight_class, data=joueur_unique, boxwex=0.3, col="lightblue", pch=10, xlab="Cat�gorie de poids")


#Premier mod�le sans recodage des modalit�s
mod.lm=lm(formula=avg_TOTAL_STR_att~weight_class,data=joueur_unique)
anova(mod.lm)
summary(mod.lm)


#Regroupement des classes de poids
  # pour le reg 
base_reg<-joueur_unique[(joueur_unique$wins + joueur_unique$losses>3) 
                        & joueur_unique$weight_class == "Welterweight", 
                        c("fighter","weight_class","ratio_victoire","avg_BODY_att","avg_HEAD_att","avg_GROUND_att") ]

  # pour anova
data_anova<-joueur_unique[(joueur_unique$wins + joueur_unique$losses>3)
                                    &!is.na(joueur_unique$avg_TOTAL_STR_att),c("fighter",
                                                                     "avg_TOTAL_STR_att","weight_class","avg_HEAD_att")]

data_anova[which(data_anova$weight_class %in% c("Flyweight","Bantamweight")),"categorie_poids2"]<-"poid_plume_homme"
data_anova[which(data_anova$weight_class %in% c("Featherweight","Lightweight")),"categorie_poids2"]<-"poid_leger_homme"
data_anova[which(data_anova$weight_class %in% c("Welterweight","Middleweight")),"categorie_poids2"]<-"poid_moyen_homme"
data_anova[which(data_anova$weight_class %in% c("Light Heavyweight","Heavyweight")),"categorie_poids2"]<-"poid_lourd_homme"
data_anova[which(data_anova$weight_class %in% c("Women's Strawweight","Women's Flyweight")),"categorie_poids2"]<-"poid_plumme_femme"
data_anova[which(data_anova$weight_class %in% c("Women's Bantamweight","Women's Featherweight")),"categorie_poids2"]<-"poid_moyen_femme"

# visualisation des la nouvelle base , classes de  poids regrouppées
hist(data_anova$avg_TOTAL_STR_att)

#Analyse graphique nb de coups en fonction de la classe de poids
boxplot(formula=avg_TOTAL_STR_att~categorie_poids2, data=data_anova, boxwex=0.3, col="lightblue", pch=10, xlab="Cat�gorie de poids")

#Cr�ation du mod�le
mod.lm=lm(formula=avg_TOTAL_STR_att~categorie_poids2,data=data_anova)
anova(mod.lm)
summary(mod.lm)

# C - V�rification des hypoth�ses
# C1 - Valeurs ajust�es / r�sidus studentis�s (ind�pendance, structure de variance, points aberrants)
rstud = rstudent(mod.lm)
plot(rstud,pch=20,ylab="R�sidus studentis�s",ylim=c(-3,3))
abline(h=c(0), col="grey",lty=1,lwd=2)
abline(h=c(-2,2), col="grey",lty=2,lwd=2)
length(rstud[rstud >1.95 | rstud < -1.95])/length(rstud)

# C2 - Distance de Cook (points influents)
res.cook=cooks.distance(model=mod.lm)
plot(res.cook, type="h",ylab="Distances de Cook", ylim=c(0,0.6))
abline(h=0.5,col="gray",lty=2)

# C3 - Droite de Henry (normalit�)
res.qq=qqnorm(rstud, pch=20, ylim=c(-3,7),xlim=c(-3,3))
qqline(rstud, lty=2, lwd=2, col=2)

data_anova<-joueur_unique[(joueur_unique$wins + joueur_unique$losses>3) &!is.na(joueur_unique$avg_TOTAL_STR_att),c("fighter","avg_TOTAL_STR_att","weight_class","avg_HEAD_att","Stance")]
data_anova[which(data_anova$weight_class %in% c("Flyweight","Bantamweight")),"categorie_poids2"]<-"poid_plume_homme"
data_anova[which(data_anova$weight_class %in% c("Featherweight","Lightweight")),"categorie_poids2"]<-"poid_leger_homme"
data_anova[which(data_anova$weight_class %in% c("Welterweight","Middleweight")),"categorie_poids2"]<-"poid_moyen_homme"
data_anova[which(data_anova$weight_class %in% c("Light Heavyweight","Heavyweight")),"categorie_poids2"]<-"poid_lourd_homme"
data_anova[which(data_anova$weight_class %in% c("Women's Strawweight","Women's Flyweight")),"categorie_poids2"]<-"poid_plumme_femme"
data_anova[which(data_anova$weight_class %in% c("Women's Bantamweight","Women's Featherweight")),"categorie_poids2"]<-"poid_moyen_femme"

data_anova[which(data_anova$Stance %in% c("Open Stance","Sideways","Switch","")),"Stance2"]<-"autres"
data_anova[which(data_anova$Stance %in% c("Orthodox")),"Stance2"]<-"Orthodox"
data_anova[which(data_anova$Stance %in% c("Southpaw")),"Stance2"]<-"Southpaw"

table(data_anova$Stance2)
aggregate(avg_TOTAL_STR_att~Stance2, data = data_anova, mean)
boxplot(formula=avg_TOTAL_STR_att~Stance2, data=data_anova, boxwex=0.3, col="lightblue", pch=10, xlab="Cat�gorie de poids")


#Analyse graphique
boxplot(formula=avg_TOTAL_STR_att~categorie_poids2, data=data_anova, boxwex=0.3, col="lightblue", pch=10, xlab="Cat�gorie de poids")
boxplot(formula=avg_TOTAL_STR_att~Stance, data=data_anova, boxwex=0.3, col="lightblue", pch=10, xlab="Cat�gorie de poids")

#Chnagement de levels
str(data_anova)
data_anova$categorie_poids2<-as.factor(data_anova$categorie_poids2)
levels(data_anova$categorie_poids2)<-c("")
levels.default(data_anova$categorie_poids2)

# Via lm
#Mod�le sans int�raction
mod.lm=lm(formula=avg_TOTAL_STR_att~categorie_poids2+Stance2,data=data_anova)
summary(mod.lm)
anova(mod.lm)
#Mod�le avec int�raction
mod.lm=lm(formula=avg_TOTAL_STR_att~categorie_poids2*Stance2,data=data_anova)
summary(mod.lm)
anova(mod.lm)

# Mod�le seulement avec int�raction 
mod.lm=lm(formula=avg_TOTAL_STR_att~categorie_poids2:Stance2,data=data_anova)
summary(mod.lm)
anova(mod.lm)

model1 <- aov(avg_TOTAL_STR_att~categorie_poids2+altitude+canopy+height)


# Signification et choix des classes
#Regroupement des classes de poids
data_anova<-joueur_unique[(joueur_unique$wins + joueur_unique$losses>3) &!is.na(joueur_unique$avg_TOTAL_STR_att),c("fighter","avg_TOTAL_STR_att","weight_class","avg_HEAD_att", "Stance")]
data_anova[which(data_anova$weight_class %in% c("Flyweight","Bantamweight")),"categorie_poids2"]<-"poid_plume_homme"
data_anova[which(data_anova$weight_class %in% c("Featherweight","Lightweight")),"categorie_poids2"]<-"poid_leger_homme"
data_anova[which(data_anova$weight_class %in% c("Welterweight","Middleweight")),"categorie_poids2"]<-"poid_moyen_homme"
data_anova[which(data_anova$weight_class %in% c("Light Heavyweight","Heavyweight")),"categorie_poids2"]<-"poid_lourd_homme"
data_anova[which(data_anova$weight_class %in% c("Women's Strawweight","Women's Flyweight")),"categorie_poids2"]<-"poid_plumme_femme"
data_anova[which(data_anova$weight_class %in% c("Women's Bantamweight","Women's Featherweight")),"categorie_poids2"]<-"poid_moyen_femme"
hist(data_anova$avg_TOTAL_STR_att)
#Cr�ation du mod�le
mod.lm=lm(formula=avg_TOTAL_STR_att~categorie_poids2,data=data_anova)
anova(mod.lm)
summary(mod.lm)


##################################################################################
# 5- Calculer une ANOVA multi
# Prédire le nombre de coup tenté en fonction :
# 
# 1. De la catégorie de poids 
# 2. Du style de combat <- Stance
# 
# Tester les modèles :
# 1. Sans interaction
# 2. Avec interaction
# 3. Hiérarchique (style dépend du poids)
# landed = coup tenté, Att = coup réussi
##################################################################################
str(joueur_unique)

# Sans interaction 
anova.lm <- lm(avg_TOTAL_STR_att ~ categorie_poids2 + Stance, data=data_anova)
summary(anova.lm)
anova(anova.lm)

# Avec interaction
anova_i.lm<-lm(avg_TOTAL_STR_att ~ categorie_poids2 + Stance + categorie_poids2*Stance, data=data_anova)
summary(anova_i.lm)
anova(anova_i.lm)

# Hiérarchique (style dépend du poids)
anova_h.lm<-lm(avg_TOTAL_STR_att ~ categorie_poids2 + Stance + categorie_poids2:Stance, data=data_anova)
summary(anova_h.lm)
anova(anova_h.lm)

# CONCLUSION ANOVA
# Le modele avec la categorie de poids seule est plus explicative.
#  on ne garde pas ce modele. Pas besoin de verifier les hypotheses sur lkes residus


##################################################################################
# 6 - Calculer une ANCOVA  

# Prédire le nombre de victoire / nombre de match en fonction (ratio victoire=quanti):
# 1. De la catégorie de poids (variable quali)
# 2. Du nombre de coup total (variable quanti)
# 
# Calculer un modèle de régression linéaire simple par catégorie
# de poids entre pour prédire le nombre de victoire / nombre de match en fonction :
# 1. Du nombre de coup total
# 
# Quelle solution vous semble la meilleure ? 

##################################################################################

#Regroupement des classes de poids

#enleve les na et les joureurs ayant combattus moins des 3 combats et les joureurs ayant un avg de coup total non nulle
data_anova<- na.omit(joueur_unique[(joueur_unique$wins + joueur_unique$losses>3)
                                   &!is.na(joueur_unique$avg_TOTAL_STR_att) ,
                                   c("fighter","avg_TOTAL_STR_att","weight_class","avg_HEAD_att", "Stance", "ratio_victoire")])

data_anova[which(data_anova$weight_class %in% c("Flyweight","Bantamweight")),
           "categorie_poids2"]<-"poid_plume_homme"
data_anova[which(data_anova$weight_class %in% c("Featherweight","Lightweight")),
           "categorie_poids2"]<-"poid_leger_homme"
data_anova[which(data_anova$weight_class %in% c("Welterweight","Middleweight")),
           "categorie_poids2"]<-"poid_moyen_homme"
data_anova[which(data_anova$weight_class %in% c("Light Heavyweight","Heavyweight")),
           "categorie_poids2"]<-"poid_lourd_homme"
data_anova[which(data_anova$weight_class %in% c("Women's Strawweight","Women's Flyweight"))
           ,"categorie_poids2"]<-"poid_plumme_femme"
data_anova[which(data_anova$weight_class %in% c("Women's Bantamweight","Women's Featherweight")),"categorie_poids2"]<-"poid_moyen_femme"


levels(data_anova$categorie_poids2) <- c("")
levels.default(data_anova$categorie_poids2)

#### ancova (modele 2, les droites ayant coef de codirecteur####
ancova.lm <- lm(formula = ratio_victoire ~ categorie_poids2+avg_TOTAL_STR_att:categorie_poids2, data=data_anova) # :

summary(ancova.lm)
# Call:
#   lm(formula = ratio_victoire ~ categorie_poids2 + avg_TOTAL_STR_att:categorie_poids2, 
#      data = data_anova)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.60161 -0.09640 -0.00525  0.11939  0.45794 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                          0.5136902  0.0349049  14.717  < 2e-16 ***
#   categorie_poids2poid_lourd_homme                    -0.0283502  0.0516140  -0.549  0.58299    
# categorie_poids2poid_moyen_femme                     0.2678650  0.1276652   2.098  0.03624 *  
#   categorie_poids2poid_moyen_homme                    -0.0105870  0.0444486  -0.238  0.81181    
# categorie_poids2poid_plume_homme                    -0.0847688  0.0650607  -1.303  0.19302    
# categorie_poids2poid_plumme_femme                    0.0171141  0.1149909   0.149  0.88173    
# categorie_poids2poid_leger_homme:avg_TOTAL_STR_att   0.0005196  0.0003206   1.621  0.10545    
# categorie_poids2poid_lourd_homme:avg_TOTAL_STR_att   0.0010895  0.0004191   2.599  0.00953 ** 
#   categorie_poids2poid_moyen_femme:avg_TOTAL_STR_att  -0.0011658  0.0009563  -1.219  0.22320    
# categorie_poids2poid_moyen_homme:avg_TOTAL_STR_att   0.0007916  0.0002693   2.940  0.00339 ** 
#   categorie_poids2poid_plume_homme:avg_TOTAL_STR_att   0.0012291  0.0004671   2.632  0.00868 ** 
#   categorie_poids2poid_plumme_femme:avg_TOTAL_STR_att  0.0001109  0.0006634   0.167  0.86735    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1713 on 721 degrees of freedom
# (6 observations deleted due to missingness)
# Multiple R-squared:  0.04046,	Adjusted R-squared:  0.02582 
# F-statistic: 2.764 on 11 and 721 DF,  p-value: 0.001607
anova(ancova.lm)

# Analysis of Variance Table
# 
# Response: ratio_victoire
# Df  Sum Sq  Mean Sq F value    Pr(>F)    
# categorie_poids2                     5  0.1154 0.023079  0.7865 0.5595442    
# categorie_poids2:avg_TOTAL_STR_att   6  0.7767 0.129442  4.4112 0.0002165 ***
#   Residuals                          721 21.1568 0.029344                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# #### ancova LM 2 (modèle 1 le plus complexe, une droite par categorie de poids)####
# ancovalm2.lm <- lm(formula = ratio_victoire ~ categorie_poids2+avg_TOTAL_STR_att, data=data_anova)
# summary(ancovalm2.lm)

# Call:
#   lm(formula = ratio_victoire ~ categorie_poids2 + avg_TOTAL_STR_att, 
#      data = data_anova)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.61507 -0.09549 -0.00526  0.12082  0.49338 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                        0.4931140  0.0206384  23.893  < 2e-16 ***
#   categorie_poids2poid_lourd_homme   0.0230991  0.0191753   1.205   0.2287    
# categorie_poids2poid_moyen_femme   0.0613847  0.0446296   1.375   0.1694    
# categorie_poids2poid_moyen_homme   0.0166251  0.0163460   1.017   0.3095    
# categorie_poids2poid_plume_homme  -0.0075468  0.0212695  -0.355   0.7228    
# categorie_poids2poid_plumme_femme -0.0596560  0.0320567  -1.861   0.0632 .  
# avg_TOTAL_STR_att                  0.0007208  0.0001643   4.388 1.32e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1716 on 726 degrees of freedom
# (6 observations deleted due to missingness)
# Multiple R-squared:  0.03093,	Adjusted R-squared:  0.02292 
# F-statistic: 3.862 on 6 and 726 DF,  p-value: 0.0008403




anova(ancova.lm, ancovalm2.lm)
# Analysis of Variance Table
# 
# Model 1: ratio_victoire ~ categorie_poids2 + avg_TOTAL_STR_att:categorie_poids2
# Model 2: ratio_victoire ~ categorie_poids2 + avg_TOTAL_STR_att
# Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1    721 21.157                           
# 2    726 21.367 -5  -0.21007 1.4318 0.2106



#### ancova LM 3- modele 3 : ANOVA, ####
ancovalm3.lm <- lm(formula = ratio_victoire ~ categorie_poids2, data=data_anova)
summary(ancovalm3.lm)
# Call:
#   lm(formula = ratio_victoire ~ categorie_poids2, data = data_anova)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.56684 -0.07725 -0.00513  0.11817  0.45150 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                        0.5668444  0.0121314  46.726   <2e-16 ***
#   categorie_poids2poid_lourd_homme   0.0097176  0.0191674   0.507   0.6123    
# categorie_poids2poid_moyen_femme   0.0744006  0.0450864   1.650   0.0993 .  
# categorie_poids2poid_moyen_homme   0.0104062  0.0164875   0.631   0.5281    
# categorie_poids2poid_plume_homme  -0.0009616  0.0214811  -0.045   0.9643    
# categorie_poids2poid_plumme_femme -0.0183479  0.0310253  -0.591   0.5544    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1737 on 727 degrees of freedom
# (6 observations deleted due to missingness)
# Multiple R-squared:  0.005234,	Adjusted R-squared:  -0.001608 
# F-statistic: 0.765 on 5 and 727 DF,  p-value: 0.5752

anova(ancovalm2.lm, ancovalm3.lm)
# Analysis of Variance Table
# 
# Model 1: ratio_victoire ~ categorie_poids2 + avg_TOTAL_STR_att
# Model 2: ratio_victoire ~ categorie_poids2
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1    726 21.367                                  
# 2    727 21.933 -1  -0.56659 19.251 1.316e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#### Régression linéaire  ####
res.lm<-lm(ratio_victoire ~ avg_TOTAL_STR_att, data = data_anova)
summary(res.lm)
# anova(ancovalm2.lm, res.lm)
# Call:
#   lm(formula = ratio_victoire ~ avg_TOTAL_STR_att, data = data_anova)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.61121 -0.09225 -0.00092  0.11771  0.44214 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       0.5165848  0.0163928  31.513  < 2e-16 ***
#   avg_TOTAL_STR_att 0.0005593  0.0001509   3.706 0.000227 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1722 on 737 degrees of freedom
# Multiple R-squared:  0.01829,	Adjusted R-squared:  0.01696 
# F-statistic: 13.73 on 1 and 737 DF,  p-value: 0.0002266
# plot(ratio_victoire~avg_TOTAL_STR_att , data = base_reg ,pch=16)
# abline(res.lm,col="red",lwd=2)

#analyse graphique
plot(res.lm, which = 1:4)
# a reduire

##################################################################################
# CHOIX DES VARIABLES 
##################################################################################
##################################################################################
# 1 - Import des donn�es 
##################################################################################
library(data.table)
library(lubridate)
library(car)
library(compute.es)
library(effects)
library(ggplot2)
library(multcomp)
library(pastecs)
library(WRS)
library(tidyverse)
library(leaps)
# Importer la base de donn�es 
# data.csv
data <- read.csv("C:/Users/ejosse/OneDrive - Business & Decision/Enseignement/Master MEDAS/2ieme ann�e/2020-2021/Pour �tudiant/Data/data.csv",sep = ",")

# Pensez � bien v�rifier le format de vos champs !!!
# Pensez � regarder si des erreurs de saisie / valeurs aberrantes sont pr�sentes
summary(data)
str(data)

data$date<-as.Date(data$date)
data<-data[year(data$date)>2000,]
##################################################################################
# 2 - Cr�ation d'une table de combattant unique
##################################################################################

# Etape 1 : faire un dataframe avec toutes les variables concernant le joueur bleu + Date + Gagnant + Cat�gorie de poids + Nombre de round
nom_colonne<-colnames(data)
col_rouge<-nom_colonne[nom_colonne %like% "^R_"]
col_bleu<-nom_colonne[nom_colonne %like% "^B_"]

data_rouge<-data[ ,c("date","Winner","title_bout","weight_class","no_of_rounds", col_rouge) ]
data_bleu<-data[ ,c("date","Winner","title_bout","weight_class","no_of_rounds", col_bleu) ]



colnames(data_rouge)<-c("date","Winner","title_bout","weight_class","no_of_rounds",substr(col_rouge,3,1000) )
colnames(data_bleu)<-c("date","Winner","title_bout","weight_class","no_of_rounds",substr(col_bleu,3,1000) )


base_joueur_match<-rbind(data_rouge,data_bleu)

date_max<-aggregate(date ~ fighter , data = base_joueur_match , max )

joueur_unique <- merge(base_joueur_match,date_max, 
                       by.x = c("date","fighter") ,by.y = c("date","fighter"),
                       all.x = F , all.y = F)


doublon<-data.frame(table(joueur_unique$fighter))
doublon[doublon$Freq>1,]

# Etape 2 : faire un dataframe avec toutes les variables concernant le joueur rouge + Date + Gagnant + Cat�gorie de poids + Nombre de round
# Etape 3 : Renommer les variables pour que les noms de colonnes soit identiques entre les deux dataframe cr��s ci-dessus 
# Etape 4 : Concatener les deux dataframes en un seul
# Etape 5 : S�lectionner seulement la ligne correspondant au dernier combat par combattant

##################################################################################
# 3 - Calculer la r�gression lin�aire simple entre le poids et la taille 
##################################################################################
attach(joueur_unique)

data_taille_poids <- na.omit(joueur_unique[,c("Weight_lbs","Height_cms")])
summary(data_taille_poids)
# A - Analyse graphique
plot(data_taille_poids$Weight_lbs,data_taille_poids$Height_cms)
table(data_taille_poids$Height_cms)
# B - Construction du mod�le 
str(data_taille_poids)
res.lm<-lm( Weight_lbs~Height_cms  , data = data_taille_poids)
?lm
summary(res.lm)

plot(Weight_lbs~Height_cms , data =data_taille_poids ,pch=16)
abline(res.lm,col="red",lwd=2)

# B1 - Estimation des param�tres (m�thode des moindres carr�s)
# B2 - Test global du mod�le (test F) / tests de nullit� descoefficients
# B3 - Qualit� du mod�le (coefficient R�)
ecart<-cbind(data_taille_poids,res.lm$fitted.values,res.lm$residuals,rstud)
# C - V�rification des hypoth�ses
# C1 - Valeurs ajust�es / r�sidus studentis�s (ind�pendance, structure de variance, points aberrants)
rstud = rstudent(res.lm)
plot(rstud,pch=20,ylab="R�sidus studentis�s",ylim=c(-3,3))
abline(h=c(0), col="grey",lty=1,lwd=2)
abline(h=c(-2,2), col="grey",lty=2,lwd=2)
length(rstud[rstud >1.95 | rstud < -1.95])/length(rstud)

# C2 - Distance de Cook (points influents)
res.cook=cooks.distance(model=res.lm)
plot(res.cook, type="h",ylab="Distances de Cook", ylim=c(0,0.6))
abline(h=0.5,col="gray",lty=2)

# C3 - Droite de Henry (normalit�)
res.qq=qqnorm(rstud, pch=20, ylim=c(-3,7),xlim=c(-3,3))
qqline(rstud, lty=2, lwd=2, col=2)


# D - Pr�diction 
# Quel serait la taille d'une personne pesant 135 lbs
hist(taille_poid$Height_cms)
new <- data.frame(Height_cms = seq(160, 200, 5))

yy <- cbind(new,predict(res.lm, new, interval="prediction"))

##################################################################################
# 4- Calculer la r�gression lin�aire multiple entre le ratio de victoire et 
# Le nombre de coup � la t�te / le nombre de coup au corp / le nombre de coup au sol 
##################################################################################
# A - Analyse graphique

joueur_unique$ratio_victoire<-joueur_unique$wins/(joueur_unique$wins + joueur_unique$losses)
hist(joueur_unique$ratio_victoire)

#On garde seulement les combattants avec plus de 3 matchs
base_reg<-joueur_unique[(joueur_unique$wins + joueur_unique$losses>3) & joueur_unique$weight_class == "Welterweight", c("fighter","weight_class","ratio_victoire","avg_BODY_att","avg_HEAD_att","avg_GROUND_att") ]
hist(base_reg$ratio_victoire)
table(joueur_unique$wins + joueur_unique$losses)
summary(base_reg)
str(base_reg)

#Analyse graphique
plot(base_reg)
cor(base_reg$ratio_victoire , base_reg$avg_BODY_att)
cor(base_reg$ratio_victoire , base_reg$avg_HEAD_att)
cor(base_reg$ratio_victoire , base_reg$avg_GROUND_att)
cor(base_reg$avg_GROUND_att , base_reg$avg_HEAD_att)

# B - Construction du mod�le 
# B1 - Estimation des param�tres (m�thode des moindres carr�s)
# B2 - Test global du mod�le (test F) / tests de nullit� descoefficients
# B3 - Qualit� du mod�le (coefficient R�)
res.lm <- lm(ratio_victoire~avg_GROUND_att+avg_HEAD_att+avg_BODY_att, data= base_reg)
summary(res.lm)

plot(Weight_lbs~Height_cms , data =data_taille_poids ,pch=16)
abline(res.lm,col="red",lwd=2)

# B1 - Estimation des param�tres (m�thode des moindres carr�s)
# B2 - Test global du mod�le (test F) / tests de nullit� descoefficients
# B3 - Qualit� du mod�le (coefficient R�)

# C - V�rification des hypoth�ses
# C1 - Valeurs ajust�es / r�sidus studentis�s (ind�pendance, structure de variance, points aberrants)
rstud = rstudent(res.lm)
plot(rstud,pch=20,ylab="R�sidus studentis�s",ylim=c(-3,3))
abline(h=c(0), col="grey",lty=1,lwd=2)
abline(h=c(-2,2), col="grey",lty=2,lwd=2)
length(rstud[rstud >1.95 | rstud < -1.95])/length(rstud)

# C2 - Distance de Cook (points influents)
res.cook=cooks.distance(model=res.lm)
plot(res.cook, type="h",ylab="Distances de Cook", ylim=c(0,0.6))
abline(h=0.5,col="gray",lty=2)

# C3 - Droite de Henry (normalit�)
res.qq=qqnorm(rstud, pch=20, ylim=c(-3,7),xlim=c(-3,3))
qqline(rstud, lty=2, lwd=2, col=2)


############################################################################
#############ANOVA entre nombre de coup tent� et cat�gorie de poids retravailler
############################################################################

#Analyse de la variable weight_class
data.frame(table(joueur_unique$weight_class))
aggregate(avg_TOTAL_STR_att~weight_class, data = joueur_unique, mean)
hist(joueur_unique$avg_TOTAL_STR_att)
boxplot(formula=avg_TOTAL_STR_att~weight_class, data=joueur_unique, boxwex=0.3, col="lightblue", pch=10, xlab="Cat�gorie de poids")


#Premier mod�le sans recodage des modalit�s
mod.lm=lm(formula=avg_TOTAL_STR_att~weight_class,data=joueur_unique)
anova(mod.lm)
summary(mod.lm)

#Regroupement des classes de poids
data_anova<-joueur_unique[(joueur_unique$wins + joueur_unique$losses>3) &!is.na(joueur_unique$avg_TOTAL_STR_att),c("fighter","avg_TOTAL_STR_att","weight_class","avg_HEAD_att")]
data_anova[which(data_anova$weight_class %in% c("Flyweight","Bantamweight")),"categorie_poids2"]<-"poid_plume_homme"
data_anova[which(data_anova$weight_class %in% c("Featherweight","Lightweight")),"categorie_poids2"]<-"poid_leger_homme"
data_anova[which(data_anova$weight_class %in% c("Welterweight","Middleweight")),"categorie_poids2"]<-"poid_moyen_homme"
data_anova[which(data_anova$weight_class %in% c("Light Heavyweight","Heavyweight")),"categorie_poids2"]<-"poid_lourd_homme"
data_anova[which(data_anova$weight_class %in% c("Women's Strawweight","Women's Flyweight")),"categorie_poids2"]<-"poid_plumme_femme"
data_anova[which(data_anova$weight_class %in% c("Women's Bantamweight","Women's Featherweight")),"categorie_poids2"]<-"poid_moyen_femme"
hist(data_anova$avg_TOTAL_STR_att)

#Analyse graphique
boxplot(formula=avg_TOTAL_STR_att~categorie_poids2, data=data_anova, boxwex=0.3, col="lightblue", pch=10, xlab="Cat�gorie de poids")

#Cr�ation du mod�le
mod2.lm=lm(formula=avg_TOTAL_STR_att~categorie_poids2,data=data_anova)
anova(mod2.lm)
summary(mod2.lm)

# C - V�rification des hypoth�ses
# C1 - Valeurs ajust�es / r�sidus studentis�s (ind�pendance, structure de variance, points aberrants)
rstud = rstudent(mod2.lm)
plot(rstud,pch=20,ylab="R�sidus studentis�s",ylim=c(-3,3))
abline(h=c(0), col="grey",lty=1,lwd=2)
abline(h=c(-2,2), col="grey",lty=2,lwd=2)
length(rstud[rstud >1.95 | rstud < -1.95])/length(rstud)

# C2 - Distance de Cook (points influents)
res.cook=cooks.distance(model=mod2.lm)
plot(res.cook, type="h",ylab="Distances de Cook", ylim=c(0,0.6))
abline(h=0.5,col="gray",lty=2)

# C3 - Droite de Henry (normalit�)
res.qq=qqnorm(rstud, pch=20, ylim=c(-3,7),xlim=c(-3,3))
qqline(rstud, lty=2, lwd=2, col=2)


############################################################################
#############ANOVA � deux facteurs entre nombre de coup tent� et cat�gorie de poids retravailler + Stance
############################################################################


############################################################################
#############ANOVA � deux facteurs entre nombre de coup tent� et cat�gorie de poids retravailler + Stance
############################################################################

#Analyse stance
table(joueur_unique$Stance)
aggregate(avg_TOTAL_STR_att~Stance, data = joueur_unique, mean)
boxplot(formula=avg_TOTAL_STR_att~Stance, data=joueur_unique, boxwex=0.3, col="lightblue", pch=10, xlab="Cat�gorie de poids")
#Analyse weight_class
aggregate(avg_TOTAL_STR_att~weight_class, data = joueur_unique, mean)
hist(joueur_unique$avg_TOTAL_STR_att)
boxplot(formula=avg_TOTAL_STR_att~weight_class, data=joueur_unique, boxwex=0.3, col="lightblue", pch=10, xlab="Cat�gorie de poids")


data_anova<-joueur_unique[(joueur_unique$wins + joueur_unique$losses>3) &!is.na(joueur_unique$avg_TOTAL_STR_att),c("fighter","avg_TOTAL_STR_att","weight_class","avg_HEAD_att","Stance")]
data_anova[which(data_anova$weight_class %in% c("Flyweight","Bantamweight")),"categorie_poids2"]<-"poid_plume_homme"
data_anova[which(data_anova$weight_class %in% c("Featherweight","Lightweight")),"categorie_poids2"]<-"poid_leger_homme"
data_anova[which(data_anova$weight_class %in% c("Welterweight","Middleweight")),"categorie_poids2"]<-"poid_moyen_homme"
data_anova[which(data_anova$weight_class %in% c("Light Heavyweight","Heavyweight")),"categorie_poids2"]<-"poid_lourd_homme"
data_anova[which(data_anova$weight_class %in% c("Women's Strawweight","Women's Flyweight")),"categorie_poids2"]<-"poid_plumme_femme"
data_anova[which(data_anova$weight_class %in% c("Women's Bantamweight","Women's Featherweight")),"categorie_poids2"]<-"poid_moyen_femme"

data_anova[which(data_anova$Stance %in% c("Open Stance","Sideways","Switch","")),"Stance2"]<-"autres"
data_anova[which(data_anova$Stance %in% c("Orthodox")),"Stance2"]<-"Orthodox"
data_anova[which(data_anova$Stance %in% c("Southpaw")),"Stance2"]<-"Southpaw"

table(data_anova$Stance2)
aggregate(avg_TOTAL_STR_att~Stance2, data = data_anova, mean)
boxplot(formula=avg_TOTAL_STR_att~Stance2, data=data_anova, boxwex=0.3, col="lightblue", pch=10, xlab="Cat�gorie de poids")


#Analyse graphique
boxplot(formula=avg_TOTAL_STR_att~categorie_poids2, data=data_anova, boxwex=0.3, col="lightblue", pch=10, xlab="Cat�gorie de poids")
boxplot(formula=avg_TOTAL_STR_att~Stance, data=data_anova, boxwex=0.3, col="lightblue", pch=10, xlab="Cat�gorie de poids")

#Chnagement de levels
str(data_anova)
data_anova$categorie_poids2<-as.factor(data_anova$categorie_poids2)
levels(data_anova$categorie_poids2)<-c("")
levels.default(data_anova$categorie_poids2)

# Via lm
#Mod�le sans int�raction
mod.lm=lm(formula=avg_TOTAL_STR_att~categorie_poids2+Stance2,data=data_anova)
summary(mod.lm)
anova(mod.lm)
#Mod�le avec int�raction
mod.lm=lm(formula=avg_TOTAL_STR_att~categorie_poids2*Stance2,data=data_anova)
summary(mod.lm)
anova(mod.lm)

# Mod�le seulement avec int�raction 
mod.lm=lm(formula=avg_TOTAL_STR_att~categorie_poids2:Stance2,data=data_anova)
summary(mod.lm)
anova(mod.lm)

model1 <- aov(avg_TOTAL_STR_att~categorie_poids2+altitude+canopy+height)

############################################################################
#############Comparaison ancova VS reg lin�aire en fonction des cat�gorie de poids 
############################################################################


data_ancova<-na.omit(joueur_unique[(joueur_unique$wins + joueur_unique$losses>3) &!is.na(joueur_unique$avg_TOTAL_STR_att),c("fighter","age","ratio_victoire","avg_TOTAL_STR_att","weight_class","avg_HEAD_att","Stance")])
data_ancova[which(data_ancova$weight_class %in% c("Flyweight","Bantamweight")),"categorie_poids2"]<-"poid_plume_homme"
data_ancova[which(data_ancova$weight_class %in% c("Featherweight","Lightweight")),"categorie_poids2"]<-"poid_leger_homme"
data_ancova[which(data_ancova$weight_class %in% c("Welterweight","Middleweight")),"categorie_poids2"]<-"poid_moyen_homme"
data_ancova[which(data_ancova$weight_class %in% c("Light Heavyweight","Heavyweight")),"categorie_poids2"]<-"poid_lourd_homme"
data_ancova[which(data_ancova$weight_class %in% c("Women's Strawweight","Women's Flyweight")),"categorie_poids2"]<-"poid_plumme_femme"
data_ancova[which(data_ancova$weight_class %in% c("Women's Bantamweight","Women's Featherweight")),"categorie_poids2"]<-"poid_moyen_femme"

data_ancova[which(data_ancova$Stance %in% c("Open Stance","Sideways","Switch","")),"Stance2"]<-"autres"
data_ancova[which(data_ancova$Stance %in% c("Orthodox")),"Stance2"]<-"Orthodox"
data_ancova[which(data_ancova$Stance %in% c("Southpaw")),"Stance2"]<-"Southpaw"

data_ancova[data_ancova$age < 25 , "Age2"]<-"-25ans"
data_ancova[data_ancova$age >= 25 & data_ancova$age < 30, "Age2"]<-"25-30ans"
data_ancova[data_ancova$age >= 30 & data_ancova$age < 35, "Age2"]<-"30-35ans"
data_ancova[data_ancova$age >= 35 , "Age2"]<-"+35ans"

summary(data_ancova)

#Analyse graphique
boxplot(formula=ratio_victoire~categorie_poids2, data=data_ancova, boxwex=0.3, col="lightblue", pch=10, xlab="Cat�gorie de poids")
plot(data_ancova$ratio_victoire,data_ancova$avg_TOTAL_STR_att)
#Cr�ation du mod�le

#Mod�le avec coefficient B par modalit� 
mod.ancova1=lm(formula=ratio_victoire~Age2+avg_TOTAL_STR_att,data=data_ancova)
mod.ancova2=lm(formula=ratio_victoire~Age2+avg_TOTAL_STR_att:Age2,data=data_ancova)

summary(mod.ancova1)
anova(mod.ancova1)

anova(mod.ancova1,mod.ancova2)



############################################################################
#############S�lection du meilleurs mod�le
############################################################################

colnames(joueur_unique)
data_select<-na.omit(joueur_unique[(joueur_unique$wins + joueur_unique$losses>3) ,
                                   c("fighter","age","ratio_victoire","weight_class","Stance","avg_HEAD_att","avg_TOTAL_STR_att","avg_BODY_att","avg_CLINCH_att","avg_DISTANCE_att","avg_GROUND_att","avg_KD","avg_SUB_ATT","Height_cms")])

data_select[which(data_select$weight_class %in% c("Flyweight","Bantamweight")),"categorie_poids2"]<-"poid_plume_homme"
data_select[which(data_select$weight_class %in% c("Featherweight","Lightweight")),"categorie_poids2"]<-"poid_leger_homme"
data_select[which(data_select$weight_class %in% c("Welterweight","Middleweight")),"categorie_poids2"]<-"poid_moyen_homme"
data_select[which(data_select$weight_class %in% c("Light Heavyweight","Heavyweight")),"categorie_poids2"]<-"poid_lourd_homme"
data_select[which(data_select$weight_class %in% c("Women's Strawweight","Women's Flyweight")),"categorie_poids2"]<-"poid_plumme_femme"
data_select[which(data_select$weight_class %in% c("Women's Bantamweight","Women's Featherweight")),"categorie_poids2"]<-"poid_moyen_femme"
data_select$categorie_poids2<-as.factor(data_select$categorie_poids2)

data_select[which(data_select$Stance %in% c("Open Stance","Sideways","Switch","")),"Stance2"]<-"autres"
data_select[which(data_select$Stance %in% c("Orthodox")),"Stance2"]<-"Orthodox"
data_select[which(data_select$Stance %in% c("Southpaw")),"Stance2"]<-"Southpaw"
data_select$Stance2<-as.factor(data_select$Stance2)

# faire des classes d'age
data_select[data_select$age < 25 , "Age2"]<-"-25ans"
data_select[data_select$age >= 25 & data_select$age < 30, "Age2"]<-"25-30ans"
data_select[data_select$age >= 30 & data_select$age < 35, "Age2"]<-"30-35ans"
data_select[data_select$age >= 35 , "Age2"]<-"+35ans"
data_select$Age2<-as.factor(data_select$Age2)

data_select<-na.omit(data_select[,!colnames(data_select) %in% c("fighter","age","Stance","weight_class")])
summary(data_select)
str(data_select)

full.model <- lm(ratio_victoire ~., data = data_select)
summary(full.model)

simple.model <- lm(ratio_victoire ~1, data = data_select)
summary(simple.model)

backward <- stepAIC(full.model, direction = "backward")
#ratio_victoire ~ avg_HEAD_att + avg_DISTANCE_att + avg_GROUND_att + 
#  avg_KD + avg_SUB_ATT + Height_cms

forward <- stepAIC(simple.model, direction="forward", scope=list(lower=simple.model, upper=full.model))
#ratio_victoire ~ avg_GROUND_att + avg_KD + avg_DISTANCE_att + 
#  avg_SUB_ATT + avg_HEAD_att + Height_cms

stepwise_aic <- stepAIC(simple.model, direction="both", scope=list(lower=simple.model, upper=full.model))
#ratio_victoire ~ avg_GROUND_att + avg_KD + avg_DISTANCE_att + 
#  avg_SUB_ATT + avg_HEAD_att + Height_cms
summary(stepwise_aic)


n = dim(data_select)[1]
stepwise_bic <- stepAIC(simple.model, direction="both", scope=list(lower=simple.model, upper=full.model),k=log(n))
#ratio_victoire ~ avg_GROUND_att + avg_KD + avg_DISTANCE_att + avg_SUB_ATT
summary(stepwise_bic)

# # **************************************************************************************************
# # La variable à prédire est « Winner »
# # Prédire le gagnant du match en fonction de
# # B_avg_body_landed – R_avg_body_landed mise en classe
# # Prédire le gagnant du match en fonction de
# # R_Stance et B_Stance# 
# # Créer un nouvelle indicateur pour savoir si les deux combattants ont le même style de combat
# # # **************************************************************************************************
# 
# 
# # on a deux fois plus de gagnant rouge a cause des resultat de 2010 avant pas de couleur, les regles du sport on evoluées
# # on filtre en recuperant les données en 2010 => apeu pres 50/50 => il faut reequilibre
# colnames(data)
# # pensez aux dates! souvent des probleme de formats => Lubridate
# data$date <- as.Date(data$date)
# 
# # librairie lubridate On ne conserve que les valeurs où year > 2010
# data <- data[year(data$date) > 2010,]
# data_reg <- na.omit(data[, c("B_avg_BODY_landed",
#                             "R_avg_BODY_landed", 
#                             "Winner",
#                             "R_Stance",
#                             "B_Stance")])
# 
# data_reg <- 
# data_reg$Winner
# 
# # unique(data_reg$B_avg_BODY_landed)
# data_reg$Winner =  data_reg$Winner != 'Draw'
# summary(data_reg$B_avg_BODY_landed)
# summary(data_reg$R_avg_BODY_landed)
# 
# 
# table(data_reg$Winner)
# 
# data_reg[data_reg$B_avg_BODY_landed < 10 , "B_BODY"]<-"-10"
# data_reg[data_reg$B_avg_BODY_landed >= 10 & data_reg$B_avg_BODY_landed < 20, "B_BODY"]<-"10-20"
# data_reg[data_reg$B_avg_BODY_landed >= 20 & data_reg$B_avg_BODY_landed< 30, "B_BODY"]<-"20-30"
# data_reg[data_reg$B_avg_BODY_landed >= 30 , "B_BODY"]<-"+30"
# 
# data_reg$B_BODY<-as.factor(data_reg$B_BODY)
# 
# data_reg$R_Stance <-as.factor(data_reg$R_Stance)
# data_reg$B_Stance <-as.factor(data_reg$B_Stance)
# 
#   
# data_reg[data_reg$B_Stance==data_reg$R_Stance,"diff_body_att"] <- "NON"
# data_reg[data_reg$B_Stance==data_reg$R_Stance,"diff_body_att"] <- "OUI"
# 
# # model 
# model_quali<-glm(Winner~diff_body_att_class ,data=data_reg,family=binomial)
# model_quali
#       
# summary(model_quali)
# exp(coef(model_quali))


############################################################################
############# Regression logistique - Issue du match en fonction de l'allonge(distance)
############################################################################
setwd("C:/Users/CFOUQUE/!!perso/!!MEDAS/STAT2")
data <- read.csv("C:/Users/CFOUQUE/!!perso/!!MEDAS/STAT2/Data/data.csv",
           sep = ",")
str(data)
data$date <- as.Date(data$date)
str(data)
table(data$B_wins)
# Pensez � bien v�rifier le format de vos champs !!!
# Pensez � regarder si des erreurs de saisie / valeurs aberrantes sont pr�sentes
#Selection des variables & ann�e
table(data$Winner)

data<-data[year(data$date)>2010 & data$Winner %in% c("Blue","Red"),c("Winner","B_Stance","R_Stance","B_avg_DISTANCE_att","R_avg_DISTANCE_att")]
# supprime les style de combat vide
data<-data[-which(data$B_Stance=="" |data$R_Stance=="") ,]
# supprime les na dans la distance de combat (allonge)
data<-data[-which(is.na(data$B_avg_DISTANCE_att)),]
data<-data[-which(is.na(data$R_avg_DISTANCE_att)),]

#Echantillonnage � 50/50 sur la variable � pr�dire
red<-data[data$Winner=="Red",]
blue<-data[data$Winner=="Blue",]
sample_red<-sample(1:dim(red)[1],1000)
sample_blue<-sample(1:dim(blue)[1],1000)
data_reg<-rbind(red[sample_red,],blue[sample_blue,])
# table(data_reg$Winner) # 1000 de chaque

#Cr�ation des variables explicatives  d(iférence de distance entre le bleu et le rouge)
data_reg$diff_dist_att <- data_reg$B_avg_DISTANCE_att-data_reg$R_avg_DISTANCE_att
data_reg$diff_win <- data_reg$B_wins-data_reg$R_wins
# data_reg

#  distribution de la variable diff_dist_att en fonction de la couleur du gagnant

hist(data_reg$diff_dist_att,breaks = 100)
summary(data_reg$diff_dist_att)
ggplot(data_reg, aes(x = diff_dist_att)) +
  geom_histogram(aes(color = Winner, fill = Winner), 
                 position = "identity", bins = 30, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))

#Mise en classe diff_dist_att : 5 classes en fonction de la difference de distance 

data_reg[data_reg$diff_dist_att< -50,"diff_dist_att_class"]<-"++Rouge"
data_reg[data_reg$diff_dist_att< -15 & data_reg$diff_dist_att>= -50 ,"diff_dist_att_class"]<-"+Rouge"
data_reg[data_reg$diff_dist_att< 15 & data_reg$diff_dist_att>= -15 ,"diff_dist_att_class"]<-"egal"
data_reg[data_reg$diff_dist_att< 50 & data_reg$diff_dist_att>= 15 ,"diff_dist_att_class"]<-"+Bleu"
data_reg[ data_reg$diff_dist_att>= 50 ,"diff_dist_att_class"]<-"++Bleu"

# table(data_reg$diff_dist_att_class)
# ++Bleu ++Rouge   +Bleu  +Rouge    egal 
# 296     285     447     465     507 

# Calcul des indicateurs de style , creation de la colonne style_similaire 
data_reg[data_reg$B_Stance==data_reg$R_Stance,"style_similaire"]<-"oui"
data_reg[data_reg$B_Stance!=data_reg$R_Stance,"style_similaire"]<-"non"

#Mise au format factor edes variables numerique
str(data_reg)
summary(data_reg)

data_reg$diff_dist_att_class<-as.factor(data_reg$diff_dist_att_class)
data_reg$style_similaire<-as.factor(data_reg$style_similaire)
data_reg$Winner <- as.factor(data_reg$Winner)
# et gestion des levels 
levels(data_reg$Winner)
levels(data_reg$diff_dist_att_class)
levels(data_reg$style_similaire)

data_reg$diff_dist_att_class<-factor(data_reg$diff_dist_att_class, levels=c("egal","++Bleu","++Rouge","+Bleu","+Rouge"))
data_reg$Winner<-factor(data_reg$Winner, levels=c("Red","Blue"))

# Construction du mod�le
table(data_reg$Winner)
model_quali<-glm(Winner~diff_dist_att_class,data=data_reg,family= binomial(logit))
#interpr�tation
model_quali
summary(model_quali)
exp(coef(model_quali))


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


#    Red Blue
# 0  622  575
# 1  378  425

#Taux de bien class�
(m.confusion[1,1]+m.confusion[2,2]) / sum(m.confusion)
# 1] 0.5235 => 52 % , a paine plus que le hasard

#Sensibilit� 
(m.confusion[2,2]) / (m.confusion[2,2]+m.confusion[1,2])

#Sensibilit� 
(m.confusion[2,2]) / (m.confusion[2,2]+m.confusion[1,2])
# [1] 0.425
#Sp�cificit� 
(m.confusion[1,1]) / (m.confusion[1,1]+m.confusion[2,1])
# [1] 0.622

#ODs ratio
exp(cbind(coef(model_quali), confint(model_quali)))
install.packages("questionr")
library(questionr)
odds.ratio(model_quali)
install.packages("GGally")
library(GGally)
install.packages("broom.helpers")
library(broom.helpers)
ggcoef_model(model_quali, exponentiate = TRUE)

###############################################################
# La variable à prédire est « Winner » =  quanli = r logistique
# 
# Construire le meilleur modèle possible
###############################################################
# factorisation des varibales

#

data_select<-na.omit(joueur_unique[(joueur_unique$wins + joueur_unique$losses>3 & joueur_unique$Winner %in% c("Blue","Red")) ,
                                   c("Winner","age","weight_class","Stance","avg_HEAD_att","avg_TOTAL_STR_att","avg_BODY_att","avg_CLINCH_att","avg_DISTANCE_att","avg_GROUND_att","avg_KD","avg_SUB_ATT","Height_cms")])
data_select[which(data_select$weight_class %in% c("Flyweight","Bantamweight")),"categorie_poids2"]<-"poid_plume_homme"
data_select[which(data_select$weight_class %in% c("Featherweight","Lightweight")),"categorie_poids2"]<-"poid_leger_homme"
data_select[which(data_select$weight_class %in% c("Welterweight","Middleweight")),"categorie_poids2"]<-"poid_moyen_homme"
data_select[which(data_select$weight_class %in% c("Light Heavyweight","Heavyweight")),"categorie_poids2"]<-"poid_lourd_homme"
data_select[which(data_select$weight_class %in% c("Women's Strawweight","Women's Flyweight")),"categorie_poids2"]<-"poid_plumme_femme"
data_select[which(data_select$weight_class %in% c("Women's Bantamweight","Women's Featherweight")),"categorie_poids2"]<-"poid_moyen_femme"
data_select$categorie_poids2<-as.factor(data_select$categorie_poids2)
data_select[which(data_select$Stance %in% c("Open Stance","Sideways","Switch","")),"Stance2"]<-"autres"
data_select[which(data_select$Stance %in% c("Orthodox")),"Stance2"]<-"Orthodox"
data_select[which(data_select$Stance %in% c("Southpaw")),"Stance2"]<-"Southpaw"
data_select$Stance2<-as.factor(data_select$Stance2)
data_select[data_select$age < 25 , "Age2"]<-"-25ans"
data_select[data_select$age >= 25 & data_select$age < 30, "Age2"]<-"25-30ans"
data_select[data_select$age >= 30 & data_select$age < 35, "Age2"]<-"30-35ans"
data_select[data_select$age >= 35 , "Age2"]<-"+35ans"
data_select$Age2<-as.factor(data_select$Age2)
data_select$Winner <-as.factor(data_select$Winner) # exclude=NULL
# unique(data_select$Winner)
library(MASS)
summary(data_select)
str(data_select)
data_select <- na.omit(data_select)
levels(data_select$Winner)
simple.model <- glm(Winner ~1, data = data_select, family = binomial)
summary(simple.model)
stepwise_aic <- stepAIC(simple.model, direction="both", scope=list(lower=simple.model, upper=full.model))
# best model
# Winner ~ avg_BODY_att + age + avg_CLINCH_att
summary(stepwise_aic)
n = dim(data_select)[1]
stepwise_bic <- stepAIC(simple.model, direction="both", scope=list(lower=simple.model, upper=full.model),k=log(n))
summary(stepwise_bic)
# model 
model_quali<-glm(Winner ~ avg_BODY_att + age, data=data_select, family = binomial) # binomial
model_quali
summary(model_quali)
exp(coef(model_quali))
# model_quali$
# Matrice de confution
appren.p <- cbind(data_reg, predict(model_quali, newdata = data_reg, type = "link", se = TRUE))
# use the model to predict with new data
# predOut <- predict(object = poissonOut, newdata = newDat, type = "response")
