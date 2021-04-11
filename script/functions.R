
load_libraries <- function(){
    # data.frame amélioré
    if ("data.table" %in% rownames(installed.packages()) == FALSE) { install.packages("data.table", dependencies=TRUE)};library(data.table)
    # gestion des dates
    if ("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate", dependencies=TRUE)};library(lubridate)
    # R Companion to Applied Regression (Functions to Accompany J. Fox and S. Weisberg, AThird Edition, Sage, 2019.)
    if ("car" %in% rownames(installed.packages()) == FALSE) {install.packages("car", dependencies=TRUE)};library(car)
    # Several functions are available for calculating the most widely used effect sizes (ES),
    # along with their variances, confidence intervals and p-values
    if ("compute.es" %in% rownames(installed.packages()) == FALSE) {install.packages("compute.es", dependencies=TRUE)};library(compute.es)
    # Graphical and tabular effect displays, e.g., of interactions, for various statistical models with linear predictors.
    if ("effects" %in% rownames(installed.packages()) == FALSE) {install.packages("effects", dependencies=TRUE)};library(effects)
    # Create Elegant Data Visualisations Using the Grammar of Graphics
    if ("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2", dependencies=TRUE)};library(ggplot2)
    # Simultaneous Inference in General Parametric Models
    if ("multcomp" %in% rownames(installed.packages()) == FALSE) {install.packages("multcomp" ,dependencies=TRUE)};library(multcomp)
    # Package for Analysis of Space-Time Ecological Series
    if ("pastecs" %in% rownames(installed.packages()) == FALSE) {install.packages("pastecs", dependencies=TRUE)};library(pastecs)
    # A Collection of Robust Statistical Methods
    if ("WRS2" %in% rownames(installed.packages()) == FALSE) {install.packages("WRS2", dependencies=TRUE)};library(WRS2)
    # set of packages that work in harmony because they share common data representations and 'API' design.
    if ("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse", dependencies=TRUE)};library(tidyverse)
    if ("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr", dependencies=TRUE)};library(dplyr)
    # Regression subset selection, including exhaustive search.
    if ("leaps" %in% rownames(installed.packages()) == FALSE) {install.packages("leaps", dependencies=TRUE)};library(leaps)
    # Graphes de corrélation
    if ("corrplot" %in% rownames(installed.packages()) == FALSE) {install.packages("corrplot",dependencies=TRUE)};library(corrplot)
    # lib de graphes avancés
    if ("highcharter" %in% rownames(installed.packages()) == FALSE) {install.packages("highcharter",dependencies=TRUE)};library(highcharter)
    # Tools for reordering and modifying factor levels  with Categorical Variables (Factors)
    if("forcats" %in% rownames(installed.packages()) == FALSE) {install.packages("forcats",dependencies=TRUE)};library(forcats)
    if("skimr" %in% rownames(installed.packages()) == FALSE) {install.packages("skimr",dependencies=TRUE)};library(skimr)
    # créer des graphiques prêts à être publiés
    if("ggpubr" %in% rownames(installed.packages()) == FALSE) {install.packages("ggpubr",dependencies=TRUE)};library(ggpubr)
    # Extraire et visualiser les résultats d’analyses de données multivariées
    if("factoextra" %in% rownames(installed.packages()) == FALSE) {install.packages("factoextra",dependencies=TRUE)};library(factoextra)
    # Surrogate Residuals for Ordinal and General Regression Models
    if("sure" %in% rownames(installed.packages()) == FALSE) {install.packages("sure",dependencies=TRUE)};library(sure)
    # Fonctions diverses pour les graphiques "Grid"(grilles)
    if("gridExtra" %in% rownames(installed.packages()) == FALSE) {install.packages("gridExtra",dependencies=TRUE)};library(gridExtra)
    
    if ("cowplot" %in% rownames(installed.packages()) == FALSE) {install.packages("cowplot", dependencies=TRUE)};library(cowplot)
    if ("ggplotify" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplotify", dependencies=TRUE)};library(ggplotify)
    
    # pour le probleme de CONFLIT de select() entre les deux librairies MASS et dplyr
    require(MASS)
    require(dplyr)
    # puis utiliser dplyr::select() pour utiliser le select() de la librairie dplyr.
    # car sinon par Par defaut c'est le select de la  la librairie MASS
    
    source('http://www.sthda.com/upload/rquery_t_test.r')
}

# install_lib <- function(lib) {
#     if (lib %in% rownames(installed.packages()) == FALSE) {
#         install.packages(lib, dependencies=TRUE)
#     }
#     message(lib)
#     library(eval(lib))
#     
#     return(TRUE)
# }

# stat descriptives
desc_stat <- function(data, data_reg_stay, data_reg_quit, data_a, data_b, source_var, target_var) {
    message(paste("summary de data_stay$", source_var, sep=""))
    print(summary(data_a[[source_var]]))
    message(paste("summary de data_quit$", source_var, sep=""))
    print(summary(data_b[[source_var]]))
    message(paste("var de data_stay", source_var))
    print(var(data_a[[source_var]]))
    message(paste("var de data_quit", source_var))
    print(var(data_b[[source_var]]))
    
    # déclaration en variable globale nécessaire pour ggplotify::as.grob(~hist(...))
    data_reg_a <<- data_reg_stay
    data_reg_b <<- data_reg_quit
    # analyse visuelle
    # histogramme de chaque population
    graphA <- ggplotify::as.grob(~hist(data_reg_a[[source_var]], xlab=source_var, ylab=target_var, main = paste("Histogram of", source_var)))
    graphB <- ggplotify::as.grob(~hist(data_reg_b[[source_var]], xlab=source_var, ylab=target_var, main = paste("Histogram of" , source_var)))
    # comparaison des dispersions
    graphC <- ggplotify::as.grob(~boxplot(data[[source_var]] ~ data[[target_var]], xlab=source_var, ylab=target_var))
    # plot de variable source par variable cible
    graphD <- ggplot(data, aes(x = source_var, fill = target_var)) +
        geom_density(alpha = 0.7) +
        scale_fill_manual(values = c('black', 'white'), name ='') +
        theme_classic() +
        labs(title = paste(source_var, " by ", target_var)) +
        theme(legend.position = 'bottom')
    cowplot::plot_grid(graphA, graphB, graphC, graphD, labels=c("A", "B", "C", "D"), ncol = 2, nrow = 2)
}
# Tests de student et de wilcoxon
test_stat <- function(data_reg_stay, data_reg_quit, source_var) {
    message("Test de student")
    print(rquery.t.test(data_reg_quit[[source_var]], data_reg_stay[[source_var]]))
    message("si on est éloigné d'une loi normale, Use a non parametric test like Wilcoxon test.")
    message("Test de Wilcoxon")
    print(wilcox.test(data_reg_quit[[source_var]], data_reg_stay[[source_var]]))
}


search_cors <- function(data, data_stay, data_quit, quali_var) {

    # les modalités
    modalities <- as.list(unique(data[[quali_var]]))

    # Tableau de contingences
    frequence_var <- data.frame(table(data$Attrition_Flag, data[[quali_var]]))
    names(frequence_var)[names(frequence_var) == "Var1"] <- "Attrition_Flag"
    names(frequence_var)[names(frequence_var) == "Var2"] <- quali_var

    for (modality in modalities) {
        my_ratio <- c("0","1")
        my_ratio[1] <- round((frequence_var$Freq[frequence_var[[quali_var]] == modality & frequence_var$Attrition_Flag == "0"]) / (sum(frequence_var$Freq[frequence_var$Attrition_Flag == "0"])), 3)
        my_ratio[2] <- round((frequence_var$Freq[frequence_var[[quali_var]] == modality & frequence_var$Attrition_Flag == "1"]) / (sum(frequence_var$Freq[frequence_var$Attrition_Flag == "1"])), 3)
        message(paste("Ratio", modality, " restés sur total restés et partis sur total partis"))
        print(my_ratio)
    }

    graphA <- data_quit %>%
        dplyr::select(Attrition_Flag, quali_var) %>%
        ggplot(aes(x = Attrition_Flag, fill = get(quali_var))) +
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
        labs(title = paste(quali_var, "distribution"),
             x = "Attrited Customers", y = "count", fill=quali_var)
    
    #  pour les clients
    graphB <- data_stay %>%
        dplyr::select(Attrition_Flag, quali_var) %>%
        ggplot(aes(x = Attrition_Flag, fill = get(quali_var), label="ggg")) +
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
        labs(title = paste(quali_var, "distribution"),
             x = "Existing Customers", y = "count", fill=quali_var)

    cowplot::plot_grid(graphA, graphB, labels=c("A", "B"), ncol = 2, nrow = 1)
}
