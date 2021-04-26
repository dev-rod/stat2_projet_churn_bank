
# Chargement des librairies du projet
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

    if ("caret" %in% rownames(installed.packages()) == FALSE) {install.packages("caret", dependencies=TRUE)};library(caret)
    if ("pROC" %in% rownames(installed.packages()) == FALSE) {install.packages("pROC", dependencies=TRUE)};library(pROC)
    if ("ROCR" %in% rownames(installed.packages()) == FALSE) {install.packages("ROCR", dependencies=TRUE)};library(ROCR)
    
    if ("correlationfunnel" %in% rownames(installed.packages()) == FALSE) {install.packages("correlationfunnel", dependencies=TRUE)};library(correlationfunnel)
    if ("ggalluvial" %in% rownames(installed.packages()) == FALSE) {install.packages("ggalluvial", dependencies=TRUE)};library(ggalluvial)
    #if ("tidymodels" %in% rownames(installed.packages()) == FALSE) {install.packages("tidymodels", dependencies=TRUE)};library(tidymodels)
    
    if ("questionr" %in% rownames(installed.packages()) == FALSE) {install.packages("questionr", dependencies=TRUE)};library(questionr)
    if ("broom.helpers" %in% rownames(installed.packages()) == FALSE) {install.packages("broom.helpers", dependencies=TRUE)};library(broom.helpers)
    if ("GGally" %in% rownames(installed.packages()) == FALSE) {install.packages("GGally", dependencies=TRUE)};library(GGally)
    
    # pour le probleme de CONFLIT de select() entre les deux librairies MASS et dplyr
    require(MASS)
    require(dplyr)
    # puis utiliser dplyr::select() pour utiliser le select() de la librairie dplyr.
    # car sinon par Par defaut c'est le select de la  la librairie MASS
    
    #source('http://www.sthda.com/upload/rquery_t_test.r')
    source('script/fonction_rquery.t.test.R')
}

# stat descriptives
desc_stat <- function(data, data_stay, data_quit, source_var, target_var) {

    message(paste("summary de data_stay$", source_var, sep=""))
    print(summary(data_stay[[source_var]]))
    message(paste("variance de data_stay", source_var))
    print(var(data_stay[[source_var]]))

    message(paste("summary de data_quit$", source_var, sep=""))
    print(summary(data_quit[[source_var]]))
    message(paste("variance de data_quit", source_var))
    print(var(data_quit[[source_var]]))
    
    # déclaration en variable globale nécessaire pour ggplotify::as.grob(~hist(...))
    source_var <<- source_var
    target_var <<- target_var
    # analyse visuelle
    # histogramme de chaque population
    graphA <- ggplotify::as.grob(~hist(data_stay[[source_var]], xlab=source_var, ylab=target_var, main = paste("[STAYED] Histo of", source_var)))
    graphB <- ggplotify::as.grob(~hist(data_quit[[source_var]], xlab=source_var, ylab=target_var, main = paste("[QUIT] Histo of" , source_var)))
    # comparaison des dispersions
    graphC <- ggplotify::as.grob(~boxplot(data[[source_var]] ~ data[[target_var]], xlab=source_var, ylab=target_var))
    # plot de variable source par variable cible
    graphD <- ggplot(data, aes(x = get(source_var), fill = get(target_var))) +
        xlab(source_var)+
        geom_density(alpha = 0.7) +
        scale_fill_manual(values = c('black', 'white'), name ='') +
        theme_classic() +
        labs(title = paste(source_var, " by ", target_var)) +
        theme(legend.position = 'bottom')
    # TODO to refactor
    # p1 <- data %>%
    #     select(Total_Trans_Ct,Attrition_Flag) %>%
    #     ggplot(aes(x=Total_Trans_Ct,fill=Attrition_Flag)) +
    #     geom_bar(alpha=0.4,position="dodge") +
    #     labs(title="Distribution of Total Transaction Count by Customer type", x="Total Transaction Count", y="Count")
    cowplot::plot_grid(graphA, graphB, graphC, graphD, labels=c("A", "B", "C", "D"), ncol = 2, nrow = 2)
}
# Tests de student et de wilcoxon
test_stat <- function(data_reg_stay, data_reg_quit, source_var, wording) {
    message("Test de student")
    print(rquery.t.test(data_reg_quit[[source_var]], data_reg_stay[[source_var]], wording=wording))
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

spearman_graph_correlation <- function(data) {
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
}

estimate_nb_cluster <- function(scaled_df_data){
    
    # determining nb clusters with elbow method (warning very low)
    # basée sur la minimisation de la somme des carrés des écarts à l’intérieur des clusters (SSwithin).
    fviz_nbclust(scaled_df_data, kmeans, method = "wss") +
        geom_vline(xintercept = 4, linetype = 2)+
        labs(subtitle = "Elbow method")
    
    # determining nb clusters with Silhouette method (quickly)
    # basée sur la maximisation du paramètre appelé “average silhouette”.
    # fviz_nbclust(scaled_df_data, kmeans, method = "silhouette")+
    #     labs(subtitle = "Silhouette method")
    
    # determining nb clusters with gap statistics method (very very low)
    # basée sur la comparaison de la variation totale intra-cluster pour différentes valeurs de k
    # avec leurs valeurs attendues sous une distribution de référence nulle des données.
    # fviz_nbclust(scaled_df_data, kmeans, nstart = 25, method = "gap_stat", nboot = 50)+
    #     labs(subtitle = "Gap statistic method")
    
    # if ("NbClust" %in% rownames(installed.packages()) == FALSE) {install.packages("NbClust", dependencies=TRUE)};library(NbClust)
    # nb <- NbClust(scaled_df_data, distance = "euclidean", min.nc = 2,
    #               max.nc = 10, method = "kmeans")
}

graphe_nuage_clusters <- function(kmeans_out, data_k){
    # nuage de points des 4 clusters
    # TODO variabiliser les couleurs
    factoextra::fviz_cluster(kmeans_out, data = data_k,
                             palette = c("#ceec97", "#f4b393", "#fc60a8", "#7a28cb"),
                             geom = "point",
                             ellipse.type = "convex",
                             ggtheme = theme_bw()
    )
    # nuage de points des 4 clusters avec numéro des individus (peu lisible)
    # factoextra::fviz_cluster(res.km, data_kmeans, ellipse.type = "norm")
}

graphe_acp_nuage_clusters <-  function(acp_out, kmeans_out, target_data){
    # TODO variabiliser Attrition_Flag
    
    # Coordonnées des individus
    ind.coord <- as.data.frame(factoextra::get_pca_ind(acp_out)$coord)
    
    # Ajouter des clusters obtenus à l'aide de l'algorithme K-means
    ind.coord$cluster <- factor(kmeans_out$cluster)
    
    # Ajout de la variable cible à partir de l'ensemble de données d'origine
    ind.coord$Attrition_Flag <- target_data
    
    # inspection des données
    #print(head(ind.coord))
    
    # Pourcentage de la variance expliquée par les dimensions
    eigenvalue <- round(get_eigenvalue(acp_out), 1)
    variance.percent <- eigenvalue$variance.percent
    print(head(eigenvalue))
    
    pca_cluster <- ind.coord %>%
        group_by(Attrition_Flag, cluster) %>%
        count() %>%
        as.data.frame()
    
    percentage_total <- pca_cluster %>%
        group_by(Attrition_Flag) %>%
        summarise(per_tot=n/sum(n)*100)
    
    pca_cluster <- cbind(pca_cluster,'%'=round(percentage_total$per_tot,1))
    
    #c1 <- fviz_eig(acp_out)
    
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
}

# target var is first column, df is only with numeric types (not factors)
graph_target_correlation <- function(df, target_var_name){
    # correlations
    correlation = cor(df)
    # correlation as data.frame
    target_corr = as.data.frame(correlation[,1])
    # correlation column name
    colnames(target_corr) <- 'Correlation'
    # sort dataframe
    target_corr <- target_corr %>% arrange(desc(Correlation))
    # exclude target
    target_corr <- target_corr %>% filter(Correlation<1) 
    # round
    target_corr <- round(target_corr, 2)
    
    # PLOT CORRELATION
    target_corr %>% arrange(desc(Correlation)) %>%
        ggplot(aes(x=Correlation,
                   y=reorder(rownames(target_corr),Correlation),
                   fill=Correlation)) +
        geom_col(color='black') + labs(title=paste(target_var_name, ' Correlation'), y='') +
        theme_classic() +
        theme(legend.position = 'none')
}



select_best_model <- function(training_data){
    # Construction du modèle
    full.model <- glm(Attrition_Flag~., data=training_data, family=binomial(logit))
    simple.model <- glm(Attrition_Flag~1, data=training_data, family=binomial(logit))
    
    # backward <- stepAIC(full.model, direction = "backward")
    # 
    # forward <- stepAIC(simple.model, direction="forward", scope=list(lower=simple.model, upper=full.model))
    
    # challenge itératif avec ajout d'une nouvelle variable
    stepwise_aic <- stepAIC(simple.model, direction="both", scope=list(lower=simple.model, upper=full.model))
    message("################################ summary(stepwise_aic) ")
    print(summary(stepwise_aic))
    return(stepwise_aic)
}