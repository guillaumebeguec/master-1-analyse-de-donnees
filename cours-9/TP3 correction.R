## =============================================================
## TP 3 — Analyse des Correspondances Multiples (ACM)
## Correction
## =============================================================

# Installation des packages si nécessaire
if(!require(FactoMineR)) install.packages("FactoMineR")
if(!require(factoextra)) install.packages("factoextra")
install.packages("ggpubr") #si des erreurs su l'install de factoextra

# Chargement des librairies
library(FactoMineR)
library(factoextra)

cat("\n=== Partie 1 — Calcul et Visualisation (FactoMineR & factoextra) ===\n")

data(tea)
head(tea)
summary(tea)

for (i in 1:ncol(tea)) {
  if (is.factor(tea[,i])) {
    levels(tea[,i]) <- paste(colnames(tea)[i], levels(tea[,i]), sep = "_")
  }
}

new_tea <- tea[, c("Tea", "How", "how", "sugar", "price", "where",
                        "sex", "age_Q", "SPC", "tea.time")]

res.mca <- MCA(ma_selection, graph = FALSE)
plot(res.mca)
fviz_eig(res.mca, addlabels = TRUE, main = "Éboulis des valeurs propres (ACM)")

fviz_mca_var(res.mca, 
             repel = TRUE, 
             title = "Nuage des modalités - ACM Tea")

fviz_mca_biplot(res.mca, 
                geom.ind = "point", 
                col.ind = "gray90",  

                col.var = "blue",    
                label = "var",       
                repel = TRUE,         
              
                title = "Biplot ACM - Tea Dataset")

