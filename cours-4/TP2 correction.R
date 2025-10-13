## ===============================================
## TP 2 — Analyse en Composantes Principales (ACP)
## Correction
## ===============================================

# Installation des packages si nécessaire
install.packages("FactoMineR")

# Chargement des librairies
library(FactoMineR)

cat("\n=== Partie 1 — Lancement d'une ACP avec FactoMineR ===\n")

# 1. 
data(decathlon)
head(decathlon)

# 2.
# scale.unit=TRUE est la valeur par défaut, donc l'ACP est bien normée (centrée-réduite).
res.pca <- PCA(decathlon[, 1:10], graph = FALSE)
res.pca

# 3.
# Le tableau des valeurs propres contient la variance, le % de variance et le % cumulé pour chaque axe.
print(res.pca$eig)

# 4.
plot(res.pca, choix = "var")

# 8.
plot(res.pca, choix = "ind")


# 10
library(FactoMineR)
library(quantmod)

## 12. 
tickers <- c(
  "GLE.PA",   # Société Générale (Banque)
  "BNP.PA",   # BNP Paribas (Banque)
  "TTE.PA",   # TotalEnergies (Énergie)
  "SAN.PA",   # Sanofi (Santé)
  "AIR.PA",   # Airbus (Aéronautique)
  "MC.PA",    # LVMH (Luxe)
  "OR.PA",    # L'Oréal (Cosmétiques)
  "KER.PA",   # Kering (Luxe)
  "RMS.PA",   # Hermès (Luxe)
  "RI.PA"     # Pernod Ricard (Spiritueux)
)
tickers <- c("GLE.PA", "BNP.PA")
getSymbols(tickers, from = "2022-01-01", to = "2023-12-31")

## 13. Préparation et calcul des rendements
# On fusionne les prix de clôture ajustés (Ad) de chaque action en un seul tableau
prices <- Ad(get(tickers[1]))
for (ticker in tickers[-1]) {
  prices <- merge(prices, Ad(get(ticker)))
}

# On calcule les rendements journaliers et on nettoie le tableau
returns_df <- as.data.frame(diff(log(prices)))[-1,]

## 14. Lancement de l'ACP
res.pca.fin <- PCA(returns_df, scale.unit = TRUE, graph = FALSE)

## 15. Analyse de la variance expliquée
# Affichage du tableau des valeurs propres
print(res.pca.fin$eig)

# Visualisation avec un barplot
barplot(res.pca.fin$eig[, "percentage of variance"], 
        main = "Éboulis des valeurs propres",
        names.arg = 1:nrow(res.pca.fin$eig))

# Affichage du cercle des corrélations
plot(res.pca.fin, choix = "var", title = "Cercle des Corrélations - Actions du CAC40")

plot(res.pca.fin$ind$coord[,1], 
     type = 'l', 
     main = "Évolution de la CP1 (Facteur Marché)",
     xlab = "Jours de Bourse (2022-2023)",
     ylab = "Valeur de la CP1")
