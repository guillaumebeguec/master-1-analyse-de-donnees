## ===============================
## TP 1 — Analyse de données (R)
## Correction avec R de base et dplyr
## ===============================

# Installation des packages si nécessaire
install.packages(c("dplyr", "writexl", "readxl"))

# Chargement des librairies
library(dplyr)
library(writexl)
library(readxl)

cat("\n=== Partie 1 — Bases de R ===\n")

# 1. Création et affichage des classes
x_char    <- "bonjour 2"
class(x_char)
x_char

x_numeric <- 3.14
class(x_numeric)

x_logical <- TRUE
x_false <- FALSE
class(x_logical)

x_null    <- NULL
class(x_null)

# 2. Vecteur et liste
v <- c(1, 5, 6)
v>4
v_inferieur_a_10 <- v[v<10]
v_inferieur_a_10
L <- list(1, "a", TRUE)
class(v)
class(L)
L[[4]]

# 3. Factor
pays_vec <- c("France", "France", "Italie", "Italie", "Germany")
pays_fac <- factor(pays_vec, levels = c("France", "Italie", "Espagne"))
print(levels(pays_fac))
pays_fac

# 4. Import CSV (en ligne)
rio_url <- "https://raw.githubusercontent.com/flother/rio2016/master/athletes.csv"
athletes_raw <- read.csv(rio_url, encoding = "UTF-8", stringsAsFactors = FALSE)
athletes_raw_factors <- read.csv(rio_url, encoding = "UTF-8", stringsAsFactors = TRUE)
class(athletes_raw)
head(athletes_raw)
str(athletes_raw)

# 5. Bonus : Import et export Excel
getwd()
mon_path <- "/Users/guillaumebeguec/documents/analyse-de-donnees-m1"
setwd(mon_path) # Décommentez pour définir votre répertoire de travail
write_xlsx(athletes_raw, path = "athletes_rio2016.xlsx")
athletes_xlsx <- read_excel("athletes_rio2016.xlsx")
head(athletes_xlsx)


cat("\n=== Partie 2 — Manipulations de données ===\n")

# 6. Noms de colonnes et dimensions
names(athletes_raw)
nrow(athletes_raw)
ncol(athletes_raw)

# 7. Classes des colonnes et conversion
sapply(athletes_raw, class)
class(athletes_raw$bronze)

# Conversion en numeric si besoin
class(athletes_raw$height)

athletes_raw_sans_NA <- na.omit(athletes_raw)


athletes_raw$height <- as.numeric(athletes_raw$height)

athletes_raw$height <- as.character(athletes_raw$height)
athletes_raw$heightx4 <- 4 * athletes_raw$height
mean(athletes_raw_sans_NA$height)


athletes_raw$weight <- as.numeric(athletes_raw$weight)
athletes_raw$gold <- as.numeric(athletes_raw$gold)
athletes_raw$silver <- as.numeric(athletes_raw$silver)
athletes_raw$bronze <- as.numeric(athletes_raw$bronze)
athletes_raw_character <- sapply(athletes_raw, as.character)
athletes_raw_character <- data.frame(athletes_raw_character)
sapply(athletes_raw_character, class)

# Conversion en numeric (dplyr)
# athletes_raw <- athletes_raw %>%
#   mutate(across(c(height, weight, gold, silver, bronze), as.numeric))


# 8. Sous-tableau
# R de base
athletes_raw[, 3]

athletes <- athletes_raw[, c("name", "nationality", "sex", "date_of_birth", "height", "weight", "sport", "gold", "silver", "bronze")]
athletes_2 <- athletes_raw[2, c("name", "nationality", "sex", "date_of_birth", "height", "weight", "sport", "gold", "silver", "bronze")]


# dplyr
# athletes <- athletes_raw %>%
#   select(name, nationality, sex, date_of_birth, height, weight, sport, gold, silver, bronze)
head(athletes)


# 9. Colonne Âge
athletes$age <- as.numeric(difftime(as.Date("2016-08-21"), as.Date(athletes$date_of_birth), units = "days")) / 365.25
summary(athletes$age)


# 10. Colonne total
# R de base
athletes$total <- rowSums(athletes[, c("gold", "silver", "bronze")], na.rm = TRUE)
# dplyr
# athletes <- athletes %>%
#   mutate(total = rowSums(across(c(gold, silver, bronze)), na.rm = TRUE))


# 11. Filtre (pays)
# R de base
fr <- athletes[athletes$nationality == "FRA", ]


fr_usa_chn <- athletes[athletes$nationality %in% c("FRA", "USA", "CHN"), ]

# dplyr
fr <- athletes %>% filter(nationality == "FRA")
fr_usa_chn <- athletes %>% filter(nationality %in% c("FRA", "USA", "CHN"))


# 12. Tri
# R de base
top10 <- head(
  athletes[order(-athletes$total), c("name", "nationality", "sport", "total")],
  10
)

# dplyr
# top10 <- athletes %>%
#   arrange(desc(total)) %>%
#   select(name, nationality, sport, total) %>%
#   head(10)
print(top10)


# 13. Pourcentage d'or
# R de base
fr$Gold_pct <- ifelse(fr$total > 0, (fr$gold / fr$total) * 100, 0)
# dplyr
# fr <- fr %>%
#   mutate(Gold_pct = ifelse(total > 0, (gold / total) * 100, 0))
head(fr[, c("name", "sport", "gold", "total", "Gold_pct")])


# 14. Variables numériques
# R de base

sapply(athletes, is.numeric)
athletes_num <- athletes[, sapply(athletes, is.numeric)]


athletes_num <- na.omit(athletes_num)
# dplyr
# athletes_num <- athletes %>%
#   select_if(is.numeric) %>%
#   na.omit()


summary(athletes_num)


# 15. Corrélations
cor_matrix <- cor(athletes_num)
cor_matrix 


# Commentaires sur les corrélations (voir énoncé)

mean(cor_matrix)


# 16. Centrage-réduction
X_cr <- scale(athletes_num)

colMeans(X_cr) # Vérification : proches de 0
apply(X_cr, 2, sd) # Vérification : proches de 1


# 17. Ajouter des rownames
rownames(X_cr) <- rownames(athletes_num)
# Note: On utilise ici les rownames de athletes_num car na.omit() les a conservés.

# 18. Distances
d <- dist(X_cr, method = "euclidean")
D <- as.matrix(d)
dim(D)


# 19. Voisins d’un athlète
who <- rownames(X_cr)[grep("PHELPS", rownames(X_cr), ignore.case = TRUE)[1]]
sort(D[who, ])[1:6]
