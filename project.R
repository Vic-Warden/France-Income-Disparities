# ======================================================================
# OBJECTIF DE CETTE ANALYSE
# L’objectif de cette analyse est de comprendre la répartition géographique des niveaux de vie et d’identifier les disparités socio-économiques entre territoires. À partir du revenu médian, du taux de pauvreté et des indices d’inégalités on voudrait être capable de :
# - Mesurer les contrastes régionaux entre zones aisées et zones défavorisées
# - Analyser la structure du revenu (part des salaires, du chômage, des retraites et des autres revenus) selon les départements
# - Observer le lien entre pauvreté et inégalités
# ======================================================================
 
# Install necessary packages (remove the # to run once)
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("ggplot2")
# install.packages("maps")
# install.packages("mapproj")
# install.packages("FactoMineR")
# install.packages("factoextra")
# install.packages("gplots")
# install.packages("corrplot")
# install.packages("ellipse")
# install.packages("car")
# install.packages("lmtest")
# install.packages("writexl")
 
# Load necessary packages
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(mapproj)
library(FactoMineR)
library(factoextra)
library(gplots)
library(corrplot)
library(ellipse)
library(car)
library(lmtest)
library(writexl)
 
 
 
# Plotting options
options(repr.plot.width = 14, repr.plot.height = 8)
 
 
# ======================================================================
# PRÉSENTATION DU JEU DE DONNÉES
# Le jeu de données que nous analyysons provient de la base FILOSOFI 2018 (Fichier Localisé Social et Fiscal) publiée par l’INSEE. 
# Cette base combine des données fiscales anonymisées issues de la Direction Générale des Finances Publiques (DGFiP) avec des données démographiques de l’INSEE.
# ======================================================================
 
# Read the data from the Excel file
data <- read_excel("datasets.xlsx", sheet = 1)
 
# Convert the data to a data frame
data <- as.data.frame(data)
 
dim(data)
names(data)
 
 
# On renomme les colonnes avec des noms courts. Ce sont les codes officiels de l'INSEE :
# | Code | Signification |
# |---|---|
# | PIMP | Part des ménages fiscaux imposés (%) |
# | TP60 | Taux de bas revenus au seuil de 60 % (%), notre indicateur de pauvreté |
# | Q1, MED, Q3 | 1er quartile, médiane, 3e quartile du revenu par UC (€) |
# | D1 … D9 | Déciles du revenu par UC (€) |
# | EQ | Écart interquartile / médiane |
# | RD | Rapport interdécile D9/D1 |
# | S80S20 | Ratio S80/S20 |
# | GINI | Indice de Gini |
# | PACT | Part des revenus d'activité (%) = PTSA + PCHO + PBEN |
# | PTSA | dont salaires et traitements (%) |
# | PCHO | dont indemnités de chômage (%) |
# | PBEN | dont revenus des activités non salariées (%) |
# | PPEN | Part des pensions, retraites et rentes (%) |
# | PAUT | Part des autres revenus, surtout du patrimoine (%) |
# | NOTE | Note de précaution de l'INSEE |
 
# Rename the columns
rename_columns <- data
names(rename_columns) <- c("IRIS", "LIBIRIS", "COM", "LIBCOM", "DEP", "LIBDEP", "PIMP", "TP60", "Q1", "MED", "Q3", "EQ", "D1", "D2", "D3", "D4", "D6", "D7", "D8", "D9", "RD", "S80S20", "GINI", "PACT", "PTSA", "PCHO", "PBEN", "PPEN", "PAUT", "NOTE")
 
# Check the structure and first few rows of the renamed data frame
str(rename_columns)
head(rename_columns)
 
 
# ======================================================================
# PRÉPARATION DES DONNÉES
# - On supprime les IRIS dont la note de précaution vaut 5 car ce sont les données jugées anormales par l'INSEE.
# - On compte les valeurs manquantes, qui viennent du secret statistique.
# - On ajoute la région, une variable qualitative.
# ======================================================================
 
# Remove rows with NOTE equal to 5
table(rename_columns$NOTE)
rename_columns <- rename_columns[rename_columns$NOTE != 5, ]
 
# Check the dimensions of the data frame after removing rows
dim(rename_columns)
 
# Missing values per variable
colSums(is.na(rename_columns))
 
# Region of each department
rename_columns$REGION <- NA
rename_columns$REGION[rename_columns$DEP %in% c("01","03","07","15","26","38","42","43","63","69","73","74")] <- "Auvergne-Rhone-Alpes"
rename_columns$REGION[rename_columns$DEP %in% c("21","25","39","58","70","71","89","90")]                     <- "Bourgogne-Franche-Comte"
rename_columns$REGION[rename_columns$DEP %in% c("22","29","35","56")]                                         <- "Bretagne"
rename_columns$REGION[rename_columns$DEP %in% c("18","28","36","37","41","45")]                               <- "Centre-Val de Loire"
rename_columns$REGION[rename_columns$DEP %in% c("2A","2B")]                                                   <- "Corse"
rename_columns$REGION[rename_columns$DEP %in% c("08","10","51","52","54","55","57","67","68","88")]           <- "Grand Est"
rename_columns$REGION[rename_columns$DEP %in% c("02","59","60","62","80")]                                    <- "Hauts-de-France"
rename_columns$REGION[rename_columns$DEP %in% c("75","77","78","91","92","93","94","95")]                     <- "Ile-de-France"
rename_columns$REGION[rename_columns$DEP %in% c("14","27","50","61","76")]                                    <- "Normandie"
rename_columns$REGION[rename_columns$DEP %in% c("16","17","19","23","24","33","40","47","64","79","86","87")] <- "Nouvelle-Aquitaine"
rename_columns$REGION[rename_columns$DEP %in% c("09","11","12","30","31","32","34","46","48","65","66","81","82")] <- "Occitanie"
rename_columns$REGION[rename_columns$DEP %in% c("44","49","53","72","85")]                                    <- "Pays de la Loire"
rename_columns$REGION[rename_columns$DEP %in% c("04","05","06","13","83","84")]                               <- "PACA"
rename_columns$REGION[rename_columns$DEP %in% c("972","974")]                                                 <- "DOM"
rename_columns$REGION <- as.factor(rename_columns$REGION)
table(rename_columns$REGION, useNA = "ifany")
 
 
# ======================================================================
# TABLEAU PAR DÉPARTEMENT
# Les données sont au niveau du quartier. Pour comparer les départements, on calcule pour chacun la moyenne de ses IRIS. On obtient un tableau d'une ligne par département.
# Pour D9/D1 et S80/S20, on prend la médiane et non la moyenne. Ces rapports deviennent énormes dans les quartiers où le 1er décile est proche de 0 €, et la moyenne n'aurait plus de sens.
# ======================================================================
 
# Variables to aggregate by department
vars <- c("MED","Q1","Q3","D1","D2","D3","D4","D6","D7","D8","D9", "TP60","PIMP","GINI","EQ","PACT","PTSA","PCHO","PBEN","PPEN","PAUT")
 
# Aggregate the data by department and region, calculating the mean for selected variables
dep <- aggregate(rename_columns[, vars], by = list(DEP = rename_columns$DEP, LIBDEP = rename_columns$LIBDEP, REGION = rename_columns$REGION), FUN = mean, na.rm = TRUE)
 
# Calculate the median of RD and S80S20 by department
dep_ratio <- aggregate(rename_columns[, c("RD", "S80S20")], by = list(DEP = rename_columns$DEP), FUN = median, na.rm = TRUE)
 
# Merge the median ratios back into the department-level data frame
dep <- merge(dep, dep_ratio, by = "DEP")
 
# Number of IRIS per department
dep$n_iris <- as.vector(table(rename_columns$DEP)[dep$DEP])
rownames(dep) <- dep$LIBDEP
dim(dep)
head(dep)
 
# Metropolitan France only (used for maps and models)
dep_m <- dep[dep$REGION != "DOM", ]
dep_m$REGION <- droplevels(dep_m$REGION)
dim(dep_m)
 
 
# ======================================================================
# DESCRIPTION, RÉSUMÉ STATISTIQUE ET VISUALISATION
# ======================================================================
 
summary(rename_columns[, c("MED","TP60","GINI","RD","PTSA","PPEN")])
 
# Same table as in visualization_state.r, exported to Excel for the report
tem <- rename_columns[, c("MED","TP60","PIMP","D1","D9","RD","S80S20","GINI","PTSA","PCHO","PBEN","PPEN","PAUT")]
 
# Summary statistics
summary_stats <- data.frame(
  Min. = round(apply(tem, 2, min, na.rm = TRUE), 3),
  `1st Qu.` = round(apply(tem, 2, quantile, probs = 0.25, na.rm = TRUE), 3),
  Median = round(apply(tem, 2, median, na.rm = TRUE), 3),
  Mean = round(apply(tem, 2, mean, na.rm = TRUE), 3),
  `3rd Qu.` = round(apply(tem, 2, quantile, probs = 0.75, na.rm = TRUE), 3),
  Max. = round(apply(tem, 2, max, na.rm = TRUE), 3)
)
summary_stats
 
# Transpose the data frame
summary_stats <- t(summary_stats)
 
# Convert the transposed summary statistics to a data frame
summary_stats_df <- as.data.frame(summary_stats)
 
# Add a new column 'Measure' to the summary statistics data frame
summary_stats_df$Measure <- rownames(summary_stats_df)
 
# Remove row names before exporting to Excel
rownames(summary_stats_df) <- NULL
write_xlsx(summary_stats_df, path = "summary_stats.xlsx")
 
 
# Interprétation.
# La moyenne du revenu médian (21 746 €) est supérieure à sa médiane (20 860 €) : la distribution est asymétrique à droite. 
# D9/D1 et S80/S20 ont des valeurs aberrantes (moyennes de 18,5 et de plus de 1 000), d'où l'agrégation par la médiane. Les valeurs manquantes sont ignorées pendant l'analyse.
 
 
# --- Histogrammes, Q-Q plots et test de Shapiro-Wilk
# On applique le Shapiro test au tableau par département. Le Shapiro test n'accepte pas plus de 5 000 valeurs : on ne peut donc pas l'appliquer aux 12 394 IRIS.
 
# Visualization of key variables
options(repr.plot.width = 16, repr.plot.height = 14)
a <- c("MED", "TP60", "PIMP", "GINI", "RD", "PTSA", "PPEN", "PAUT")
par(mfrow = c(4, 4))    
 
# Loop through the variables and create histograms, Q-Q plots, and perform Shapiro-Wilk tests
for (i in 1:length(a)){
  sub = dep_m[a[i]][,1]   
  hist(sub, main = paste("Hist. of", a[i], sep = " "), xlab = a[i])
  qqnorm(sub, main = paste("Q-Q Plot of", a[i], sep = " "))
  qqline(sub)
  if (i == 1) {
    s.t <- shapiro.test(sub)
  } else {
    s.t <- rbind(s.t, shapiro.test(sub))
  }
}
# Reset plotting layout
par(mfrow = c(1, 1))
s.t <- s.t[, 1:2]      
s.t <- cbind(a, s.t)   
s.t
 
 
# Interprétation. 
# Seuls TP60 (p = 0,71) et GINI (p = 0,46) suivent une loi normale. Pour les autres variables, p < 0,05 : on rejette la normalité.
 
 
# --- Variable qualitative : répartition des IRIS par région
 
# Visualization of regional distribution
options(repr.plot.width = 14, repr.plot.height = 7)
Region = rename_columns$REGION
 
# Count the number of IRIS in each region
counts <- sort(table(Region), decreasing = TRUE) 
percentages <- 100 * counts / length(Region)
par(mar = c(10, 4, 2, 1))
 
# Create a bar plot of the percentages of IRIS in each region
bp <- barplot(percentages, ylab = "Percentage", col = "lightblue", las = 2, ylim = c(0, 35))
text(x = bp, y = percentages + 1.5, paste("n=", counts), cex = 0.8)
par(mar = c(5, 4, 4, 2))
 
 
# Interprétation. 
# L'Île-de-France représente 31 % des IRIS : seules les communes de plus de 10 000 habitants sont couvertes, donc les zones urbaines sont surreprésentées.
 
 
# ======================================================================
# OBJECTIF 1 : CONTRASTES ENTRE ZONES AISÉES ET ZONES DÉFAVORISÉES
# ======================================================================
# --- Revenu médian par département
 
# Visualization of median income by department
options(repr.plot.width = 18, repr.plot.height = 8)
 
# Lollipop chart of median income by department
print(ggplot(dep, aes(x = reorder(LIBDEP, MED), y = MED)) + geom_point(size = 3, color = "red") + geom_segment(aes(x = reorder(LIBDEP, MED), xend = reorder(LIBDEP, MED), y = 0, yend = MED)) + labs(title = "Median income per consumption unit by department", x = "", y = "Median income (euros)") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8)))
 
# Richest departments
head(dep[order(-dep$MED), c("LIBDEP", "REGION", "MED", "TP60", "GINI")], 10)
 
# Poorest departments
head(dep[order(dep$MED), c("LIBDEP", "REGION", "MED", "TP60", "GINI")], 10)
 
 
# --- Cartes de chaleur géographique
 
# Department names as written in map_data("france")
dep_m$region <- iconv(dep_m$LIBDEP, from = "UTF-8", to = "ASCII//TRANSLIT")
dep_m$region[dep_m$DEP == "21"] <- "Cote-Dor"
dep_m$region[dep_m$DEP == "22"] <- "Cotes-Darmor"
dep_m$region[dep_m$DEP == "2A"] <- "Corse du Sud"
dep_m$region[dep_m$DEP == "95"] <- "Val-Doise"
 
# Merge the map with our data
france <- map_data("france")
map <- merge(france, dep_m, by = "region", all.x = T)
map <- map[order(map$order), ]
 
options(repr.plot.width = 10, repr.plot.height = 8)
 
# Map of the median income
print(ggplot(map, aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = MED)) + geom_path() + scale_fill_gradientn(colours = rev(heat.colors(10))) + coord_map() + labs(title = "Median income per consumption unit", x = "Longitude", y = "Latitude") + guides(fill = guide_legend(title = "Median income")))
 
# Map of the low-income rate
print(ggplot(map, aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = TP60)) + geom_path() + scale_fill_gradientn(colours = rev(heat.colors(10))) + coord_map() + labs(title = "Low-income rate (poverty)", x = "Longitude", y = "Latitude") + guides(fill = guide_legend(title = "Low-income rate (%)")))
 
 
# --- Boîtes à moustaches par région
 
# Boxplot of the median income of the IRIS by region
options(repr.plot.width = 14, repr.plot.height = 8)
par(mar = c(11, 5, 3, 1))
 
# Create a boxplot of median income by region
boxplot(rename_columns$MED ~ rename_columns$REGION, xlab = "", ylab = "Median income (euros)", las = 2, col = "lightblue", main = "Median income of the IRIS by region")
par(mar = c(5, 4, 4, 2))
 
 
# Interprétation.
# - Départements les plus riches : Paris (33 280 €), Hauts-de-Seine (31 440 €), Yvelines.
# - Départements les plus pauvres : La Réunion (13 780 €), Ardennes, Pas-de-Calais.
# - La carte de la pauvreté est presque l'inverse de celle du revenu. La pauvreté est forte dans le Nord, le Nord-Est et sur le pourtour méditerranéen.
# - L'Île-de-France a la médiane la plus haute, mais aussi l'écart interquartile le plus grand. C'est la région la plus hétérogène.
 
 
# ======================================================================
# OBJECTIF 2 : STRUCTURE DU REVENU
# Barres empilées, et carte de la part des retraites.
# ======================================================================
 
# Mean share of each income source by region
sources <- c("PTSA", "PCHO", "PBEN", "PPEN", "PAUT")
struct_reg <- aggregate(rename_columns[, sources], by = list(Region = rename_columns$REGION), FUN = mean, na.rm = TRUE)
names(struct_reg) <- c("Region", "Salaries", "Unemployment", "Self.employed", "Pensions", "Other")
 
# Convert the data to long format and plot the stacked bars
struct_reg_long <- pivot_longer(struct_reg, cols = -Region, names_to = "Source", values_to = "Share")
options(repr.plot.width = 14, repr.plot.height = 8)
print(ggplot(struct_reg_long, aes(x = Region, y = Share, fill = Source)) + geom_bar(stat = "identity") + theme_minimal() + labs(title = "Composition of declared income by region", x = "Region", y = "Share (%)") + theme(axis.text.x = element_text(angle = 45, hjust = 1)))
 
# Map of the share of pensions
options(repr.plot.width = 10, repr.plot.height = 8)
print(ggplot(map, aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = PPEN)) + geom_path() + scale_fill_gradientn(colours = rev(heat.colors(10))) + coord_map() + labs(title = "Share of pensions in declared income", x = "Longitude", y = "Latitude") + guides(fill = guide_legend(title = "Pensions (%)")))
 
 
# Interprétation. 
# L'Île-de-France et les métropoles vivent surtout des salaires (69 % en Île-de-France). Le centre et l'ouest de la France vivent davantage des retraites (jusqu'à 40 % dans l'Allier).
 
 
# ======================================================================
# OBJECTIF 3 : LIEN ENTRE PAUVRETÉ ET INÉGALITÉS
# ======================================================================
# --- Matrice de corrélations (départements)
 
# Correlation matrix of the department-level variables
st <- dep_m[, c("MED", "TP60", "PIMP", "GINI", "RD", "S80S20", "PTSA", "PCHO", "PBEN", "PPEN", "PAUT")]
corMatrix <- cor(as.matrix(st))
 
# Plot the correlation matrix
options(repr.plot.width = 11, repr.plot.height = 11)
col <- colorRampPalette(c("red", "yellow", "blue"))
corrplot.mixed(corMatrix, order = "AOE", lower = "number", lower.col = "black", number.cex = .8, upper = "ellipse", upper.col = col(10), diag = "u", tl.pos = "lt", tl.col = "black")
 
 
# Interprétation. 
# Le taux de bas revenus est corrélé négativement au revenu médian (r = −0,74) et positivement aux inégalités : Gini r = 0,66, D9/D1 r = 0,73.
 
 
# --- Pauvreté vs inégalités
 
# Income classes, as IncomeType in visualization_state.r
dep_income <- dep_m %>% mutate(IncomeType = factor(ifelse(MED < 18500, "Under18500", ifelse(MED < 20000, "18500-20000", ifelse(MED < 22000, "20000-22000", "Above22000")))))
options(repr.plot.width = 15, repr.plot.height = 9)
 
# Create a scatter plot of Gini index vs low-income rate
print(ggplot(dep_income, aes(x = GINI, y = TP60)) + geom_point(aes(shape = IncomeType, color = REGION, size = PTSA)) + geom_smooth(method = 'lm', formula = y ~ x) + geom_text(aes(label = LIBDEP), size = 2.8, vjust = -1.2, check_overlap = TRUE) + labs(x = "Gini index", y = "Low-income rate (%)"))
 
 
# Interprétation. 
# La relation est positive : plus un département est inégalitaire, plus il est pauvre. 
# Paris et les Hauts-de-Seine s'en écartent : forte inégalité, mais faible pauvreté.
 
 
# --- Test du χ² et AFC : classes de pauvreté × classes d'inégalité
# On découpe TP60 et GINI des IRIS en 4 classes (aux seuils des quartiles).
 
# Poverty and inequality classes at IRIS level
d_cl <- na.omit(rename_columns[, c("TP60", "GINI")]) %>% mutate(
  Poverty = factor(ifelse(TP60 < 17, "Pov.low", ifelse(TP60 < 25, "Pov.medium",
                   ifelse(TP60 < 39, "Pov.high", "Pov.very.high"))),
                   levels = c("Pov.low", "Pov.medium", "Pov.high", "Pov.very.high")),
  Inequality = factor(ifelse(GINI < 0.33, "Gini.low", ifelse(GINI < 0.37, "Gini.medium",
                      ifelse(GINI < 0.41, "Gini.high", "Gini.very.high"))),
                      levels = c("Gini.low", "Gini.medium", "Gini.high", "Gini.very.high")))
 
# Contingency table and chi-square test
cont_table <- table(d_cl$Poverty, d_cl$Inequality)
cont_table
chisq.test(cont_table)
 
# Correspondence Analysis
ca_result <- CA(cont_table, graph = FALSE)
get_eigenvalue(ca_result)
options(repr.plot.width = 9, repr.plot.height = 7)
print(fviz_ca_biplot(ca_result, repel = TRUE))
 
 
# Interprétation.
# - Test du χ² : χ² = 2 896, p < 2,2e-16. On rejette l'indépendance : pauvreté et inégalités sont liées.
# - AFC : la Dim 1 (84 %) associe pauvreté faible et inégalités faibles d'un côté, pauvreté forte et inégalités fortes de l'autre.
 
 
# --- Régression linéaire simple et multiple
 
# Simple linear model
model <- lm(TP60 ~ GINI, data = dep_m)
summary(model)
 
# Multiple linear model (PTSA is left out: the shares sum to 100 %)
Modele_full <- lm(TP60 ~ GINI + PCHO + PBEN + PPEN + PAUT, data = dep_m)
summary(Modele_full)
 
# Global test against the null model
Modele0 <- lm(TP60 ~ 1, data = dep_m)
anova(Modele0, Modele_full)
 
# Check the assumptions of the multiple model
e <- residuals(Modele_full)
f <- fitted(Modele_full)
options(repr.plot.width = 14, repr.plot.height = 10)
par(mfrow = c(2, 2))
plot(f, e, xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")
hist(e, main = "Histogram of residuals", xlab = "Residuals")
qqnorm(e)
qqline(e, col = "red")
plot(e, type = "l", main = "Residuals over Observation Order")
abline(h = 0, col = "red")
par(mfrow = c(1, 1))
 
shapiro.test(e)        # H0: residuals are normally distributed
bptest(Modele_full)    # H0: constant variance
dwtest(Modele_full)    # H0: no autocorrelation
vif(Modele_full)       # Multicollinearity
 
 
# Interprétation.
# - Régression simple. TP60 = −22,8 + 141,4 × GINI. Une hausse de 0,01 du Gini correspond à +1,4 point de pauvreté (p < 0,001), avec R² = 0,43.
# - Régression multiple. R² ajusté = 0,76, et le test global contre le modèle nul est significatif (p < 0,001).
# - Hypothèses. Linéarité, indépendance (DW p = 0,40), normalité (Shapiro p = 0,35) et absence de multicolinéarité (VIF < 5) sont vérifiées. L'homoscédasticité ne l'est pas (Breusch-Pagan p < 0,001).
 
 
# ======================================================================
# SYNTHÈSE : ACP SUR LES DÉPARTEMENTS
# ======================================================================
 
# PCA on standardized variables
tem <- dep_m[, c("MED", "TP60", "PIMP", "GINI", "RD", "PTSA", "PCHO", "PBEN", "PPEN", "PAUT")]
pca_result <- prcomp(tem, center = TRUE, scale. = TRUE)
print(pca_result, digits = 3)
round(summary(pca_result)$importance, 3)
 
# Scree plot of PCA
options(repr.plot.width = 9, repr.plot.height = 6)
plot(pca_result, type = "l", main = "Scree Plot")
 
# Biplot of PCA
options(repr.plot.width = 15, repr.plot.height = 13)
biplot(pca_result, scale = 0, cex = c(0.6, 0.9), main = "Biplot of PCA on French departments")
 
 
# Interprétation.
# - On garde 3 composantes : critère de Kaiser (valeurs propres 4,40, 2,29 et 1,73), coude à la 4e composante, et 84,3 % de variance cumulée.
# - PC1 (44 %) : niveau de vie et précarité. MED et PIMP s'opposent à TP60, GINI et RD.
# - PC2 (23 %) : structure du revenu. Les salaires s'opposent aux retraites et aux revenus non salariés.
# - Biplot : les Hauts-de-Seine, les Yvelines et Paris sont du côté de MED, les Pyrénées-Orientales, les Ardennes et l'Aisne du côté de TP60 et GINI.
 
 
# ======================================================================
# CONCLUSION
# - Contrastes territoriaux. L'Île-de-France et les métropoles sont aisées, alors que le Nord-Est, le pourtour méditerranéen et les DOM sont défavorisés.
# - Structure du revenu. Le principal clivage oppose les territoires de salariés aux territoires de retraités.
# - Pauvreté et inégalités. Elles sont liées positivement : r = 0,66, χ² significatif, R² = 0,43 en régression simple et 0,76 en régression multiple.
# Limites : revenu déclaré (sans les aides sociales), champ urbain, moyennes non pondérées, valeurs manquantes.
# Source : INSEE, Filosofi 2018, revenus déclarés à l'IRIS.
# ======================================================================
 