# AY_TrainTest_Predictor_Correl

library(data.table)
library(fst)
library(stringr)
library(ggplot2)
library(corrplot)
library(dplyr)

setwd("/Users/ztttttt/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Nationwide_SA/data/inputs/ML_001_TrainTest_datasets")

#### Correlation matrix ####
# PM2.5 data, for training & testing, with PMF sites
# PM2.5 datasets include all predictors
pm_predictors = read_fst("ML_TrainTest_001_pm_2011-20.fst")
pm_predictors <-
  plyr::rename(
    pm_predictors,
    c("sRoadiness[1]" = "sRoadiness_1", "sRoadiness[1.5]" = "sRoadiness_1.5", "sRoadiness[2]" = "sRoadiness_2",
      "sRoadCount[1]" = "sRoadCount_1", "sRoadCount[1.5]" = "sRoadCount_1.5", "sRoadCount[2]" = "sRoadCount_2",
      "VMT / area" = "VMT_area", "Concentration" = "PMF_conc"))
names(pm_predictors); dim(pm_predictors)
head(pm_predictors)

pm_predictors_use = dplyr::select(pm_predictors, PMF_conc:PM25_TOT, -(year:cell_id))
setpm_predictors_use(pm_predictors_use)
head(pm_predictors_use)
dim(pm_predictors_use)

# Get correlation matrix
num <- names(pm_predictors_use)[sapply(pm_predictors_use, is.numeric)]
if (nrow(pm_predictors_use) > 1e6) pm_predictors_use <- pm_predictors_use[sample(.N, 1e6)]
cmat_pear <- 
  cor(pm_predictors_use[, ..num], 
      use = "pairwise.complete.obs", method = "pearson")
cmat_sprm <- 
  cor(pm_predictors_use[, ..num], 
      use = "pairwise.complete.obs", method = "spearman")

#### Correlation Pearson ####
# Order variables by hierarchical clustering on 1 - |r|
hc_pear   <- hclust(as.dist(1 - abs(cmat_pear)), method = "average")
ord_pear  <- rownames(cmat_pear)[hc_pear$order]

# ---- 1. Clustered lower-triangle heatmap ----

cl_pear <- as.data.table(as.table(cmat_pear))
setnames(cl_pear, c("Var1", "Var2", "r"))
cl_pear[, `:=`(Var1 = factor(Var1, levels = ord_pear), Var2 = factor(Var2, levels = ord_pear))]
cl_pear <- cl_pear[as.integer(Var1) > as.integer(Var2)]          # lower triangle only

p_heat_pear <- 
  ggplot(cl_pear, aes(Var2, Var1, fill = r)) +
  geom_tile(color = "white", linewidth = 0.2) +
  geom_text(data = cl_pear[abs(r) >= 0.8], aes(label = sprintf("%.2f", r)), size = 2) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       limits = c(-1, 1), name = "Pearson r") +
  coord_fixed() +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid = element_blank())
p_heat_pear
ggsave("Predictor_corr_heatmap_Pearson.png", p_heat, width = 12, height = 11, dpi = 300)

# ---- 2. Strongly correlated pairs ----
pairs_pear <- cl_pear[abs(r) >= 0.7][order(-abs(r))]
pairs_pear[, Pair := factor(paste(Var1, "\u2013", Var2), levels = rev(paste(Var1, "\u2013", Var2)))]
fwrite(pairs_pear[, .(Var1, Var2, r)], "Predictor_corr_pairs_above_0.7.csv")

p_pairs_pear <- ggplot(pairs_pear, aes(r, Pair, fill = r > 0)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c(`TRUE` = "#B2182B", `FALSE` = "#2166AC"), guide = "none") +
  labs(x = "Spearman r", y = NULL, title = "Predictor pairs with |r| \u2265 0.7") +
  theme_minimal(base_size = 14)
p_pairs_pear
ggsave("Predictor_corr_pairs_Pearson.png", p_pairs, width = 8, height = max(3, 0.25 * nrow(pairs)), dpi = 300)

# ---- 3. Dendrogram with |r| = 0.7 cut line ----
png("Predictor_corr_dendrogram_Pearson.png", width = 12, height = 6, units = "in", res = 300)
plot(hc_pear, main = "Predictor clusters (distance = 1 - |r|)", xlab = "", sub = "", cex = 0.7)
abline(h = 1 - 0.7, col = "#B2182B", lty = 2)
dev.off()

#### Correlation Spearman ####
# Order variables by hierarchical clustering on 1 - |r|
hc_sprm   <- hclust(as.dist(1 - abs(cmat_sprm)), method = "average")
ord_sprm  <- rownames(cmat_sprm)[hc_sprm$order]

# ---- 1. Clustered lower-triangle heatmap ----
cl_sprm <- as.data.table(as.table(cmat_sprm))
setnames(cl_sprm, c("Var1", "Var2", "r"))
cl_sprm[, `:=`(Var1 = factor(Var1, levels = ord_sprm), Var2 = factor(Var2, levels = ord_sprm))]
cl_sprm <- cl_sprm[as.integer(Var1) > as.integer(Var2)]          # lower triangle only

p_heat_sprm <- ggplot(cl_sprm, aes(Var2, Var1, fill = r)) +
  geom_tile(color = "white", linewidth = 0.2) +
  geom_text(data = cl_sprm[abs(r) >= 0.8], aes(label = sprintf("%.2f", r)), size = 2) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       limits = c(-1, 1), name = "Spearman r") +
  coord_fixed() +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid = element_blank())
p_heat_sprm
ggsave("Predictor_corr_heatmap_Spearman.png", p_heat, width = 12, height = 11, dpi = 300)

# ---- 2. Strongly correlated pairs ----
pairs_sprm <- cl_sprm[abs(r) >= 0.7][order(-abs(r))]
pairs_sprm[, Pair := factor(paste(Var1, "\u2013", Var2), levels = rev(paste(Var1, "\u2013", Var2)))]
fwrite(pairs_sprm[, .(Var1, Var2, r)], "Predictor_corr_pairs_above_0.7.csv")

p_pairs_sprm <- ggplot(pairs_sprm, aes(r, Pair, fill = r > 0)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c(`TRUE` = "#B2182B", `FALSE` = "#2166AC"), guide = "none") +
  labs(x = "Spearman r", y = NULL, title = "Predictor pairs with |r| \u2265 0.7") +
  theme_minimal(base_size = 10)
ggsave("Predictor_corr_pairs_Spearman.png", p_pairs, width = 8, height = max(3, 0.25 * nrow(pairs)), dpi = 300)

# ---- 3. Dendrogram with |r| = 0.7 cut line ----
png("Predictor_corr_dendrogram_Spearman.png", width = 12, height = 6, units = "in", res = 300)
plot(hc_sprm, main = "Predictor clusters (distance = 1 - |r|)", xlab = "", sub = "", cex = 0.7)
abline(h = 1 - 0.7, col = "#B2182B", lty = 2)
dev.off()




