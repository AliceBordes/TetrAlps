library(remotes)
remotes::install_github("giocomai/ganttrify")
library("ganttrify")
library(tibble)
library(ggplot2)


project <- tibble::tribble(
  ~wp, ~activity, ~start_date, ~end_date,
  "Axe 1, art. Domaine vital", "RSF (travail sur les prédicteurs)", "2024-09", "2025-01",
  "Axe 1, art. Domaine vital", "RSF (DM saisonnier : segmentation)", "2024-09", "2025-01",
  "Axe 1, art. Domaine vital", "RSF (échelle : indiv, pop)", "2024-09", "2025-01",
  "Axe 1, art. Effets anthropiques", "SSF (exploration)", "2025-01", "2025-02",
  "Axe 1, art. Effets anthropiques", "SSF (zone d'influence de l'effet)", "2025-02", "2025-10",
  "Axe 1, art. Effets anthropiques", "SSF (cumul des pressions)", "2025-02", "2025-10",
  "Axe 2, art. Taux de survie","Revue de littérature", "2025-09", "2025-10",
  "Axe 2, art. Taux de survie","Analyses exploratoires", "2025-09", "2025-10",
  "Axe 2, art. Taux de survie","Développement du modèle", "2025-09", "2026-06",
  "Axe 3, art. Management","Efficacité des refuges (RSF)","2025-11", "2026-06",
  "Axe 3, art. Management","Efficacité des câbles (SSF)", "2025-11", "2026-06",
  "Académique","Collaboration avec C. Fleming (Orlando)", "2025-03", "2025-07",
  "Académique","Enseignement", "2025-09", "2025-12",
  "Académique", "Rédaction", "2024-11", "2025-01", # 1st article, during the week of "residence d'écriture"
  "Académique", "Rédaction", "2025-07", "2025-10", # 2nd article
  "Académique", "Rédaction", "2026-01", "2026-04", # 3rd article
  "Académique", "Rédaction", "2026-04", "2026-09", # rédaction finale
  "Académique","Soutenance", "2026-09", "2026-10")

# Conversion caractères en date
project$start_date <- as.Date(paste0(project$start_date, "-01"), format = "%Y-%m-%d")
project$end_date <- as.Date(paste0(project$end_date, "-01"), format = "%Y-%m-%d")



letters <- tibble::tribble(
  ~activity, ~spot_type, ~spot_date,
  "Axe 1, art. Domaine vital", "A", "2024-10",
  "RSF (travail sur les prédicteurs)", "B", "2024-11",
  "RSF (DM saisonnier : segmentation)", "C", "2024-11",
  "RSF (échelle : indiv, pop)", "D", "2024-11",
  "SSF (exploration)", "E", "2025-01",
  "Axe 3, art. Management", "F", "2025-12",
  "Enseignement", "G", "2025-11"
)

# Conversion caractères en date
letters$spot_date <- as.Date(paste0(letters$spot_date, "-01"), format = "%Y-%m-%d")


ganttrify(
  project = project,
  spots = letters,
  project_start_date = "2024-09",
  font_family = "Roboto Condensed",
  alpha_activity = 0.6,
  colour_palette = c("#FF6666", "#66CC99", "#FFCC33", "#CC99FF","#99CCFF"),
  month_number_label = FALSE,
  by_date = TRUE,
  mark_years=TRUE,
  label_wrap = 38 #nb de charactères max par ligne
)+
  ggtitle("Rétroplanning de thèse")

ggplot2::ggsave(filename = "C:/Users/albordes/Documents/PhD/TetrAlps/Retroplanning_these.jpg", width = 12, height = 8, bg = "white")

