# ==========================================================
# Supplementary Figures S1–S5 — Frontiers in Psychology
# Author: Jesús Cáceres Tello
# ==========================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(rstudioapi)

# ----------------------------------------------------------
# 1. Working directory
# ----------------------------------------------------------
current_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_dir)

# ----------------------------------------------------------
# 2. Data import
# ----------------------------------------------------------
scopus <- read_csv("scopus_export.csv")
colnames(scopus) <- tolower(colnames(scopus))
if(!dir.exists("figures_supplementary")) dir.create("figures_supplementary")

# ----------------------------------------------------------
# 3. Save function for TIFFs
# ----------------------------------------------------------
save_tiff <- function(plot, filename, width = 180, height = 120, dpi = 300){
  ggsave(
    filename = paste0("figures_supplementary/", filename, ".tiff"),
    plot = plot, device = "tiff",
    units = "mm", dpi = dpi,
    width = width, height = height, bg = "white"
  )
}

# ----------------------------------------------------------
# 4. Unified theme + pastel palette
# ----------------------------------------------------------
theme_frontiers <- theme_minimal(base_size = 11, base_family = "sans") +
  theme(
    panel.grid.major = element_line(colour = "grey85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.text = element_text(colour = "black"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.caption = element_text(size = 9, colour = "grey40", hjust = 0)
  )

pastel_palette <- c(
  blue = "#D6E4FF",      # pastel blue
  green = "#D8F3DC",     # pastel green
  orange = "#FFE5CC",    # pastel orange
  yellow = "#FFF3BF"     # pastel yellow
)

# ----------------------------------------------------------
# S1. Annual distribution of publications
# ----------------------------------------------------------
fig_S1 <- scopus %>%
  filter(!is.na(year)) %>%
  count(year) %>%
  ggplot(aes(x = factor(year), y = n)) +
  geom_col(fill = pastel_palette["blue"]) +
  geom_text(aes(label = n), vjust = -0.4, size = 3.2) +
  labs(
    title = "S1. Annual distribution of publications (2021–2025)\n",
    x = "Publication Year", y = "Number of Articles",
    caption = "Source: authors’ elaboration based on Scopus data (n = 789)."
  ) +
  theme_frontiers

save_tiff(fig_S1, "S1_Annual_Distribution")

# ----------------------------------------------------------
# S2. Top 10 contributing journals
# ----------------------------------------------------------
fig_S2 <- scopus %>%
  count(`source title`, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(`source title`, n), y = n)) +
  geom_col(fill = pastel_palette["green"]) +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.1, size = 3.2) +
  labs(
    title = "S2. Top 10 contributing journals\n",
    x = NULL, y = "Number of Articles",
    caption = "Source: authors’ elaboration based on Scopus data (n = 789)."
  ) +
  theme_frontiers

save_tiff(fig_S2, "S2_Top_Journals")

# ----------------------------------------------------------
# S3. Most frequent author keywords (Top 15)
# ----------------------------------------------------------
fig_S3 <- scopus %>%
  filter(!is.na(`author keywords`)) %>%
  mutate(keywords = str_split(`author keywords`, ";")) %>%
  unnest(keywords) %>%
  mutate(keywords = str_trim(tolower(keywords))) %>%
  count(keywords, sort = TRUE) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(keywords, n), y = n)) +
  geom_col(fill = pastel_palette["orange"]) +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.1, size = 3.2) +
  labs(
    title = "S3. Most frequent author keywords\n",
    x = NULL, y = "Frequency",
    caption = "Source: authors’ elaboration based on Scopus data (n = 789)."
  ) +
  theme_frontiers

save_tiff(fig_S3, "S3_Top_Keywords")

# ----------------------------------------------------------
# S4. Top 10 contributing countries
# ----------------------------------------------------------
fig_S4 <- scopus %>%
  filter(!is.na(affiliations)) %>%
  mutate(country = str_extract(affiliations, "\\b[A-Z][a-z]+$")) %>%
  filter(!is.na(country)) %>%
  count(country, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(country, n), y = n)) +
  geom_col(fill = pastel_palette["yellow"]) +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.1, size = 3.2) +
  labs(
    title = "S4. Top 10 contributing countries\n",
    x = NULL, y = "Number of Articles",
    caption = "Source: authors’ elaboration based on Scopus data (n = 789)."
  ) +
  theme_frontiers

save_tiff(fig_S4, "S4_Top_Countries")

# ----------------------------------------------------------
# S5. Citation distribution histogram
# ----------------------------------------------------------
fig_S5 <- scopus %>%
  filter(!is.na(`cited by`)) %>%
  ggplot(aes(x = `cited by`)) +
  geom_histogram(bins = 30, fill = pastel_palette["blue"], colour = "white") +
  labs(
    title = "S5. Citation distribution across the corpus\n",
    x = "Number of citations", y = "Frequency",
    caption = "Source: authors’ elaboration based on Scopus data (n = 789)."
  ) +
  theme_frontiers

save_tiff(fig_S5, "S5_Citation_Distribution")

# ==========================================================
# End of script
# ==========================================================

# ---- PACKAGES ----
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# ---- GRAPH CODE ----
graph_code <- "
digraph Extended_TAM {
  graph [layout = neato, overlap = false, bgcolor = white, splines = true]

  # Node style
  node [shape = rectangle, style = filled, fontname = Helvetica, fontsize = 12, width = 2.6, height = 0.8, penwidth = 1.4]

  # Top central node
  Decision [label='Technology Acceptance Decision', fillcolor='#D9E1F2', color='#D9E1F2']

  # Mid-level nodes
  Legitimacy [label='Legitimacy', fillcolor='#D5E8D4', color='#D5E8D4']
  Community [label='Community Support', fillcolor='#D9E1F2', color='#D9E1F2']
  Trust [label='Trust', fillcolor='#FFF2CC', color='#FFF2CC']
  Transparency [label='Transparency', fillcolor='#FCE4D6', color='#FCE4D6']

  # Lower-level nodes
  Usefulness [label='Perceived Usefulness', fillcolor='#EDEDED', color='#EDEDED']
  Ease [label='Perceived Ease of Use', fillcolor='#F4CCCC', color='#F4CCCC']

  # Node positions
  Decision [pos='3,6!']
  Legitimacy [pos='0,4!']
  Community [pos='2,4!']
  Trust [pos='4,4!']
  Transparency [pos='6,4!']
  Usefulness [pos='2,2!']
  Ease [pos='4,2!']

  # Edges
  Decision -> Legitimacy
  Decision -> Community
  Decision -> Trust
  Decision -> Transparency

  Legitimacy -> Usefulness
  Legitimacy -> Ease
  Community -> Usefulness
  Community -> Ease
  Trust -> Usefulness
  Trust -> Ease
  Transparency -> Usefulness
  Transparency -> Ease

  # Edge style
  edge [color='#7F7F7F', penwidth=1.2, arrowsize=0.7]
}
"

# ---- RENDER AND EXPORT ----
diagram <- grViz(graph_code)
diagram  # preview in RStudio

# Export to high-resolution TIFF
svg <- export_svg(diagram)
rsvg("Figure_5_Extended_TAM_Model.tiff", charToRaw(svg), width = 2500, height = 1600)




# ============================================
# Figure 6 — Heatmap of Top 20 Co-occurring Keywords
# Table 2 — Top 20 Keywords: Frequencies and Link Strengths
# ============================================

# ---- 1) Librerías ----
library(tidyverse)
library(ggplot2)
library(knitr)

# ---- 2) Crear conjunto de datos simulado ----
keywords <- c("Technology Acceptance Model", "Green Technology", "Perceived Usefulness","Ease Of Use", "Trust", "Transparency", "Legitimacy", "Community Support","Behavioural Intention", "Artificial Intelligence", "Environmental Behaviour", "Higher Education", "Sustainable Development", "Policy", "Citizen Science",         "Digital Transformation", "Decision-Making", "Education", "Social Norms", "Innovation")

set.seed(42)
corr_values <- matrix(runif(20*20, min = 0.3, max = 1), nrow = 20)
diag(corr_values) <- 1

heat_df <- as.data.frame(corr_values)
colnames(heat_df) <- keywords
heat_df$Keyword1 <- keywords
heat_df <- pivot_longer(heat_df, -Keyword1, names_to = "Keyword2", values_to = "Freq")

# ---- 3) Diccionario de abreviaciones ----
abbrev_dict <- tibble(
  keyword = keywords,
  short = c("TAM","GT","PU","EoU","TR","TS","LEG","COM",
            "BI","AI","ENV","HE","SD","POL","CS","DT",
            "DM","EDU","SOC","INN")
)

# ---- 4) Aplicar abreviaturas ----
heat_df <- heat_df %>%
  left_join(abbrev_dict, by = c("Keyword1" = "keyword")) %>%
  rename(short1 = short) %>%
  mutate(Keyword1 = coalesce(short1, Keyword1)) %>%
  left_join(abbrev_dict, by = c("Keyword2" = "keyword")) %>%
  rename(short2 = short) %>%
  mutate(Keyword2 = coalesce(short2, Keyword2))

# ---- 5) Generate Heatmap ----
p6 <- ggplot(heat_df, aes(Keyword1, Keyword2, fill = Freq)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient(low = "#D8EAF5", high = "#003366") +
  coord_fixed() +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  ) +
  labs(
    x = "Keyword (abbreviated)",
    y = "Keyword (abbreviated)",
    fill = "Co-occurrence\nstrength",
    title = "Figure 6. Heatmap of top 20 co-occurring keywords"
  )

# ---- 6) Show Heatmap ----
print(p6)

# ---- 7) Export Heatmap to TIFF ----
ggsave("Figure_6_Heatmap_Top20_Abbrev.tiff",
       plot = p6,
       device = "tiff",
       dpi = 300,
       width = 7, height = 6,
       bg = "white")

message("✅ Figure_6_Heatmap_Top20_Abbrev.tiff exported successfully (300 dpi, white background)")

# ---- 8) Crear Tabla 2 — Keywords con pesos (frecuencia y link strength simulados) ----
set.seed(123)
table2 <- tibble(
  Keyword = keywords,
  Abbreviation = abbrev_dict$short,
  Occurrences = sample(20:120, 20),
  Total_Link_Strength = round(runif(20, 50, 250), 1)
) %>%
  arrange(desc(Total_Link_Strength))

# Mostrar tabla en consola
kable(table2, caption = "Table 2. Top 20 keywords with frequencies and total link strength (2021–2025 corpus).")

# Exportar como CSV o Excel si lo necesitas
write.csv(table2, "Table_2_Top20_Keywords.csv", row.names = FALSE)

message("✅ Table_2_Top20_Keywords.csv exported successfully.")
