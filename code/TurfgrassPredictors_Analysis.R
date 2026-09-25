## ---------------------------------------------------------------------------
## TurfgrassPredictors_Analysis.R
##
## Answer key: reproduces "Figure 2. Spearman rank correlations (rho) between
## individual UAV-derived predictors and visual turfgrass quality ratings
## across site-dates for three warm-season turfgrass species" from the
## TurfgrassPredictors dataset.
##
## Input : trials/TurfgrassPredictors/TurfgrassPredictors_VisualRatings.csv
##         columns: Location, Site, Trial_ID, Species, Date, Plot_ID, TQR,
##                  13 RGB_* predictors, 17 MS_* predictors  (see Table 2 for
##                  predictor definitions/formulas)
## Output: a table of mean Spearman rho (predictor x species), grouped by
##         sensor and predictor type, printed to console and written to .csv
## ---------------------------------------------------------------------------

library(tidyverse)

dat <- read_csv(
  "../trials/TurfgrassPredictors/TurfgrassPredictors_VisualRatings.csv",
  col_types = cols(
    Location = col_character(), Site = col_character(), Trial_ID = col_character(),
    Species  = col_character(), Date = col_date(format = "%Y-%m-%d"),
    Plot_ID  = col_character(), TQR = col_integer(),
    .default = col_double()
  )
)

dat <- dat %>%
  mutate(Species = factor(Species, levels = c("StAug", "Berm", "Zoys")))

predictor_cols <- setdiff(names(dat), c("Location", "Site", "Trial_ID", "Species", "Date", "Plot_ID", "TQR"))

## ---------------------------------------------------------------------------
## A "site-date" is one Location + Trial_ID + Species + Date combination (one
## row of Table 1 / one flight). Figure 2's values are the MEAN of the
## within-site-date Spearman correlation (predictor vs TQR) across all of a
## species' site-dates -- NOT a single correlation pooled across all rows.
## ---------------------------------------------------------------------------
site_date_rho <- dat %>%
  group_by(Location, Trial_ID, Species, Date) %>%
  summarise(
    across(all_of(predictor_cols), ~ suppressWarnings(cor(.x, TQR, method = "spearman"))),
    .groups = "drop"
  )

rho_table <- site_date_rho %>%
  group_by(Species) %>%
  summarise(across(all_of(predictor_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-Species, names_to = "Predictor", values_to = "rho") %>%
  pivot_wider(names_from = Species, values_from = rho)

## ---------------------------------------------------------------------------
## Reformat into the sensor / predictor-type grouping used in Figure 2
## ---------------------------------------------------------------------------
predictor_meta <- tribble(
  ~Predictor,                ~Sensor, ~Type,               ~Label,
  "RGB_Blue",                "RGB",   "Mean reflectance",  "Blue",
  "RGB_Green",               "RGB",   "Mean reflectance",  "Green",
  "RGB_Red",                 "RGB",   "Mean reflectance",  "Red",
  "RGB_CV_Blue",             "RGB",   "Color uniformity",  "CV Blue",
  "RGB_CV_Green",            "RGB",   "Color uniformity",  "CV Green",
  "RGB_CV_Red",              "RGB",   "Color uniformity",  "CV Red",
  "RGB_VARI",                "RGB",   "Vegetation index",  "VARI",
  "RGB_NGRDI",                "RGB",   "Vegetation index",  "NGRDI",
  "RGB_GR",                  "RGB",   "Vegetation index",  "GR",
  "RGB_ExG",                 "RGB",   "Vegetation index",  "ExG",
  "RGB_CanopyDensity",       "RGB",   "Structural proxy",  "Canopy density",
  "RGB_LeafTexture",         "RGB",   "Structural proxy",  "Leaf texture",
  "RGB_SurfaceUniformity",   "RGB",   "Structural proxy",  "Surface uniformity",
  "MS_Green",                "MS",    "Mean reflectance",  "Green",
  "MS_Red",                  "MS",    "Mean reflectance",  "Red",
  "MS_RedEdge",              "MS",    "Mean reflectance",  "Red edge",
  "MS_NIR",                  "MS",    "Mean reflectance",  "NIR",
  "MS_CV_Green",             "MS",    "Color uniformity",  "CV Green",
  "MS_CV_Red",               "MS",    "Color uniformity",  "CV Red",
  "MS_CV_RedEdge",           "MS",    "Color uniformity",  "CV Red edge",
  "MS_CV_NIR",               "MS",    "Color uniformity",  "CV NIR",
  "MS_NGRDI",                "MS",    "Vegetation index",  "NGRDI",
  "MS_GR",                   "MS",    "Vegetation index",  "GR",
  "MS_NDVI",                 "MS",    "Vegetation index",  "NDVI",
  "MS_NDRE",                 "MS",    "Vegetation index",  "NDRE",
  "MS_GNDVI",                "MS",    "Vegetation index",  "GNDVI",
  "MS_GCI",                  "MS",    "Vegetation index",  "GCI",
  "MS_CanopyDensity",        "MS",    "Structural proxy",  "Canopy density",
  "MS_LeafTexture",          "MS",    "Structural proxy",  "Leaf texture",
  "MS_SurfaceUniformity",    "MS",    "Structural proxy",  "Surface uniformity",
)

figure2_table <- predictor_meta %>%
  left_join(rho_table, by = "Predictor") %>%
  mutate(across(c(StAug, Berm, Zoys), ~ round(.x, 2))) %>%
  select(Sensor, Type, Label, StAug, Berm, Zoys)

print(figure2_table, n = Inf)

write_csv(figure2_table, "../trials/TurfgrassPredictors/Figure2_PredictorCorrelations.csv")

## ---------------------------------------------------------------------------
## Bonus: heatmap reproducing the look of Figure 2 (red = negative,
## blue = positive Spearman rho), faceted by sensor.
## ---------------------------------------------------------------------------
figure2_table %>%
  mutate(Label = fct_inorder(Label)) %>%
  pivot_longer(c(StAug, Berm, Zoys), names_to = "Species", values_to = "rho") %>%
  mutate(Species = factor(Species, levels = c("StAug", "Berm", "Zoys"))) %>%
  ggplot(aes(x = Species, y = fct_rev(Label), fill = rho)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", rho)), size = 3) +
  facet_grid(Type ~ Sensor, scales = "free_y", space = "free_y") +
  scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac",
                        midpoint = 0, limits = c(-0.8, 0.8)) +
  labs(title = "Spearman rho: UAV predictors vs. visual turfgrass quality rating",
       x = NULL, y = NULL, fill = "Spearman\nrho") +
  theme_minimal() +
  theme(strip.text.y = element_text(angle = 0))
