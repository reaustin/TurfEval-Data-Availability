# TurfEval Data Availability

Data and code supporting Austin, R. E., Tan, B., Miller, G. L., Carbajal, E. M., & Milla-Lewis, S. R. Pixels, Patterns, and Perception: Toward Automated Turfgrass Quality Assessment in the Breeding and Evaluation Pipeline Using UAV Imagery. The Plant Phenome Journal. In review.

This repository contains the plot-level data and analysis code used to evaluate relationships between UAV-derived image predictors and visual turfgrass quality ratings on the NTEP 1 to 9 scale. It includes the trial and plot boundary geometry, the predictor and rating tables used in the analysis, the code that generates the correlation results, and the R code that fits the random forest models reported in the manuscript.

Raw UAV imagery is not included here because of its size and because the trials include unreleased breeding material. See Data availability below.

## Repository structure

```
code/
  RaterVariability_Analysis.R          # computes Table 3
  TurfgrassPredictors_Analysis.R       # computes Figure 2
  rf_classification.R                  # Random Forest TQ classification pipeline
trials/
  RaterVariability/
    RaterVariability_VisualRatings.csv
  TurfgrassPredictors/
    TurfgrassPredictors_VisualRatings.csv
  shapefiles/
    <Location>_<Trial_ID>.zip          # 8 plot-boundary shapefiles
```

---

## Dataset 1 — `trials/RaterVariability/RaterVariability_VisualRatings.csv`

Data for reproducing **Table 3** (rater agreement and scale use by experience level): 168 plots each rated by 13 independent raters (4 Beginner, 5 Intermediate, 4 Expert).

| Column | Type | Units | Description |
|---|---|---|---|
| `RaterID` | string | — | Unique rater code, `R01`–`R13` |
| `PlotID` | integer | — | Plot number, 1–168 |
| `Level` | string | — | Rater experience level: `Beginner`, `Intermediate`, or `Expert` |
| `VisualRating` | integer | NTEP 1–9 scale | Visual turfgrass quality rating. See manuscript Methods for the NTEP visual rating protocol used by raters. |

---

## Dataset 2 — `trials/TurfgrassPredictors/TurfgrassPredictors_VisualRatings.csv`

Data for reproducing **Figure 2** (Spearman rank correlations between UAV-derived predictors and visual turfgrass quality ratings, by species), using the trial/date/plot structure of **Table 1** and the predictor definitions of **Table 2**. 1,260 rows (168 site-date plots × up to 5 repeated NTEP rating dates), spanning 8 trial instances across 3 species (St. Augustinegrass, bermudagrass, zoysiagrass) at 4 locations.

Full predictor definitions, formulas, and the UAV image-processing methodology (plot delineation, radiometric calibration, SVM canopy classification, Canny edge detection, DSM-derived canopy height) are documented in the **Methods** section of the manuscript; formulas are reproduced below from manuscript Table 2 for convenience.

### Metadata columns

| Column | Type | Units | Description |
|---|---|---|---|
| `Location` | string | — | Trial location (e.g., Arapahoe, NC) |
| `Site` | string | — | Named site at that location (e.g., Neuse River Turf) |
| `Trial_ID` | string | — | Internal trial code (e.g., `23SOFT`, `19BER`); see manuscript Table 1 |
| `Species` | string | — | Turfgrass species: `StAug` (St. Augustinegrass), `Berm` (bermudagrass), `Zoys` (zoysiagrass) |
| `Date` | date (`YYYY-MM-DD`) | — | Rating / imagery collection date |
| `Plot_ID` | string | — | Plot identifier within the trial (unique within `Location` + `Trial_ID`; repeats across a trial's multiple rating dates for the same physical plot) |
| `TQR` | integer | NTEP 1–9 scale | Turf Quality Rating — the visual response variable predicted from UAV imagery |

### RGB sensor predictors (13 columns, prefix `RGB_`)

| Column | Formula / definition | Units | Reference |
|---|---|---|---|
| `RGB_Blue` | Mean plot pixel value, blue band | reflectance fraction (0–1) | — |
| `RGB_Green` | Mean plot pixel value, green band | reflectance fraction (0–1) | — |
| `RGB_Red` | Mean plot pixel value, red band | reflectance fraction (0–1) | — |
| `RGB_CV_Blue` | SD ÷ mean, blue band | unitless ratio | — |
| `RGB_CV_Green` | SD ÷ mean, green band | unitless ratio | — |
| `RGB_CV_Red` | SD ÷ mean, red band | unitless ratio | — |
| `RGB_VARI` | (G − R) / (G + R − B) | unitless index | Gitelson et al., 2002 |
| `RGB_ExG` | 2G − R − B | unitless index | Woebbecke et al., 1995 |
| `RGB_NGRDI` | (G − R) / (G + R) | unitless index | Tucker, 1979 |
| `RGB_GR` | G / R | unitless index | Adamsen et al., 1999 |
| `RGB_CanopyDensity` | Percent green cover from SVM binary classification | percent (0–100) | — |
| `RGB_LeafTexture` | Proportion of Canny edge pixels to total plot pixels | proportion (0–1) | Canny, 1986 |
| `RGB_SurfaceUniformity` | SD of canopy height ÷ mean canopy height, per plot | unitless ratio | — |

### Multispectral (MS) sensor predictors (17 columns, prefix `MS_`)

| Column | Formula / definition | Units | Reference |
|---|---|---|---|
| `MS_Green` | Mean plot pixel value, green band | reflectance fraction (0–1) | — |
| `MS_Red` | Mean plot pixel value, red band | reflectance fraction (0–1) | — |
| `MS_RedEdge` | Mean plot pixel value, red-edge band | reflectance fraction (0–1) | — |
| `MS_NIR` | Mean plot pixel value, NIR band | reflectance fraction (0–1) | — |
| `MS_CV_Green` | SD ÷ mean, green band | unitless ratio | — |
| `MS_CV_Red` | SD ÷ mean, red band | unitless ratio | — |
| `MS_CV_RedEdge` | SD ÷ mean, red-edge band | unitless ratio | — |
| `MS_CV_NIR` | SD ÷ mean, NIR band | unitless ratio | — |
| `MS_NGRDI` | (G − R) / (G + R) | unitless index | Tucker, 1979 |
| `MS_GR` | G / R | unitless index | Adamsen et al., 1999 |
| `MS_NDVI` | (NIR − R) / (NIR + R) | unitless index | Rouse et al., 1974 |
| `MS_NDRE` | (NIR − RE) / (NIR + RE) | unitless index | Barnes et al., 2000 |
| `MS_GNDVI` | (NIR − G) / (NIR + G) | unitless index | Gitelson et al., 1996 |
| `MS_GCI` | (NIR / G) − 1 | unitless index | Gitelson et al., 2003 |
| `MS_CanopyDensity` | Percent green cover from SVM binary classification | percent (0–100) | — |
| `MS_LeafTexture` | Proportion of Canny edge pixels to total plot pixels | proportion (0–1) | Canny, 1986 |
| `MS_SurfaceUniformity` | SD of canopy height ÷ mean canopy height, per plot | unitless ratio | — |

---

## Shapefiles — `trials/shapefiles/*.zip`

Real plot-boundary polygons for 8 trial instances, sourced from the underlying GIS layers and **stripped of all rating/quality attributes** (visual/disease/color/uniformity ratings and any other date-stamped assessment columns were removed) **and of entry/cultivar identity** (`Entry`/`entry`, `Cultivar_N`, `germ`, `tmt`); only plot-identity and layout fields remain. All files share CRS **NAD83 / North Carolina State Plane (US feet), EPSG:2264**.

| File | Location | Trial_ID | Species | Plots | Source date | Retained fields |
|---|---|---|---|---|---|---|
| `Arapahoe_23SOFT.zip` | Arapahoe, NC | 23SOFT | StAug | 60 | 2024-08-16 | label, label_id, row, col, ID, Loc, Rep, Trial_Numb |
| `Burgaw_23SOFT.zip` | Burgaw, NC | 23SOFT | StAug | 57† | 2024-09-26 | label, label_id, Columns, Rows, ID, Loc, Colomns, Rows_1, ID_1, ID_X, ID_Y, Rep |
| `Burgaw_23ZOFT.zip` | Burgaw, NC | 23ZOFT | Zoys | 48 | 2024-09-26 | label, label_id, Columns, Rows, ID, Location, Columns_1, Rows_1, ID_1, ID_X, ID_Y |
| `EagleSprings_23SOFT.zip` | Eagle Springs, NC | 23SOFT | StAug | 60 | 2024-10-08 | ID |
| `EagleSprings_23ZOFT.zip` | Eagle Springs, NC | 23ZOFT | Zoys | 48 | 2024-10-08 | ID |
| `Raleigh_23SAUG.zip` | Raleigh, NC | 23SAUG | StAug | 60 | 2024-10-04 | label, label_id, ID, columns, rows, Species, Loc, ID_1, ID_X, ID_Y, columns_1, rows_1, Plot, rep |
| `Raleigh_19BER.zip` | Raleigh, NC | 19BER | Berm | 111 | 2024-09-30 | PlotId, rep, ID |
| `Raleigh_19ZOY.zip` | Raleigh, NC | 19ZOY | Zoys | 117 | 2024-09-30 | PlotId, rep, ID |

† The source shapefile for `Burgaw_23SOFT` contains 57 plot polygons, not the 60 reported elsewhere for this trial — a discrepancy in the underlying GIS data that has not been reconciled.

---

## Code — `code/`

| Script | Purpose |
|---|---|
| `RaterVariability_Analysis.R` | Reads Dataset 1 and computes Table 3 (mean rating, mean per-plot range, mean pairwise RMSE, offset-removed pairwise RMSE, mean pairwise Spearman ρ, by rater experience level). |
| `TurfgrassPredictors_Analysis.R` | Reads Dataset 2 and computes Figure 2 (per-site-date Spearman ρ between each predictor and TQR, averaged by species). |
| `rf_classification.R` | Random Forest **ordinal classification** of TQ (1–9) from UAV predictors, with within-fold SMOTE class balancing, permutation variable importance, and both a random 80/20 split and a site-date holdout evaluation strategy (see manuscript Methods §5b). **Before running:** set the `project_dir` variable near the top of the script to your local path to the `TurfCenter/locations/All` analysis project folder — no input path is hardcoded. |

## Data availability

The plot-level data and analysis code supporting this study are openly available at https://github.com/reaustin/TurfEval-Data-Availability. Plot-boundary shapefiles are provided with all rating and entry identity attributes removed.

Raw UAV imagery is not deposited. The image datasets are large, and the trials include unreleased breeding material from the NC State turfgrass breeding program. Raw imagery is available from the corresponding author on reasonable request, subject to agreement with the breeding program.

## License

Code in `code/` is released under the MIT License. Data in `trials/` is released under CC BY 4.0. See [LICENSE](LICENSE) and [LICENSE-DATA](LICENSE-DATA).

## Citation

If you use these materials, please cite:

> Austin, R. E., Tan, B., Miller, G. L., Carbajal, E. M., & Milla-Lewis, S. R. Pixels, Patterns, and Perception: Toward Automated Turfgrass Quality Assessment in the Breeding and Evaluation Pipeline Using UAV Imagery. *The Plant Phenome Journal*. In review.
