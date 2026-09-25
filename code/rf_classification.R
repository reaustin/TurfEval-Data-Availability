###############################################################################
# Random Forest — ORDINAL CLASSIFICATION of turfgrass quality (TQ, 1–9)
#
# Converted from the regression version (model_num). Substantive changes are
# tagged [FIX n] inline and summarized at the top of each block. The pooled-
# per-sensor design is preserved: run once with sensor = 'M3M_RGB' and once
# with sensor = 'M3M_MS' to get the two panels that parallel Figure Y.
#
# Things that need YOUR confirmation are tagged [CONFIRM] — don't trust the
# defaults blindly; they are placeholders chosen to be safe, not correct-by-fiat.
###############################################################################

library(tidyverse)
library(tidyr)
library(dplyr)
library(randomForest)
library(caret)
library(ggplot2)
library(stringr)
library(themis)     # [FIX 1] within-fold SMOTE (DMwR is archived on CRAN; see below)
library(recipes)    # [FIX 1] backend for themis::step_smote

###############################################################################
# INPUT/OUTPUT LOCATION — SET THIS BEFORE RUNNING
#
# Point this at your local copy of the "All" analysis project folder (the one
# containing Analysis/R/tables/... and Analysis/R/figures/...). Every path
# below is derived from it, so nothing else in this block needs to change.
###############################################################################
project_dir <- "SET/ME/TO/YOUR/TurfCenter/locations/All"   # <-- set this manually

if (!dir.exists(project_dir)) {
  stop("project_dir does not exist -- set it to your 'TurfCenter/locations/All' folder before running.")
}

# Set input parameters: P1, M3M_MS, M3M_RGB, AltumPT, ALL
sensor     <- 'M3M_RGB'
base_dir   <- paste0(project_dir, '/Analysis/R/tables/All/MasterTable/')
data_dir   <- paste0(project_dir, '/Analysis/R/tables/', sensor, '/')
outfig     <- paste0(project_dir, '/Analysis/R/figures/', sensor, '/')
parameters <- paste0(project_dir, '/Analysis/R/tables/supporting/ParameterLookup_', sensor, '.csv')
trial_data <- paste0(project_dir, '/Analysis/R/tables/supporting/TrialInfo.csv')

#### Read in the data
filename   <- paste(c(sensor, 'MasterTable.csv'), collapse = "_")
df         <- read.csv(paste0(base_dir, filename), header = TRUE, sep = ",")
param      <- read.csv(parameters, header = TRUE, sep = ",")
trial_info <- read.csv(trial_data, header = TRUE, sep = ",")


### select out the parameters/trials of interest and clean the data
data <- df %>%
  mutate(plot = paste0(sensor, '-', trial, '-', date, '-', ID)) %>%
  select(c('TQ', 'plot', param$parameter)) %>%
  filter(complete.cases(.)) %>%
  select(-dsm_mean)                 # drop dsm_mean (surface uniformity is the normalized DSM std dev, kept)


###############################################################################
# SPECIES LOOKUP + TRIAL EXCLUSION + JOIN QA
#
# This master table contains exactly six trial codes:
#   23SOFT, 23SAUG -> St. Augustinegrass ; 19ZOY, 23ZOFT -> zoysiagrass ;
#   19BER -> bermudagrass ; 21CAT -> centipedegrass (EXCLUDED).
#
# [EXCLUDE] Exclusion is by TRIAL CODE, not species name, so a deliberately
#   dropped trial of a KEPT species (e.g. 23BGN bermuda, at a different location
#   with different sensors — not in this file, but listed for clarity) can be
#   removed without dropping the species. Only 21CAT is present-and-excluded here.
#
# [JOIN QA] 19BER was missing from the supplied lookup (the lookup served a
#   broader study set); it is added below. Any trial code in the data still
#   absent from the lookup is reported as a tripwire for future join failures.
###############################################################################

exclude_trials <- c("21CAT", "23BGN")   # centipede; + 23BGN if it ever appears

# species lookup, house-style names; 19BER added; whitespace/spelling normalized
species_lut <- trial_info %>%
  mutate(trial   = trimws(trial),
         species = recode(trimws(species),
                          "St. Augustine" = "St. Augustinegrass",
                          "Bermuda"       = "bermudagrass",
                          "Zoyzia"        = "zoysiagrass",
                          "Centipede"     = "centipedegrass",
                          .default = NA_character_)) %>%
  bind_rows(tibble(trial = "19BER", species = "bermudagrass")) %>%
  distinct(trial, .keep_all = TRUE)

# parse trial + date out of plot (plot = sensor-trial-date-ID; sensor has no '-')
data <- data %>%
  mutate(.parts    = str_split(plot, "-"),
         trial     = map_chr(.parts, 2),
         date      = map_chr(.parts, 3),
         site_date = paste0(trial, "-", date)) %>%
  select(-.parts) %>%
  left_join(species_lut, by = "trial")

# JOIN QA tripwire: trial codes with no species match (excluding known-excluded)
unmatched <- data %>%
  filter(is.na(species), !(trial %in% exclude_trials)) %>%
  distinct(trial)
if (nrow(unmatched) > 0) {
  message("[JOIN QA] UNEXPECTED unmatched trial codes (will be dropped): ",
          paste(unmatched$trial, collapse = ", "))
} else {
  message("[JOIN QA] All non-excluded trial codes matched the lookup.")
}

# EXCLUSION (before the split): drop excluded trials + any unmatched
n_before <- nrow(data)
data <- data %>% filter(!is.na(species), !(trial %in% exclude_trials))
message("[EXCLUDE QA] Rows dropped (", paste(exclude_trials, collapse = "/"),
        " + unmatched): ", n_before - nrow(data), " ; remaining: ", nrow(data))

# site-date counts per species (cross-check vs brief: StA 8 / bermudagrass 3 / zoysiagrass 5)
print(data %>% distinct(species, site_date) %>% count(species, name = "n_sitedates"))


###############################################################################
# [FIX 2] CLASSIFICATION OUTCOME
# TQ was numeric -> the forest was doing regression (importance = %IncMSE, and
# accuracy/F1/adjacent-accuracy are undefined). Coerce TQ to an ordered set of
# CLASS labels. Labels are made R-safe ("Q2","Q3",...) so caret doesn't choke;
# a helper maps them back to integers for ordinal RMSE / adjacent accuracy.
###############################################################################

# RARE-CLASS HANDLING (decision made; MUST be reflected in Methods §5b).
# Q1 had only 3 plots in trainData (~2 per CV fold) — too few for SMOTE at any
# usable neighbor count, and too few to evaluate as its own class. Q1 is
# collapsed into Q2, forming a "<=2 / very poor" floor class. Applied to TQ
# BEFORE the split and identically for BOTH sensors, so the RGB and MS models
# share the same class set and the importance panels stay comparable.
# Methods note to add: rating-1 plots (n=3) were merged with rating 2 prior to
# modeling; the operational scale (6+ = acceptable) makes the 1-vs-2 distinction
# immaterial, so the bottom class is reported as <=2.
data$TQ <- ifelse(data$TQ == 1, 2, data$TQ)

lvls <- sort(unique(data$TQ))
data$TQ_cls <- factor(paste0("Q", data$TQ), levels = paste0("Q", lvls))

# map class label -> integer rating (for ordinal-as-numeric RMSE & adjacent acc)
to_int <- function(f) as.integer(sub("Q", "", as.character(f)))


### add a random variable to help identify feature importance
# [CONFIRM] uniform(1,100) is fine as a permutation-importance noise floor
# (importance is scale-free), but its range matches nothing else. Harmless;
# noting it so a reviewer asking "why uniform?" has an answer.
set.seed(132)
data$random <- runif(n = nrow(data), min = 1, max = 100)


### STEP 2.5: repeats and folds
num_repeats = 3
num_folds   = 5


###############################################################################
# [FIX 3] 80/20 SPLIT (was p = 0.7; methods §5b say stratified 80/20)
# [FIX 4] With TQ_cls a FACTOR, createDataPartition now stratifies on the actual
#         rating CLASSES (methods: "preserve the proportional representation of
#         rating classes"), not numeric quartiles as it did on numeric TQ.
###############################################################################
set.seed(132)
trainIndex <- createDataPartition(data$TQ_cls, p = 0.8, list = FALSE)
trainData  <- data[trainIndex, ]
testData   <- data[-trainIndex, ]


###############################################################################
# [FIX 1] SMOTE — actually applied now, INSIDE each CV fold.
#
# The original wiring did nothing: preProcessOptions needs a matching
# `preProcess` arg (absent); caret SMOTE goes through trControl$sampling (absent);
# and SMOTE() came from DMwR (not loaded, and archived on CRAN). caret's built-in
# sampling = "smote" still depends on DMwR, so this uses a themis-backed custom
# sampling list instead — applied within each resample fold, which is what the
# methods claim ("within training folds").
#
# [CONFIRM] If you have DMwR installed and want the manual perc.over/perc.under
# ratios from your old smote_func, that's a different spec — tell me and I'll
# wire it. Default themis SMOTE balances all classes to the majority.
###############################################################################
smote_sampling <- list(
  name  = "SMOTE (themis, within-fold)",
  func  = function(x, y) {
    df_in <- data.frame(x, .outcome = y, check.names = FALSE)
    rec <- recipe(.outcome ~ ., data = df_in) %>%
      step_smote(.outcome, neighbors = 3) %>%   # neighbors=3 (needs >=4/fold); the
                                                # collapsed Q2 floor (n=14 -> ~11/fold) clears it
      prep(retain = TRUE)
    out <- juice(rec)
    list(x = out[, setdiff(names(out), ".outcome"), drop = FALSE],
         y = out$.outcome)
  },
  first = TRUE                       # SMOTE before any other preprocessing
)


### STEP 2.75: fixed seeds for reproducibility (one vector per fold + final)
# tuneLength = 3 -> 3 mtry candidates -> each fold seed needs length 3.
seeds <- vector(mode = "list", length = num_repeats * num_folds + 1)
for (i in seq_len(num_repeats * num_folds)) seeds[[i]] <- c(77, 151, 459)
seeds[[num_repeats * num_folds + 1]] <- 27


###############################################################################
# STEP 3: train control — classification, SMOTE within folds
###############################################################################
control_cat <- trainControl(
  method          = "repeatedcv",
  number          = num_folds,
  repeats         = num_repeats,
  classProbs      = FALSE,              # not needed for accuracy/F1; keep off
  summaryFunction = defaultSummary,     # Accuracy/Kappa for multiclass
  sampling        = smote_sampling,     # [FIX 1] SMOTE lives HERE
  savePredictions = "final",
  seeds           = seeds
)


###############################################################################
# STEP 4: train the CLASSIFICATION forest
# [FIX 2] outcome is TQ_cls (factor); [FIX] importance = TRUE so permutation
#         importance (MeanDecreaseAccuracy) is actually computed and stored.
#         Drop plot AND the numeric TQ from the model frame.
###############################################################################
model_cat <- train(
  TQ_cls ~ .,
  data      = trainData[, !(names(trainData) %in%
                            c("plot", "TQ", "species", "trial", "date", "site_date"))],
  method    = "rf",
  metric    = "Accuracy",
  trControl = control_cat,
  tuneLength = 3,
  importance = TRUE
)

print(model_cat)
model_cat$bestTune$mtry

###############################################################################
# STEP 5: PERMUTATION IMPORTANCE — pulled from the fitted forest, NOT varImp()
#
# varImp() would (a) read per-class columns and (b) min-max rescale to 0–100,
# pinning the least-important predictor at 0 by construction — which would make
# the random baseline land at exactly 0 as an ARTIFACT, not as evidence.
# importance(type = 1, scale = FALSE) gives the raw mean decrease in accuracy
# (the overall column), so the random predictor floats to its true noise level.
###############################################################################
imp_raw <- randomForest::importance(model_cat$finalModel, type = 1, scale = FALSE)
imp_df  <- data.frame(
  Variable = rownames(imp_raw),
  MDA      = imp_raw[, "MeanDecreaseAccuracy"] * 100   # percentage points
)
imp_df <- imp_df[order(imp_df$MDA, decreasing = TRUE), ]
print(imp_df)   # <- QA this: the 'random' row should sit near the FLOOR, not mid-pack


###############################################################################
# STEP 6: TEST-SET METRICS (the 80/20 "split" picture for the performance table)
#
# Per-species rows + a POOLED Overall row. Overall is computed on the full test
# set, NOT averaged across species (averaging 8/3/5-weighted species does not
# recover pooled accuracy). Metrics: overall accuracy, macro-F1, adjacent-class
# accuracy (within 1 and within 2), and ordinal-as-numeric RMSE in rating units.
#
# NOTE: the model scale is 2–9 (Q1 collapsed into Q2), so RMSE/adjacent accuracy
# are on the 2–9 scale. State this in the table caption.
#
# macro-F1 is computed by hand (averaged over classes PRESENT in the actuals for
# that subset), so the convention is explicit and reproduces on species subsets
# where some rating classes are absent.
###############################################################################

# ordinal-classification metric helper (works on integer ratings)
ord_metrics <- function(act_int, pred_int) {
  classes <- sort(unique(act_int))                 # classes present in actuals
  f1 <- sapply(classes, function(c) {
    tp <- sum(pred_int == c & act_int == c)
    fp <- sum(pred_int == c & act_int != c)
    fn <- sum(pred_int != c & act_int == c)
    prec <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
    rec  <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
    if (is.na(prec) || is.na(rec) || (prec + rec) == 0) 0 else 2 * prec * rec / (prec + rec)
  })
  data.frame(
    n_plots    = length(act_int),
    accuracy   = mean(pred_int == act_int),
    macro_f1   = mean(f1),
    adjacent_1 = mean(abs(pred_int - act_int) <= 1),
    adjacent_2 = mean(abs(pred_int - act_int) <= 2),
    mse        = mean((pred_int - act_int)^2),
    rmse       = sqrt(mean((pred_int - act_int)^2))   # rating units, 2–9 scale
  )
}

# assemble test-set results with species attached
test_res <- testData %>%
  mutate(act_int  = to_int(TQ_cls),
         pred_int = to_int(predict(model_cat, newdata = .))) %>%
  select(species, site_date, act_int, pred_int)

# site-date n per species (from full data, for the table's n column)
sd_counts <- data %>% distinct(species, site_date) %>% count(species, name = "n_sitedates")

# per-species rows
by_species <- test_res %>%
  group_by(species) %>%
  group_modify(~ ord_metrics(.x$act_int, .x$pred_int)) %>%
  ungroup() %>%
  left_join(sd_counts, by = "species")

# pooled Overall row (all species together — NOT an average)
overall <- ord_metrics(test_res$act_int, test_res$pred_int) %>%
  mutate(species = "Overall",
         n_sitedates = sum(sd_counts$n_sitedates))

# final table for this sensor
perf_tbl <- bind_rows(by_species, overall) %>%
  mutate(sensor = sensor, strategy = "split") %>%
  select(species, sensor, strategy, n_sitedates, n_plots,
         accuracy, macro_f1, adjacent_1, adjacent_2, mse, rmse)

print(perf_tbl)

# QA reminders to eyeball on the printout:
#  - RMSE should equal sqrt(MSE) exactly (both columns carried for the check)
#  - n_sitedates should read StA 8 / bermudagrass 3 / zoysiagrass 5
#  - Overall accuracy should NOT equal the mean of the three species accuracies
write.csv(perf_tbl, paste0(outfig, sensor, "_rf_performance_split.csv"), row.names = FALSE)


###############################################################################
# STEP 7: importance figure data — merge labels/groups for the Figure-Y parallel
# (production build is the separate matplotlib/ggplot spec; this just assembles
#  clean, labeled data with the random baseline flagged.)
###############################################################################
order_max <- max(param$order)
param_ext <- rbind(param,
                   data.frame(parameter = "random", label = "Random",
                              order = order_max + 1)[names(param)],
                   stringsAsFactors = FALSE)

var_imp <- merge(imp_df, param_ext, by.x = "Variable", by.y = "parameter", all.x = TRUE)
var_imp$baseline <- var_imp$Variable == "random"
var_imp <- var_imp[order(var_imp$MDA, decreasing = FALSE), ]
var_imp$label <- factor(var_imp$label, levels = var_imp$label)

# quick prototype (NOT the production figure); baseline drawn as a reference line
ggplot(var_imp, aes(x = label, y = MDA, fill = baseline)) +
  geom_col(color = "black") +
  geom_hline(yintercept = var_imp$MDA[var_imp$baseline], linetype = "dashed") +
  coord_flip() +
  labs(x = "Predictor", y = "Mean decrease in accuracy (percentage points)") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 7),
        axis.title  = element_text(size = 10))

# write importance data for the production figure build
write.csv(var_imp[order(var_imp$MDA, decreasing = TRUE), ],
          paste0(outfig, sensor, "_rf_importance.csv"), row.names = FALSE)


###############################################################################
# STEP 8: SITE-DATE HOLDOUT (generalizability stress-test — beat 3 data)
#
# Methods §5b: trained on all site-dates except one randomly selected site-date
# PER SPECIES, evaluated on the excluded ones. Pooled design (consistent with the
# split): three site-dates held out (one per species), ONE model trained on the
# pooled remainder, evaluated on the held-out dates broken out by species + a
# pooled Overall.
#
# Deliberately reuses control_cat, smote_sampling, to_int, and ord_metrics from
# the split so SMOTE/seed/metric definitions CANNOT drift between the two
# strategies — that shared machinery is what makes split-vs-holdout comparable.
# Only difference: importance = FALSE (holdout doesn't feed the figure).
#
# Metrics are identical to the split (same 5 reported + QA columns) so the
# degradation gap (split - holdout) is a clean subtraction. Expectation: holdout
# BELOW split, with the largest gap for bermuda/zoysia (thin data) and the
# smallest for St. Augustinegrass. Holdout >= split would signal leakage.
#
# [CONFIRM] Single fixed random holdout (methods-faithful). It rests on ONE draw,
#   so for bermuda (trains on just 2 site-dates) the number is fragile by design
#   — that fragility is the reported caveat, not a bug. To SEE the draw variance
#   before trusting it, optionally loop the seed; not reported, just inspected.
#
# SMOTE note: pooled training drops only 3 of 16 site-dates, so the training
#   class distribution stays close to the full data (which trained fine at
#   neighbors = 3); a rare-class SMOTE error is unlikely here. If bermuda's
#   thinner contribution does trip it, that is the thin-data reality surfacing.
###############################################################################

set.seed(456)   # fixed -> reproducible holdout selection
held_out <- data %>%
  distinct(species, site_date) %>%
  group_by(species) %>%
  slice_sample(n = 1) %>%
  ungroup()
message("[HOLDOUT] Site-date held out per species:")
print(held_out)

ho_train <- data %>% filter(!(site_date %in% held_out$site_date))
ho_test  <- data %>% filter( site_date %in% held_out$site_date)

# pooled holdout model — SAME control/SMOTE/tuneLength as the split
model_ho <- train(
  TQ_cls ~ .,
  data      = ho_train[, !(names(ho_train) %in%
                           c("plot", "TQ", "species", "trial", "date", "site_date"))],
  method    = "rf",
  metric    = "Accuracy",
  trControl = control_cat,
  tuneLength = 3,
  importance = FALSE
)

# evaluate on the held-out site-dates
ho_res <- ho_test %>%
  mutate(act_int  = to_int(TQ_cls),
         pred_int = to_int(predict(model_ho, newdata = .))) %>%
  select(species, site_date, act_int, pred_int)

# site-dates REMAINING in training per species (fragility: bermuda -> 2)
train_sd <- ho_train %>% distinct(species, site_date) %>%
  count(species, name = "n_train_sitedates")

ho_by_species <- ho_res %>%
  group_by(species) %>%
  group_modify(~ ord_metrics(.x$act_int, .x$pred_int)) %>%
  ungroup() %>%
  left_join(held_out, by = "species") %>%
  left_join(train_sd, by = "species") %>%
  rename(heldout_site_date = site_date)

# pooled Overall on the union of held-out plots (NOT an average)
ho_overall <- ord_metrics(ho_res$act_int, ho_res$pred_int) %>%
  mutate(species = "Overall",
         heldout_site_date = paste(held_out$site_date, collapse = " + "),
         n_train_sitedates = NA_integer_)

ho_tbl <- bind_rows(ho_by_species, ho_overall) %>%
  mutate(sensor = sensor, strategy = "holdout") %>%
  select(species, sensor, strategy, heldout_site_date, n_train_sitedates, n_plots,
         accuracy, macro_f1, adjacent_1, adjacent_2, mse, rmse)

print(ho_tbl)

# QA reminders to eyeball:
#  - RMSE = sqrt(MSE) on every row (same check as split)
#  - holdout accuracy should be <= split accuracy (the degradation gap)
#  - n_train_sitedates: StA 7 / bermudagrass 2 / zoysiagrass 4
#  - degradation gap (split - holdout) expected largest for bermuda/zoysia
write.csv(ho_tbl, paste0(outfig, sensor, "_rf_performance_holdout.csv"), row.names = FALSE)
