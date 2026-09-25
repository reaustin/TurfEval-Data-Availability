## ---------------------------------------------------------------------------
## RaterVariability_Analysis.R
##
## Answer key: reproduces "Table 3. Rater agreement and scale use by
## experience level" from the RaterVariability visual-rating dataset.
##
## Input : trials/RaterVariability/RaterVariability_VisualRatings.csv
##         columns: RaterID, PlotID, Level, VisualRating
## Output: table3 data frame printed to console (and written to .csv)
## ---------------------------------------------------------------------------

library(tidyverse)

ratings <- read_csv(
  "../trials/RaterVariability/RaterVariability_VisualRatings.csv",
  col_types = cols(
    RaterID      = col_character(),
    PlotID       = col_integer(),
    Level        = col_character(),
    VisualRating = col_double()
  )
)

ratings <- ratings %>%
  mutate(Level = factor(Level, levels = c("Beginner", "Intermediate", "Expert")))

## ---------------------------------------------------------------------------
## Helper: compute the five Table 3 statistics for one set of raters
##   - Mean rating              : average of every rating in the set
##   - Mean per-plot range      : max - min across raters, averaged over plots
##   - Mean pairwise RMSE       : RMSE between each pair of raters' raw ratings,
##                                 averaged over all pairs
##   - Offset-removed pairwise
##       RMSE                   : same, but each rater's ratings are first
##                                 centered on that rater's own mean, so
##                                 systematic differences in scale placement
##                                 (one rater running high/low) are excluded
##   - Mean pairwise rho        : Spearman rank correlation between each pair
##                                 of raters, averaged over all pairs
## Pairwise metrics are averaged over rater PAIRS, not inflated by group size.
## ---------------------------------------------------------------------------
summarize_raters <- function(df) {

  wide <- df %>%
    select(RaterID, PlotID, VisualRating) %>%
    pivot_wider(names_from = RaterID, values_from = VisualRating) %>%
    arrange(PlotID)

  rater_cols <- setdiff(names(wide), "PlotID")
  mat <- as.matrix(wide[, rater_cols])

  mean_rating <- mean(mat, na.rm = TRUE)

  plot_ranges <- apply(mat, 1, function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  mean_range  <- mean(plot_ranges, na.rm = TRUE)

  rater_pairs <- combn(rater_cols, 2, simplify = FALSE)

  pairwise <- map_dfr(rater_pairs, function(pair) {
    x <- mat[, pair[1]]
    y <- mat[, pair[2]]
    keep <- complete.cases(x, y)
    x <- x[keep]; y <- y[keep]

    rmse        <- sqrt(mean((x - y)^2))
    x_centered  <- x - mean(x)
    y_centered  <- y - mean(y)
    rmse_offset <- sqrt(mean((x_centered - y_centered)^2))
    rho         <- cor(x, y, method = "spearman")

    tibble(rmse = rmse, rmse_offset = rmse_offset, rho = rho)
  })

  tibble(
    `Raters (n)`                     = length(rater_cols),
    `Mean rating`                    = mean_rating,
    `Mean per-plot range`            = mean_range,
    `Mean pairwise RMSE`             = mean(pairwise$rmse),
    `Offset-removed pairwise RMSE`   = mean(pairwise$rmse_offset),
    `Mean pairwise rho`              = mean(pairwise$rho)
  )
}

## ---------------------------------------------------------------------------
## Build Table 3: one row per experience level, plus an "All Raters" row
## computed across all 13 raters together (not a simple average of the rows
## above -- pairwise metrics for "All Raters" include every rater PAIR,
## including pairs that span two different experience levels).
## ---------------------------------------------------------------------------
by_level <- ratings %>%
  group_split(Level) %>%
  map_dfr(~ summarize_raters(.x) %>% mutate(Level = as.character(unique(.x$Level)), .before = 1))

all_raters <- summarize_raters(ratings) %>%
  mutate(Level = "All Raters", .before = 1)

table3 <- bind_rows(by_level, all_raters) %>%
  mutate(Level = factor(Level, levels = c("Beginner", "Intermediate", "Expert", "All Raters"))) %>%
  arrange(Level) %>%
  mutate(across(
    c(`Mean rating`, `Mean per-plot range`, `Mean pairwise RMSE`,
      `Offset-removed pairwise RMSE`, `Mean pairwise rho`),
    ~ round(.x, 2)
  ))

print(table3)

write_csv(table3, "../trials/RaterVariability/Table3_RaterAgreement.csv")

## ---------------------------------------------------------------------------
## Bonus: visualize how each rater used the 1-9 scale, grouped by experience
## level -- motivates why the summary statistics above differ by group.
## ---------------------------------------------------------------------------
ratings %>%
  ggplot(aes(x = RaterID, y = VisualRating, fill = Level)) +
  geom_boxplot() +
  facet_wrap(~ Level, scales = "free_x") +
  labs(
    title = "Distribution of visual ratings by rater and experience level",
    x = "Rater", y = "Visual rating (1-9 NTEP scale)"
  ) +
  theme_bw() +
  theme(legend.position = "none")
