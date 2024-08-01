# Example code to create a forest plot

library(tidyverse)
library(forestploter)
library(grid)

# Create some synthetic outcomes and calculate required statistics

synthetic_outcome_summary <- tibble(
  metric = paste0("metric", 1:10),
  label1 = rep('Control', 10),
  label2 = rep('Treatment', 10),
  mean1 = runif(10, 1, 10),
  sd1 = runif(10, 4, 7),
  ss1 = rep(1000, 10),
  diff = runif(10, -1.5, 1.5),
  sd2 = runif(10, 4, 7),
  ss2 = rep(1000, 10),
  sig = rep(0.05, 10)
) %>%
  rowwise() %>%
  mutate(mean2 = mean1 + diff,
         LCI1 = mean1 - qt(0.975, ss1 - 1) * sd1 / sqrt(ss1),
         UCI1 = mean1 + qt(0.975, ss1 - 1) * sd1 / sqrt(ss1),
         LCI2 = mean2 - qt(0.975, ss1 - 1) * sd2 / sqrt(ss2),
         UCI2 = mean2 + qt(0.975, ss1 - 1) * sd2 / sqrt(ss2),
         se_abs = sqrt(((ss1 - 1) * (sd1 ** 2) + (ss2 - 1) * (sd2 ** 2)) / (ss1 + ss2 - 2)) * sqrt(1 / ss1 + 1 / ss2),
         LCI_abs = mean2 - mean1 - qt(1 - sig/2, ss1 + ss2 - 2) * se_abs,
         UCI_abs = mean2 - mean1 + qt(1 - sig/2, ss1 + ss2 - 2) * se_abs,
         rel_diff = (mean2 - mean1) / mean1,
         se_rel = sqrt(1/(mean1 ** 2) * (sd2 ** 2 / ss2) + (mean2 ** 2) / (mean1 ** 4) * (sd1 ** 2 / ss1)),
         LCI_rel = rel_diff - qt(1 - sig/2, ss1 + ss2 - 2) * se_rel,
         UCI_rel = rel_diff + qt(1 - sig/2, ss1 + ss2 - 2) * se_rel,
         p_value_abs = 2 * pnorm(-abs(diff / se_abs)),
         signif_abs = if_else(p_value_abs <= sig, "significant", "not significant"),
         p_value_rel = 2 * pnorm(-abs(rel_diff / se_rel)),
         signif_rel = if_else(p_value_rel <= sig, "significant", "not significant")
  )

# Create labels

synthetic_outcome_summary <- synthetic_outcome_summary %>%
  mutate(comparison = paste0('Group 1: ', label1, '\nGroup 2: ', label2),
         group_1_summ = paste0('Mean: ', round(mean1, digits = 3),
                               '\n', (1 - sig) * 100,
                               '%CI (', round(LCI1, digits = 3), ', ',
                               round(UCI1, digits = 3), ')'),
         group_2_summ = paste0('Mean: ', round(mean2, digits = 3),
                               '\n', (1 - sig) * 100,
                               '%CI (', round(LCI2, digits = 3), ', ',
                               round(UCI2, digits = 3), ')'),
         abs_diff_summ = paste0('Mean: ', round(diff, digits = 3),
                                '\n', (1 - sig) * 100,
                                '%CI (', round(LCI_abs, digits = 3), ', ',
                                round(UCI_abs, digits = 3), ')'),
         rel_diff_summ = paste0('Mean: ', round(rel_diff, digits = 3) * 100,
                                '%\n', (1 - sig) * 100,
                                '%CI (', round(LCI_rel, digits = 3) * 100, '%, ',
                                round(UCI_rel, digits = 3) * 100, '%)'),
         sig_flag = if_else(signif_rel == "significant", TRUE, FALSE)
  )

sig_positive <- which(synthetic_outcome_summary$LCI_rel > 0)
sig_negative <- which(synthetic_outcome_summary$UCI_rel < 0)

forest_plot_data <- synthetic_outcome_summary %>%
  select(`Metric` = metric,
         `Groups` = comparison,
         `Group 1` = group_1_summ,
         `Group 2` = group_2_summ,
         `Relative Difference` = rel_diff_summ,
         rel_diff,
         LCI_rel,
         UCI_rel) %>%
  mutate(`Relative Difference (%): CI` = paste(rep(" ", 40), collapse = " "))

forest_plot <- forest(
  data = forest_plot_data %>%
    select(`Metric`, `Groups`, `Group 1`, `Group 2`, `Relative Difference`, `Relative Difference (%): CI`),
  est = forest_plot_data$rel_diff * 100,
  lower = forest_plot_data$LCI_rel * 100,
  upper = forest_plot_data$UCI_rel * 100,
  ci_column = 6,
  xref_line = 0,
  arrow_lab = c("Group 1 Greater", "Group 2 Greater")
) %>%
  edit_plot(col = 5, row = sig_negative, gp = gpar(fontface = 'bold', col = 'red')) %>%
  edit_plot(col = 5, row = sig_positive, gp = gpar(fontface = 'bold', col = 'darkgreen')) %>%
  edit_plot(col = 6, row = sig_negative, which = 'ci', gp = gpar(fontface = 'bold', fill = 'red', col = 'red')) %>%
  edit_plot(col = 6, row = sig_positive, which = 'ci', gp = gpar(fontface = 'bold', fill = 'darkgreen', col = 'darkgreen')) %>%
  add_border(part = "header", row = 1, gp = gpar(lwd = 1))
forest_plot


