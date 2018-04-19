# dataset_analysis.R
#
# Script to analyze the P.1203 model scores.
# Authors: Werner Robitza, David Lindegren

library(tidyverse)
library(magrittr)
library(extrafont)
library(broom)
library(ggforce)
library(ggrepel)
library(xtable)

# To get the Libertine font working, copy it to the system fonts directory.
# You then only need to call this once:
# font_import()

loadfonts()

# =============================================================================
# FUNCTIONS

options(scipen=9999)

plot_margin = 0.1
theme_set(
  theme_bw() + 
  theme(
    text = element_text(family = "Times", color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    plot.margin = unit(rep(plot_margin, 4), "cm")
  )
)

save_plot <- function(name, p = last_plot(), width = 3.8, height = 2.2) {
  ggsave(
    plot = p + theme(text = element_text(size = 8)),
    device="pdf",
    filename = paste0("figures/", name, ".pdf"),
    width = width,
    height = height
  )
}

# =============================================================================
# Read data

d = read_csv("../data/subjective_scores/mos.csv") %>%
  separate(pvs_id, into = c("db_id", "src_id", "hrc_id"), remove = FALSE)

d.ratings = read_csv("../data/subjective_scores/ratings.csv") %>%
  separate(pvs_id, into = c("db_id", "src_id", "hrc_id"), remove = FALSE)

d.o46 = read_csv("../data/O46.csv") %>% 
  separate(pvs_id, into = c("db_id", "src_id", "hrc_id"), remove = FALSE)

# =============================================================================
# Mobile vs. PC mapping

d.mobile_vs_pc = d %>% 
  filter(grepl("TR", db_id)) %>% 
  select(db_id, pvs_id, context, mos, ci) %>%
  gather(key, value, mos, ci) %>%
  unite(indicator, context, key) %>%
  spread(indicator, value)

d.mobile_vs_pc %>% 
  ggplot(aes(x = pc_mos, y = mobile_mos)) +
  geom_point(size = 0.3) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw=TRUE)) +
  geom_errorbar(aes(ymin = mobile_mos - pc_ci, ymax = mobile_mos + pc_ci), alpha = 0.2) +
  geom_errorbarh(aes(xmin = pc_mos - mobile_ci, xmax = pc_mos + mobile_ci), alpha = 0.2) +
  coord_cartesian(xlim = c(1, 5), ylim = c(1, 5)) +
  xlab("PC MOS") +
  ylab("Mobile MOS")
save_plot("pc_vs_mobile")

d.mobile_vs_pc %$% cor(mobile_mos, pc_mos)
model.mobile_vs_pc = lm(mobile_mos ~ poly(pc_mos, 2, raw=TRUE), data = d.mobile_vs_pc)
model.mobile_vs_pc %>%
  tidy %>%
  mutate(estimate = round(estimate, 3)) %>% 
  pull(estimate)
model.mobile_vs_pc %>% augment %>% pull(.resid) %>% sqrt() %>% mean(na.rm=TRUE)

# =============================================================================
# MOS vs CI

# General CI information
d %>% pull(ci) %>% summary
aov(formula = ci ~ context * db_id + hrc_id + src_id, data = d) %>% summary

d %>% group_by(context) %>% 
  do(
    tidy(summary(.$ci))
  )

# SOS coefficient  
nls(
  formula = sd^2 ~ a*(-mos^2 + 6*mos - 5),
  data = d %>% filter(context == "pc"),
  start = list(a = 0.20) 
) %>% tidy

d %>% filter(context == "pc") %>% 
  ggplot(aes(x = mos, y = sd, color = db_id)) +
  annotate("polygon",
    x=1.5+0.5*cos(seq(pi,2*pi,length.out=100)),
    y=0+0.5*sin(seq(0,pi,length.out=100)),alpha = .2) + 
  annotate("polygon",
    x=2.5+0.5*cos(seq(pi,2*pi,length.out=100)),
    y=0+0.5*sin(seq(0,pi,length.out=100)),alpha = .2) + 
  annotate("polygon",
    x=3.5+0.5*cos(seq(pi,2*pi,length.out=100)),
    y=0+0.5*sin(seq(0,pi,length.out=100)),alpha = .2) + 
  annotate("polygon",
    x=4.5+0.5*cos(seq(pi,2*pi,length.out=100)),
    y=0+0.5*sin(seq(0,pi,length.out=100)),alpha = .2) + 
  geom_point(
    size = 0.3, alpha = 1, color = "black"
  ) + 
  geom_smooth(
    method = "nls",
    formula = y^2 ~ a*(-x^2 + 6*x - 5),
    method.args = list(
      start = list(a = 0.02)
    ),
    se = FALSE,
    size = 0.5,
    alpha = 0.8
  ) +
  scale_color_brewer(palette = "Set1", name = "Database") +
  coord_cartesian(xlim = c(1, 5),ylim = c(0,1.2)) +
  xlab("MOS") + ylab("StDev")

save_plot("mos_vs_ci")

# =============================================================================
# Model performance

# per-HRC MOS against O.46 unadjusted
d.ratings %>%
  group_by(context, db_id, hrc_id) %>% 
  summarize(
    hrc_mos = mean(rating),
    hrc_ci = qnorm(0.975) * sd(rating) / sqrt(n())
  ) %>%
  left_join(
    d.o46 %>%
      group_by(context, mode, db_id, hrc_id) %>%
      summarize(hrc_O46 = mean(O46))
  ) %>%
  filter(context == "pc") %>%
  filter(mode == 3) %>% 
  ggplot(aes(
    x = hrc_mos,
    y = hrc_O46,
    label = hrc_id
  )) +
  geom_point(size = 0.5) +
  geom_errorbarh(
    aes(x = hrc_mos, xmin = hrc_mos - hrc_ci, xmax = hrc_mos + hrc_ci),
    alpha = 0.2
  ) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  coord_cartesian(xlim = c(1, 5), ylim = c(1, 5)) +
  xlab("HRC MOS") + ylab("Average O.46 per HRC (no linear adj.)") +
  geom_text_repel(
    size = 1.8,
    segment.size = 0.3,
    segment.alpha = 0.7,
    arrow = arrow(length = unit(0.01, 'npc')),
    point.padding = 0.1,
    max.iter = 3e3,
    family = 'Times'
  ) +
  facet_wrap(~db_id)
save_plot("mos_vs_o46-hrcs", width = 8, height = 4.2)

# per-DB adjustment coefficients (if needed)
d %>%
  left_join(d.o46) %>%
  group_by(db_id, context, mode) %>% 
    do(lm(.$mos ~ .$O46, data = .) %>% tidy) %>% 
    select(term, estimate) %>% spread(term, estimate)

# per-DB adjustment performance
d.linear_params = 
  d %>%
  left_join(d.o46) %>%
  group_by(context, db_id, mode) %>%
  do(lm(mos ~ O46, data = .) %>% tidy) %>%
  select(context, db_id, mode, term, estimate) %>%
  spread(term, estimate) %>% 
  rename(param_O46 = O46, param_intercept = `(Intercept)`)

d.linear_params %>%
  group_by(context, db_id) %>%
  summarize(
    mean_intercept = mean(param_intercept),
    mean_slope = mean(param_O46)
  ) %>%
  xtable(
    caption = "Mean intercepts and slopes for linear adjustment per DB.",
    align = rep("r", ncol(.) + 1),
    digits = 3
  ) %>%
  print.xtable(
    booktabs = TRUE,
    include.rownames = FALSE
  )

# linear adjustment parameters
d.linear_adjusted = d %>%
  left_join(d.linear_params) %>%
  left_join(d.o46) %>%
  mutate(
    O46_fitted = pmin(5, pmax(1, O46 * param_O46 + param_intercept))  
  ) %>%
  select(-starts_with("param"))

# summarize into statistics  
d.performance_per_db = d.linear_adjusted %>% 
  mutate(
    error_star = pmax(0, abs(mos - O46_fitted) - ci)
  ) %>%
  group_by(context, mode, db_id) %>% 
  summarize(
    rmse = sqrt(mean((mos - O46_fitted)^2)),
    rmse_star = sqrt(mean(error_star^2)),
    plcc = cor(mos, O46_fitted),
    srocc = cor(mos, O46_fitted, method = "spearman")
  )

# per-database performance
d.performance_per_db %>% 
  xtable(
    caption = "Per-database performance for different contexts and modes.",
    align = rep("r", ncol(.) + 1),
    digits = 3
  ) %>%
  print.xtable(
    booktabs = TRUE,
    include.rownames = FALSE
  )

d.performance_per_db %>%
  group_by(context, mode) %>% 
  summarize(
    mean_rmse = mean(rmse),
    mean_rmse_star = mean(rmse_star),
    mean_plcc = mean(plcc),
    mean_srocc = mean(srocc)
  ) %T>% print %>% 
  group_by(context) %>% 
  summarize(
    mean_rmse = mean(mean_rmse),
    mean_rmse_star = mean(mean_rmse_star),
    mean_plcc = mean(mean_plcc),
    mean_srocc = mean(mean_srocc)
  )

d.performance_per_db %>% 
  group_by(context, mode) %>% 
  summarize(
    min_plcc = min(plcc),
    max_plcc = max(plcc)
  )

d.linear_adjusted %>%
  ggplot(aes(
    x = mos,
    y= O46_fitted
  )) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", size = 0.5) +
  xlab("MOS") + ylab("O.46 (linear adj.)") +
  coord_cartesian(xlim = c(1, 5), ylim = c(1, 5)) +
  facet_grid(context ~ mode)
save_plot("mos_vs_o46-context", width = 8, height = 4)

# =============================================================================
# Some more detailed analyses

# HRCs with a large spread in MOS?
d %>%
  filter(context == "pc") %>% 
  group_by(context, db_id, hrc_id) %>% 
  summarize(
    hrc_mos_spread = abs(max(mos) - min(mos))
  ) %>%
  mutate(
    above_05 = ifelse(hrc_mos_spread >= 0.5, TRUE, FALSE),
    above_10 = ifelse(hrc_mos_spread >= 1, TRUE, FALSE),
    above_2 = ifelse(hrc_mos_spread >= 2, TRUE, FALSE)
  ) %>% 
  group_by(above_05, above_10, above_2) %>% tally()
