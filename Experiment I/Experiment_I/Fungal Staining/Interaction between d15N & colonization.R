setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment I/Experiment_I/UC davis stable isotope")

library(tidyverse); library(readxl)
library(lme4); library(car); library(emmeans); library(patchwork)

# Reading stable isotope data
iso <- read_excel("stable_isotope.xlsx") |>
  mutate(Sample = str_squish(Sample)) |>
  select(Sample, d15N)

setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment I/Experiment_I/Fungal Staining/")

myco_I <- read_excel("Fungal_quantification.xlsx")  
meta_I <- read_excel("metadata.xlsx")


# Reading colonization data
myco_pct <- myco_I |>
  mutate(pct_total      = 100 * (Total - None)         / Total,
         pct_hyphae     = 100 * (hyphae + `Vescicle+arbuscules+ hyphae`)     / Total,
         pct_arbuscules = 100 * (arbuscules + `Vescicle+arbuscules+ hyphae`) / Total,
         pct_vesicles   = 100 * (vesicles + `Vescicle+arbuscules+ hyphae`)   / Total) |>
  select(Sample, pct_total, pct_hyphae, pct_arbuscules, pct_vesicles)

# Joing the colonization and d15N data
d15_dat <- myco_pct |>
  left_join(meta_I, by = "Sample") |>
  left_join(iso,    by = "Sample") |>
  mutate(Block_ID = factor(Block_ID))

# ---- Model: separate colonization slope for High N and Low N --------

library(glmmTMB)
library(car)
library(emmeans)
library(ggeffects)
library(dplyr)
library(ggplot2)
library(patchwork)

# ---- Data -------------------------------------------------------------
d15_dat <- d15_dat |>
  mutate(across(c(Nutrient_treatment, Water_treatment, AMF, Block_ID), factor))

amf_dat <- filter(d15_dat, AMF == "Yes")   # match your AMF level label

# ---- Model and tests --------------------------------------------------
m_d15 <- glmmTMB(
  d15N ~ (AMF + pct_total) * Nutrient_treatment * Water_treatment + (1 | Block_ID),
  family = gaussian(),
  data   = d15_dat
)

aov_tab <- Anova(m_d15, type = "II")
aov_tab

fmt_p <- function(p) {
  ifelse(p < 0.001, "P < 0.001",
         paste0("P = ", formatC(p, format = "f", digits = 3)))
}

p_amf <- aov_tab["AMF", "Pr(>Chisq)"]
p_n   <- aov_tab["Nutrient_treatment", "Pr(>Chisq)"]
p_int <- aov_tab["pct_total:Nutrient_treatment", "Pr(>Chisq)"]

# Slopes of d15N vs colonization at each N level (AMF plants, averaged over water)
tr <- emtrends(m_d15, ~ Nutrient_treatment, var = "pct_total",
               at = list(AMF = "Yes")) |>
  test() |>
  as.data.frame()


# Plot settings 
n_cols   <- c("#0072B2", "#E69F00") 
n_shapes <- c(16, 17)               
n_lines <- c("dashed", "solid")

# Shared y-limits for both panels, with headroom for panel A's labels
y_lim <- c(floor(min(d15_dat$d15N)), ceiling(max(d15_dat$d15N)) + 2.5)

# Panel A: AMF presence
pA <- ggplot(d15_dat, aes(AMF, d15N, colour = Nutrient_treatment,
                          fill = Nutrient_treatment)) +
  geom_boxplot(position = position_dodge(0.75), width = 0.6,
               alpha = 0.25, outlier.shape = NA) +
  geom_point(aes(shape = Nutrient_treatment),
             position = position_jitterdodge(jitter.width = 0.15,
                                             dodge.width = 0.75, seed = 1),
             size = 2, alpha = 0.8) +
  annotate("text", x = 0.45, y = y_lim[2], hjust = 0, vjust = 1, size = 3.8,
           label = paste0("AMF: ", fmt_p(p_amf), "\nNitrogen: ", fmt_p(p_n))) +
  scale_colour_manual(values = n_cols) +
  scale_fill_manual(values = n_cols) +
  scale_shape_manual(values = n_shapes) +
  coord_cartesian(ylim = y_lim) +
  guides(colour = "none", fill = "none", shape = "none") +
  labs(x = "AMF inoculation", y = expression(delta^{15}*N)) +
  theme_classic(base_size = 13)

# Panel B: colonization intensity (AMF plants only) 
pred <- predict_response(m_d15,
                         terms = c("pct_total [all]", "Nutrient_treatment"),
                         condition = c(AMF = "Yes"),
                         margin = "marginalmeans") |>
  as.data.frame() |>
  mutate(group = factor(group, levels = levels(d15_dat$Nutrient_treatment)))

rng <- amf_dat |>
  group_by(group = Nutrient_treatment) |>
  summarise(lo = min(pct_total), hi = max(pct_total))

pred <- pred |>
  inner_join(rng, by = "group") |>
  filter(x >= lo, x <= hi)

# Slope + interaction labels 
slope_lines <- sprintf("%s: slope = %.3f, %s",
                       tr$Nutrient_treatment, tr$pct_total.trend, fmt_p(tr$p.value))

lab_B <- paste(c(slope_lines,
                 paste0("Colonization \u00d7 N: ", fmt_p(p_int))),
               collapse = "\n")

pB <- ggplot() +
  geom_ribbon(data = pred,
              aes(x, ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.15) +
  geom_line(data = pred,
            aes(x, predicted, colour = group, linetype = group),
            linewidth = 1) +
  geom_point(data = amf_dat,
             aes(pct_total, d15N, colour = Nutrient_treatment,
                 shape = Nutrient_treatment),
             size = 2.5, alpha = 0.85) +
  annotate("text", x = min(amf_dat$pct_total), y = y_lim[2],
           hjust = 0, vjust = 1, size = 3.8, lineheight = 1.1,
           label = lab_B) +
  scale_colour_manual(values = n_cols) +
  scale_fill_manual(values = n_cols) +
  scale_shape_manual(values = n_shapes) +
  scale_linetype_manual(values = n_lines) +
  coord_cartesian(ylim = y_lim) +
  labs(x = "Total AMF colonization (%)", y = NULL,
       colour = "Nutrient", fill = "Nutrient",
       shape = "Nutrient", linetype = "Nutrient") +
  theme_classic(base_size = 13)

# ---- Combine and save -------------------------------------------------
fig_d15 <- (pA | pB) +
  plot_layout(guides = "collect", widths = c(1, 1.4)) +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom")

fig_d15

ggsave("fig_d15N_colonization.png", fig_d15, width = 9, height = 5, dpi = 600)


# New way of making plot with arbuscule, hyphae and vesicles

# Long format: one row per plant per structure 
struct_lv <- c(pct_hyphae     = "Hyphae",
               pct_arbuscules = "Arbuscules",
               pct_vesicles   = "Vesicles")

d15_long <- d15_dat |>
  pivot_longer(all_of(names(struct_lv)), names_to = "structure", values_to = "pct") |>
  mutate(structure = factor(struct_lv[structure], levels = struct_lv))

# Checks: non-mycorrhizal plants at 0 for every structure, no missing values
d15_long |> filter(AMF == "No") |> group_by(structure) |> summarise(max_pct = max(pct))
sum(is.na(d15_long$pct))

fmt_p <- function(p) ifelse(p < 0.001, "< 0.001", sprintf("= %.3f", p))

# One model per structure: slope per water level 
fits <- map(set_names(levels(d15_long$structure)), \(s) {
  d <- filter(d15_long, structure == s)
  list(data  = d,
       model = lmer(d15N ~ pct * Water_treatment + Water_treatment * Nutrient_treatment +
                      (1 | Block_ID), data = d))
})

aov_list <- map(fits, \(f) Anova(f$model, type = 2))
aov_list

# Labels, tags and factor levels for the faceted plot 
lines_all <- lines_all |>
  mutate(structure = factor(structure, levels = struct_lv))

n_lv <- c("High Nitrogen", "Low Nitrogen")

# Slope labels: both water lines in every panel, stacked
lab_df <- slopes_all |>
  mutate(structure = factor(structure, levels = struct_lv),
         vj = ifelse(Water_treatment == "High water supply", 1.3, 2.9)) |>
  crossing(Nutrient_treatment = n_lv)

# Panel letters: a–c High N row, d–f Low N row
tag_df <- expand_grid(Nutrient_treatment = n_lv,
                      structure = factor(struct_lv, levels = struct_lv)) |>
  mutate(tag = letters[1:6])

#  Plot
make_row <- function(n) {
  ggplot(filter(d15_long, Nutrient_treatment == n),
         aes(pct, d15N, color = Water_treatment)) +
    geom_ribbon(data = filter(lines_all, Nutrient_treatment == n),
                aes(x = pct, ymin = asymp.LCL, ymax = asymp.UCL, fill = Water_treatment),
                inherit.aes = FALSE, alpha = 0.15) +
    geom_line(data = filter(lines_all, Nutrient_treatment == n),
              aes(x = pct, y = emmean, color = Water_treatment),
              inherit.aes = FALSE, linewidth = 1) +
    geom_point(size = 2.5, alpha = 0.8) +
    geom_text(data = filter(lab_df, Nutrient_treatment == n),
              aes(x = Inf, y = Inf, label = label, color = Water_treatment, vjust = vj),
              inherit.aes = FALSE, hjust = 1.03, size = 3, show.legend = FALSE) +
    geom_text(data = filter(tag_df, Nutrient_treatment == n),
              aes(x = -Inf, y = Inf, label = tag),
              inherit.aes = FALSE, hjust = -0.6, vjust = 1.3,
              size = 5, fontface = "bold") +
    facet_wrap(~ structure, scales = "free_x") +
    scale_color_manual(values = water_cols) +
    scale_fill_manual(values = water_cols, guide = "none") +
    coord_cartesian(ylim = y_lim) +
    labs(title = n,
         x = "Colonization (%)",
         y = expression(delta^15*N)) +
    theme_classic(base_size = 14) +
    theme(legend.title     = element_blank(),
          plot.title       = element_text(face = "bold", size = 16, hjust = 0.5),
          strip.background = element_blank(),
          strip.text       = element_text(size = 13),
          axis.title.y     = element_text(size = 18),
          panel.spacing    = unit(1.5, "lines"))
}

p_struct <- (make_row("High Nitrogen") / make_row("Low Nitrogen")) +
  plot_layout(guides = "collect", axis_titles = "collect") &
  theme(legend.position = "top")

p_struct
ggsave("d15N_structures_WxN.png", p_struct, width = 12, height = 7, dpi = 300)

# Summary
# d15N declined with hyphal colonization regardless of water supply, 
# while arbuscule and vesicle colonization showed no relationship with d15N.

library(glmmTMB); library(car); library(emmeans); library(ggeffects)
library(dplyr); library(tidyr); library(ggplot2)

lvls   <- c("Hyphae", "Arbuscules", "Vesicles")
n_lvls <- levels(d15_dat$Nutrient_treatment)

# Models 
m_hyp <- glmmTMB(d15N ~ (AMF + pct_hyphae) * Nutrient_treatment * Water_treatment + (1 | Block_ID),
                 family = gaussian(), data = d15_dat)

m_arb <- glmmTMB(d15N ~ (AMF + pct_arbuscules) * Nutrient_treatment * Water_treatment + (1 | Block_ID),
                 family = gaussian(), data = d15_dat)

m_ves <- glmmTMB(d15N ~ (AMF + pct_vesicles) * Nutrient_treatment * Water_treatment + (1 | Block_ID),
                 family = gaussian(), data = d15_dat)

# ANOVA tables
aov_hyp <- Anova(m_hyp, type = "II"); aov_hyp
aov_arb <- Anova(m_arb, type = "II"); aov_arb
aov_ves <- Anova(m_ves, type = "II"); aov_ves

# Holm correction on the structure × N interaction 
p_raw <- c(Hyphae     = aov_hyp["pct_hyphae:Nutrient_treatment", "Pr(>Chisq)"],
           Arbuscules = aov_arb["pct_arbuscules:Nutrient_treatment", "Pr(>Chisq)"],
           Vesicles   = aov_ves["pct_vesicles:Nutrient_treatment", "Pr(>Chisq)"])

p_holm <- p.adjust(p_raw, method = "holm")
p_holm

# Slopes at each N level (AMF plants) 
tr_hyp <- emtrends(m_hyp, ~ Nutrient_treatment, var = "pct_hyphae",
                   at = list(AMF = "Yes")) |> test() |> as.data.frame()
names(tr_hyp)[names(tr_hyp) == "pct_hyphae.trend"] <- "trend"

tr_arb <- emtrends(m_arb, ~ Nutrient_treatment, var = "pct_arbuscules",
                   at = list(AMF = "Yes")) |> test() |> as.data.frame()
names(tr_arb)[names(tr_arb) == "pct_arbuscules.trend"] <- "trend"

tr_ves <- emtrends(m_ves, ~ Nutrient_treatment, var = "pct_vesicles",
                   at = list(AMF = "Yes")) |> test() |> as.data.frame()
names(tr_ves)[names(tr_ves) == "pct_vesicles.trend"] <- "trend"

tr_all <- bind_rows(Hyphae = tr_hyp, Arbuscules = tr_arb, Vesicles = tr_ves,
                    .id = "structure")
tr_all

# Model predictions 
pred_hyp <- predict_response(m_hyp, terms = c("pct_hyphae [all]", "Nutrient_treatment"),
                             condition = c(AMF = "Yes"), margin = "marginalmeans") |>
  as.data.frame()

pred_arb <- predict_response(m_arb, terms = c("pct_arbuscules [all]", "Nutrient_treatment"),
                             condition = c(AMF = "Yes"), margin = "marginalmeans") |>
  as.data.frame()

pred_ves <- predict_response(m_ves, terms = c("pct_vesicles [all]", "Nutrient_treatment"),
                             condition = c(AMF = "Yes"), margin = "marginalmeans") |>
  as.data.frame()

#  Raw data in long format (AMF plants) 
pts <- amf_dat |>
  pivot_longer(c(pct_hyphae, pct_arbuscules, pct_vesicles),
               names_to = "var", values_to = "pct") |>
  mutate(structure = case_when(var == "pct_hyphae"     ~ "Hyphae",
                               var == "pct_arbuscules" ~ "Arbuscules",
                               var == "pct_vesicles"   ~ "Vesicles"))

# How many plants have each structure above zero, by N level
pts |>
  group_by(structure, Nutrient_treatment) |>
  dplyr::summarise(n_above_zero = sum(pct > 0), n = n(), .groups = "drop")

# Observed range per structure × N (lines are drawn only within these)
rng <- pts |>
  group_by(structure, group = Nutrient_treatment) |>
  dplyr::summarise(lo = min(pct), hi = max(pct), .groups = "drop")

# Combine predictions, trim to range, add significance 
pred_all <- bind_rows(Hyphae = pred_hyp, Arbuscules = pred_arb, Vesicles = pred_ves,
                      .id = "structure") |>
  mutate(group = factor(group, levels = n_lvls)) |>
  inner_join(rng, by = c("structure", "group")) |>
  filter(x >= lo, x <= hi) |>
  left_join(tr_all |>
              transmute(structure, group = Nutrient_treatment,
                        sig = ifelse(p.value < 0.05, "P < 0.05", "n.s.")),
            by = c("structure", "group")) |>
  mutate(structure = factor(structure, levels = lvls))

pts <- pts |> mutate(structure = factor(structure, levels = lvls))

# Panel labels 
lab_df <- tr_all |>
  mutate(line = paste0(Nutrient_treatment, ": slope = ",
                       format(round(trend, 3), nsmall = 3), ", ", fmt_p(p.value))) |>
  group_by(structure) |>
  dplyr::summarise(label = paste(line, collapse = "\n"), .groups = "drop") |>
  mutate(label = paste0(label, "\n", structure, " \u00d7 N: ",
                        fmt_p(p_holm[structure]), " (Holm)"),
         structure = factor(structure, levels = lvls))

# Figure 
fig_struct <- ggplot() +
  geom_ribbon(data = pred_all,
              aes(x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.15) +
  geom_line(data = pred_all,
            aes(x, predicted, colour = group, linetype = sig), linewidth = 1) +
  geom_point(data = pts,
             aes(pct, d15N, colour = Nutrient_treatment, shape = Nutrient_treatment),
             size = 2.2, alpha = 0.85) +
  geom_text(data = lab_df, aes(x = -Inf, y = Inf, label = label),
            hjust = -0.04, vjust = 1.2, size = 3.1, lineheight = 1.1) +
  facet_wrap(~ structure, scales = "free_x") +
  scale_colour_manual(values = n_cols) +
  scale_fill_manual(values = n_cols) +
  scale_shape_manual(values = n_shapes) +
  scale_linetype_manual(values = c("P < 0.05" = "solid", "n.s." = "dashed")) +
  coord_cartesian(ylim = y_lim) +
  labs(x = "Colonization (%)", y = expression(delta^{15}*N),
       colour = "Nutrient", fill = "Nutrient", shape = "Nutrient", linetype = "Slope") +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

fig_struct
ggsave("figS_d15N_structures.png", fig_struct, width = 11, height = 4.8, dpi = 600)












