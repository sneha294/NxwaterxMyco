setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment I/Experiment_I/Carbon_nitrogen_analysis/Carbon_nitrogen_analysis")

# This file can be opened and analysed from Snehanjana's one drive

# Loading all the packages
library(dplyr)
library(readxl)
library(tidyr)
library(car)
library(emmeans)
library(lme4)
library(ggplot2)
library(ggpubr)
library(ggsignif)
library(cowplot)


# Reading excel sheet data

Leaf_costech <- read_excel("Leaf_carbon_nitrogen.xlsx")
Stem_costech <- read_excel("Stem_carbon_nitrogen.xlsx")
Root_costech <- read_excel("Root_carbon_nitrogen.xlsx")
metadata <- read_excel("metadata.xlsx")

# Merging leaf, stem, root and metadata sheets

merge_costech <- Leaf_costech %>%
  left_join(Stem_costech, by = "Sample") %>%
  left_join(Root_costech, by = "Sample") %>%
  left_join( metadata, by = "Sample")

# Making two columns of whole plant nitrogen biomass and carbon cost to
# acquire and use nutrients

merge_costech <- merge_costech %>%
  mutate(Whole_plant_nitrogen_biomass = `Total Leaf nitrogen biomass`+ `Total Root nitrogen biomass`+ `Total stem nitrogen biomass`,
         Carbon_cost_to_acquire_nutrients = Total_root_carbon_biomass/ Whole_plant_nitrogen_biomass)

merge_costech <- merge_costech |>
  mutate(Water_treatment = factor(Water_treatment,
                                  levels = c("High water supply", "Low water supply")),
         Nutrient_treatment = factor(Nutrient_treatment,
                                     levels = c("High Nitrogen", "Low Nitrogen")),
         AMF = factor(AMF, levels = c("No", "Yes")))

# confirm no NAs were introduced by a spelling mismatch
summary(merge_costech[c("Water_treatment", "Nutrient_treatment", "AMF")])


# merge_costech <- merge_costech %>% filter(Total_root_carbon_biomass < 150)

# Dry leaf weight
DLW_lmer <- lmer(`Dry Leaf weight (g)` ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_costech)

Anova(DLW_lmer)
summary(DLW_lmer)
hist(residuals(DLW_lmer))
shapiro.test(residuals(DLW_lmer))

emmeans(DLW_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey")

emmeans(DLW_lmer, pairwise ~ Nutrient_treatment * Water_treatment * AMF, adjust = "tukey")


em_DLW <- as.data.frame(emmeans(DLW_lmer, ~AMF * Nutrient_treatment * Water_treatment)) 


# Root carbon biomass

Root_lmer <- lmer(sqrt(Total_root_carbon_biomass) ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_costech)

Anova(Root_lmer)
summary(Root_lmer)
hist(residuals(Root_lmer))
shapiro.test(residuals(Root_lmer)) 

plot(Root_lmer)

emmeans(Root_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey")

#ggplot 

em_Root <- as.data.frame(emmeans(Root_lmer, ~ AMF * Nutrient_treatment * Water_treatment), type = "response")


# Whole plant nitrogen biomass

WPN_lmer <- lmer(Whole_plant_nitrogen_biomass ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_costech)

Anova(WPN_lmer)
summary(WPN_lmer)
hist(residuals(WPN_lmer))

shapiro.test(residuals(WPN_lmer)) 

plot(WPN_lmer)


emmeans(WPN_lmer, pairwise ~ AMF * Nutrient_treatment, adjust = "tukey")

emmeans(WPN_lmer, pairwise ~ Water_treatment * Nutrient_treatment * AMF, adjust = "tukey")

emmeans(WPN_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey")
emmeans(WPN_lmer, pairwise ~ AMF, adjust = "tukey")


em_WPN <- as.data.frame(emmeans(WPN_lmer, ~AMF * Nutrient_treatment * Water_treatment)) 


# NEW PLOTS, SAME RESULTS


recode_trt <- function(d) {
  d %>% mutate(
    Water_treatment = recode(Water_treatment,
                             "Drought" = "Low water supply",
                             "Well watered" = "High water supply"),
    Nutrient_treatment = recode(Nutrient_treatment,
                                "High nitrogen" = "High Nitrogen",
                                "Low nitrogen"  = "Low Nitrogen",
                                "High" = "High Nitrogen",
                                "Low"  = "Low Nitrogen"))
}

plot_trt <- function(em, raw, yvar, model, ylab, from_zero = TRUE) {
  if (!all(c("lo", "hi") %in% names(em))) {
    em <- em %>% mutate(lo = emmean - SE, hi = emmean + SE)
  }
  p <- ggplot(em, aes(Water_treatment, emmean, color = AMF)) +
    geom_point(data = raw, aes(y = .data[[yvar]]),
               size = 2.5, alpha = 0.3, position = jdodge) +
    geom_errorbar(aes(ymin = lo, ymax = hi),
                  width = 0.2, linewidth = 1, position = dodge) +
    geom_point(shape = 15, size = 5, position = dodge) +
    geom_text(data = tibble(Nutrient_treatment = "Low Nitrogen", label = sig_label(model)),
              aes(x = Inf, y = Inf, label = label), inherit.aes = FALSE,
              hjust = 1.05, vjust = 1.2, size = 4, lineheight = 0.9) +
    scale_color_manual(values = c(No = "#D55E00", Yes = "#009E73"),
                       labels = c(No = "AMF absent", Yes = "AMF present")) +
    scale_x_discrete(limits = c("Low water supply", "High water supply"),
                     labels = c("Low water\nsupply", "High water\nsupply")) +
    facet_wrap(~ Nutrient_treatment) +
    labs(x = NULL, y = ylab) +
    theme_classic(base_size = 14) +
    theme(legend.title = element_blank(),
          legend.position = "top",
          strip.background = element_blank(),
          strip.text = element_text(size = 14))
  
  if (from_zero) {
    p + scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + expand_limits(y = 0)
  } else {
    p + scale_y_continuous(expand = expansion(mult = c(0.05, 0.2)))
  }
}

# Back-transform a sqrt-scale model
sqrt_em <- function(model) {
  as.data.frame(emmeans(model, ~ AMF * Nutrient_treatment * Water_treatment)) %>%
    mutate(lo = pmax(emmean - SE, 0)^2,
           hi = (emmean + SE)^2,
           emmean = emmean^2)
}

#  emmeans 
em_WPN  <- as.data.frame(emmeans(WPN_lmer, ~ AMF * Nutrient_treatment * Water_treatment)) %>%
  recode_trt()
em_Root <- sqrt_em(Root_lmer) %>% recode_trt()
em_tran <- sqrt_em(Tran_lmer) %>% recode_trt()

costech_raw <- merge_costech %>% recode_trt()
tran_raw    <- Transpiration %>% recode_trt()

#  Plots
p_WPN <- plot_trt(em_WPN, costech_raw, "Whole_plant_nitrogen_biomass", WPN_lmer,
                  expression(atop(displaystyle("Whole-plant biomass"),
                                  displaystyle("nitrogen (gN)"))))

p_Root <- plot_trt(em_Root, costech_raw, "Total_root_carbon_biomass", Root_lmer,
                   expression(atop(displaystyle("Root biomass"),
                                   displaystyle("carbon (gC)"))))

p_tran <- plot_trt(em_tran, tran_raw, "Cumulative_transpiration_rate", Tran_lmer,
                   expression(atop(displaystyle("Whole-plant"),
                                   displaystyle("transpiration (g Water)"))))

#  a & b
fig_NC <- make_fig(p_WPN, p_Root, c("a", "b"))
fig_NC
ggsave("Figure_N_rootC_ExpI.png", fig_NC, width = 8, height = 9, dpi = 600)

# c
fig_tran <- p_tran +
  labs(tag = "c") +
  force_panelsizes(cols = unit(2.6, "in"), rows = unit(2.6, "in")) +
  theme(plot.tag = element_text(size = 14))
fig_tran
ggsave("Figure_transpiration_ExpI.png", fig_tran, width = 8, height = 4.6, dpi = 600)






# Model for carbon cost to acquire and use nutrients

CC_lmer <- lmer(log(Carbon_cost_to_acquire_nutrients) ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_costech)

Anova(CC_lmer)
summary(CC_lmer)
hist(residuals(CC_lmer))
shapiro.test(residuals(CC_lmer)) 

plot(CC_lmer)

emmeans(CC_lmer, pairwise ~ AMF, adjust = "tukey")

emmeans(CC_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey")


emmeans(CC_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey")


# emmeans for carbon cost
em_CC <- as.data.frame(emmeans(CC_lmer, ~AMF * Nutrient_treatment * Water_treatment, type = "response")) 


## NEW PLOTS, SAME ANALYSIS

# ---- Model ----
CC_lmer <- lmer(log(Carbon_cost_to_acquire_nutrients) ~
                  Water_treatment * Nutrient_treatment * AMF + (1 | Block_ID),
                data = merge_costech)

car::Anova(CC_lmer, type = 2)

em_CC <- as.data.frame(
  emmeans(CC_lmer, ~ AMF * Water_treatment * Nutrient_treatment, type = "response")
)

# ---- Plot ----
dodge  <- position_dodge(width = 0.8)
jdodge <- position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8)

p_CC <- ggplot(em_CC, aes(Water_treatment, response, color = AMF)) +
  geom_point(data = merge_costech,
             aes(y = Carbon_cost_to_acquire_nutrients),
             size = 2.5, alpha = 0.3, position = jdodge) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.2, linewidth = 1, position = dodge) +
  geom_point(shape = 15, size = 5, position = dodge) +
  geom_text(data = tibble(Nutrient_treatment = factor("Low Nitrogen",
                                                      levels = levels(merge_costech$Nutrient_treatment)),
                          label = sig_label(CC_lmer)),
            aes(x = Inf, y = Inf, label = label), inherit.aes = FALSE,
            hjust = 1.05, vjust = 1.2, size = 3, lineheight = 0.9) +
  scale_color_manual(values = c(No = "#D55E00", Yes = "#009E73"),
                     labels = c(No = "AMF absent", Yes = "AMF present")) +
  scale_x_discrete(labels = c("High water supply" = "High water\nsupply",
                              "Low water supply"  = "Low water\nsupply"))  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  expand_limits(y = 0) +
  facet_wrap(~ Nutrient_treatment) +
  labs(x = NULL, y = expression(atop("Carbon cost to acquire",
                                     "and use nutrients (gC "*gN^-1*")"))) +
  theme_classic(base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "top",
        strip.background = element_blank(),
        strip.text = element_text(size = 14),
        panel.spacing.x = unit(2, "lines"),
        axis.title.y = element_text(margin = margin(r = 8)),
        plot.margin = margin(t = 10, r = 5, b = 10, l = 5))

print(p_CC)

ggsave("Figure_CC_ExpI.png", p_CC, width = 7, height = 5, dpi = 300)



# Transpiration
setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment I/Experiment_I/Transpiration rate")

Transpiration <- read_excel("cumulative_transpiration.xlsx")

#Transpiration <- Transpiration %>% filter(Cumulative_transpiration_rate < 3500)

Tran_lmer <- lmer(sqrt(Cumulative_transpiration_rate) ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = Transpiration) 

Anova(Tran_lmer)
summary(Tran_lmer)
hist(residuals(Tran_lmer))
shapiro.test(residuals(Tran_lmer)) 

plot(Tran_lmer)


emmeans(Tran_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey")
emmeans(Tran_lmer, pairwise ~ Water_treatment, adjust = "tukey")
emmeans(Tran_lmer, pairwise ~ AMF, adjust = "tukey")


# AMF x Nutrient (for finding out % diff between AMF vs no AMF)
emm_N <- as.data.frame(regrid(emmeans(Tran_lmer, ~ AMF * Nutrient_treatment)))
pct_low <- ((emm_N$response[emm_N$AMF=="No" & emm_N$Nutrient_treatment=="Low"] - 
               emm_N$response[emm_N$AMF=="Yes" & emm_N$Nutrient_treatment=="Low"]) /
              emm_N$response[emm_N$AMF=="No" & emm_N$Nutrient_treatment=="Low"]) * 100

pct_high <- ((emm_N$response[emm_N$AMF=="No" & emm_N$Nutrient_treatment=="High"] - 
                emm_N$response[emm_N$AMF=="Yes" & emm_N$Nutrient_treatment=="High"]) /
               emm_N$response[emm_N$AMF=="No" & emm_N$Nutrient_treatment=="High"]) * 100

# AMF x Water (for finding out % diff between AMF vs no AMF)
emm_W <- as.data.frame(regrid(emmeans(Tran_lmer, ~ AMF * Water_treatment)))

pct_ww <- ((emm_W$response[emm_W$AMF=="No" & emm_W$Water_treatment=="Well watered"] - 
              emm_W$response[emm_W$AMF=="Yes" & emm_W$Water_treatment=="Well watered"]) /
             emm_W$response[emm_W$AMF=="No" & emm_W$Water_treatment=="Well watered"]) * 100

pct_dr <- ((emm_W$response[emm_W$AMF=="No" & emm_W$Water_treatment=="Drought"] - 
              emm_W$response[emm_W$AMF=="Yes" & emm_W$Water_treatment=="Drought"]) /
             emm_W$response[emm_W$AMF=="No" & emm_W$Water_treatment=="Drought"]) * 100


# emmeans for transpiration
em_tran <- as.data.frame(emmeans(Tran_lmer, ~AMF * Nutrient_treatment * Water_treatment), type = "response")

# Root biomass carbon, whole plant biomass nitrogen and transpiration together

library(patchwork)
library(ggh4x)

same_size <- force_panelsizes(cols = unit(2.6, "in"), rows = unit(2.6, "in"))

fig_all <- ((p_WPN + same_size) / (p_Root + same_size) / (p_tran + same_size)) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "top",
        plot.tag = element_text(size = 14))

fig_all
ggsave("Figure_N_rootC_transpiration_ExpI.png", fig_all,
       width = 8, height = 13, dpi = 600)



# merging root biomass carbon and transpiration

# might have to change directory to Carbon_nitrogen_analysis folder HERE
setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment I/Experiment_I/Carbon_nitrogen_analysis/Carbon_nitrogen_analysis")


merge_transpiration <-merge(Transpiration, Root_costech, by = "Sample")


merge_transpiration <- merge_transpiration %>%
  mutate(Carbon_cost_to_acquire_water = (Total_root_carbon_biomass)/ Cumulative_transpiration_rate)

# Carbon cost to acquire and use water

CCW_lmer <- lmer(Carbon_cost_to_acquire_water ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_transpiration)

Anova(CCW_lmer)
summary(CCW_lmer)
hist(residuals(CCW_lmer))
shapiro.test(residuals(CCW_lmer)) 

plot(CCW_lmer)


emmeans(CCW_lmer, pairwise ~ AMF, adjust = "tukey")
emmeans(CCW_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey")

# emmeans for carbon cost of water

em_CCW <- as.data.frame(emmeans(CCW_lmer, ~AMF * Nutrient_treatment * Water_treatment))
 


# NEW PLOTS, SAME ANALYSIS

# ---- Factors (same as merge_costech) ----
merge_transpiration <- merge_transpiration |>
  mutate(Water_treatment = factor(Water_treatment,
                                  levels = c("High water supply", "Low water supply")),
         Nutrient_treatment = factor(Nutrient_treatment,
                                     levels = c("High Nitrogen", "Low Nitrogen")),
         AMF = factor(AMF, levels = c("No", "Yes")))

# ---- Model ----
CCW_lmer <- lmer(Carbon_cost_to_acquire_water ~
                   Water_treatment * Nutrient_treatment * AMF + (1 | Block_ID),
                 data = merge_transpiration)

car::Anova(CCW_lmer, type = 2)
shapiro.test(residuals(CCW_lmer))

em_CCW <- as.data.frame(
  emmeans(CCW_lmer, ~ AMF * Water_treatment * Nutrient_treatment)
)

library(patchwork)
library(ggh4x)

# ---- Nutrients: log model, so back-transform ±1 SE (like chlorophyll) ----
em_CC <- as.data.frame(emmeans(CC_lmer, ~ AMF * Water_treatment * Nutrient_treatment)) %>%
  mutate(lo = exp(emmean - SE),
         hi = exp(emmean + SE),
         emmean = exp(emmean))

p_CC <- ggplot(em_CC, aes(Water_treatment, emmean, color = AMF)) +
  geom_point(data = merge_costech, aes(y = Carbon_cost_to_acquire_nutrients),
             size = 2.5, alpha = 0.3, position = jdodge) +
  geom_errorbar(aes(ymin = lo, ymax = hi),
                width = 0.2, linewidth = 1, position = dodge) +
  geom_point(shape = 15, size = 5, position = dodge) +
  geom_text(data = tibble(Nutrient_treatment = factor("Low Nitrogen",
                                                      levels = levels(merge_costech$Nutrient_treatment)),
                          label = sig_label(CC_lmer)),
            aes(x = Inf, y = Inf, label = label), inherit.aes = FALSE,
            hjust = 1.05, vjust = 1.2, size = 4, lineheight = 0.9) +
  scale_color_manual(values = c(No = "#D55E00", Yes = "#009E73"),
                     labels = c(No = "AMF absent", Yes = "AMF present")) +
  scale_x_discrete(limits = c("Low water supply", "High water supply"),
                   labels = c("Low water\nsupply", "High water\nsupply")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  expand_limits(y = 0) +
  facet_wrap(~ Nutrient_treatment) +
  labs(x = NULL,
       y = expression(atop(displaystyle("Carbon cost to acquire"),
                           displaystyle("and use nutrients (gC "*gN^-1*")")))) +
  theme_classic(base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "top",
        strip.background = element_blank(),
        strip.text = element_text(size = 14))

# ---- Water: untransformed model, plain ±SE ----
em_CCW <- as.data.frame(emmeans(CCW_lmer, ~ AMF * Water_treatment * Nutrient_treatment))

p_CCW <- ggplot(em_CCW, aes(Water_treatment, emmean, color = AMF)) +
  geom_point(data = merge_transpiration, aes(y = Carbon_cost_to_acquire_water),
             size = 2.5, alpha = 0.3, position = jdodge) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),
                width = 0.2, linewidth = 1, position = dodge) +
  geom_point(shape = 15, size = 5, position = dodge) +
  geom_text(data = tibble(Nutrient_treatment = factor("Low Nitrogen",
                                                      levels = levels(merge_transpiration$Nutrient_treatment)),
                          label = sig_label(CCW_lmer)),
            aes(x = Inf, y = Inf, label = label), inherit.aes = FALSE,
            hjust = 1.05, vjust = 1.2, size = 4, lineheight = 0.9) +
  scale_color_manual(values = c(No = "#D55E00", Yes = "#009E73"),
                     labels = c(No = "AMF absent", Yes = "AMF present")) +
  scale_x_discrete(limits = c("Low water supply", "High water supply"),
                   labels = c("Low water\nsupply", "High water\nsupply")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  expand_limits(y = 0) +
  facet_wrap(~ Nutrient_treatment) +
  labs(x = NULL,
       y = expression(atop(displaystyle("Carbon cost to acquire"),
                           displaystyle("and use water (gC g water"^-1*")")))) +
  theme_classic(base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "top",
        strip.background = element_blank(),
        strip.text = element_text(size = 14))

# ---- Combine with the same function and size as Vcmax/Jmax ----
fig_CC <- make_fig(p_CC, p_CCW, c("a", "b"))
fig_CC

ggsave("Figure_carbon_costs_ExpI.png", fig_CC, width = 8, height = 9, dpi = 600)



# Total biomass

Total_biomass <- read_excel("dried_measurements.xlsx")

metadata <- read_excel("metadata.xlsx")

# merging metadata with dried measurement sheet
merge_biomass <-merge(Total_biomass, metadata, by = "Sample")

merge_biomass <- merge_biomass %>% mutate(`Root shoot ratio` = `Total root weight (g)`/`Total aboveground weight (g)`) 


TB_lmer <- lmer(Total_biomass ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_biomass)

Anova(TB_lmer)
summary(TB_lmer)
hist(residuals(TB_lmer))
shapiro.test(residuals(TB_lmer)) 

plot(CC_lmer)

emmeans(TB_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey")


# emmeans for biomass

em_biomass <- as.data.frame(emmeans(TB_lmer, ~AMF * Nutrient_treatment * Water_treatment))


# Total leaf area

TLA_lmer <- lmer(`Total leaf area` ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_biomass)

Anova(TLA_lmer)
summary(TLA_lmer)
hist(residuals(TLA_lmer))
shapiro.test(residuals(TLA_lmer)) 

plot(TLA_lmer)

emmeans(TLA_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey")
emmeans(TLA_lmer, pairwise ~ AMF, adjust = "tukey")

emmeans(TLA_lmer, pairwise ~ Water_treatment * Nutrient_treatment * AMF, adjust = "tukey")

emmeans(TLA_lmer, pairwise ~ AMF * Nutrient_treatment, adjust = "tukey") 

# posthoc analysis shows there is no difference in TLA in low nitrogen 
# regardless of AMF presence

# emmeans for TLA

em_TLA <- as.data.frame(emmeans(TLA_lmer, ~AMF * Nutrient_treatment * Water_treatment))


## NEW PLOTS, SAME ANALYSIS

# ---- Model ----
TLA_lmer <- lmer(`Total leaf area` ~ Water_treatment * Nutrient_treatment * AMF +
                   (1 | Block_ID), data = merge_biomass)

car::Anova(TLA_lmer, type = 2)
shapiro.test(residuals(TLA_lmer))

em_TLA <- as.data.frame(
  emmeans(TLA_lmer, ~ AMF * Water_treatment * Nutrient_treatment)
)

# ---- Plot ----
p_TLA <- ggplot(em_TLA, aes(Water_treatment, emmean, color = AMF)) +
  geom_point(data = merge_biomass, aes(y = `Total leaf area`),
             size = 2.5, alpha = 0.3, position = jdodge) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),
                width = 0.2, linewidth = 1, position = dodge) +
  geom_point(shape = 15, size = 5, position = dodge) +
  geom_text(data = tibble(Nutrient_treatment = factor("Low Nitrogen",
                                                      levels = levels(merge_biomass$Nutrient_treatment)),
                          label = sig_label(TLA_lmer)),
            aes(x = Inf, y = Inf, label = label), inherit.aes = FALSE,
            hjust = 1.05, vjust = 1.2, size = 3, lineheight = 0.9) +
  scale_color_manual(values = c(No = "red", Yes = "darkgreen"),
                     labels = c(No = "AMF absent", Yes = "AMF present")) +
  scale_x_discrete(labels = c("High water supply" = "High water\nsupply",
                              "Low water supply"  = "Low water\nsupply")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  expand_limits(y = 0) +
  facet_wrap(~ Nutrient_treatment) +
  labs(x = NULL, y = expression("Total leaf area (cm"^2*")")) +
  theme_classic(base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "top",
        strip.background = element_blank(),
        strip.text = element_text(size = 14),
        panel.spacing.x = unit(2, "lines"),
        axis.title.y = element_text(margin = margin(r = 8)),
        plot.margin = margin(t = 10, r = 5, b = 10, l = 5))

# ---- Combine: a = biomass, b = leaf area ----
fig_growth <- (p_TB / p_TLA) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "top")

print(fig_growth)

ggsave("Figure_growth_ExpI.png", fig_growth, width = 7, height = 9, dpi = 300)


# Root shoot ratio

RS_lmer <- lmer(`Root shoot ratio` ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_biomass)

Anova(RS_lmer)
summary(RS_lmer)
hist(residuals(RS_lmer))
shapiro.test(residuals(RS_lmer)) 

plot(RS_lmer)

# Tukey's test post hoc analysis

emmeans(RS_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey")
emmeans(RS_lmer, pairwise ~ AMF, adjust = "tukey")

# emmeans for RS

em_RS <- as.data.frame(emmeans(RS_lmer, ~AMF * Nutrient_treatment * Water_treatment))

## NEW PLOTS, SAME ANALYSIS

# emmeans 
em_TB  <- as.data.frame(emmeans(TB_lmer,  ~ AMF * Nutrient_treatment * Water_treatment))
em_TLA <- as.data.frame(emmeans(TLA_lmer, ~ AMF * Nutrient_treatment * Water_treatment))
em_RS  <- as.data.frame(emmeans(RS_lmer,  ~ AMF * Nutrient_treatment * Water_treatment))

#  Plots 
p_TB  <- plot_trt(em_TB,  merge_biomass, "Total_biomass", TB_lmer,
                  "Total biomass (g)")
p_TLA <- plot_trt(em_TLA, merge_biomass, "Total leaf area", TLA_lmer,
                  expression("Total leaf area ("*cm^2*")"))
p_RS  <- plot_trt(em_RS,  merge_biomass, "Root shoot ratio", RS_lmer,
                  "Root:shoot ratio")

# a & b
fig_growth <- make_fig(p_TB, p_TLA, c("a", "b"))
fig_growth
ggsave("Figure_growth_ExpI.png", fig_growth, width = 8, height = 9, dpi = 600)

# c- Root:Shoot ratio
fig_RS <- p_RS +
  labs(tag = "c") +
  force_panelsizes(cols = unit(2.6, "in"), rows = unit(2.6, "in")) +
  theme(plot.tag = element_text(size = 14))
fig_RS
ggsave("Figure_RS_ExpI.png", fig_RS, width = 8, height = 4.6, dpi = 600)

# three graphs together

fig_growth <- (p_TB /
                 (p_TLA + theme(strip.text = element_blank())) /
                 (p_RS  + theme(strip.text = element_blank()))) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = list(c("a", "b", "c"))) &
  force_panelsizes(cols = unit(2.6, "in"), rows = unit(2.6, "in")) &
  theme(legend.position = "top",
        plot.tag = element_text(size = 14))

fig_growth

ggsave("Figure_growth_RS_ExpI.png", fig_growth, width = 8, height = 13, dpi = 600)



# Narea, LMA, Nmass

# Nmass data is from UC davis Total_N

Nmass <- read_excel("Nmass.xlsx")

metadata <- read_excel("metadata.xlsx")

merge_Nmass <- merge(Nmass, metadata, by = "Sample")

merge_Nmass$Block_ID_fc <- as.factor(merge_Nmass$Block_ID)

# LMA

LMA_lmer <- lmer(log(LMA) ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID_fc), data = merge_Nmass)

Anova(LMA_lmer)
summary(LMA_lmer)
hist(residuals(LMA_lmer))
shapiro.test(residuals(LMA_lmer)) 

plot(LMA_lmer)

# Tukey's test post hoc analysis

emmeans(LMA_lmer, pairwise ~ AMF, adjust = "tukey")

# emmeans for LMA

em_LMA <- as.data.frame(emmeans(LMA_lmer, ~AMF * Nutrient_treatment * Water_treatment))



# Nmass

Nmass_lmer <- lmer(`Nmass (gN/g leaf)` ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_Nmass)

Anova(Nmass_lmer)
summary(Nmass_lmer)
hist(residuals(Nmass_lmer))
shapiro.test(residuals(Nmass_lmer)) 

plot(Nmass_lmer)

# Tukey's test post hoc analysis

emmeans(Nmass_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey")
emmeans(Nmass_lmer, pairwise ~ AMF, adjust = "tukey")

# emmeans for Nmass

em_Nmass <- as.data.frame(emmeans(Nmass_lmer, ~AMF * Nutrient_treatment * Water_treatment))


# Narea

Narea_lmer <- lmer(Narea ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID_fc), data = merge_Nmass)

Anova(Narea_lmer)

# blocking has no effect, random effect is zero, to keep everything same keeping lmer

summary(Narea_lmer)
hist(residuals(Narea_lmer))
shapiro.test(residuals(Narea_lmer)) 

plot(Narea_lmer)

# Tukey's test post hoc analysis

emmeans(Narea_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey")

# emmeans for Nmass

em_Narea <- as.data.frame(emmeans(Narea_lmer, ~AMF * Nutrient_treatment * Water_treatment))


# New plots, same results (LMA, Nmass)

library(patchwork)
library(ggh4x)

# Reusable trait plot
plot_trt <- function(em, raw, yvar, model, ylab, from_zero = TRUE) {
  p <- ggplot(em, aes(Water_treatment, emmean, color = AMF)) +
    geom_point(data = raw, aes(y = .data[[yvar]]),
               size = 2.5, alpha = 0.3, position = jdodge) +
    geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),
                  width = 0.2, linewidth = 1, position = dodge) +
    geom_point(shape = 15, size = 5, position = dodge) +
    geom_text(data = tibble(Nutrient_treatment = "Low Nitrogen", label = sig_label(model)),
              aes(x = Inf, y = Inf, label = label), inherit.aes = FALSE,
              hjust = 1.05, vjust = 1.2, size = 4, lineheight = 0.9) +
    scale_color_manual(values = c(No = "#D55E00", Yes = "#009E73"),
                       labels = c(No = "AMF absent", Yes = "AMF present")) +
    scale_x_discrete(limits = c("Low water supply", "High water supply"),
                     labels = c("Low water\nsupply", "High water\nsupply")) +
    facet_wrap(~ Nutrient_treatment) +
    labs(x = NULL, y = ylab) +
    theme_classic(base_size = 14) +
    theme(legend.title = element_blank(),
          legend.position = "top",
          strip.background = element_blank(),
          strip.text = element_text(size = 14))
  
  if (from_zero) {
    p + scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + expand_limits(y = 0)
  } else {
    p + scale_y_continuous(expand = expansion(mult = c(0.05, 0.2)))
  }
}

# ---- Data ----
em_LMA <- as.data.frame(emmeans(LMA_lmer, ~ AMF * Nutrient_treatment * Water_treatment)) %>%
  mutate(lo = exp(emmean - SE),
         hi = exp(emmean + SE),
         emmean = exp(emmean)) %>%
  recode_trt()
em_Nmass <- as.data.frame(emmeans(Nmass_lmer, ~ AMF * Nutrient_treatment * Water_treatment)) %>% recode_trt()
Nmass_raw <- merge_Nmass %>% recode_trt()

# ---- Plots ----
p_LMA <- plot_trt(em_LMA, Nmass_raw, "LMA", LMA_lmer,
                  expression("LMA (g "*cm^-2*")"))

p_Nmass <- plot_trt(em_Nmass, Nmass_raw, "Nmass (gN/g leaf)", Nmass_lmer,
                    expression(N[mass]~"(g N "*g^-1*")"))

# ---- Combine: same size as Vcmax/Jmax ----
fig_leaf <- make_fig(p_LMA, p_Nmass, c("a", "b"))
fig_leaf

ggsave("Figure_LMA_Nmass_ExpI.png", fig_leaf, width = 8, height = 9, dpi = 600)

# Narea

em_Narea <- as.data.frame(emmeans(Narea_lmer, ~ AMF * Nutrient_treatment * Water_treatment)) %>%
  recode_trt()

p_Narea <- plot_trt(em_Narea, Nmass_raw, "Narea", Narea_lmer,
                    expression(N[area]~"(g N "*cm^-2*")"))

# Standalone, same panel size as the other figures
fig_Narea <- p_Narea +
  labs(tag = "c") +
  force_panelsizes(cols = unit(2.6, "in"), rows = unit(2.6, "in")) +
  theme(plot.tag = element_text(size = 14))

fig_Narea

ggsave("Figure_Narea_ExpI.png", fig_Narea, width = 8, height = 4.6, dpi = 600)

# all three figures together

fig_leaf <- (p_LMA /
               (p_Nmass + theme(strip.text = element_blank())) /
               (p_Narea + theme(strip.text = element_blank()))) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = list(c("a", "b", "c"))) &
  force_panelsizes(cols = unit(2.6, "in"), rows = unit(2.6, "in")) &
  theme(legend.position = "top",
        plot.tag = element_text(size = 14))

fig_leaf

ggsave("Figure_LMA_Nmass_Narea_ExpI.png", fig_leaf, width = 8, height = 13, dpi = 600)



# ---- Mass-based Vcmax and Jmax ----

# 1. Join LMA onto the Vcmax/Jmax data by plant
mass_dat <- merge_stats %>%
  dplyr::left_join(dplyr::select(merge_Nmass, Sample, LMA), by = "Sample") %>%
  dplyr::filter(!is.na(Vcmax), !is.na(LMA)) %>%
  dplyr::mutate(Block_ID = as.factor(Block_ID),
                Vcmax_mass = Vcmax / (LMA * 10000),   # umol g-1 s-1
                Jmax_mass  = Jmax  / (LMA * 10000))

nrow(mass_dat)   # should be 45 (LND No Myco 1 has no A/Ci curve)

# 2. Vcmax per gram of leaf
Vcmax_mass_lmer <- lmer(Vcmax_mass ~ Water_treatment * Nutrient_treatment * AMF +
                          (1 | Block_ID), data = mass_dat)
Anova(Vcmax_mass_lmer, type = 2)
emmeans(Vcmax_mass_lmer, pairwise ~ AMF)
shapiro.test(residuals(Vcmax_mass_lmer))

# 3. Jmax per gram of leaf
Jmax_mass_lmer <- lmer(Jmax_mass ~ Water_treatment * Nutrient_treatment * AMF +
                         (1 | Block_ID), data = mass_dat)
Anova(Jmax_mass_lmer, type = 2)
emmeans(Jmax_mass_lmer, pairwise ~ AMF)
shapiro.test(residuals(Jmax_mass_lmer))
