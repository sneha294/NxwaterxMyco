setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment I/Experiment_I/Fungal Staining")

library(dplyr)
library(readxl)
library(tidyr)
library(car)
library(emmeans)
library(glmmTMB)
library(ggplot2)
library(ggh4x)


Fungal_quantification <- read_excel("Fungal_quantification.xlsx")
Fungal_quantification$Vah         <- 50 - Fungal_quantification$None
Fungal_quantification$Vah_percent <- (Fungal_quantification$Vah / 50) * 100

metadata <- read_excel("metadata.xlsx")

merge_FQ <- merge(Fungal_quantification, metadata, by = "Sample")
merge_FQ$Block_ID_fc <- as.factor(merge_FQ$Block_ID)

unique(merge_FQ$AMF)                                    # check the label is "Yes"/"No"
summary(filter(merge_FQ, AMF == "No")$Vah_percent)     # should all be 0

# Model, AMF plants only, beta-binomial on intersect counts 
amf_FQ <- merge_FQ %>% filter(AMF == "Yes")

Vah_glmm <- glmmTMB(cbind(Vah, None) ~ Water_treatment * Nutrient_treatment + (1 | Block_ID_fc),
                    family = betabinomial(link = "logit"),
                    data = amf_FQ)

Anova(Vah_glmm)
summary(Vah_glmm)
emmeans(Vah_glmm, ~ Nutrient_treatment, type = "response")

# ---- Plot helpers ------------------------------------------------------
dodge  <- position_dodge(width = 0.8)
jdodge <- position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8)

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

# Lists significant terms 
sig_label <- function(model, type = 2) {
  a <- as.data.frame(car::Anova(model, type = type))
  p <- a[[ncol(a)]]                      # works for lmer (Pr(>Chisq)) and glmmTMB
  terms <- rownames(a)
  keep <- !is.na(p) & p < 0.05 & terms != "(Intercept)"
  if (!any(keep)) return("n.s.")
  stars <- as.character(cut(p[keep], c(-Inf, 0.001, 0.01, 0.05),
                            labels = c("***", "**", "*")))
  nice <- terms[keep] |>
    str_replace_all(c("Water_treatment" = "Water",
                      "Nutrient_treatment" = "N",
                      ":" = " \u00d7 "))
  paste0(nice, stars, collapse = "\n")
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

#  Means for plotting 
# AMF plants: back-transformed from logit, ±1 SE, in %
em_col <- as.data.frame(emmeans(Vah_glmm, ~ Water_treatment * Nutrient_treatment)) %>%
  mutate(lo = plogis(emmean - SE) * 100,
         hi = plogis(emmean + SE) * 100,
         emmean = plogis(emmean) * 100,
         AMF = "Yes")

# Non-AMF plants: structural zeros at 0, no error bar
em_col <- bind_rows(em_col,
                    em_col %>% mutate(AMF = "No", emmean = 0, lo = 0, hi = 0)) %>%
  recode_trt()

col_raw <- merge_FQ %>% recode_trt()

# Plot 
p_col <- plot_trt(em_col, col_raw, "Vah_percent", Vah_glmm,
                  "Mycorrhizal colonization (%)", from_zero = FALSE)


fig_col <- p_col +
  force_panelsizes(cols = unit(2.6, "in"), rows = unit(2.6, "in"))

fig_col

ggsave("Figure_colonization_ExpI.png", fig_col, width = 8, height = 4.6, dpi = 600)
