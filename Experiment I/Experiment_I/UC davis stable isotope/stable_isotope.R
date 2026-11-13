setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment I/Experiment_I/UC davis stable isotope")

# This R script can be analysed in Snehanjana's one drive

library(dplyr)
library(readxl)
library(tidyr)
library(car)
library(emmeans)
library(lme4)
library(ggplot2)
library(cowplot)

stable_isotope <- read_excel("stable_isotope.xlsx")

metadata <- read_excel("metadata.xlsx")

merge_excel <- merge(stable_isotope, metadata, by = "Sample")

# fit model based on hypothesis (d13C)
d13C_lmer <- lmer(d13C ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID),data = merge_excel)

Anova(d13C_lmer)
summary(d13C_lmer)
hist(residuals(d13C_lmer))
shapiro.test(residuals(d13C_lmer)) # normal distribution

plot(d13C_lmer)
emmeans(d13C_lmer, pairwise ~  AMF , adjust = "tukey")

em_d13C <- as.data.frame(emmeans(d13C_lmer, ~AMF * Nutrient_treatment * Water_treatment))

# ggplot for d13C

Treatment_d13C <- ggdraw(ggplot(merge_excel, aes(y = d13C, x = AMF , color = AMF)) + geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_d13C, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_d13C, aes(x = AMF, y = emmean,ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y = expression(delta^{13}*C)) +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Water limited"),
                                 Nutrient_treatment = c("Low nitrogen" = "Low Nitrogen",  "High nitrogen" = "High Nitrogen"))) +  theme_bw(base_size = 17)) +
  draw_label("(C)", x = 0.9, y = 0.09, hjust = 0, vjust = 1,
             size = 20, fontface = "bold", color = "blue")


+ 
  geom_signif(comparisons = list(c("No", "Yes")),map_signif_level = TRUE, y_position = c(-28.5,-28)) + ylim(NA, -28)



ggsave("Treatment_d13C.jpg", width = 10, height = 7, units = "in", dpi = 600)



# fit model based on hypothesis (d15N)
d15N_lmer <- lmer(d15N ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID),data = merge_excel)

Anova(d15N_lmer)
summary(d15N_lmer)
hist(residuals(d15N_lmer))
shapiro.test(residuals(d15N_lmer)) # normal distribution

plot(d15N_lmer)

emmeans(d15N_lmer, pairwise ~  AMF  , adjust = "tukey")
emmeans(d15N_lmer, pairwise ~  Nutrient_treatment  , adjust = "tukey")


em_d15N <- as.data.frame(emmeans(d15N_lmer, ~AMF * Nutrient_treatment * Water_treatment))

# ggplot for d15N

Treatment_d15N <- ggdraw(ggplot(merge_excel, aes(y = d15N, x = AMF , color = AMF)) + geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_d15N, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_d15N, aes(x = AMF, y = emmean,ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y = expression(delta^{15}*N)) +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Water limited"),Nutrient_treatment = c("Low nitrogen" = "Low Nitrogen",  "High nitrogen" = "High Nitrogen"))) +  
  theme_bw(base_size = 17)) + draw_label("(B)", x = 0.9, y = 0.09, hjust = 0, vjust = 1,
                                        size = 20, fontface = "bold", color = "blue")

ggsave("Treatment_d15N.jpg", width = 10, height = 7, units = "in", dpi = 600)


# New plots, same results

library(patchwork)
library(ggh4x)

# Recode levels to match the other figures
em_d13C <- as.data.frame(emmeans(d13C_lmer, ~ AMF * Nutrient_treatment * Water_treatment)) %>%
  recode_trt()
em_d15N <- as.data.frame(emmeans(d15N_lmer, ~ AMF * Nutrient_treatment * Water_treatment)) %>%
  recode_trt()
iso_raw <- merge_excel %>% recode_trt()

# One plotting function for both isotopes
plot_trt <- function(em, raw, yvar, model, ylab) {
  ggplot(em, aes(Water_treatment, emmean, color = AMF)) +
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
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
    facet_wrap(~ Nutrient_treatment) +
    labs(x = NULL, y = ylab) +
    theme_classic(base_size = 14) +
    theme(legend.title = element_blank(),
          legend.position = "top",
          strip.background = element_blank(),
          strip.text = element_text(size = 14))
}

p_d13C <- plot_trt(em_d13C, iso_raw, "d13C", d13C_lmer,
                   expression(delta^{13}*C))
p_d15N <- plot_trt(em_d15N, iso_raw, "d15N", d15N_lmer,
                   expression(delta^{15}*N))

fig_iso <- make_fig(p_d13C, p_d15N, c("a", "b"))
fig_iso

ggsave("Isotopes_combined.png", fig_iso, width = 8, height = 9, dpi = 600)



