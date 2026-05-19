library(dplyr)
library(readxl)
library(tidyr)
library(car)
library(emmeans)
library(lme4)
library(ggplot2)

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

emmeans(d13C_lmer, pairwise ~  AMF * Water_treatment , adjust = "tukey")

emmeans(d13C_lmer, pairwise ~  AMF , adjust = "tukey")
emmeans(d13C_lmer, pairwise ~  AMF * Nutrient_treatment , adjust = "tukey")

AMF_d13C <- as.data.frame(emmeans(d13C_lmer, ~AMF))


em_d13C <- as.data.frame(emmeans(d13C_lmer, ~AMF * Nutrient_treatment * Water_treatment))

# ggplot for d13C

Treatment_d13C <- ggplot(merge_excel, aes(y = d13C, x = AMF , color = AMF)) + geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_d13C, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_d13C, aes(x = AMF, y = emmean,ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y = expression(delta^{13}*C)) +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Drought Condition"),Nutrient_treatment = c("Low nitrogen" = "Low N",  "High nitrogen" = "High N"))) +  theme_bw(base_size = 14) + 
  geom_signif(comparisons = list(c("No", "Yes")),map_signif_level = TRUE, y_position = c(-28.5,-28)) + ylim(NA, -28)



ggsave("Treatment_d13C.jpg", width = 10, height = 7, units = "in", dpi = 600)


AMFplot_d13C <- ggplot(merge_excel, aes(y = d13C, x = AMF, fill = AMF)) + 
  geom_boxplot(alpha = 0.5)  +
  geom_point(data = AMF_d13C, aes(y = emmean, x = AMF), 
             shape = 18, size = 4, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = AMF_d13C, aes(x = AMF, ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +
  theme_bw() +
  theme(legend.position = "none") +
  labs (x = "AMF",
        y = expression(delta^{13}*C))


ggsave("AMFplot_d13C.jpg", width = 10, height = 7, units = "in", dpi = 600)


# fit model based on hypothesis (d15N)
d15N_lmer <- lmer(d15N ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID),data = merge_excel)

Anova(d15N_lmer)
summary(d15N_lmer)
hist(residuals(d15N_lmer))
shapiro.test(residuals(d15N_lmer)) # normal distribution

plot(d15N_lmer)

emmeans(d15N_lmer, pairwise ~  AMF  , adjust = "tukey")
emmeans(d15N_lmer, pairwise ~  Nutrient_treatment  , adjust = "tukey")
emmeans(d15N_lmer, pairwise ~  AMF * Nutrient_treatment  , adjust = "tukey")
emmeans(d15N_lmer, pairwise ~  AMF * Water_treatment  , adjust = "tukey")

Nutrient_d15N <- as.data.frame(emmeans(d15N_lmer, ~Nutrient_treatment))
AMF_d15N <- as.data.frame(emmeans(d15N_lmer, ~AMF))


em_d15N <- as.data.frame(emmeans(d15N_lmer, ~AMF * Nutrient_treatment * Water_treatment))

# ggplot for CCW

Treatment_d15N <- ggplot(merge_excel, aes(y = d15N, x = AMF , color = AMF)) + geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_d15N, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_d15N, aes(x = AMF, y = emmean,ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y = expression(delta^{15}*N)) +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Drought Condition"),Nutrient_treatment = c("Low nitrogen" = "Low N",  "High nitrogen" = "High N"))) +  theme_bw(base_size = 16)

ggsave("Treatment_d15N.jpg", width = 10, height = 7, units = "in", dpi = 600)


AMFplot_d15N <- ggplot(merge_excel, aes(y = d15N, x = AMF, fill = AMF)) + 
  geom_boxplot(alpha = 0.5)  +
  geom_point(data = AMF_d15N, aes(y = emmean, x = AMF), 
             shape = 18, size = 4, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = AMF_d15N, aes(x = AMF, ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  labs (x = "AMF",
        y = expression(delta^{15}*N))


ggsave("AMFplot_d15N.jpg", width = 10, height = 7, units = "in", dpi = 600)

Nuplot_d15N <- ggplot(merge_excel, aes(y = d15N, x = Nutrient_treatment, fill = Nutrient_treatment)) + 
  geom_boxplot(alpha = 0.5)  +
  geom_point(data = Nutrient_d15N, aes(y = emmean, x = Nutrient_treatment), 
             shape = 18, size = 4, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = Nutrient_d15N, aes(x = Nutrient_treatment, ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +
  theme_bw() +
  theme(legend.position = "none") +
  labs (x = "Nutrient treatment",
        y = expression(delta^{15}*N))


ggsave("Nuplot_d15N.jpg", width = 10, height = 7, units = "in", dpi = 600)





