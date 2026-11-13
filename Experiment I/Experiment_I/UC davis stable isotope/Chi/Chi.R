
setwd("~/Documents/Experiment_I/UC davis stable isotope")
stable_isotope <- read_excel("stable_isotope.xlsx")


calc_chi_c3 <- function(leaf.d13c = NA, air = NA, year = NA) {
  
  ## Derive d13C in air as a function of calendar year, 
  ## as done in Feng et al. (1999), with option to directly modify
  ## air fractionation value
  if(is.na(year) & is.na(air)) {air =-8}
  if(is.na(year) & !is.na(air)) {air = air}
  if(!is.na(year) & is.na(air)) {air = -6.429 - 0.006*exp(0.0217*(year-1740))}
  
  # Global constants
  a = 4.4
  b = 27
  
  # Leaf carbon discrimination relative to air
  delta <- (air - leaf.d13c) / (1 + leaf.d13c * 0.001)
  chi <- (delta - a) / (b - a)
  
  return(data.frame(delta = delta, chi = chi))
  
}

Ci_Ca <- calc_chi_c3(leaf.d13c = stable_isotope$d13C) [2]
stable_isotope <- cbind(stable_isotope,Ci_Ca)


metadata <- read_excel("metadata.xlsx")

# merging metadata with stable isotope excel sheet
merge_1 <-merge(stable_isotope, metadata, by = "Sample")

#merge_1 <- merge_1 %>% filter(chi > 0.80)


## Running statistics on chi

Chi_lmer <- lmer(chi ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_1)

Anova(Chi_lmer)  # AMF treatment is significant
summary(Chi_lmer)
hist(residuals(Chi_lmer)) # not normally distributed (even after log transformation)
shapiro.test(residuals(Chi_lmer)) 

plot(Chi_lmer)

emmeans(Chi_lmer, pairwise ~ AMF)

ggplot(merge_1, aes(y = chi, x = AMF , color = AMF))+ geom_boxplot()


# emmeans for Chi

em_chi <- as.data.frame(emmeans(Chi_lmer, ~AMF * Nutrient_treatment * Water_treatment))


# ggplot with facet grid

Treatment_Chi <- ggplot(merge_1, aes(y = chi, x = AMF , color = AMF)) + 
  geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_chi, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_chi, aes(x = AMF, y = emmean ,ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y =  expression(C[i]/C[a])) +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Drought Condition"),Nutrient_treatment = c("Low nitrogen" = "Low Nitrogen", "High nitrogen" = "High Nitrogen"))) +  theme_bw(base_size = 17) + guides(color = "none") +
  theme(axis.title = element_text(size = 22)) +theme(
    strip.text.x = element_text(size = 22),   
    strip.text.y = element_text(size = 22)    
  )   


ggsave("Treatment_Chi.jpg", width = 10, height = 7, units = "in", dpi = 600)


# plot with emmeans showing significant difference with AMF treatment

em_df <- as.data.frame(emmeans(Chi_lmer, ~AMF))


AMFplot_chi <- ggplot(merge_1, aes(y = chi, x = AMF, fill = AMF)) + 
  geom_boxplot(alpha = 0.5)  +
  geom_point(data = em_df, aes(y = emmean, x = AMF), 
             shape = 18, size = 4, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_df, aes(x = AMF, ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +
  theme_bw(base_size = 17) +
  theme(legend.position = "none") +
  labs (x = "AMF",
        y = expression(C[i]/C[a]))


ggsave("AMFplot_chi.jpg", width = 10, height = 7, units = "in", dpi = 600)







