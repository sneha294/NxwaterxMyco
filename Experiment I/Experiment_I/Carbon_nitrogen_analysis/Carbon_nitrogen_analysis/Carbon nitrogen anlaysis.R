
setwd("~/Documents/Experiment_I/Carbon_nitrogen_analysis")

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

# merge_costech <- merge_costech %>% filter(Total_root_carbon_biomass < 150)

# Dry leaf weight
DLW_lmer <- lmer(`Dry Leaf weight (g)` ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_costech)

Anova(DLW_lmer)
summary(DLW_lmer)
hist(residuals(DLW_lmer))
shapiro.test(residuals(DLW_lmer))

em_DLW <- as.data.frame(emmeans(DLW_lmer, ~AMF * Nutrient_treatment * Water_treatment)) 


Treatment_DLW <- ggplot(merge_costech, aes(y = `Dry Leaf weight (g)`, x = AMF , color = AMF)) + geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_DLW, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_DLW, aes(x = AMF, y = emmean,ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y = "Dry leaf weight (g)") +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Drought Condition"),Nutrient_treatment = c("Low nitrogen" = "Low N",  "High nitrogen" = "High N"))) +  theme_bw(base_size = 17)  

ggsave("Treatment_Root.jpg", width = 10, height = 7, units = "in", dpi = 600)

# Root carbon biomass

Root_lmer <- lmer(Total_root_carbon_biomass ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_costech)

Anova(Root_lmer)
summary(Root_lmer)
hist(residuals(Root_lmer))
shapiro.test(residuals(Root_lmer)) 

plot(CC_lmer)

emmeans(Root_lmer, pairwise ~ Water_treatment * Nutrient_treatment, adjust = "tukey")

emmeans(Root_lmer, pairwise ~ AMF * Nutrient_treatment, adjust = "tukey")

emmeans(Root_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey")
emmeans(Root_lmer, pairwise ~ AMF, adjust = "tukey")


ggplot(merge_costech, aes(y = Total_root_carbon_biomass , x = Nutrient_treatment , color = AMF))+ geom_boxplot()

#ggplot 

em_Root <- as.data.frame(emmeans(Root_lmer, ~AMF * Nutrient_treatment * Water_treatment)) 


Treatment_Root <- ggplot(merge_costech, aes(y = Total_root_carbon_biomass, x = AMF , color = AMF)) + geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_Root, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_Root, aes(x = AMF, y = emmean,ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y = "Total root carbon biomass (gC)") +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Drought Condition"),Nutrient_treatment = c("Low nitrogen" = "Low N",  "High nitrogen" = "High N"))) +  theme_bw(base_size = 17)  

ggsave("Treatment_Root.jpg", width = 10, height = 7, units = "in", dpi = 600)

# Whole plant nitrogen biomass

WPN_lmer <- lmer(Whole_plant_nitrogen_biomass ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_costech)

Anova(WPN_lmer)
summary(WPN_lmer)
hist(residuals(WPN_lmer))
shapiro.test(residuals(WPN_lmer)) 

plot(WPN_lmer)

emmeans(WPN_lmer, pairwise ~ Water_treatment * Nutrient_treatment, adjust = "tukey")

emmeans(WPN_lmer, pairwise ~ AMF * Nutrient_treatment, adjust = "tukey")

emmeans(WPN_lmer, pairwise ~ Water_treatment * AMF, adjust = "tukey")

emmeans(WPN_lmer, pairwise ~ Water_treatment * Nutrient_treatment * AMF, adjust = "tukey")

emmeans(WPN_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey")
emmeans(WPN_lmer, pairwise ~ AMF, adjust = "tukey")



ggplot(merge_costech, aes(y = Whole_plant_nitrogen_biomass , x = Nutrient_treatment , color = AMF))+ geom_boxplot()


#ggplot 

em_WPN <- as.data.frame(emmeans(WPN_lmer, ~AMF * Nutrient_treatment * Water_treatment)) 


Treatment_WPN <- ggplot(merge_costech, aes(y = Whole_plant_nitrogen_biomass, x = AMF , color = AMF)) + geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_WPN, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_WPN, aes(x = AMF, y = emmean,ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y = "Whole plant nitrogen biomass (gN)") +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Drought Condition"),Nutrient_treatment = c("Low nitrogen" = "Low N",  "High nitrogen" = "High N"))) +  theme_bw(base_size = 17)  

ggsave("Treatment_WPN.jpg", width = 10, height = 7, units = "in", dpi = 600)


# Model for carbon cost to acquire and use nutrients

CC_lmer <- lmer(log(Carbon_cost_to_acquire_nutrients) ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_costech)

Anova(CC_lmer)
summary(CC_lmer)
hist(residuals(CC_lmer))
shapiro.test(residuals(CC_lmer)) 

plot(CC_lmer)

emmeans(CC_lmer, pairwise ~ Water_treatment * Nutrient_treatment, adjust = "tukey")

emmeans(CC_lmer, pairwise ~ AMF * Nutrient_treatment, adjust = "tukey")
emmeans(CC_lmer, pairwise ~ AMF, adjust = "tukey")

emmeans(CC_lmer, pairwise ~ Water_treatment * AMF, adjust = "tukey")

emmeans(CC_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey")


pairs(emmeans(CC_lmer, ~Nutrient_treatment))

# ggplot for Carbon cost to acquire and use nutrients

CC_nutrient <- as.data.frame(emmeans(CC_lmer, ~Nutrient_treatment))

Nuplot_CC <- ggplot(merge_costech, aes(y = Carbon_cost_to_acquire_nutrients, x = Nutrient_treatment, fill = Nutrient_treatment)) + 
  geom_boxplot(alpha = 0.5)  +
  geom_point(data = CC_nutrient, aes(y = emmean, x = Nutrient_treatment), 
             shape = 18, size = 4, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = CC_nutrient, aes(x = Nutrient_treatment, ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +
  theme_bw() +
  theme(legend.position = "none") +
  labs (x = "Nutrient treatment",
        y = "Carbon cost to acquire and use nutrients (gC/gN)")

ggsave("Nuplot_CC.jpg", width = 10, height = 7, units = "in", dpi = 600)


# emmeans for carbon cost
em_CC <- as.data.frame(emmeans(CC_lmer, ~AMF * Nutrient_treatment * Water_treatment, type = "response")) 

# ggplot for carbon cost nutrient (with signif)

Treatment_CC <- ggplot(merge_costech, aes(y = Carbon_cost_to_acquire_nutrients, x = AMF , color = AMF)) + geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_CC, aes(y = response, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_CC, aes(x = AMF, y = response,ymin = response - SE, ymax = response + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y = "Carbon cost to acquire and use nutrients (gC/gN)") +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Drought Condition"),Nutrient_treatment = c("Low nitrogen" = "Low N",  "High nitrogen" = "High N"))) +  theme_bw(base_size = 17)  + 
  geom_signif(comparisons = list(c("No", "Yes")), map_signif_level = TRUE, y_position = c(27, 29), bracket.size = 0.5, vjust = -0.05, size = 0.5) + ylim(5, 30)


ggsave("Treatment_CC.jpg", width = 10, height = 7, units = "in", dpi = 600)


# ggplot for carbon cost nutrient (without signif)

Treatment_CC_wsig <- ggplot(merge_costech, aes(y = Carbon_cost_to_acquire_nutrients, x = AMF , color = AMF)) + geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_CC, aes(y = response, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_CC, aes(x = AMF, y = response,ymin = response - SE, ymax = response + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y = "Carbon cost to acquire and use nutrients (gC/gN)") +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Drought Condition"),Nutrient_treatment = c("Low nitrogen" = "Low Nitrogen",  "High nitrogen" = "High Nitrogen"))) +  theme_bw(base_size = 17) + guides(color = "none") + 
  theme(axis.title = element_text(size = 20)) +theme(
    strip.text.x = element_text(size = 22),   
    strip.text.y = element_text(size = 22)    
  )   

ggsave("Treatment_CC_wsig.jpg", width = 10, height = 7, units = "in", dpi = 600)

# Transpiration

setwd("~/Documents/Experiment_I/Transpiration rate")

Transpiration <- read_excel("cumulative_transpiration.xlsx")
#Transpiration <- Transpiration %>% filter(Cumulative_transpiration_rate < 3500)

setwd("~/Documents/Experiment_I/Carbon_nitrogen_analysis")

Tran_lmer <- lmer(sqrt(Cumulative_transpiration_rate) ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = Transpiration) 

Anova(Tran_lmer)
summary(Tran_lmer)
hist(residuals(Tran_lmer))
shapiro.test(residuals(Tran_lmer)) 

plot(Tran_lmer)

emmeans(Tran_lmer, pairwise ~ Water_treatment * Nutrient_treatment, adjust = "tukey")

emmeans(Tran_lmer, pairwise ~ AMF * Nutrient_treatment, adjust = "tukey")

emmeans(Tran_lmer, pairwise ~ Water_treatment * AMF, adjust = "tukey")

emmeans(Tran_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey")
emmeans(Tran_lmer, pairwise ~ Water_treatment, adjust = "tukey")
emmeans(Tran_lmer, pairwise ~ AMF, adjust = "tukey")

ggplot(Transpiration, aes(y = Cumulative_transpiration_rate, x = AMF , color = AMF))+ geom_point()

Nutrient_tran <- as.data.frame(emmeans(Tran_lmer, ~Nutrient_treatment))
AMF_tran <-as.data.frame(emmeans(Tran_lmer, ~AMF))

# emmeans for transpiration
em_tran <- as.data.frame(emmeans(Tran_lmer, ~AMF * Nutrient_treatment * Water_treatment), type = "response")

# ggplot for Transpiration

Treatment_tran <- ggplot(Transpiration, aes(y = Cumulative_transpiration_rate, x = AMF , color = AMF)) + geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_tran, aes(y = response, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_tran, aes(x = AMF, y = response, ymin = response - SE, ymax = response + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +
  labs(x = "AMF Presence/Absence", 
       y = "Cumulative transpiration rate (gwater)") +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Drought Condition"),Nutrient_treatment = c("Low nitrogen" = "Low N",  "High nitrogen" = "High N"))) + theme_bw(base_size = 17) + 
  geom_signif(comparisons = list(c("No", "Yes")), map_signif_level = TRUE, y_position = c(3900, 4000), bracket.size = 0.5, vjust = -0.05, size = 0.5) + ylim(NA, 4500)
  

ggsave("Treatment_tran.jpg", width = 10, height = 7, units = "in", dpi = 600)

AMFplot_tran <- ggplot(Transpiration, aes(y = Cumulative_transpiration_rate, x = AMF, fill = AMF)) + 
  geom_boxplot(alpha = 0.5)  +
  geom_point(data = AMF_tran, aes(y = emmean, x = AMF), 
             shape = 18, size = 4, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = AMF_tran, aes(x = AMF, ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +
  theme_bw(base_size = 17) +
  theme(legend.position = "none") +
  labs (x = "AMF",
        y = "Cumulative transpiration rate (gwater)")

ggsave("AMFplot_tran.jpg", width = 10, height = 7, units = "in", dpi = 600)

merge_transpiration <-merge(Transpiration, Root_costech, by = "Sample")

merge_transpiration <- merge_transpiration %>%
  mutate(Carbon_cost_to_acquire_water = Total_root_carbon_biomass/ Cumulative_transpiration_rate)

# Carbon cost to acquire and use water

CCW_lmer <- lmer(Carbon_cost_to_acquire_water ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_transpiration)

Anova(CCW_lmer)
summary(CCW_lmer)
hist(residuals(CCW_lmer))
shapiro.test(residuals(CCW_lmer)) 

plot(CCW_lmer)

emmeans(CCW_lmer, pairwise ~ Water_treatment * Nutrient_treatment, adjust = "tukey")

emmeans(CCW_lmer, pairwise ~ AMF * Nutrient_treatment, adjust = "tukey")
emmeans(CCW_lmer, pairwise ~ AMF, adjust = "tukey")

emmeans(CCW_lmer, pairwise ~ Water_treatment * AMF, adjust = "tukey")

emmeans(CCW_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey")
emmeans(CCW_lmer, pairwise ~ Nutrient_treatment *Water_treatment * AMF, adjust = "tukey")

emmeans(CCW_lmer, pairwise ~ AMF, adjust = "tukey")



Nutrient_CCW <- as.data.frame(emmeans(CCW_lmer, ~Nutrient_treatment))
AMF_CCW <-as.data.frame(emmeans(CCW_lmer, ~AMF))

# emmeans for carbon cost
em_CCW <- as.data.frame(emmeans(CCW_lmer, ~AMF * Nutrient_treatment * Water_treatment))

# ggplot for CCW (with signif)

Treatment_CCW <- ggplot(merge_transpiration, aes(y = Carbon_cost_to_acquire_water, x = AMF , color = AMF)) + geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_CCW, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_CCW, aes(x = AMF, y = emmean,ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y = "Carbon cost to acquire and use water (gC/gwater)") +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Drought Condition"),Nutrient_treatment = c("Low" = "Low Nitrogen",  "High" = "High Nitrogen"))) +  theme_bw(base_size = 17)  + geom_signif(comparisons = list(c("No", "Yes")), map_signif_level = TRUE, y_position = c(0.075, 0.08), bracket.size = 0.5, vjust = -0.05, size = 0.5) + ylim(NA, 0.085)

ggsave("Treatment_CCW.jpg", width = 10, height = 7, units = "in", dpi = 600)

# ggplot for CCW (without signif)
Treatment_CCW_wsig <- ggplot(merge_transpiration, aes(y = Carbon_cost_to_acquire_water, x = AMF , color = AMF)) + geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_CCW, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_CCW, aes(x = AMF, y = emmean,ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y = "Carbon cost to acquire and use water (gC/gwater)") +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Drought Condition"),Nutrient_treatment = c("Low" = "Low Nitrogen", "High" = "High Nitrogen"))) +  theme_bw(base_size = 17) + guides(color = "none") + theme(axis.title = element_text(size = 22)) + theme(
               strip.text.x = element_text(size = 22),   
               strip.text.y = element_text(size = 22)    
             )

ggsave("Treatment_CCW_wsig.jpg", width = 10, height = 7, units = "in", dpi = 600)

Nuplot_CCW <- ggplot(merge_transpiration, aes(y = Carbon_cost_to_acquire_water, x = Nutrient_treatment, fill = Nutrient_treatment)) + 
  geom_boxplot(alpha = 0.5)  +
  geom_point(data = Nutrient_CCW, aes(y = emmean, x = Nutrient_treatment), 
             shape = 18, size = 4, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = Nutrient_CCW, aes(x = Nutrient_treatment, ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +
  theme_bw() +
  theme(legend.position = "none") +
  labs (x = "Nutrient treatment",
        y = "Carbon cost to acquire and use water (gC/gwater)")


ggsave("Nuplot_CCW.jpg", width = 10, height = 7, units = "in", dpi = 600)


AMFplot_CCW <- ggplot(merge_transpiration, aes(y = Carbon_cost_to_acquire_water, x = AMF, fill = AMF)) + 
  geom_boxplot(alpha = 0.5)  +
  geom_point(data = AMF_CCW, aes(y = emmean, x = AMF), 
             shape = 18, size = 4, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = AMF_CCW, aes(x = AMF, ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +
  theme_bw(base_size = 17) +
  theme(legend.position = "none") +
  labs (x = "AMF",
        y = "Carbon cost to acquire and use water (gC/gwater)")


ggsave("AMFplot_CCW.jpg", width = 10, height = 7, units = "in", dpi = 600)




# Beta

merge_beta <- merge(merge_costech, merge_transpiration[, c("Sample", "Carbon_cost_to_acquire_water")], by = "Sample")

merge_beta <- merge_beta %>% mutate(beta = `Carbon_cost_to_acquire_nutrients`/ `Carbon_cost_to_acquire_water`) 

#merge_beta <- merge_beta %>% filter(beta < 1500)


Beta_lmer <- lmer(sqrt(beta) ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_beta)

Anova(Beta_lmer)
summary(Beta_lmer)
hist(residuals(Beta_lmer))
shapiro.test(residuals(Beta_lmer)) 

plot(CCW_lmer)

ggplot(merge_beta, aes(y = beta, x = AMF , color = AMF))+ geom_boxplot()


pairs(emmeans(Beta_lmer, ~Nutrient_treatment))
pairs(emmeans(Beta_lmer, ~Water_treatment))
pairs(emmeans(Beta_lmer, ~AMF))

pairs(emmeans(Beta_lmer, ~AMF* Nutrient_treatment))

em_beta <- as.data.frame(emmeans(Beta_lmer, ~AMF * Nutrient_treatment * Water_treatment, type = "response"))

Treatment_beta <- ggplot(merge_beta, aes(y = beta, x = AMF , color = AMF)) + geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_beta, aes(y = response, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_beta, aes(x = AMF, y = response, ymin = response - SE, ymax = response + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y = "Carbon cost to acquire and use nutrients relative to water") +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Drought Condition"),Nutrient_treatment = c("Low nitrogen" = "Low N",  "High nitrogen" = "High N"))) +  theme_bw(base_size = 14)


ggsave("Treatment_beta.jpg", width = 10, height = 7, units = "in", dpi = 600)



# Total biomass

Total_biomass <- read_excel("dried_measurements.xlsx")

metadata <- read_excel("metadata.xlsx")

# merging metadata with dried measurement sheet
merge_biomass <-merge(Total_biomass, metadata, by = "Sample")

merge_biomass <- merge_biomass %>% mutate(`Total stem weight (g)` = `Dried Stem (g)` + `Total weight of the leaves (g)`, `Root shoot ratio` = `Total root weight (g)`/`Total stem weight (g)`) 


TB_lmer <- lmer(Total_biomass ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_biomass)

Anova(TB_lmer)
summary(TB_lmer)
hist(residuals(TB_lmer))
shapiro.test(residuals(TB_lmer)) 

plot(CC_lmer)

ggplot(merge_biomass, aes(y = Total_biomass , x = Nutrient_treatment , color = AMF))+ geom_boxplot()


emmeans(TB_lmer, pairwise ~ Water_treatment * Nutrient_treatment, adjust = "tukey") 
emmeans(TB_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey")


# emmeans for biomass

em_biomass <- as.data.frame(emmeans(TB_lmer, ~AMF * Nutrient_treatment * Water_treatment))


# ggplot with facet grid

Treatment_TB <- ggplot(merge_biomass, aes(y = Total_biomass, x = AMF , color = AMF)) + 
  geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_biomass, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_biomass, aes(x = AMF, y = emmean ,ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y =  "Total Biomass (g)") +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Drought Condition"),Nutrient_treatment = c("Low nitrogen" = "Low Nitrogen", "High nitrogen" = "High Nitrogen"))) +  theme_bw(base_size = 17) + guides(color = "none") +
  theme(axis.title = element_text(size = 22)) +theme(
    strip.text.x = element_text(size = 22),   
    strip.text.y = element_text(size = 22)    
  )   


ggsave("Treatment_TB.jpg", width = 10, height = 7, units = "in", dpi = 600)

Nuem_TB <- as.data.frame(emmeans(TB_lmer, ~Nutrient_treatment))

Nuplot_TB <- ggplot(merge_biomass, aes(y = Total_biomass, x = Nutrient_treatment, fill = Nutrient_treatment)) + 
  geom_boxplot(alpha = 0.5)  +
  geom_point(data = Nuem_TB, aes(y = emmean, x = Nutrient_treatment), 
             shape = 18, size = 4, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = Nuem_TB, aes(x = Nutrient_treatment, ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +
  theme_bw(base_size = 17) +
  theme(legend.position = "none") +
  labs (x = "Nutrient Treatment",
        y = "Total biomass")


ggsave("Nuplot_TB.jpg", width = 10, height = 7, units = "in", dpi = 600)


# Total leaf area

TLA_lmer <- lmer(Total_leaf_area ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_biomass)

Anova(TLA_lmer)
summary(TLA_lmer)
hist(residuals(TLA_lmer))
shapiro.test(residuals(TLA_lmer)) 

plot(CC_lmer)

emmeans(TLA_lmer, pairwise ~ Water_treatment, adjust = "tukey")
emmeans(TLA_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey")
emmeans(TLA_lmer, pairwise ~ AMF, adjust = "tukey")

emmeans(TLA_lmer, pairwise ~ AMF * Nutrient_treatment, adjust = "tukey")


emmeans(TLA_lmer, pairwise ~ Water_treatment * Nutrient_treatment * AMF, adjust = "tukey")

emmeans(TLA_lmer, pairwise ~ AMF * Nutrient_treatment, adjust = "tukey") 
# posthoc analysis shows there is no difference in TLA in low nitrogen regardless of AMF presence


ggplot(merge_costech, aes(y = Total_root_carbon_biomass , x = Nutrient_treatment , color = AMF))+ geom_boxplot()

# emmeans for TLA

em_TLA <- as.data.frame(emmeans(TLA_lmer, ~AMF * Nutrient_treatment * Water_treatment))


# ggplot with facet grid

Treatment_TLA <- ggplot(merge_biomass, aes(y = Total_leaf_area, x = AMF , color = AMF)) + 
  geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_TLA, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_TLA, aes(x = AMF, y = emmean ,ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y =  expression("Total leaf area (cm"^2*")")) +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Drought Condition"),Nutrient_treatment = c("Low nitrogen" = "Low N", "High nitrogen" = "High N"))) +  theme_bw(base_size = 17)


ggsave("Treatment_TLA.jpg", width = 10, height = 7, units = "in", dpi = 600)


NuAMF_TLA <- as.data.frame(emmeans(TLA_lmer, ~AMF * Nutrient_treatment))
  

NuAMFplot_TLA <- ggplot(merge_biomass,
  aes(x = AMF,
    y = Total_leaf_area,
    color = Nutrient_treatment,
    group = Nutrient_treatment
  )
) +geom_jitter(
    size = 2, alpha = 0.8,
    position = position_dodge(width = 0.8)
  ) + geom_point(
    data = NuAMF_TLA,
    aes(
      x = AMF,
      y = emmean,
      group = Nutrient_treatment
    ),
    shape = 18, size = 3, color = "black",
    position = position_dodge(width = 0.8),
    inherit.aes = FALSE
  ) + geom_errorbar(
    data = NuAMF_TLA,
    aes(
      x = AMF,
      ymin = emmean - SE,
      ymax = emmean + SE,
      group = Nutrient_treatment
    ),
    width = 0.15, color = "black", alpha = 0.6,
    position = position_dodge(width = 0.8),
    inherit.aes = FALSE
  ) + scale_color_manual(values = c("red", "dodgerblue")) +
  labs(
    x = "AMF Presence / Absence",
    y = "Total leaf area (cm²)",   # cm² looks cleaner than cm2
    color = "Nutrient treatment"
  ) +  theme_bw(base_size = 16) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("NuAMFplot_TLA.jpg", width = 10, height = 7, units = "in", dpi = 600)


NuWW_TLA <- as.data.frame(emmeans(TLA_lmer, ~Water_treatment * Nutrient_treatment))


ggplot(merge_biomass,
       aes(x = Water_treatment,
           y = Total_leaf_area,
           color = Nutrient_treatment,
           group = Nutrient_treatment
       )
) +geom_jitter(
  size = 2, alpha = 0.8,
  position = position_dodge(width = 0.8)
) + geom_point(
  data = NuWW_TLA,
  aes(x = Water_treatment,
    y = emmean,
    group = Nutrient_treatment
  ),
  shape = 18, size = 3, color = "black",
  position = position_dodge(width = 0.8),
  inherit.aes = FALSE
) + geom_errorbar(
  data = NuWW_TLA,
  aes(
    x = Water_treatment,
    ymin = emmean - SE,
    ymax = emmean + SE,
    group = Nutrient_treatment
  ),
  width = 0.15, color = "black", alpha = 0.6,
  position = position_dodge(width = 0.8),
  inherit.aes = FALSE
) + scale_color_manual(values = c("red", "dodgerblue")) +
  labs(
    x = "Water treatment",
    y = "Total leaf area (cm²)",   # cm² looks cleaner than cm2
    color = "Nutrient treatment"
  ) +  theme_bw(base_size = 16) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


# Root shoot ratio

RS_lmer <- lmer(`Root shoot ratio` ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_biomass)

Anova(RS_lmer)
summary(RS_lmer)
hist(residuals(RS_lmer))
shapiro.test(residuals(RS_lmer)) 

plot(CC_lmer)

# Tukey's test post hoc analysis

emmeans(RS_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey")
emmeans(RS_lmer, pairwise ~ AMF, adjust = "tukey")

emmeans(RS_lmer, pairwise ~ Water_treatment * Nutrient_treatment, adjust = "tukey")

emmeans(RS_lmer, pairwise ~ AMF * Nutrient_treatment, adjust = "tukey") 

emmeans(RS_lmer, pairwise ~ Water_treatment * AMF, adjust = "tukey")


# emmeans for TLA

em_RS <- as.data.frame(emmeans(RS_lmer, ~AMF * Nutrient_treatment * Water_treatment))


# ggplot with facet grid

Treatment_RS <- ggplot(merge_biomass, aes(y = `Root shoot ratio`, x = AMF , color = AMF)) + 
  geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_RS, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_RS, aes(x = AMF, y = emmean ,ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y =  "Root-Shoot ratio") +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Drought Condition"),Nutrient_treatment = c("Low nitrogen" = "Low N", "High nitrogen" = "High N"))) +  theme_bw(base_size = 17)


ggsave("Treatment_RS.jpg", width = 10, height = 7, units = "in", dpi = 600)

# Narea, LMA, Nmass



Nmass <- read_excel("Nmass.xlsx")

metadata <- read_excel("metadata.xlsx")

merge_Nmass <- merge(Nmass, metadata, by = "Sample")

# LMA

LMA_lmer <- lmer(log(LMA) ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_Nmass)

Anova(LMA_lmer)
summary(LMA_lmer)
hist(residuals(LMA_lmer))
shapiro.test(residuals(LMA_lmer)) 

plot(LMA_lmer)

# Tukey's test post hoc analysis

emmeans(LMA_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey")
emmeans(LMA_lmer, pairwise ~ AMF, adjust = "tukey")

emmeans(LMA_lmer, pairwise ~ AMF * Nutrient_treatment, adjust = "tukey") 

emmeans(LMA_lmer, pairwise ~ Water_treatment * AMF , adjust = "tukey")

emmeans(LMA_lmer, pairwise ~ Water_treatment * AMF * Nutrient_treatment, adjust = "tukey")


# emmeans for LMA

em_LMA <- as.data.frame(emmeans(LMA_lmer, ~AMF * Nutrient_treatment * Water_treatment, type = "response"))


# ggplot with facet grid

Treatment_LMA <- ggplot(merge_Nmass, aes(y = LMA, x = AMF , color = AMF)) + 
  geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_LMA, aes(y = response, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_LMA, aes(x = AMF, y = response ,ymin = response - SE, ymax = response + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y =  expression("Leaf mass per area (g/cm"^2*")")) +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Drought Condition"),Nutrient_treatment = c("Low nitrogen" = "Low N", "High nitrogen" = "High N"))) +  theme_bw(base_size = 17)


ggsave("Treatment_LMA.jpg", width = 10, height = 7, units = "in", dpi = 600)

#Nmass

Nmass_lmer <- lmer(Nmass ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_Nmass)

Anova(Nmass_lmer)
summary(Nmass_lmer)
hist(residuals(Nmass_lmer))
shapiro.test(residuals(Nmass_lmer)) 

plot(Nmass_lmer)

# Tukey's test post hoc analysis

emmeans(Nmass_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey")
emmeans(Nmass_lmer, pairwise ~ AMF, adjust = "tukey")

emmeans(Nmass_lmer, pairwise ~ AMF * Nutrient_treatment, adjust = "tukey") 

emmeans(Nmass_lmer, pairwise ~ Water_treatment * Nutrient_treatment, adjust = "tukey")

emmeans(Nmass_lmer, pairwise ~ AMF * Water_treatment, adjust = "tukey") 

emmeans(Nmass_lmer, pairwise ~ AMF * Water_treatment * Nutrient_treatment, adjust = "tukey") 

# emmeans for Nmass

em_Nmass <- as.data.frame(emmeans(Nmass_lmer, ~AMF * Nutrient_treatment * Water_treatment))


# ggplot with facet grid

Treatment_Nmass <- ggplot(merge_Nmass, aes(y = Nmass, x = AMF , color = AMF)) + 
  geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_Nmass, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_Nmass, aes(x = AMF, y = emmean ,ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y =  expression("Leaf Nitrogen content (gN g"^-1*")")) +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Drought Condition"),Nutrient_treatment = c("Low nitrogen" = "Low N", "High nitrogen" = "High N"))) +  theme_bw(base_size = 17)


ggsave("Treatment_Nmass.jpg", width = 10, height = 7, units = "in", dpi = 600)

#Narea

Narea_lmer <- lmer(Narea ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_Nmass)

Anova(Narea_lmer)
summary(Narea_lmer)
hist(residuals(Narea_lmer))
shapiro.test(residuals(Narea_lmer)) 

plot(Narea_lmer)

# Tukey's test post hoc analysis

emmeans(Narea_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey")
emmeans(Narea_lmer, pairwise ~ AMF, adjust = "tukey")

emmeans(Narea_lmer, pairwise ~ AMF * Nutrient_treatment, adjust = "tukey") 

emmeans(Narea_lmer, pairwise ~ Water_treatment * Nutrient_treatment, adjust = "tukey")

emmeans(Narea_lmer, pairwise ~ AMF * Water_treatment, adjust = "tukey") 

emmeans(Narea_lmer, pairwise ~ AMF * Water_treatment * Nutrient_treatment, adjust = "tukey") 

# emmeans for Nmass

em_Narea <- as.data.frame(emmeans(Narea_lmer, ~AMF * Nutrient_treatment * Water_treatment))


# ggplot with facet grid

Treatment_Narea <- ggplot(merge_Nmass, aes(y = Narea, x = AMF , color = AMF)) + 
  geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_Narea, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_Narea, aes(x = AMF, y = emmean ,ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y =  expression("Nitrogen per unit area of leaf (gN/cm"^2*")")) +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Drought Condition"),Nutrient_treatment = c("Low nitrogen" = "Low N", "High nitrogen" = "High N"))) +  theme_bw(base_size = 17)


ggsave("Treatment_Narea.jpg", width = 10, height = 7, units = "in", dpi = 600)

