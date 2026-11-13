setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment I/Experiment_I/Chlorophyll content")
library(tidyr)
library(dplyr)
library(cowplot)

## This script can be opened from Snehanjana's one drive

chlorophyll_data_plate_1 <- read_xlsx("Chatterjee_chlorophyll_plate1.xlsx")


chlorophyll_data_plate_2 <- read_xlsx("Chatterjee_chlorophyll_plate2.xlsx")

##combine both plate 1 and 2
both_plates <- as.data.frame(full_join(chlorophyll_data_plate_1, chlorophyll_data_plate_2))

# Reading the leaf Punch area
disc_area <- read.csv("disc_areas.csv")

# making the ids all lower case
disc_area$id <- tolower(disc_area$id)


## Get average and one row per id
chlor.df <- (both_plates) %>% 
  filter(id != "blk") %>%
  dplyr::select(id, everything()) %>%
  group_by(id) %>%
  summarize(abs.649 = mean(abs_649, na.rm = F),
            cv.649 = (sd(abs_649, na.rm = F) / abs.649) * 100,
            abs.665 = mean(abs_665, na.rm = F),
            cv.665 = (sd(abs_665, na.rm = F) / abs.665) * 100) %>%
  mutate(abs.649 = ifelse(abs.649 < 0, 0, abs.649),
         abs.665 = ifelse(abs.665 < 0, 0, abs.665))


disc_area <- disc_area %>%
  mutate(id = as.character(id))

## combine disc area with chlorophyll file
chlorophyll <- chlor.df %>% 
  left_join(disc_area, by = "id") %>%
  dplyr::select(id, abs.649, abs.665, leaf.punch.area) %>%
  mutate(chlA.ugml = (12.19 * abs.665) - (3.45 * abs.649),
         chlB.ugml = (21.99 * abs.649) - (5.32 * abs.665),
         chlA.ugml = ifelse(chlA.ugml < 0, 0, chlA.ugml),
         chlB.ugml = ifelse(chlB.ugml < 0, 0, chlB.ugml),
         chlA.gml = chlA.ugml / 1000000,
         chlB.gml = chlB.ugml / 1000000,
         chlA.g = chlA.gml * 10, # extracted in 10mL DMSO
         chlB.g = chlB.gml * 10, # extracted in 10mL DMSO
         chlA.gm2 = chlA.g / (leaf.punch.area / 10000),
         chlB.gm2 = chlB.g / (leaf.punch.area / 10000),
         chlA.mmolm2 = chlA.gm2 / 893.51 * 1000,
         chlB.mmolm2 = chlB.gm2 / 907.47 * 1000,
         chl.mmolm2 = chlA.mmolm2 + chlB.mmolm2,
         chlA.chlB = chlA.g / chlB.g) %>%
  dplyr::select(id, chlA.ugml:chlA.chlB)

chl_for_stats <- chlorophyll %>%
  dplyr::select(id, chlA.mmolm2, chlB.mmolm2, chl.mmolm2, chlA.chlB)

write.csv(chl_for_stats,
          "~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment I/Experiment_I/Chlorophyll content/chlorophyll_calculations.csv")

Metadata_exp1 <- read_excel("Metadata_exp1.xlsx")
merge_chl <- merge(chl_for_stats, Metadata_exp1, by = "id")

# Total chlorophyll
chl_lmer <- lmer(log(chl.mmolm2) ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID),data = merge_chl)

Anova(chl_lmer)
summary(chl_lmer)
hist(residuals(chl_lmer))
shapiro.test(residuals(chl_lmer)) 

plot(chl_lmer)
emmeans(chl_lmer, pairwise ~  Nutrient_treatment * Water_treatment, adjust = "tukey", type ="response")
emmeans(chl_lmer, pairwise ~  AMF * Nutrient_treatment * Water_treatment, adjust = "tukey",type ="response")

# emmeans for Total chl

em_chl <- as.data.frame(emmeans(chl_lmer, ~AMF * Nutrient_treatment * Water_treatment, type ="response"))

# ggplot for Rd

Treatment_chl <- ggdraw(ggplot(merge_chl, aes(y = chl.mmolm2 , x = AMF , color = AMF)) + 
  geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_chl, aes(y = response, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_chl, aes(x = AMF, y = response,ymin = response - SE, ymax = response + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y =expression(Total~chlorophyll~(mmol~m^{-2}))) +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Water limited"),
                                 Nutrient_treatment = c("Low nitrogen" = "Low Nitrogen",  "High nitrogen" = "High Nitrogen"))) +  
  theme_bw(base_size = 17)) +  draw_label("(D)", x = 0.9, y = 0.09, hjust = 0, vjust = 1,
                                           size = 20, fontface = "bold", color = "blue")

ggsave("Treatment_chl.jpg", width = 10, height = 7, units = "in", dpi = 600)

# New plots, same results
recode_trt <- function(d) {
  d %>% mutate(
    Water_treatment = recode(Water_treatment,
                             "Drought" = "Low water supply",
                             "Well watered" = "High water supply"),
    Nutrient_treatment = recode(Nutrient_treatment,
                                "High nitrogen" = "High Nitrogen",
                                "Low nitrogen" = "Low Nitrogen"))
}

# emmeans on the log scale, back-transformed with ±1 SE
em_chl <- as.data.frame(emmeans(chl_lmer, ~ AMF * Nutrient_treatment * Water_treatment)) %>%
  mutate(lo = exp(emmean - SE),
         hi = exp(emmean + SE),
         emmean = exp(emmean)) %>%
  recode_trt()

chl_raw <- merge_chl %>% recode_trt()   # <- your raw chlorophyll data frame

p_chl <- ggplot(em_chl, aes(Water_treatment, emmean, color = AMF)) +
  geom_point(data = chl_raw, aes(y = chl.mmolm2),
             size = 2.5, alpha = 0.3, position = jdodge) +
  geom_errorbar(aes(ymin = lo, ymax = hi),
                width = 0.2, linewidth = 1, position = dodge) +
  geom_point(shape = 15, size = 5, position = dodge) +
  geom_text(data = tibble(Nutrient_treatment = "Low Nitrogen", label = sig_label(chl_lmer)),
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
       y = expression("Total chlorophyll"~"("*mmol~m^-2*")")) +
  theme_classic(base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "top",
        strip.background = element_blank(),
        strip.text = element_text(size = 14))

p_chl


# chlorophyll a : b
chl_ratio_lmer <- lmer(chlA.chlB ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID),data = merge_chl)

Anova(chl_ratio_lmer)
summary(chl_ratio_lmer)
hist(residuals(chl_ratio_lmer))
shapiro.test(residuals(chl_ratio_lmer)) 

plot(chl_ratio_lmer)
emmeans(chl_ratio_lmer, pairwise ~  Nutrient_treatment, adjust = "tukey")
emmeans(chl_ratio_lmer, pairwise ~  Nutrient_treatment * Water_treatment, adjust = "tukey")
emmeans(chl_ratio_lmer, pairwise ~  AMF * Nutrient_treatment * Water_treatment, adjust = "tukey")










