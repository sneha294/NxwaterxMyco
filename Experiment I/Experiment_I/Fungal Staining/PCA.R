library(dplyr)

base_dir <- "~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment I/Experiment_I"

# Old master (keeps all the unchanged traits)
master <- read.csv(file.path(base_dir, "ExpI_master_data.csv")) %>%
  mutate(Sample = trimws(Sample))

# New values, renamed to match the master column names
new_LMA <- merge_Nmass %>%
  transmute(Sample = trimws(Sample), LMA = LMA)

new_bio <- merge_biomass %>%
  transmute(Sample = trimws(Sample),
            Total_biomass    = Total_biomass,
            Root_shoot_ratio = `Root shoot ratio`)

new_CC <- merge_costech %>%
  transmute(Sample = trimws(Sample),
            Carbon_cost_nutrients = Carbon_cost_to_acquire_nutrients)

# Remove the old columns, add the new ones
master_new <- master %>%
  select(-LMA, -Total_biomass, -Root_shoot_ratio, -Carbon_cost_nutrients) %>%
  left_join(new_LMA, by = "Sample") %>%
  left_join(new_bio, by = "Sample") %>%
  left_join(new_CC,  by = "Sample") %>%
  mutate(Narea = LMA * Nmass)   # Narea depends on LMA, so update it too

write.csv(master_new, file.path(base_dir, "ExpI_master_data_v2.csv"), row.names = FALSE)

library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(lme4)
library(car)
library(vegan)
library(ggrepel)
library(patchwork)
library(ggh4x)
library(flextable)
library(officer)

base_dir <- "~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment I/Experiment_I"

# ---- Data: updated master file (new LMA, biomass, root:shoot, C cost N) ----
master <- read.csv(file.path(base_dir, "ExpI_master_data_v2.csv")) |>
  mutate(Sample = trimws(Sample))

# ---- Join isotopes ----
iso <- read_excel(file.path(base_dir, "UC davis stable isotope", "stable_isotope.xlsx")) |>
  mutate(Sample = trimws(Sample)) |>
  dplyr::select(Sample, d13C, d15N)

anti_join(master, iso, by = "Sample") |> dplyr::select(Sample)   # should be empty
anti_join(iso, master, by = "Sample") |> dplyr::select(Sample)   # should be empty

master <- left_join(master, iso, by = "Sample")

# ---- Traits ----
traits <- c("Carbon_cost_nutrients", "Carbon_cost_water", "Total_biomass",
            "Total_leaf_area", "Root_shoot_ratio", "LMA", "Nmass",
            "Vcmax", "Jmax", "d13C", "d15N")
# Narea left out: it equals LMA x Nmass

# Labels for the table
trait_labels <- c(Carbon_cost_nutrients = "C cost (N)",  Carbon_cost_water = "C cost (water)",
                  Total_biomass = "Biomass",            Total_leaf_area   = "Total leaf area",
                  Root_shoot_ratio = "Root:shoot",      LMA = "LMA", Nmass = "Nmass",
                  Vcmax = "Vcmax", Jmax = "Jmax",
                  d13C = "δ¹³C",         d15N = "δ¹⁵N")

# Labels for the arrows in the biplots (plotmath, so subscripts/superscripts work)
plot_labels <- c(Carbon_cost_nutrients = "C~cost~(N)", Carbon_cost_water = "C~cost~(water)",
                 Total_biomass = "Biomass", Total_leaf_area = "Total~leaf~area",
                 Root_shoot_ratio = "Root:shoot", LMA = "LMA", Nmass = "N[mass]",
                 Vcmax = "italic(V)[cmax]", Jmax = "italic(J)[max]",
                 d13C = "delta^13*C", d15N = "delta^15*N")

pca_dat <- master |>
  dplyr::select(Sample, Block_ID, Water_treatment, Nutrient_treatment, AMF, all_of(traits)) |>
  drop_na(all_of(traits)) |>
  mutate(Water_treatment    = factor(Water_treatment, levels = c("Low water supply", "High water supply")),
         Nutrient_treatment = factor(Nutrient_treatment, levels = c("High Nitrogen", "Low Nitrogen")),
         AMF                = factor(AMF, levels = c("Absent", "Present")),
         Block_ID           = factor(Block_ID),
         Carbon_cost_nutrients = log(Carbon_cost_nutrients),
         Root_shoot_ratio      = log(Root_shoot_ratio),
         LMA                   = log(LMA))

nrow(pca_dat)   # should be 45

# ---- PCA ----
pca     <- prcomp(pca_dat[, traits], center = TRUE, scale. = TRUE)
summary(pca)
var_exp <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)

# Which PCs to interpret (eigenvalue > 1)
data.frame(PC = paste0("PC", seq_along(var_exp)),
           eigenvalue = round(pca$sdev^2, 2),
           var_pct = var_exp,
           cum_pct = cumsum(var_exp))

plot(var_exp, type = "b", xlab = "Principal component", ylab = "Variance explained (%)")

round(pca$rotation[, 1:4], 2)   # loadings PC1-PC4

scores <- bind_cols(pca_dat, as.data.frame(pca$x[, 1:4]))

# ---- Treatment effects on PC1-PC4 ----
for (pc in paste0("PC", 1:4)) {
  cat("\n====", pc, "====\n")
  m <- lmer(reformulate("Water_treatment * Nutrient_treatment * AMF + (1 | Block_ID)",
                        response = pc), data = scores)
  print(Anova(m, type = 2))
}

# ---- Whole trait space (PERMANOVA) ----
set.seed(1)
perm_mod <- adonis2(scale(pca_dat[, traits]) ~ Block_ID +
                      Water_treatment * Nutrient_treatment * AMF,
                    data = pca_dat, method = "euclidean", by = "terms",
                    permutations = how(nperm = 999, blocks = pca_dat$Block_ID))
perm_mod

# ---- Biplots ----
amf_cols <- c(Absent = "#D55E00", Present = "#009E73")

make_biplot <- function(pcx, pcy) {
  sc <- scores %>% mutate(X = .data[[pcx]], Y = .data[[pcy]])
  
  # Loading arrows, scaled to the score cloud
  ld <- as.data.frame(pca$rotation[, c(pcx, pcy)])
  names(ld) <- c("X", "Y")
  ld$trait <- plot_labels[rownames(ld)]
  k  <- 0.85 * max(abs(c(sc$X, sc$Y))) / max(abs(c(ld$X, ld$Y)))
  ld <- ld %>% mutate(X = X * k, Y = Y * k, lab_x = X * 1.07, lab_y = Y * 1.07)
  
  # Centroids +/- SE for all 8 treatment combinations
  cent <- sc %>%
    group_by(Water_treatment, Nutrient_treatment, AMF) %>%
    summarise(m1 = mean(X), se1 = sd(X) / sqrt(n()),
              m2 = mean(Y), se2 = sd(Y) / sqrt(n()), .groups = "drop")
  
  # Filled = high water (AMF colour), open = low water
  fill_key <- function(d) ifelse(d$Water_treatment == "Low water supply", "open", as.character(d$AMF))
  sc$fk   <- fill_key(sc)
  cent$fk <- fill_key(cent)
  
  # Same limits on both axes, so a square panel keeps arrow angles correct
  rng <- range(c(sc$X, sc$Y, ld$lab_x, ld$lab_y)) * 1.08
  
  ix <- as.integer(sub("PC", "", pcx))
  iy <- as.integer(sub("PC", "", pcy))
  
  ggplot() +
    geom_hline(yintercept = 0, colour = "grey88") +
    geom_vline(xintercept = 0, colour = "grey88") +
    geom_point(data = sc, aes(X, Y, colour = AMF, shape = Nutrient_treatment, fill = fk),
               size = 2, alpha = 0.35, stroke = 0.7) +
    geom_segment(data = ld, aes(x = 0, y = 0, xend = X, yend = Y),
                 arrow = arrow(length = unit(0.13, "cm")),
                 colour = "grey55", linewidth = 0.4) +
    geom_text_repel(data = ld, aes(lab_x, lab_y, label = trait),
                    parse = TRUE, size = 3.2, colour = "grey30", seed = 1,
                    min.segment.length = 0, segment.colour = "grey75",
                    segment.size = 0.3, box.padding = 0.15, point.padding = 0,
                    force = 0.5, max.overlaps = Inf) +
    geom_errorbar(data = cent, aes(x = m1, ymin = m2 - se2, ymax = m2 + se2, colour = AMF),
                  width = 0, linewidth = 0.7) +
    geom_errorbarh(data = cent, aes(y = m2, xmin = m1 - se1, xmax = m1 + se1, colour = AMF),
                   height = 0, linewidth = 0.7) +
    geom_point(data = cent, aes(m1, m2, colour = AMF, shape = Nutrient_treatment, fill = fk),
               size = 4, stroke = 1.3) +
    scale_colour_manual(values = amf_cols, name = "AMF",
                        labels = c(Absent = "Absent", Present = "Present"),
                        guide = guide_legend(order = 1,
                                             override.aes = list(shape = 16, size = 3, alpha = 1))) +
    scale_shape_manual(values = c("High Nitrogen" = 21, "Low Nitrogen" = 24), name = "Nitrogen",
                       labels = c("High Nitrogen" = "High", "Low Nitrogen" = "Low"),
                       guide = guide_legend(order = 2,
                                            override.aes = list(fill = "grey30", colour = "grey30",
                                                                size = 3, alpha = 1))) +
    scale_fill_manual(values = c(Absent = "#D55E00", Present = "#009E73", open = "white"),
                      breaks = c("Absent", "open"),
                      labels = c("High", "Low"), name = "Water supply",
                      guide = guide_legend(order = 3,
                                           override.aes = list(shape = 22, colour = "grey30",
                                                               fill = c("grey30", "white"),
                                                               size = 3, alpha = 1))) +
    coord_cartesian(xlim = rng, ylim = rng, expand = FALSE, clip = "off") +
    labs(x = paste0(pcx, " (", var_exp[ix], "%)"),
         y = paste0(pcy, " (", var_exp[iy], "%)")) +
    theme_classic(base_size = 14) +
    theme(legend.position = "right",
          legend.box = "vertical",
          legend.title = element_text(size = 11, face = "bold"),
          legend.text = element_text(size = 11),
          legend.key.size = unit(0.4, "cm"))
}

panel_size <- force_panelsizes(cols = unit(3.3, "in"), rows = unit(3.3, "in"))

p_pc12 <- make_biplot("PC1", "PC2")
p_pc13 <- make_biplot("PC1", "PC3")
p_pc14 <- make_biplot("PC1", "PC4")

# All three panels together (a, b, c)
fig_pca <- (p_pc12 / p_pc13 / p_pc14) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = list(c("a", "b", "c"))) &
  panel_size &
  theme(legend.position = "right",
        legend.box = "vertical",
        legend.spacing.y = unit(0.3, "cm"),
        plot.tag = element_text(size = 14))

fig_pca

ggsave(file.path(base_dir, "Figure_PCA_ExpI_v2.png"), fig_pca,
       width = 8, height = 13, dpi = 600, device = ragg::agg_png)

# ---- Loadings table (CSV + Word) ----
load_tab <- as.data.frame(round(pca$rotation[, 1:4], 3))
load_tab <- cbind(Trait = trait_labels[rownames(load_tab)], load_tab)
load_tab <- rbind(load_tab,
                  data.frame(Trait = "Variance explained (%)",
                             PC1 = var_exp[1], PC2 = var_exp[2],
                             PC3 = var_exp[3], PC4 = var_exp[4]),
                  data.frame(Trait = "Eigenvalue",
                             PC1 = round(pca$sdev[1]^2, 2), PC2 = round(pca$sdev[2]^2, 2),
                             PC3 = round(pca$sdev[3]^2, 2), PC4 = round(pca$sdev[4]^2, 2)))

write.csv(load_tab, file.path(base_dir, "PCA_loadings_ExpI_11traits_v2.csv"), row.names = FALSE)

pc_heads <- setNames(paste0("PC", 1:4, " (", sprintf("%.1f", var_exp[1:4]), "%)"),
                     paste0("PC", 1:4))

ft <- flextable(load_tab) |>
  set_header_labels(values = as.list(pc_heads)) |>
  colformat_double(digits = 2) |>
  hline(i = 11, border = fp_border(width = 1)) |>
  autofit()

ft

save_as_docx(ft, path = file.path(base_dir, "Table_S_PCA_loadings_v2.docx"))
