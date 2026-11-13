setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment I/Experiment_I/Hobo readings/HOBO_tempRH")
library(tidyverse)
library(lubridate)
library(readxl)

rh_folder    <- "~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment I/Experiment_I/Hobo readings/HOBO_tempRH"        # folder with the 4 RH-sensor Excel files
light_file   <- "~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment I/Experiment_I/Hobo readings/Hobo light/block 3_light.xlsx"
growth_start <- ymd("2024-05-27")
growth_end   <- ymd("2024-07-29")
light_threshold <- 1000            
temp_in_fahrenheit <- FALSE  


parse_dt <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  parse_date_time(as.character(x),
                  orders = c("mdy HMS", "mdy HM", "mdy IMS p", "ymd HMS"))
}


rh_files <- list.files(rh_folder, pattern = "tempRH.*\\.xlsx?$", full.names = TRUE)
rh_files <- rh_files[!grepl("^~\\$", basename(rh_files))]   # remove ~$ lock files
basename(rh_files)
file.exists(rh_files)

read_rh <- function(path) {
  raw <- read_excel(path, skip = 0)
  dt_col   <- names(raw)[str_detect(names(raw), regex("date.*time", ignore_case = TRUE))][1]
  temp_col <- names(raw)[str_detect(names(raw), regex("temp",       ignore_case = TRUE))][1]
  rh_col   <- names(raw)[str_detect(names(raw), regex("rh|humidit",  ignore_case = TRUE))][1]
  tibble(
    sensor_id = tools::file_path_sans_ext(basename(path)),
    datetime  = parse_dt(raw[[dt_col]]),
    temp      = as.numeric(raw[[temp_col]]),
    rh        = as.numeric(raw[[rh_col]])
  )
}

rh <- map_dfr(rh_files, read_rh)
if (temp_in_fahrenheit) rh <- rh %>% mutate(temp = (temp - 32) * 5/9)


lt_raw <- read_excel(light_file, skip = 0)
lt_dt    <- names(lt_raw)[str_detect(names(lt_raw), regex("date.*time", ignore_case = TRUE))][1]
lt_light <- names(lt_raw)[str_detect(names(lt_raw), regex("light|lux|intens", ignore_case = TRUE))][1]

light <- tibble(
  datetime = parse_dt(lt_raw[[lt_dt]]),
  light    = as.numeric(lt_raw[[lt_light]])
) %>%
  mutate(hour = hour(datetime))

day_hours <- light %>%
  group_by(hour) %>%
  summarise(mean_light = mean(light, na.rm = TRUE), .groups = "drop") %>%
  filter(mean_light > light_threshold) %>%
  pull(hour)

day_hours   


rh_day <- rh %>%
  mutate(date = as_date(datetime), hour = hour(datetime)) %>%
  filter(date >= growth_start, date <= growth_end) %>%
  filter(hour %in% day_hours) %>%
  mutate(
    esat   = 611.0 * exp(17.27 * temp / (temp + 237.3)),  # Pa
    vpd_Pa = esat * (1 - rh / 100)                        # Pa
  ) %>%
  filter(!is.na(temp), !is.na(vpd_Pa))

sensor_summary <- rh_day %>%
  group_by(sensor_id) %>%
  summarise(
    Tg_C       = mean(temp,   na.rm = TRUE),
    Dg_Pa      = mean(vpd_Pa, na.rm = TRUE),
    first_day  = min(date),
    last_day   = max(date),
    n_readings = n(),
    .groups = "drop"
  )

print(sensor_summary)

window_start <- ymd("2024-07-01")
window_end   <- ymd("2024-07-29")

july_climate <- rh_day %>%
  filter(date >= window_start, date <= window_end) %>%
  group_by(sensor_id) %>%
  summarise(Tg_C = mean(temp), Dg_Pa = mean(vpd_Pa),
            first_day = min(date), last_day = max(date), n = n(),
            .groups = "drop")
print(july_climate)

# temp 29
# VPD 1707


# calculating chi and beta, Following Cheaib et al. (2025)

#  helper functions
calc_patm <- function(z) {
  kPo=101325; kTo=298.15; kL=0.0065; kG=9.80665; kR=8.3143; kMa=0.028963
  kPo * (1.0 - kL*z/kTo)^(kG*kMa/(kR*kL))
}

# viscosity and density of water (Huber et al. 2009) 

calc_density_h2o <- function(temp, z) {   
  patm = calc_patm(z)
  my_lambda = 1788.316 + 1.55053*temp - 0.4695911*temp^2 +
    (3.096363e-3)*temp^3 - (7.341182e-6)*temp^4
  po = 5918.499 + 58.05267*temp - 1.1253317*temp^2 +
    (6.6123869e-3)*temp^3 - (1.4661625e-5)*temp^4
  vinf = 0.6980547 - (7.435626e-4)*temp + (3.704258e-5)*temp^2 -
    (6.315724e-7)*temp^3 + (9.829576e-9)*temp^4 -
    (1.197269e-10)*temp^5 + (1.005461e-12)*temp^6 -
    (5.437898e-15)*temp^7 + (1.69946e-17)*temp^8 -
    (2.295063e-20)*temp^9
  pbar = (1e-5)*patm
  vau = vinf + my_lambda/(po + pbar)
  (1e3/vau)                              
}

calc_viscosity_h2o <- function(temp, z) {  
  tk_ast  = 647.096; rho_ast = 322.0; mu_ast = 1e-6
  rho = calc_density_h2o(temp, z)
  tbar = (temp + 273.15)/tk_ast
  rbar = rho/rho_ast
  mu0 = 1.67752 + 2.20462/tbar + 0.6366564/tbar^2 - 0.241605/tbar^3
  mu0 = 1e2*sqrt(tbar)/mu0
  h_array = rbind(
    c(0.520094, 0.0850895, -1.08374, -0.289555, 0.0, 0.0),
    c(0.222531, 0.999115, 1.88797, 1.26613, 0.0, 0.120573),
    c(-0.281378, -0.906851, -0.772479, -0.489837, -0.257040, 0.0),
    c(0.161913, 0.257399, 0.0, 0.0, 0.0, 0.0),
    c(-0.0325372, 0.0, 0.0, 0.0698452, 0.0, 0.0),
    c(0.0, 0.0, 0.0, 0.0, 0.00872102, 0.0),
    c(0.0, 0.0, 0.0, -0.00435673, 0.0, -0.000593264)
  )
  mu1 = 0.0
  for (i in 1:6) {
    coef2 = 0.0
    for (j in 1:7) coef2 = coef2 + h_array[j, i] * (rbar - 1.0)^(j-1)
    mu1 = mu1 + (((1.0/tbar) - 1.0)^(i-1)) * coef2
  }
  mu1 = exp(rbar * mu1)
  mu0 * mu1 * mu_ast                      
}

calc_nstar <- function(temp, z) calc_viscosity_h2o(temp, z) / calc_viscosity_h2o(25, z)

calc_gammastar_pa <- function(temp, z) {
  rat = calc_patm(z)/calc_patm(0)
  gammastar25 = 4.332 * rat; Hgm = 37830; R = 8.314
  temp_k = 273.15 + temp
  gammastar25 * exp((Hgm/R)*(1/298.15 - 1/temp_k))
}

calc_km_pa <- function(temp, z) {
  patm = calc_patm(z); rat = patm/calc_patm(0); R = 8.314; O2 = 2.09476e5
  Kc25 = 41.03*rat; Ko25 = 28210*rat; Hkc = 79430; Hko = 36380
  temp_k = 273.15 + temp
  Kc = Kc25*exp(Hkc*((temp_k-298.15)/(298.15*R*temp_k)))
  Ko = Ko25*exp(Hko*((temp_k-298.15)/(298.15*R*temp_k)))
  Kc * (1 + (O2*1e-6*patm)/Ko)
}

#  greenhouse conditions from my experiment
tmp    <- 29.0    # July daytime mean temperature, celcius (HOBO sensor)
z      <- 990     # Lubbock elevation, m
CO2    <- 424.61     # ambient greenhouse CO2, ppm (from NOAA)
vpd_Pa <- 1707    # July daytime mean VPD, already in Pa
air    <- -8    # atmospheric d13C, 2024

# environmental terms, all derived from tmp, z, CO2
patm  <- calc_patm(z)
ca    <- CO2 * 1e-6 * patm          # CO2 partial pressure, Pa
gstar <- calc_gammastar_pa(tmp, z)  # Gammastar, Pa
Km    <- calc_km_pa(tmp, z)         # K, Pa (only used in beta)
nstar <- calc_nstar(tmp, z)         # relative viscosity

 
a <- 4.4; b <- 28; f <- 12

setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment I/Experiment_I/UC davis stable isotope")


stable_isotope <- read_excel("stable_isotope.xlsx")

metadata <- read_excel("metadata.xlsx")

merge_excel <- merge(stable_isotope, metadata, by = "Sample")

data_clean <- merge_excel            


# Eqn 1: discrimination
data_clean$big_D13 <- (air - data_clean$d13C) / (1 + data_clean$d13C/1000)
# Eqn 2: chi
data_clean$chi <- (data_clean$big_D13 - a + f*gstar/ca) / (b - a)
# Eqn 5: beta
data_clean$beta <- 1.6 * nstar * vpd_Pa * (data_clean$chi - gstar/ca)^2 /
  ((1 - data_clean$chi)^2 * (Km + gstar))

# ---- checks ----
summary(data_clean$chi);  hist(data_clean$chi)
summary(data_clean$beta); hist(data_clean$beta)

Betaleaf_lmer <- lmer(beta ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = data_clean)

Anova(Betaleaf_lmer)
summary(Betaleaf_lmer)
hist(residuals(Betaleaf_lmer))
shapiro.test(residuals(Betaleaf_lmer)) 

plot(Betaleaf_lmer)

pairs(emmeans(Betaleaf_lmer, ~AMF))


em_betaleaf <- as.data.frame(emmeans(Betaleaf_lmer, ~AMF * Nutrient_treatment * Water_treatment))

Treatment_betaleaf <- ggplot(data_clean, aes(y = beta, x = AMF , color = AMF)) + geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_betaleaf, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_betaleaf, aes(x = AMF, y = emmean, ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y = "Leaf β") +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Drought Condition"),Nutrient_treatment = c("Low nitrogen" = "Low N",  "High nitrogen" = "High N"))) +  theme_bw(base_size = 14)


ggsave("Treatment_beta.jpg", width = 10, height = 7, units = "in", dpi = 600)
