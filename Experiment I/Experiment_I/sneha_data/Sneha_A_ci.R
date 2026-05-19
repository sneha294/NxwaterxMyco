setwd("~/Documents/Experiment_I/ sneha_data")

library(readLicorData)
library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
library(reshape)
library(car)
library(emmeans)
library(multcompView)
library(lme4)
library(ggplot2)

#light curves from Gibson

setwd("~/Documents/Experiment_I/ sneha_data/sneha_myco_2024_Gibson")

## read all unix licor files and convert into .csv format.
## The following loop should run only once.

files <- list.files()

for (i in 1:length(files)) {
  a <- licorData(location = files[i])
  write.csv(a, paste0(files[i], ".csv"), row.names = FALSE)
}


#light curves from Ozzie

setwd("~/Documents/Experiment_I/ sneha_data/sneha_myco_2024_ozzie/")

## read all unix licor files and convert into .csv format.
## The following loop should run only once.

files <- list.files()

for (i in 1:length(files)) {
  a <- licorData(location = files[i])
  write.csv(a, paste0(files[i], ".csv"), row.names = FALSE)
}

#light curves from Yadi

setwd("~/Documents/Experiment_I/ sneha_data/sneha_myco_2024_Yadi/")

## read all unix licor files and convert into .csv format.
## The following loop should run only once.

files <- list.files()

for (i in 1:length(files)) {
  a <- licorData(location = files[i])
  write.csv(a, paste0(files[i], ".csv"), row.names = FALSE)
}

a <- read.csv("2024-07-21-1040_aci.csv")
a_final<- a %>% select(obs, elapsed, date, id, A , Ci, Ca, gsw, CO2_s, CO2_r,
                      H2O_s, H2O_r, Qin, VPDleaf, Flow, Tair) %>% mutate(machine_name = "Yadi")
 
b <- read.csv("2024-07-21-respiration.csv")
b_final<- b %>% select(obs, elapsed, date, id, A , Ci, Ca, gsw, CO2_s, CO2_r,
                       H2O_s, H2O_r, Qin, VPDleaf, Flow, Tair) %>% mutate(machine_name = "Yadi")

c <- read.csv("2024-07-22-sneha_myco_aci.csv")
c_final<- c %>% select(obs, elapsed, date, id, A , Ci, Ca, gsw, CO2_s, CO2_r,
                       H2O_s, H2O_r, Qin, VPDleaf, Flow, Tair) %>% mutate(machine_name = "Yadi")

c_resp <- c_final %>% filter(id %in% c("LNDNoMyco6_resp", "LNWWMyco6_resp"))
c_final <- c_final %>% filter(!id %in% c("LNDNoMyco6_resp", "LNWWMyco6_resp"))

d <- read.csv("aci_20_7_2024_yadi.csv")
d_final<- d %>% select(obs, elapsed, date, id, A , Ci, Ca, gsw, CO2_s, CO2_r,
                       H2O_s, H2O_r, Qin, VPDleaf, Flow, Tair) %>% mutate(machine_name = "Yadi")

d_resp <- d_final %>% filter(id %in% "hnwwmyco4_resp")
d_final <- d_final %>% filter(!id %in% "hnwwmyco4_resp")


e <- read.csv("2024-07-20-a_ci.csv")
e_final<- e %>% select(obs, elapsed, date, id, A , Ci, Ca, gsw, CO2_s, CO2_r,
                       H2O_s, H2O_r, Qin, VPDleaf, Flow, Tair) %>% mutate(machine_name = "Ozzie")

e_resp <- e_final %>% filter(id %in% "HnwwnoMyco4_resp")
e_final <- e_final %>% filter(!id %in% "HnwwnoMyco4_resp")

f <- read.csv("2024-07-21-respiration.csv")
f_final<- f %>% select(obs, elapsed, date, id, A , Ci, Ca, gsw, CO2_s, CO2_r,
                       H2O_s, H2O_r, Qin, VPDleaf, Flow, Tair) %>% mutate(machine_name = "Ozzie")

g <- read.csv("2024-07-21-sneha_aci.csv")
g_final<- g %>% select(obs, elapsed, date, id, A , Ci, Ca, gsw, CO2_s, CO2_r,
                       H2O_s, H2O_r, Qin, VPDleaf, Flow, Tair) %>% mutate(machine_name = "Ozzie")

h <- read.csv("2024-07-22-sneha_myco_aci.csv")
h_final<- h %>% select(obs, elapsed, date, id, A , Ci, Ca, gsw, CO2_s, CO2_r,
                       H2O_s, H2O_r, Qin, VPDleaf, Flow, Tair) %>% mutate(machine_name = "Ozzie")

h_resp <- h_final %>% filter(id %in% c("HNDmyco6_resp", "HNDnomyco6_resp"))
h_final <- h_final %>% filter(!id %in% c("HNDmyco6_resp", "HNDnomyco6_resp"))

i <- read.csv("2024-07-20-a_ci.csv")
i_final<- i %>% select(obs, elapsed, date, id, A , Ci, Ca, gsw, CO2_s, CO2_r,
                       H2O_s, H2O_r, Qin, VPDleaf, Flow, Tair) %>% mutate(machine_name = "Gibson")

i_resp <- i_final %>% filter(id %in% "hndmyco4_resp")
i_final <- i_final %>% filter(!id %in% "hndmyco4_resp")

j <- read.csv("2024-07-21-1050_aci.csv")
j_final<- j %>% select(obs, elapsed, date, id, A , Ci, Ca, gsw, CO2_s, CO2_r,
                       H2O_s, H2O_r, Qin, VPDleaf, Flow, Tair) %>% mutate(machine_name = "Gibson")

j_final <- j_final %>% filter(!id %in% NA)

k <- read.csv("2024-07-21respiration.csv")
k_final<- k %>% select(obs, elapsed, date, id, A , Ci, Ca, gsw, CO2_s, CO2_r,
                       H2O_s, H2O_r, Qin, VPDleaf, Flow, Tair) %>% mutate(machine_name = "Gibson")

l <- read.csv("2024-07-22-sneha_myco_aci.csv")
l_final<- l %>% select(obs, elapsed, date, id, A , Ci, Ca, gsw, CO2_s, CO2_r,
                       H2O_s, H2O_r, Qin, VPDleaf, Flow, Tair) %>% mutate(machine_name = "Gibson")

l_resp <- l_final %>% filter(id %in% c("HNWWnomyco6_resp", "LNWWnomyco6_resp"))
l_final <- l_final %>% filter(!id %in% c("HNWWnomyco6_resp", "LNWWnomyco6_resp"))

#A_Ci curves

A_ci_merged <- rbind(a_final, c_final, d_final, e_final, g_final, h_final, i_final, j_final, l_final)

A_ci_merged$id <- tolower(A_ci_merged$id)


# respiration

resp_merged <- rbind(b_final, c_resp, d_resp, e_resp, f_final, h_resp, i_resp, k_final, l_resp)

resp_merged$id <- tolower(resp_merged$id)

library(plantecophys)

unique(A_ci_merged$id)

ids <- levels(as.factor(e$id))


#hndmyco1

e_pre = subset(e, id == ids[1])
e_pre$id <- tolower(e_pre$id)
e_pre[, c(6,9:10)]

hndmyco1 <- fitaci(filter(A_ci_merged,id == "hndmyco1"), # should be e_pre
                   varnames = list(ALEAF="A",
                   Tleaf= "Tair",
                   Ci= "Ci",
                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= FALSE)

summary(hndmyco1)
plot(hndmyco1)
hndmyco1_coef <- t(coef(hndmyco1))


hndmyco1_dataframe <- cbind(e_pre[1, c(8,7,13, 15, 18,29,71)],
                               hndmyco1[[2]][1,1],
                               hndmyco1[[2]][1,2],
                               hndmyco1[[2]][2,1],
                               hndmyco1[[2]][2,2],
                               hndmyco1[[2]][3,1],
                               hndmyco1[[2]][3,2],
                               hndmyco1$RMSE,
                               hndmyco1$Ci_transition,
                               hndmyco1$citransition,
                               hndmyco1$Km,
                               hndmyco1$GammaStar,
                               hndmyco1$fitmethod,
                               hndmyco1$Tcorrect,
                               hndmyco1$fitTPU)





colnames(hndmyco1_dataframe) <- c('id', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                  'vpd_leaf', 'temperature_leaf','vcmax_tleaf', 'vcmax_tleaf_se',
                                  'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                  'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- c()

curve_fits <- rbind(curve_fits, hndmyco1_dataframe)


#hndmyco2

hndmyco2 <- fitaci(filter(A_ci_merged, id == "hndmyco2"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tair",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= FALSE)

summary(hndmyco2)
plot(hndmyco2)
hndmyco2_coef <- t(coef(hndmyco2)) 

#Vcmax    Jmax       Rd      TPU
#100.5949 163.818 1.125495 8.051971

#hndmyco3

hndmyco3 <- fitaci(filter(A_ci_merged, id == "hndmyco3"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tair",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= FALSE)

summary(hndmyco3)
plot(hndmyco3)
hndmyco3_coef <- t(coef(hndmyco3)) 

# Vcmax    Jmax       Rd      TPU
# 107.1501 186.761 2.593131 8.193236

#hndmyco4    SHOW TO PAWAN

hndmyco4 <- fitaci(filter(A_ci_merged, id == "hndmyco4"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tair",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= FALSE)

summary(hndmyco4)
plot(hndmyco4)
hndmyco4_coef <- t(coef(hndmyco4)) 

# Vcmax     Jmax         Rd      TPU
# 98.16553 164.1666 -0.2217587 6.934647

#hndmyco5    

hndmyco5 <- fitaci(filter(A_ci_merged, id == "hndmyco5"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tair",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= FALSE)

summary(hndmyco5)
plot(hndmyco5)
hndmyco5_coef <- t(coef(hndmyco5))

# Vcmax     Jmax        Rd      TPU
# 91.74865 151.2432 0.7494005 8.936833

#hndmyco6    

hndmyco6 <- fitaci(filter(A_ci_merged, id == "hndmyco6"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tair",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= FALSE)

summary(hndmyco6)
plot(hndmyco6)
hndmyco6_coef<- t(coef(hndmyco6))

# Vcmax     Jmax         Rd      TPU
# 115.7039 188.6463 -0.8440063 8.820997


# hnwwmyco1

hnwwmyco1 <- fitaci(filter(A_ci_merged, id == "hnwwmyco1"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tair",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= FALSE)

summary(hnwwmyco1)
plot(hnwwmyco1)
hnwwmyco1_coef <- t(coef(hnwwmyco1))

#  Vcmax     Jmax       Rd      TPU
# 103.0672 173.6172 4.045219 9.007756

# hnwwmyco2

hnwwmyco2 <- fitaci(filter(A_ci_merged, id == "hnwwmyco2"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tair",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= FALSE)

summary(hnwwmyco2)
plot(hnwwmyco2)
hnwwmyco2_coef <- t(coef(hnwwmyco2))

# Vcmax     Jmax         Rd      TPU
# 72.31164 111.8749 -0.9000916 5.503447

# hnwwmyco3

hnwwmyco3 <- fitaci(filter(A_ci_merged, id == "hnwwmyco3"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tair",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= FALSE)

summary(hnwwmyco3)
plot(hnwwmyco3)
hnwwmyco3_coef<- t(coef(hnwwmyco3))

# Vcmax    Jmax         Rd     TPU
# 83.38614 137.421 -0.6028219 6.01827

# hnwwmyco4

hnwwmyco4 <- fitaci(filter(A_ci_merged, id == "hnwwmyco4"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tair",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= FALSE)

summary(hnwwmyco4)
plot(hnwwmyco4)
hnwwmyco4_coef<- t(coef(hnwwmyco4))

# Vcmax     Jmax          Rd      TPU
# 86.01628 151.8702 -0.07216575 6.453966

# hnwwmyco5

hnwwmyco5 <- fitaci(filter(A_ci_merged, id == "hnwwmyco5"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tair",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= FALSE)

summary(hnwwmyco5)
plot(hnwwmyco5)
hnwwmyco5_coef <- t(coef(hnwwmyco5))

# Vcmax     Jmax         Rd      TPU
# 89.52132 148.3899 -0.4032587 7.391021

# # hnwwmyco6

hnwwmyco6 <- fitaci(filter(A_ci_merged, id == "hnwwmyco6"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tair",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= FALSE)

summary(hnwwmyco6)
plot(hnwwmyco6)
hnwwmyco6_coef <- t(coef(hnwwmyco6))

# Vcmax     Jmax        Rd     TPU
# 72.2083 124.2499 0.3030885 6.60064

# lndmyco1

lndmyco1 <- fitaci(filter(A_ci_merged, id == "lndmyco1"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tair",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= FALSE)

summary(lndmyco1)
plot(lndmyco1)
lndmyco1_coef <- t(coef(lndmyco1))

# Vcmax     Jmax       Rd      TPU
# 126.6601 195.1757 4.761979 9.241215

# lndmyco2

lndmyco2 <- fitaci(filter(A_ci_merged, id == "lndmyco2"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tair",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= FALSE)

summary(lndmyco2)
plot(lndmyco2)
lndmyco2_coef<- t(coef(lndmyco2))

# Vcmax     Jmax       Rd      TPU
# 71.31537 117.2189 1.330153 8.181987

# lndmyco3

lndmyco3 <- fitaci(filter(A_ci_merged, id == "lndmyco3"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tair",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= FALSE)

summary(lndmyco3)
plot(lndmyco3)
lndmyco3_coef <-t(coef(lndmyco3))

#  Vcmax    Jmax       Rd      TPU
# 121.2551 192.543 3.208357 9.059802

# lndmyco4

lndmyco4 <- fitaci(filter(A_ci_merged, id == "lndmyco4"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tair",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= FALSE)

summary(lndmyco4)
plot(lndmyco4)
lndmyco4_coef <- t(coef(lndmyco4))

#  Vcmax     Jmax       Rd      TPU
# 110.953 196.9068 3.599949 8.396761

# lndmyco5

lndmyco5 <- fitaci(filter(A_ci_merged, id == "lndmyco5"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tair",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= FALSE)

summary(lndmyco5)
plot(lndmyco5)
lndmyco5_coef <- t(coef(lndmyco5))

#Vcmax     Jmax        Rd      TPU
# 29.7468 45.62098 0.8700913 3.228913

# lndmyco6 ID does not exist

# lnwwmyco1

lnwwmyco1 <- fitaci(filter(A_ci_merged, id == "lnwwmyco1"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tair",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= FALSE)

summary(lnwwmyco1)
plot(lnwwmyco1)
lnwwmyco1_coef <- t(coef(lnwwmyco1))

# Vcmax     Jmax       Rd      TPU
# 71.74292 111.1404 4.069162 6.820721


# lnwwmyco2

lnwwmyco2 <- fitaci(filter(A_ci_merged, id == "lnwwmyco2"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tair",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= FALSE)

summary(lnwwmyco2)
plot(lnwwmyco2)
lnwwmyco2_coef <- t(coef(lnwwmyco2))

#  Vcmax     Jmax        Rd      TPU
# 89.23258 150.9023 -0.490827 7.275184


# lnwwmyco3

lnwwmyco3 <- fitaci(filter(A_ci_merged, id == "lnwwmyco3"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tair",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= FALSE)

summary(lnwwmyco3)
plot(lnwwmyco3)
lnwwmyco3_coef <- t(coef(lnwwmyco3))

# Vcmax     Jmax    Rd      TPU
# 89.36956 150.4499 2.822 8.710953

# lnwwmyco4

lnwwmyco4 <- fitaci(filter(A_ci_merged, id == "lnwwmyco4"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tair",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= FALSE)

summary(lnwwmyco4)
plot(lnwwmyco4)
lnwwmyco4_coef<- t(coef(lnwwmyco4))

#  Vcmax     Jmax       Rd      TPU
# 105.6411 172.2098 1.845606 8.414152

# lnwwmyco5 ID does not exist

# lnwwmyco6 

lnwwmyco6 <- fitaci(filter(A_ci_merged, id == "lnwwmyco6"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tair",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= FALSE)

summary(lnwwmyco6)
plot(lnwwmyco6)
lnwwmyco6_coef<- t(coef(lnwwmyco6))

# Vcmax     Jmax       Rd      TPU
# 85.25894 152.6817 3.209779 8.569972

# hndnomyco1

hndnomyco1 <- fitaci(filter(A_ci_merged, id == "hndnomyco1"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tair",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= FALSE)

summary(hndnomyco1)
plot(hndnomyco1)
hndnomyco1_coef<- t(coef(hndnomyco1))

# Vcmax     Jmax       Rd     TPU
# 107.7791 158.8406 1.768956 8.94222

# hndnomyco2

hndnomyco2 <- fitaci(filter(A_ci_merged, id == "hndnomyco2"),
                     varnames = list(ALEAF="A",
                                     Tleaf= "Tair",
                                     Ci= "Ci",
                                     PPFD= "Qin"),
                     fitTPU = T,
                     Tcorrect= FALSE)

summary(hndnomyco2)
plot(hndnomyco2)
hndnomyco2_coef<- t(coef(hndnomyco2))

# Vcmax     Jmax       Rd      TPU
# 90.46998 149.8816 1.965216 8.782249

# hndnomyco3

hndnomyco3 <- fitaci(filter(A_ci_merged, id == "hndnomyco3"),
                     varnames = list(ALEAF="A",
                                     Tleaf= "Tair",
                                     Ci= "Ci",
                                     PPFD= "Qin"),
                     fitTPU = T,
                     Tcorrect= FALSE)

summary(hndnomyco3)
plot(hndnomyco3)
hndnomyco3_coef<- t(coef(hndnomyco3))

# Vcmax     Jmax       Rd      TPU
# 108.8978 191.1193 4.668871 9.914075

# hndnomyco4    # added citransition

hndnomyco4 <- fitaci(filter(A_ci_merged, id == "hndnomyco4"),
                     varnames = list(ALEAF="A",
                                     Tleaf= "Tair",
                                     Ci= "Ci",
                                     PPFD= "Qin"),
                     citransition = 300,
                     fitTPU = T,
                     Tcorrect= FALSE)

summary(hndnomyco4)
plot(hndnomyco4)
hndnomyco4_coef<- t(coef(hndnomyco4))

# Vcmax     Jmax        Rd TPU
# 79.21368 114.5677 0.5499458  NA

#  hndnomyco5     

hndnomyco5 <- fitaci(filter(A_ci_merged, id == "hndnomyco5"),
                     varnames = list(ALEAF="A",
                                     Tleaf= "Tair",
                                     Ci= "Ci",
                                     PPFD= "Qin"),
                     fitTPU = T,
                     Tcorrect= FALSE)

summary(hndnomyco5)
plot(hndnomyco5)
hndnomyco5_coef<- t(coef(hndnomyco5))

# Vcmax     Jmax        Rd      TPU
# 109.3583 180.9578 0.2855238 8.710744

#  hndnomyco6     

hndnomyco6 <- fitaci(filter(A_ci_merged, id == "hndnomyco6"),
                     varnames = list(ALEAF="A",
                                     Tleaf= "Tair",
                                     Ci= "Ci",
                                     PPFD= "Qin"),
                     fitTPU = T,
                     Tcorrect= FALSE)

summary(hndnomyco6)
plot(hndnomyco6)
hndnomyco6_coef<- t(coef(hndnomyco6))

# Vcmax     Jmax        Rd      TPU
# 107.7905 176.9812 0.7098161 9.309268

#  hnwwnomyco1     

hnwwnomyco1 <- fitaci(filter(A_ci_merged, id == "hnwwnomyco1"),
                     varnames = list(ALEAF="A",
                                     Tleaf= "Tair",
                                     Ci= "Ci",
                                     PPFD= "Qin"),
                     fitTPU = T,
                     Tcorrect= FALSE)

summary(hnwwnomyco1)
plot(hnwwnomyco1)
hnwwnomyco1_coef <- t(coef(hnwwnomyco1))

# Vcmax     Jmax       Rd     TPU
# 91.29504 144.5366 1.164058 8.44001

#  hnwwnomyco2     

hnwwnomyco2 <- fitaci(filter(A_ci_merged, id == "hnwwnomyco2"),
                      varnames = list(ALEAF="A",
                                      Tleaf= "Tair",
                                      Ci= "Ci",
                                      PPFD= "Qin"),
                      fitTPU = T,
                      Tcorrect= FALSE)

summary(hnwwnomyco2)
plot(hnwwnomyco2)
hnwwnomyco2_coef<- t(coef(hnwwnomyco2))

# Vcmax    Jmax       Rd      TPU
# 91.73249 130.807 2.161801 8.713821

#  hnwwnomyco3     

hnwwnomyco3 <- fitaci(filter(A_ci_merged, id == "hnwwnomyco3"),
                      varnames = list(ALEAF="A",
                                      Tleaf= "Tair",
                                      Ci= "Ci",
                                      PPFD= "Qin"),
                      fitTPU = T,
                      Tcorrect= FALSE)

summary(hnwwnomyco3)
plot(hnwwnomyco3)
hnwwnomyco3_coef<- t(coef(hnwwnomyco3))

# Vcmax     Jmax       Rd      TPU
# 123.8117 197.0987 1.721062 9.332436

#  hnwwnomyco4     

hnwwnomyco4 <- fitaci(filter(A_ci_merged, id == "hnwwnomyco4"),
                      varnames = list(ALEAF="A",
                                      Tleaf= "Tair",
                                      Ci= "Ci",
                                      PPFD= "Qin"),
                      fitTPU = T,
                      Tcorrect= FALSE)

summary(hnwwnomyco4)
plot(hnwwnomyco4)
hnwwnomyco4_coef<- t(coef(hnwwnomyco4))

# Vcmax     Jmax        Rd      TPU
# 62.84986 111.8602 0.5998134 5.781383


#  hnwwnomyco5     

hnwwnomyco5 <- fitaci(filter(A_ci_merged, id == "hnwwnomyco5"),
                      varnames = list(ALEAF="A",
                                      Tleaf= "Tair",
                                      Ci= "Ci",
                                      PPFD= "Qin"),
                      fitTPU = T,
                      Tcorrect= FALSE)

summary(hnwwnomyco5)
plot(hnwwnomyco5)
hnwwnomyco5_coef<- t(coef(hnwwnomyco5))

# Vcmax     Jmax         Rd      TPU
# 80.82799 134.0209 -0.2944108 7.220077

#  hnwwnomyco6     

hnwwnomyco6 <- fitaci(filter(A_ci_merged, id == "hnwwnomyco6"),
                      varnames = list(ALEAF="A",
                                      Tleaf= "Tair",
                                      Ci= "Ci",
                                      PPFD= "Qin"),
                      fitTPU = T,
                      Tcorrect= FALSE)

summary(hnwwnomyco6)
plot(hnwwnomyco6)
hnwwnomyco6_coef<- t(coef(hnwwnomyco6))

# Vcmax    Jmax       Rd      TP
# 140.6212 239.487 5.197923 13.54077

#  lndnomyco1     # ID wasn't found

lndnomyco1 <- fitaci(filter(A_ci_merged, id == "lndnomyco1"),
                      varnames = list(ALEAF="A",
                                      Tleaf= "Tair",
                                      Ci= "Ci",
                                      PPFD= "Qin"),
                      fitTPU = T,
                      Tcorrect= FALSE)

summary(lndnomyco1)
plot(lndnomyco1)
lndnomyco1_coef <- t(coef(lndnomyco1))

#  lndnomyco2     

lndnomyco2 <- fitaci(filter(A_ci_merged, id == "lndnomyco2"),
                     varnames = list(ALEAF="A",
                                     Tleaf= "Tair",
                                     Ci= "Ci",
                                     PPFD= "Qin"),
                     fitTPU = T,
                     Tcorrect= FALSE)

summary(lndnomyco2)
plot(lndnomyco2)
lndnomyco2_coef<- t(coef(lndnomyco2))

#  Vcmax     Jmax        Rd TPU
# 54.33098 83.70942 0.6311314  NA

#  lndnomyco3     

lndnomyco3 <- fitaci(filter(A_ci_merged, id == "lndnomyco3"),
                     varnames = list(ALEAF="A",
                                     Tleaf= "Tair",
                                     Ci= "Ci",
                                     PPFD= "Qin"),
                     fitTPU = T,
                     Tcorrect= FALSE)

summary(lndnomyco3)
plot(lndnomyco3)
lndnomyco3_coef<- t(coef(lndnomyco3))

# Vcmax     Jmax      Rd      TPU
# 92.12319 154.0717 1.97803 7.759006

#  lndnomyco4     

lndnomyco4 <- fitaci(filter(A_ci_merged, id == "lndnomyco4"),
                     varnames = list(ALEAF="A",
                                     Tleaf= "Tair",
                                     Ci= "Ci",
                                     PPFD= "Qin"),
                     fitTPU = T,
                     Tcorrect= FALSE)

summary(lndnomyco4)
plot(lndnomyco4)
lndnomyco4_coef<- t(coef(lndnomyco4))

# Vcmax     Jmax       Rd      TPU
# 90.08431 142.1234 2.878984 7.661331

#  lndnomyco5     

lndnomyco5 <- fitaci(filter(A_ci_merged, id == "lndnomyco5"),
                     varnames = list(ALEAF="A",
                                     Tleaf= "Tair",
                                     Ci= "Ci",
                                     PPFD= "Qin"),
                     fitTPU = T,
                     Tcorrect= FALSE)

summary(lndnomyco5)
plot(lndnomyco5)
lndnomyco5_coef<- t(coef(lndnomyco5))

# Vcmax     Jmax      Rd      TPU
# 117.4018 207.5955 5.81524 11.30149

#  lndnomyco6     

lndnomyco6 <- fitaci(filter(A_ci_merged, id == "lndnomyco6"),
                     varnames = list(ALEAF="A",
                                     Tleaf= "Tair",
                                     Ci= "Ci",
                                     PPFD= "Qin"),
                     fitTPU = T,
                     Tcorrect= FALSE)

summary(lndnomyco6)
plot(lndnomyco6)
lndnomyco6_coef<- t(coef(lndnomyco6))

# Vcmax     Jmax       Rd      TPU
# 101.7609 177.3358 2.436868 8.472555

#  lnwwnomyco1     

lnwwnomyco1 <- fitaci(filter(A_ci_merged, id == "lnwwnomyco1"),
                     varnames = list(ALEAF="A",
                                     Tleaf= "Tair",
                                     Ci= "Ci",
                                     PPFD= "Qin"),
                     fitTPU = T,
                     Tcorrect= FALSE)

summary(lnwwnomyco1)
plot(lnwwnomyco1)
lnwwnomyco1_coef <- t(coef(lnwwnomyco1))

# Vcmax     Jmax         Rd      TPU
# 82.10353 133.6553 -0.7656931 7.223756

#  lnwwnomyco2     

lnwwnomyco2 <- fitaci(filter(A_ci_merged, id == "lnwwnomyco2"),
                      varnames = list(ALEAF="A",
                                      Tleaf= "Tair",
                                      Ci= "Ci",
                                      PPFD= "Qin"),
                      fitTPU = T,
                      Tcorrect= FALSE)

summary(lnwwnomyco2)
plot(lnwwnomyco2)
lnwwnomyco2_coef<- t(coef(lnwwnomyco2))

# Vcmax     Jmax        Rd      TPU
# 62.55642 110.2747 0.7841852 6.687387

#  lnwwnomyco3     

lnwwnomyco3 <- fitaci(filter(A_ci_merged, id == "lnwwnomyco3"),
                      varnames = list(ALEAF="A",
                                      Tleaf= "Tair",
                                      Ci= "Ci",
                                      PPFD= "Qin"),
                      fitTPU = T,
                      Tcorrect= FALSE)

summary(lnwwnomyco3)
plot(lnwwnomyco3)
lnwwnomyco3_coef<- t(coef(lnwwnomyco3))

# Vcmax     Jmax        Rd      TPU
# 82.53074 127.0465 0.3591353 6.433764


#  lnwwnomyco4     

lnwwnomyco4 <- fitaci(filter(A_ci_merged, id == "lnwwnomyco4"),
                      varnames = list(ALEAF="A",
                                      Tleaf= "Tair",
                                      Ci= "Ci",
                                      PPFD= "Qin"),
                      fitTPU = T,
                      Tcorrect= FALSE)

summary(lnwwnomyco4)
plot(lnwwnomyco4)
lnwwnomyco4_coef<- t(coef(lnwwnomyco4))

# Vcmax     Jmax       Rd      TPU
# 84.2271 133.6201 1.693313 7.153628

#  lnwwnomyco5     

lnwwnomyco5 <- fitaci(filter(A_ci_merged, id == "lnwwnomyco5"),
                      varnames = list(ALEAF="A",
                                      Tleaf= "Tair",
                                      Ci= "Ci",
                                      PPFD= "Qin"),
                      fitTPU = T,
                      Tcorrect= FALSE)

summary(lnwwnomyco5)
plot(lnwwnomyco5)
lnwwnomyco5_coef<- t(coef(lnwwnomyco5))

# Vcmax     Jmax       Rd      TPU
# 76.75576 122.6766 1.825357 8.611623

#  lnwwnomyco6     

lnwwnomyco6 <- fitaci(filter(A_ci_merged, id == "lnwwnomyco6"),
                      varnames = list(ALEAF="A",
                                      Tleaf= "Tair",
                                      Ci= "Ci",
                                      PPFD= "Qin"),
                      fitTPU = T,
                      Tcorrect= FALSE)

summary(lnwwnomyco6)
plot(lnwwnomyco6)
lnwwnomyco6_coef<- t(coef(lnwwnomyco6))

# Vcmax     Jmax       Rd      TPU
# 88.44738 140.2933 2.987079 8.563599


Vcmax_df <- rbind(hndmyco1_coef, hndmyco2_coef, hndmyco3_coef, hndmyco4_coef,
                  hndmyco5_coef, hndmyco6_coef, hnwwmyco1_coef, hnwwmyco2_coef,
                  hnwwmyco3_coef, hnwwmyco4_coef, hnwwmyco5_coef, hnwwmyco6_coef,
                  lndmyco1_coef, lndmyco2_coef, lndmyco3_coef, lndmyco4_coef,
                  lndmyco5_coef, lnwwmyco1_coef, lnwwmyco2_coef, lnwwmyco3_coef,
                  lnwwmyco4_coef, lnwwmyco6_coef, hndnomyco1_coef, hndnomyco2_coef,
                  hndnomyco3_coef, hndnomyco4_coef, hndnomyco5_coef, hndnomyco6_coef,
                  hnwwnomyco1_coef, hnwwnomyco2_coef, hnwwnomyco3_coef, hnwwnomyco4_coef,
                  hnwwnomyco5_coef, hnwwnomyco6_coef, lndnomyco2_coef, lndnomyco3_coef,
                  lndnomyco4_coef, lndnomyco5_coef, lndnomyco6_coef, lnwwnomyco1_coef,
                  lnwwnomyco2_coef, lnwwnomyco3_coef, lnwwnomyco4_coef, lnwwnomyco5_coef,
                  lnwwnomyco6_coef)

Sample <- c("HND Myco 1","HND Myco 2","HND Myco 3","HND Myco 4",
"HND Myco 5", "HND Myco 6","HNWW Myco 1","HNWW Myco 2","HNWW Myco 3",
"HNWW Myco 4","HNWW Myco 5", "HNWW Myco 6", "LND Myco 1", "LND Myco 2",
"LND Myco 3", "LND Myco 4", "LND Myco 5", "LNWW Myco 1", "LNWW Myco 2",
"LNWW Myco 3","LNWW Myco 4","LNWW Myco 6", "HND No Myco 1","HND No Myco 2",
"HND No Myco 3", "HND No Myco 4", "HND No Myco 5", "HND No Myco 6", "HNWW No Myco 1", "HNWW No Myco 2", "HNWW No Myco 3", "HNWW No Myco 4", "HNWW No Myco 5", "HNWW No Myco 6", "LND No Myco 1", "LND No Myco 2", "LND No Myco 3",
"LND No Myco 4", "LND No Myco 5", "LND No Myco 6", "LNWW No Myco 1", "LNWW No Myco 2", "LNWW No Myco 3", "LNWW No Myco 4", "LNWW No Myco 5", "LNWW No Myco 6")


Vcmax_df <- cbind(Sample, Vcmax_df)

metadata <- read_excel("metadata.xlsx")


merge_stats <-merge(Vcmax_df, metadata, by = "Sample") 
merge_stats$Vcmax <- as.numeric(merge_stats$Vcmax)
merge_stats$Jmax <- as.numeric(merge_stats$Jmax)
merge_stats$Rd <- as.numeric(merge_stats$Rd)

merge_stats <- merge_stats %>% mutate(`Jmax/Vcmax` = (Jmax/Vcmax))

Anet <- cbind(Sample, A_ci_merged)


# fit model based on hypothesis (Vcmax)
Vcmax_lmer <- lmer(Vcmax ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_stats)

Anova(Vcmax_lmer)
summary(Vcmax_lmer)
hist(residuals(Vcmax_lmer))
shapiro.test(residuals(Vcmax_lmer)) # normal distribution

plot(Vcmax_lmer)


emmeans(Vcmax_lmer, pairwise ~ AMF * Nutrient_treatment, adjust = "tukey")

emmeans(Vcmax_lmer, pairwise ~ Water_treatment * Nutrient_treatment, adjust = "tukey")

emmeans(Vcmax_lmer, pairwise ~ Water_treatment * AMF, adjust = "tukey")

# emmeans for Vcmax

Nutrient_Vcmax <- as.data.frame(emmeans(Vcmax_lmer, ~Nutrient_treatment))

em_Vcmax <- as.data.frame(emmeans(Vcmax_lmer, ~AMF * Nutrient_treatment * Water_treatment))

# ggplot for Vcmax

Treatment_Vcmax_w_sig <- ggplot(merge_stats, aes(y = Vcmax, x = AMF , color = AMF)) + geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_Vcmax, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_Vcmax, aes(x = AMF, y = emmean,ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y = expression(Vcmax[25]~ "(µmol"~m^{-2}~s^{-1}*")")) +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Drought Condition"),Nutrient_treatment = c("Low nitrogen" = "Low Nitrogen", 
  "High nitrogen" = "High Nitrogen"))) +  theme_bw(base_size = 14) + guides(color = "none") +
  theme(axis.title = element_text(size = 22)) +theme(
    strip.text.x = element_text(size = 22),   
    strip.text.y = element_text(size = 22)    
  )   
  
ggsave("Treatment_Vcmax_w_sig.jpg", width = 10, height = 7, units = "in", dpi = 600)
  
  
Treatment_Vcmax <- ggplot(merge_stats, aes(y = Vcmax, x = AMF , color = AMF)) + geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_Vcmax, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_Vcmax, aes(x = AMF, y = emmean,ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y = expression(Vcmax[25]~ "(µmol"~m^{-2}~s^{-1}*")")) +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Drought Condition"),Nutrient_treatment = c("Low nitrogen" = "Low Nitrogen", "High nitrogen" = "High Nitrogen"))) +  theme_bw(base_size = 14) + geom_signif(comparisons = list(c("No", "Yes")), map_signif_level = TRUE, test.args = list(exact = FALSE), y_position = c(144,145), vjust = -0.05, size = 0.5) + expand_limits(y = 160)


ggsave("Treatment_Vcmax.jpg", width = 10, height = 7, units = "in", dpi = 600)

Nuplot_Vcmax <- ggplot(merge_stats, aes(y = Vcmax, x = Nutrient_treatment, fill = Nutrient_treatment)) + 
  geom_boxplot(alpha = 0.5)  +
  geom_point(data = Nutrient_Vcmax, aes(y = emmean, x = Nutrient_treatment), 
             shape = 18, size = 4, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = Nutrient_Vcmax, aes(x = Nutrient_treatment, ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +
  theme_bw() +
  theme(legend.position = "none") +
  labs (x = "Nutrient treatment",
        y = expression(Vcmax[25]~ "(µmol/m²/s)"))


ggsave("Nuplot_Vcmax.jpg", width = 10, height = 7, units = "in", dpi = 600)



# Jmax

# fit model based on hypothesis (Jmax)
Jmax_lmer <- lmer(Jmax ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID),  data = merge_stats)

Anova(Jmax_lmer)
summary(Jmax_lmer)
hist(residuals(Jmax_lmer))
shapiro.test(residuals(Jmax_lmer)) # normal distribution

plot(Jmax_lmer)

# emmeans for Jmax

em_Jmax <- as.data.frame(emmeans(Jmax_lmer, ~AMF * Nutrient_treatment * Water_treatment))

# ggplot for Jmax

Treatment_Jmax <- ggplot(merge_stats, aes(y = Jmax, x = AMF , color = AMF)) + geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_Jmax, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_Jmax, aes(x = AMF, y = emmean,ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y = expression(Jmax[25]~ "(µmol"~m^{-2}~s^{-1}*")")) +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Drought Condition"),Nutrient_treatment = c("Low nitrogen" = "Low N",  "High nitrogen" = "High N"))) +  theme_bw(base_size = 14)  

ggsave("Treatment_Jmax.jpg", width = 10, height = 7, units = "in", dpi = 600)


# fit model based on hypothesis (Rd)
Rd_lmer <- lmer(Rd ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_stats)

Anova(Rd_lmer)
summary(Rd_lmer)
hist(residuals(Rd_lmer))
shapiro.test(residuals(Rd_lmer)) # normally distributed

plot(Rd_lmer)

# emmeans for Rd

em_Rd <- as.data.frame(emmeans(Rd_lmer, ~AMF * Nutrient_treatment * Water_treatment))

# ggplot for Rd

Treatment_Rd <- ggplot(merge_stats, aes(y = Rd, x = AMF , color = AMF)) + geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_Rd, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_Rd, aes(x = AMF, y = emmean,ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y =expression(R[d] ~ "(µmol"~m^{-2}~s^{-1}*")")) +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Drought Condition"),Nutrient_treatment = c("Low nitrogen" = "Low N",  "High nitrogen" = "High N"))) +  theme_bw(base_size = 14)  

ggsave("Treatment_Rd.jpg", width = 10, height = 7, units = "in", dpi = 600)


# fit model based on hypothesis (Anet)
Anet_lmer <- lmer(A ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = A_gsw_merge)

Anova(Anet_lmer)
summary(Anet_lmer)
hist(residuals(Anet_lmer))
shapiro.test(residuals(Anet_lmer)) # normal distribution

plot(Anet_lmer)

# emmeans for Anet

em_Anet <- as.data.frame(emmeans(Anet_lmer, ~AMF * Nutrient_treatment * Water_treatment))

# ggplot for Anet

Treatment_Anet <- ggplot(A_gsw_merge, aes(y = A, x = AMF , color = AMF)) + geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_Anet, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_Anet, aes(x = AMF, y = emmean,ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y = expression(A[net] ~ "(µmol"~m^{-2}~s^{-1}*")")) +
  facet_grid(Water_treatment ~ Nutrient_treatment,  
             labeller = labeller(Water_treatment = c("Well watered" = "Well Watered", "Drought" = "Drought Condition"),Nutrient_treatment = c("Low nitrogen" = "Low N",  "High nitrogen" = "High N"))) +  theme_bw(base_size = 14)  

ggsave("Treatment_Anet.jpg", width = 10, height = 7, units = "in", dpi = 600)

Nutrient_Anet <- as.data.frame(emmeans(Anet_lmer, ~Nutrient_treatment))


Nuplot_Anet <- ggplot(A_gsw_merge, aes(y = A, x = Nutrient_treatment, fill = Nutrient_treatment)) + 
  geom_boxplot(alpha = 0.5)  +
  geom_point(data = Nutrient_Anet, aes(y = emmean, x = Nutrient_treatment), 
             shape = 18, size = 4, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = Nutrient_Anet, aes(x = Nutrient_treatment, ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +
  theme_bw() +
  theme(legend.position = "none") +
  labs (x = "Nutrient treatment",
        y = expression(A[net] ~ "(µmol/m²/s)"))


ggsave("Nuplot_Anet.jpg", width = 10, height = 7, units = "in", dpi = 600)


# Jmax/Vcmax

# fit model based on hypothesis (Jmax/Vcmax)
Jmax_Vcmax_lmer <- lmer(Jmax/Vcmax ~ Water_treatment * Nutrient_treatment * AMF + (1|Block_ID),  data = merge_stats)

Anova(Jmax_Vcmax_lmer)
summary(Jmax_Vcmax_lmer)
hist(residuals(Jmax_Vcmax_lmer))
shapiro.test(residuals(Jmax_Vcmax_lmer)) # normal distribution

plot(Jmax_Vcmax_lmer)
























































