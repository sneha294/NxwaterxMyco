library(readLicorData)
library(dplyr)
library(readxl)
library(tidyr)

# Light curves

a <- licorData("2024-07-20-a_ci")
write.csv(a, "~/Documents/Experiment I/Gas exchange data/2024-07-20-a_ci.csv",row.names = FALSE) 

a_final <- a %>% select(obs,  elapsed, date, id, A, Ci, Ca, gsw, CO2_s, CO2_r,
                        H2O_s, H2O_r, Qin, VPDleaf, Flow, Tair)

b <- licorData("2024-07-21-1040_aci")
write.csv(b, "~/Documents/Experiment I/Gas exchange data/2024-07-21-1040_aci.csv",row.names = FALSE) 

b_final <- b %>% select(obs, elapsed, date, id, A, Ci, Ca, gsw, CO2_s, CO2_r,
                        H2O_s, H2O_r, Qin, VPDleaf, Flow, Tair)


c <- licorData("2024-07-21-1050_aci")
write.csv(c, "~/Documents/Experiment I/Gas exchange data/2024-07-21-1050_aci.csv",row.names = FALSE) 

c_final <- c %>% select(obs, elapsed, date, id, A, Ci, Ca, gsw, CO2_s, CO2_r,
                        H2O_s, H2O_r, Qin, VPDleaf, Flow, Tair)

d <- licorData("2024-07-21-sneha_aci")
write.csv(d, "~/Documents/Experiment I/Gas exchange data/2024-07-21-sneha_aci.csv",row.names = FALSE)

d_final <- d %>% select(obs, elapsed, date, id, A, Ci, Ca, gsw, CO2_s, CO2_r,
                        H2O_s, H2O_r, Qin, VPDleaf, Flow, Tair)

e <- licorData("2024-07-22-sneha_myco_aci")
write.csv(e, "~/Documents/Experiment I/Gas exchange data/2024-07-22-sneha_myco_aci.csv",
          row.names = FALSE) 

e_final <- e %>% select(obs, elapsed, date, id, A, Ci, Ca, gsw, CO2_s, CO2_r,
                        H2O_s, H2O_r, Qin, VPDleaf, Flow, Tair)

f <- licorData("aci_20_7_2024_yadi")
write.csv(f, "~/Documents/Experiment I/Gas exchange data/aci_20_7_2024_yadi.csv",
          row.names = FALSE) 

f_final <- f %>% select(obs, elapsed, date, id, A, Ci, Ca, gsw, CO2_s, CO2_r,
                        H2O_s, H2O_r, Qin, VPDleaf, Flow, Tair)

g <- licorData("2024-07-20-a_ci[85].")
write.csv(g, "~/Documents/Experiment I/Gas exchange data/2024-07-20-a_ci[85].csv")

g_final <- g %>% select(obs, elapsed, date, id, A, Ci, Ca, gsw, CO2_s, CO2_r,
                        H2O_s, H2O_r, Qin, VPDleaf, Flow, Tair)

h <- licorData("2024-07-22-sneha_myco_aci[85].")
write.csv(h, "~/Documents/Experiment I/Gas exchange data/2024-07-22-sneha_myco_aci[85].csv",
          row.names = FALSE) 

h_final <- h %>% select(obs, elapsed, date, id, A, Ci, Ca, gsw, CO2_s, CO2_r,
                        H2O_s, H2O_r, Qin, VPDleaf, Flow, Tair)

i <- licorData("2024-07-21-sneha_aci[59].")
write.csv(i, "~/Documents/Experiment I/Gas exchange data/2024-07-21-sneha_aci[59].csv",
          row.names = FALSE) 

i_final <- i %>% select(obs, elapsed, date, id, A, Ci, Ca, gsw, CO2_s, CO2_r,
                        H2O_s, H2O_r, Qin, VPDleaf, Flow, Tair)

# Respiration

b_resp <- licorData("2024-07-21-respiration")
write.csv(b_resp, "~/Documents/Experiment I/Gas exchange data/2024-07-21-respiration.csv",row.names = FALSE)

b_resp_final <- b_resp %>% select(obs, elapsed, id, photo_resp, A, Ci, Ca, gsw, CO2_s, CO2_r,
                        H2O_s, H2O_r, Qin, VPDleaf, Flow, Tair)

a_resp <- licorData("2024-07-21respiration")
write.csv(a_resp, "~/Documents/Experiment I/Gas exchange data/2024-07-21respiration.csv",row.names = FALSE)

a_resp_final <- a_resp %>% select(obs, elapsed, id, photo_resp, A, Ci, Ca, gsw, CO2_s, CO2_r,
                                  H2O_s, H2O_r, Qin, VPDleaf, Flow, Tair)


c_resp <- read_excel("Respiration.xlsx")

c_resp_final <- c_resp %>% select(obs, elapsed, id, photo_resp, A, Ci, Ca, gsw, CO2_s, CO2_r,
                                  H2O_s, H2O_r, Qin, VPDleaf, Flow, Tair)



library(reshape)

merged_resp <- rbind(b_resp_final, a_resp_final, c_resp_final)

merged_resp$A <- merged_resp$A * -1

merged_resp_final <- merged_resp %>% 
  group_by(id) %>% 
  summarise(respiration = mean(A))

merged_resp_final$id <- tolower(merged_resp_final$id)

merged_resp_final <- merged_resp_final %>% 
  separate(id, c("id", "a"), sep = "_", remove = TRUE) %>% 
  select(-a)

merged <- rbind(a_final, b_final, c_final, d_final, e_final, f_final, g_final, h_final, i_final)
merged$id <- tolower(merged$id)

merged_2 <- left_join(merged, merged_resp_final, by = "id" ) %>% 
  arrange(id, elapsed)

library(plantecophys)

### HNDmyco

# Hndmyco1

Hndmyco1 <- 
  fitaci(filter(merged, id == "Hndmyco1"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin"),
                         fitTPU = T,
                         Tcorrect = FALSE)
plot(Hndmyco1)
t(coef(Hndmyco1))


# HNDMyco2

HNDMyco2 <- 
  fitaci(filter(merged, id == "HNDMyco2"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin"),
                         fitTPU = TRUE,
                         Tcorrect = FALSE)
plot(HNDMyco2)
t(coef(HNDMyco2))

# Hndmyco3

Hndmyco3 <- 
  fitaci(filter(merged, id == "Hndmyco3"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(Hndmyco3)
t(coef(Hndmyco3))

# Hndmyco4

Hndmyco4 <- 
  fitaci(filter(merged, id == "Hndmyco4"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE), , fitmethod='bilinear')
plot(Hndmyco4)
t(coef(Hndmyco4))

# Hndmyco5

Hndmyco5 <- 
  fitaci(filter(merged, id == "Hndmyco5"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(Hndmyco5)
t(coef(Hndmyco5))

# HNDmyco6

HNDmyco6 <- 
  fitaci(filter(merged, id == "HNDmyco6"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE), fitmethod='bilinear')
plot(HNDmyco6)
t(coef(HNDmyco6))


### HNWWMyco

# Hnwwmyco1

Hnwwmyco1 <- 
  fitaci(filter(merged, id == "Hnwwmyco1"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(Hnwwmyco1)
t(coef(Hnwwmyco1))


# HNWWMyco2

# Giving Error: Could not fit curve - check quality of data or fit using fitmethod='bilinear'
# I Fit the curve using fitmethod = "bilinear".

HNWWMyco2 <- 
  fitaci(filter(merged, id == "HNWWMyco2"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE), fitmethod='bilinear')
plot(HNWWMyco2)
t(coef(HNWWMyco2))


# Hnwwmyco3_1

Hnwwmyco3_1 <- 
  fitaci(filter(merged, id == "Hnwwmyco3_1"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE), fitmethod='bilinear')
plot(Hnwwmyco3_1)
t(coef(Hnwwmyco3_1))

# Hnwwmyco4_1

Hnwwmyco4_1 <- 
  fitaci(filter(merged, id == "Hnwwmyco4_1"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(Hnwwmyco4_1)
t(coef(Hnwwmyco4_1))

# Hnwwmyco5

# Giving Error: Could not fit curve - check quality of data or fit using fitmethod='bilinear'
# I Fit the curve using fitmethod = "bilinear".

Hnwwmyco5 <- 
  fitaci(filter(merged, id == "Hnwwmyco5"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE), fitmethod='bilinear')
plot(Hnwwmyco5)
t(coef(Hnwwmyco5))

# HnWWmyco6

HnWWmyco6 <-  
  fitaci(filter(merged, id == "HnWWmyco6"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(HnWWmyco6)
t(coef(HnWWmyco6))

### HNDNomyco

# HNDNoMyco1

HNDNoMyco1 <- 
  fitaci(filter(merged, id == "HNDNoMyco1"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(HNDNoMyco1)
t(coef(HNDNoMyco1))

# HNdnomyco2

HNdnomyco2 <- 
  fitaci(filter(merged, id == "HNdnomyco2"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(HNdnomyco2)
t(coef(HNdnomyco2))

# Hndnomyco3

Hndnomyco3 <- 
  fitaci(filter(merged, id == "Hndnomyco3"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(Hndnomyco3)
t(coef(Hndnomyco3))

# Hndnomyco4

Hndnomyco4 <- 
  fitaci(filter(merged, id == "Hndnomyco4"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(Hndnomyco4)
t(coef(Hndnomyco4))

# HNDnomyco5

# Giving error of Could not fit curve - check quality of data or fit using fitmethod='bilinear'.
# I Fit the curve using fitmethod = "bilinear".

HNDnomyco5 <- 
  fitaci(filter(merged, id == "HNDnomyco5"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE), fitmethod = "bilinear")
plot(HNDnomyco5)
t(coef(HNDnomyco5))

# HNDnomyco6

HNDnomyco6 <- 
  fitaci(filter(merged, id == "HNDnomyco6"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(HNDnomyco6)
t(coef(HNDnomyco6))

### HNWWnomyco

# Hnwwnomyco1

Hnwwnomyco1<- 
  fitaci(filter(merged, id == "Hnwwnomyco1"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(Hnwwnomyco1)
t(coef(Hnwwnomyco1))

# Hnwwnomyco2

Hnwwnomyco2<- 
  fitaci(filter(merged, id == "Hnwwnomyco2"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(Hnwwnomyco2)
t(coef(Hnwwnomyco2))

# Hnwwnomyco3

Hnwwnomyco3<- 
  fitaci(filter(merged, id == "Hnwwnomyco3"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE), fitmethod = "bilinear")
plot(Hnwwnomyco3)
t(coef(Hnwwnomyco3))

# hnwwnomyco4

hnwwnomyco4<- 
  fitaci(filter(merged, id == "hnwwnomyco4"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(hnwwnomyco4)
t(coef(hnwwnomyco4))

# Hnwwnomyco5

Hnwwnomyco5 <- 
  fitaci(filter(merged, id == "Hnwwnomyco5"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(Hnwwnomyco5)
t(coef(Hnwwnomyco5))

# HNWWnomyco6

HNWWnomyco6 <- 
  fitaci(filter(merged, id == "HNWWnomyco6"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(HNWWnomyco6)
t(coef(HNWWnomyco6))

### LNWWMyco

# Lnwwmyco1

Lnwwmyco1 <- 
  fitaci(filter(merged, id == "Lnwwmyco1"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(Lnwwmyco1)
t(coef(Lnwwmyco1))

# LnWwmyco2

LnWwmyco2 <- 
  fitaci(filter(merged, id == "LnWwmyco2"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(LnWwmyco2)
t(coef(LnWwmyco2))



# lnwwmyco3

lnwwmyco3 <- 
  fitaci(filter(merged, id == "lnwwmyco3"),
       varnames = list(ALEAF = "A",
                       Tleaf = "Tair",
                       Ci = "Ci",
                       PPFD = "Qin",
                       fitTPU = TRUE,
                       Tcorrect = FALSE))
plot(lnwwmyco3)
t(coef(lnwwmyco3))


# lnwwmyco4

lnwwmyco4 <- 
  fitaci(filter(merged, id == "lnwwmyco4"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(lnwwmyco4)
t(coef(lnwwmyco4))

# There's no lnwwmyco5
# lnwwmyco6


# LNDMyco

# Lndmyco1

Lndmyco1 <- 
  fitaci(filter(merged, id == "Lndmyco1"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(Lndmyco1)
t(coef(Lndmyco1))


# Lndmyco2

Lndmyco2 <- 
  fitaci(filter(merged, id == "Lndmyco2"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(Lndmyco2)
t(coef(Lndmyco2))

# lndmyco3

lndmyco3 <- 
  fitaci(filter(merged, id == "lndmyco3"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(lndmyco3)
t(coef(lndmyco3))

# Lndmyco4

Lndmyco4 <- 
  fitaci(filter(merged, id == "Lndmyco4"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(Lndmyco4)
t(coef(Lndmyco4))

# LNDMyco5

LNDMyco5 <- 
  fitaci(filter(merged, id == "LNDMyco5"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(LNDMyco5)
t(coef(LNDMyco5))

# There's no LNDMyco6

### LNDNoMyco

# LNDNoMyco1



# LNDNoMyco2

LNDNoMyco2 <- 
  fitaci(filter(merged, id == "LNDNoMyco2"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(LNDNoMyco2)
t(coef(LNDNoMyco2))

# Lndnomyco3

Lndnomyco3 <- 
  fitaci(filter(merged, id == "Lndnomyco3"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(Lndnomyco3)
t(coef(Lndnomyco3))


# Lndnomyco4

Lndnomyco4 <- 
  fitaci(filter(merged, id == "Lndnomyco4"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(Lndnomyco4)
t(coef(Lndnomyco4))

# LNDNoMyco5

LNDNoMyco5 <- 
  fitaci(filter(merged, id == "LNDNoMyco5"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(LNDNoMyco5)
t(coef(LNDNoMyco5))


### LNWWNoMyco

# LNWWNoMyco1


LNWWNoMyco1 <- 
  fitaci(filter(merged, id == "LNWWNoMyco1"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(LNWWNoMyco1)
t(coef(LNWWNoMyco1))

# LnWwnomyco2_true (first curve was not good enough)

LnWwnomyco2_true <- 
  fitaci(filter(merged, id == "LnWwnomyco2_true"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(LnWwnomyco2_true)
t(coef(LnWwnomyco2_true))

# Lnwwnomyco3

Lnwwnomyco3 <- 
  fitaci(filter(merged, id == "Lnwwnomyco3"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE), fitmethod = "bilinear")
plot(Lnwwnomyco3)
t(coef(Lnwwnomyco3))

# Lnwwnomyco4

Lnwwnomyco4 <- 
  fitaci(filter(merged, id == "Lnwwnomyco4"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(Lnwwnomyco4)
t(coef(Lnwwnomyco4))

# LNWWNoMyco5

LNWWNoMyco5 <- 
  fitaci(filter(merged, id == "LNWWNoMyco5"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(LNWWNoMyco5)
t(coef(LNWWNoMyco5))

# LNWWnomyco6

LNWWnomyco6 <- 
  fitaci(filter(merged, id == "LNWWnomyco6"),
         varnames = list(ALEAF = "A",
                         Tleaf = "Tair",
                         Ci = "Ci",
                         PPFD = "Qin",
                         fitTPU = TRUE,
                         Tcorrect = FALSE))
plot(LNWWnomyco6)
t(coef(LNWWnomyco6))







# curve_list <- list()
# vcmax_list <- list()

# for (i in unique(merged$id)) {
  
#  if (sum(grep("resp", i, invert = T)) == 1) & !is.na(i) & i != 'Hnwwmyco4') {
#   curve_list[[i]] <-   fitaci(filter(merged, id == i),
#                                varnames = list(ALEAF = "A",
#                                                Tleaf = "Tair",
#                                                Ci = "Ci",
#                                                PPFD = "Qin",
#                                                fitTPU = TRUE,
#                                                Tcorrect = FALSE), fitmethod='bilinear')
#    vcmax_list[[i]] <- t(coef(curve_list[[i]]))
#    print(plot(curve_list[[i]]))
#    print(paste(i, "curve fit"))
  }

}

# vcmax_df <- as.data.frame(do.call("rbind", vcmax_list))
# vcmax_df$sample <- names(vcmax_list)
# write.csv(vcmax_df, "Gas exchange data.csv")
