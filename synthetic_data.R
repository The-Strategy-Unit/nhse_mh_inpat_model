#### generating synthetic data for testing in tool
# adapted from NHS-R blog - https://nhsrcommunity.com/blog/create-synthetic-data.html

#### setup ####

##packages required

install.packages("synthpop")

library(synthpop)
library(tidyverse)
library(dplyr)
library(stringi)

##deanonymise icb codes from base file before synth'ing

df <- read.csv("C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/MH inpat modelling/backup 20250103/nhse_mh_inpat_model/baseline_aggregate_QHL.csv")

dist_icb_res <- df |> 
  distinct(residence_icb_code) |> 
  rename(icb = residence_icb_code)

dist_icb_prov <- df |> 
  distinct(provider_icb_code) |> 
  rename(icb = provider_icb_code)

dist_icb <- bind_rows(dist_icb_res, dist_icb_prov) |> 
  distinct(icb)

rm(dist_icb_res,dist_icb_prov)

dist_icb <- dist_icb |>
  rowwise() |>  
  mutate(icb_code = paste0(stri_rand_strings(1, 1, "[A-Z]"), stri_rand_strings(1, 3, "[0-9]")),
    icb_name = paste0(icb_code,": Anytown ICB_",icb_code))

#### replace original icb codes and names with above ####

df <- df |> 
  left_join(dist_icb, by = c("residence_icb_code" = "icb")) |> 
  left_join(dist_icb, by = c("provider_icb_code" = "icb")) |> 
  select(18:21,5:17) |> 
  rename(residence_icb_code = icb_code.x,
         residence_icb_name = icb_name.x,
         provider_icb_code = icb_code.y,
         provider_icb_name = icb_name.y)

#### create synthetic version ####

syn_df <- syn(df, seed = 1234)

df_new <- syn_df$syn # to extract the data frame

#recalculate the oap_flag

df_new <- df_new |> 
  mutate(oap_flag = if_else(residence_icb_code == provider_icb_code, 0, 1))

# write csv to working directory

write.csv(df_new, file = "baseline_aggregate_pseudo_icb.csv", quote = FALSE, row.names = FALSE)
