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

df <- read.csv("C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/MH inpat modelling/backup 20250103/nhse_mh_inpat_model/baseline_aggregate_QWU.csv")

dist_icb_res <- df |> 
  group_by(residence_icb_code) |>
  summarise(bd = sum(bed_days)) |> 
  rename(icb = residence_icb_code) |> 
  arrange(desc(bd))

dist_icb_prov <- df |> 
  group_by(provider_icb_code) |>
  summarise(bd = sum(bed_days)) |> 
  rename(icb = provider_icb_code) |> 
  arrange(desc(bd))

dist_icb <- bind_rows(dist_icb_res, dist_icb_prov) |>
  filter(!is.na(icb)) |> 
  group_by(icb) %>%
  filter(bd == max(bd)) %>%
  arrange(desc(bd)) |>
  rowid_to_column("row_id")

dist_icb <- dist_icb |>  
  mutate(icb_code = paste0("AT",row_id),
    icb_name = paste0(icb_code,": Anytown ICB_",row_id))

rm(dist_icb_res,dist_icb_prov)

#### replace original icb codes and names with above ####

df <- df |> 
  left_join(dist_icb, by = c("residence_icb_code" = "icb")) |> 
  left_join(dist_icb, by = c("provider_icb_code" = "icb")) |> 
  select(20:21,24:25,5:17) |> 
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

#remove any non-relevant activity created by the randomisation (i.e. neither resident or treated in main ICB)

icb_main <- dist_icb |> 
  arrange(desc(bd)) |> 
  head(1) |> 
  select(4) |> 
  pull(icb_code)

df_new <- df_new |> 
  filter(residence_icb_code == icb_main | provider_icb_code == icb_main)

# write csv to working directory

write.csv(df_new, file = "baseline_aggregate_pseudo_icb.csv", quote = FALSE, row.names = FALSE)
