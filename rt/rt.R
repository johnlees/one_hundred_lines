rm(list = ls())

library(EpiEstim)
library(dplyr)
library(tidyverse)
library(readxl)

# paths
DATA_PATH = 'data/'
OUT_PATH = 'rt/outputs'

# read data
df_linelist <- read_excel(paste0(DATA_PATH, 'Linelist_grp4.xlsx'))

# compute incidence
df_incidence <- df_linelist %>% 
  count(onset_date, sort=TRUE, name='I')

####Completando las fechas faltantes####
complete_dates <- function(df){
  all_dates <- data.frame(dates = seq.Date(min(df$onset_date), 
                                           max(df$onset_date), 
                                           by = 'day')
                          )  
  df <- left_join(all_dates, df)
  df$I[is.na(df$I)] <- 0
  return(df)
} 

df_incidence <- df_incidence %>% complete_dates()

compute_rt <- function(df,
                       rt_days=14,
                       mean_si=6.48,
                       std_si=3.83){
  rt_days <- as.integer(rt_days)
  t_start <- seq_along(df$dates)[2:(length(df$dates) - rt_days)]
  t_end = t_start + rt_days
  rt_data  <- estimate_R(df, method = "parametric_si",
                         config = make_config(list(
                           mean_si = mean_si, std_si = std_si,
                           t_start= t_start, t_end = t_end )))
  
  df_rt <- rt_data$R
  df_rt$window_start <- min(df$dates) + df_rt$t_start
  df_rt$window_end <- min(df$dates) + df_rt$t_end
  df_rt <- df_rt %>% filter(window_end <= '2022-12-31')
  return(df_rt)
}

df_rt <- df_incidence %>% compute_rt()
df_rt_60 <- df_incidence_60 %>% compute_rt() 

# save data
write_csv(df_rt, paste0(OUT_PATH, 'rt_all_ages.csv'))
write_csv(df_rt_60, paste0(OUT_PATH, 'rt_60_plus.csv'))