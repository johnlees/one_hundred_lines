rm(list = ls())

library(EpiEstim)
library(dplyr)
library(tidyverse)
library(readxl)
library(lubridate)

complete_dates <- function(df){
  all_dates <- data.frame(dates = seq.Date(min(df$dates), 
                                           max(df$dates), 
                                           by = 'day')
  )  
  df <- left_join(all_dates, df)
  df$I[is.na(df$I)] <- 0
  return(df)
} 

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
                           t_start = t_start, t_end = t_end )))
  return(rt_data)
}

# paths
DATA_PATH = '../data/'
OUT_PATH = 'outputs/'

# read data
df_linelist <- readxl::read_excel(paste0(DATA_PATH, 'Linelist_grp4.xlsx'))

# compute incidence
df_incidence <- df_linelist %>% 
  count(.data$onset_date, sort = TRUE, name = 'I')

# format incidence
df_incidence$onset_date <- as.Date(df_incidence$onset_date, format = "%m/%d/%y")

# clean incidence
df_incidence <- df_incidence[-1, ]
df_incidence$I[is.na(df_incidence$I)] <- 0

# rename column
names(df_incidence)[1] <- 'dates'

# complete dates
df_incidence <- df_incidence %>% complete_dates()

# compute rt
df_rt <- df_incidence %>% compute_rt()

# save data
write_csv(df_rt, paste0(OUT_PATH, 'rt_all_ages.csv'))

# plotting data
png(file = paste0(OUT_PATH, "graphs_rt.png"), width = 1000, height = 650)
plot(df_rt, legend = FALSE)
dev.off()