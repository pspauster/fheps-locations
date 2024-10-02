library(tidyverse)
library(tabulapdf)
library(janitor)

extract_list <- extract_tables("reports/ll145-report-cy2023q4-dec.pdf")

extract_df <- map_dfr(extract_list, ~bind_rows(as.data.frame(.x))) %>% 
  filter(`V1` != "CFHEPS Amount")

column_names <- extract_list[[1]][1,]

names(extract_df) <- column_names

fheps_payments <- extract_df %>% 
  clean_names()
#format other columns

fheps_payments %>% write_csv("data/fheps_payments_2023q4.csv")

