library(tidyverse)
library(tabulapdf)
library(janitor)

extract_list <- extract_tables("reports/ll145-report-cy2023q4-dec.pdf")

extract_df <- map_dfr(extract_list, ~bind_rows(as.data.frame(.x))) %>% 
  filter(`V1` != "CFHEPS Amount")

column_names <- extract_list[[1]][1,]

names(extract_df) <- column_names

fheps_payments <- extract_df %>% 
  clean_names() %>% 
  mutate(cfheps_amount = as.numeric(str_replace_all(cfheps_amount, "[$,]", "")),
         month = my(month))

fheps_payments %>% write_csv("data/fheps_payments_2023q4.csv")

fheps_payments %>% 
  group_by(zip) %>% 
  summarize(number_payments = n(),
            total_payments = sum(cfheps_amount, na.rm = T)) %>% 
  arrange(desc(number_payments)) %>% 
  write_csv("data/payments_by_zip.csv")

