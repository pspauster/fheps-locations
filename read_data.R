library(tidyverse)
library(tabulapdf)
library(janitor)

read_LL145_report <- function(report) {
  extract_list <- extract_tables(report)
  
  extract_df <- map_dfr(extract_list, ~bind_rows(as.data.frame(.x))) %>% 
    filter(!str_detect(`V1`, "FHEPS"))
  
  column_names <- extract_list[[1]][1,] %>% replace(1, "cfheps_amount")
  
  names(extract_df) <- column_names
  
  fheps_payments <- extract_df %>% 
    clean_names() %>% 
    mutate(cfheps_amount = as.numeric(str_replace_all(cfheps_amount, "[$,]", "")),
           month = parse_date_time(month, orders = c("my", "mdy")),
           report_stub = report)
  
  return(fheps_payments)
}

clean_ll145_report <- function(report_df) {
  report_df %>% 
    group_by(zip, month) %>% 
    summarize(number_payments = n(),
              total_payments = sum(cfheps_amount, na.rm = T)) %>% 
    arrange(desc(number_payments))
}

pdfs<- list.files("reports")

all_months <- map_df(pdfs, ~read_LL145_report(paste0("reports/",.x)))

write_csv(all_months, "data/LL145_reports_all_months.csv")

all_months_sum <- clean_ll145_report(all_months)

write_csv(all_months_sum, "LL145_Reports_zip_code_all_months")


##

test <- read_LL145_report("reports/ll145-report-cy2023q4-dec.pdf")

clean_ll145_report(test)



###

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

