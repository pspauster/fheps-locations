library(tidyverse)
library(tidycensus)

march24_sum <- read_csv("data/LL145_reports_all_months.csv") %>% 
  filter(month == as.Date("2024-03-01")) %>% 
  group_by(zip=as.character(zip)) %>% 
  summarize(no_payments = n(),
            amount_total = sum(cfheps_amount, na.rm = T))

vars <- load_variables(2022, acs5, cache=T)

#check the population merging for population double counting.

nyc_zips_df <- read_csv("https://data.cityofnewyork.us/resource/pri4-ifjk.csv") %>%
  mutate(label = strsplit(as.character(label), ", ")) %>%
  unnest(label) %>% 
  select(-the_geom)

nyc_zips <- nyc_zips_df %>% 
  pull(label)

zip_county_xwalk <- read_csv("https://data.ny.gov/resource/juva-r6g2.csv?$LIMIT=10000") %>% 
  select(zip_code, county) %>% 
  mutate(zip_code = as.character(zip_code),
         county = if_else(zip_code == "10470", "Bronx", county))

zip_census <- get_acs(
        geography = "zcta", 
        variables = c(medincome = "B19013_001"), 
        #state = "NY",
        year = 2022,
        output = "wide") 
  #filter(GEOID %in% nyc_zips)

joined <- full_join(march24_sum, nyc_zips_df, by = c("zip"="label")) %>% 
  mutate(no_payments = if_else(is.na(no_payments), 0 , no_payments),
         nyc = if_else(!is.na(pop_est), T, F)) %>% 
  left_join(zip_census, by = c("zip" = "GEOID")) %>% 
  left_join(zip_county_xwalk, by = c("zip" = "zip_code")) %>% 
  group_by(nyc) %>% 
  mutate(total_pop = sum(pop_est),
         total_vouchers = sum(no_payments)) %>% 
  ungroup() %>% 
  mutate(
         prop_pop = (pop_est/total_pop)*100,
         prop_vouchers = no_payments/total_vouchers*100)

write_csv(joined, "data/march24_zip_sum.csv")

#stats for piece
joined %>%
  group_by(county) %>% 
  summarize(pop = sum(pop_est, na.rm = T),
            vouchers = sum(no_payments, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(   prop_pop = pop/sum(pop)*100,
            prop_vouchers = vouchers/sum(vouchers)*100)

top10_zip <- joined %>%
  arrange(desc(no_payments)) %>% 
  head(10) %>% 
  pull(zip)

joined %>%
  filter(nyc == T) %>% 
  mutate(top10 = if_else(zip %in% top10_zip, T, F)) %>% 
  group_by(top10) %>% 
  summarize(zips = n(),
            med_inc = mean(medincomeE,na.rm=T),
            pop = sum(prop_pop, na.rm = T),
            vouchers = sum(prop_vouchers, na.rm = T))
