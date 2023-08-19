library(VancouvR)
library(dplyr)
library(stringr)

# Set API Option from .Renviron
VancouverOpenDataApiKey <- Sys.getenv("vancouver_open_data_api_key")

coffee_keywords <-
  c("'Blenz'",
    "'JJ Bean'",
    "'McDonalds*'",
    "'Starbucks*'",
    "'Tim Horton*'")
api_where_string <-
  str_c(coffee_keywords, collapse = " OR businesstradename LIKE ")
active_businesses <-
  VancouvR::get_cov_data(dataset_id = "business-licences", where = api_where_string)

coffee_franchises <- active_businesses %>%
  mutate(
    formatted_tradename = case_when(
      grepl("Blenz", businesstradename) ~ "blenz",
      grepl("JJ Bean", businesstradename) ~ "jj_bean",
      grepl("McDonalds", businesstradename) ~ "mcdonalds",
      grepl("McDonald's", businesstradename) ~ "mcdonalds",
      grepl("Starbucks", businesstradename) ~ "starbucks",
      grepl("Tim Hortons", businesstradename) ~ "tim_hortons",
      grepl("Tim Horton's", businesstradename) ~ "tim_hortons"
    ),
    licenseyear = substr(expireddate, 0, 4)
  ) %>%
  filter(
    formatted_tradename %in% c("blenz",
                               "jj_bean",
                               "mcdonalds",
                               "starbucks",
                               "tim_hortons")
  )


# How many businesses are closing and open in each year
license_status_count_by_year <- coffee_franchises %>%
  group_by(formatted_tradename, folderyear, status) %>%
  tally()

# How are employee numbers changing each year
employee_count <- coffee_franchises %>%
  group_by(formatted_tradename, licenseyear) %>%
  tally()

# How many consecutive years of operation for each location
consecutive_years_of_operation <-
  coffee_franchises %>% group_by(house, street, formatted_tradename) %>% distinct(folderyear) %>% tally()

# Filter only open business
active_businesses <- coffee_franchises %>%
  filter(expireddate > format(Sys.time(), '%Y-%m-%d'))

# Grouping businesses by unique owners
business_owner_count <- active_businesses %>%
  group_by(businessname, formatted_tradename) %>% tally()

# How many of each franchise are operating in the City of Vancouver
franchise_count <- active_businesses %>%
  group_by(formatted_tradename) %>% tally()

# How many employees are hired at each location
premises_and_employee_count <-
  active_businesses %>% group_by(formatted_tradename) %>%
  summarise(premises_count = n(),
            registered_employee_count = sum(as.numeric(numberofemployees)))
