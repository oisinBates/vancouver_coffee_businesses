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
  paste0(
    "status='Issued' AND (",
    str_c(coffee_keywords, collapse = " OR businesstradename LIKE "),
    ")"
  )

coffee_businesses <-
  VancouvR::get_cov_data(dataset_id = "business-licences", where = api_where_string)


# Starbucks has premises registered under the business types of `Wholesale  Dealer` and `Instruction` at `2930 VIRTUAL WAY`.
# JJ Bean had a premises registered from 2013 to 2018 registered under the business types of `Manufacturer - Food` at `460 RAILWAY ST`.
# Since these business types are not customer facing, they are filtered out of this dataframe.
coffee_chains <- coffee_businesses %>%
  mutate(
    formatted_tradename = case_when(
      grepl("Blenz", businesstradename) ~ "blenz",
      grepl("JJ Bean", businesstradename) ~ "jj_bean",
      grepl("McDonalds", businesstradename) ~ "mcdonalds",
      grepl("McDonald's", businesstradename) ~ "mcdonalds",
      grepl("Starbucks", businesstradename) ~ "starbucks",
      grepl("Tim Hortons", businesstradename) ~ "tim_hortons",
      grepl("Tim Horton's", businesstradename) ~ "tim_hortons"
    )
  ) %>%
  filter(
    formatted_tradename %in% c("blenz",
                               "jj_bean",
                               "mcdonalds",
                               "starbucks",
                               "tim_hortons")
  ) %>%
  filter(businesstype %in% c("Ltd Service Food Establishment",
                             "Restaurant Class 1")) %>%
  rename("year" = "folderyear")


# Manually adding 'localarea' for the one Downtown store that is missing this value.
# Multiple other stores are missing 'localarea' values, but these are not Downtown.
coffee_chains$localarea[coffee_chains$house == "490" &
                          coffee_chains$street == "HORNBY ST"] <-
  "Downtown"

# How many businesses are closing and opening each year?
license_count_downtown_by_year <- coffee_chains %>%
  filter(localarea == "Downtown") %>%
  group_by(formatted_tradename, year) %>%
  tally(name = "number_of_locations_downtown")

license_count_not_downtown_by_year <- coffee_chains %>%
  filter(localarea != "Downtown" | is.na(localarea)) %>%
  group_by(formatted_tradename, year) %>%
  tally(name = "numer_of_locations_not_downtown")

license_count_by_year <- coffee_chains %>%
  group_by(formatted_tradename, year) %>%
  tally(name = "number_of_locations") %>%
  inner_join(license_count_downtown_by_year) %>%
  inner_join(license_count_not_downtown_by_year)

# How are employee and location numbers changing each year?
premises_and_employee_count <-
  coffee_chains %>%
  group_by(formatted_tradename, year) %>%
  summarise(location_count = n(),
            employee_count = sum(as.numeric(numberofemployees))) %>%
  mutate(average_number_employees_per_location = employee_count / location_count) %>%
  mutate(across(c(
    'average_number_employees_per_location'
  ), round, 2))

# How many consecutive years of operation for each location?
consecutive_years_of_operation <-
  coffee_chains %>%
  group_by(house, street, formatted_tradename) %>%
  distinct(year) %>%
  tally(name = "consecutive_years_of_operation") %>%
  arrange(formatted_tradename, consecutive_years_of_operation)

# Filter only open business
open_coffee_chains <- coffee_chains %>%
  filter(expireddate > format(Sys.time(), '%Y-%m-%d'))

# How many unique owners are there?
unique_business_owners <- open_coffee_chains %>%
  group_by(businessname, formatted_tradename) %>%
  tally(name = "numer_of_locations")  %>%
  arrange(formatted_tradename, numer_of_locations)
