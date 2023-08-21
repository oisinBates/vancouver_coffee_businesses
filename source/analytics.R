library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(VancouvR)

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

### Start data wrangling/csv generation ###
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

write_csv(license_count_by_year,
          "output/raw_data/license_count_by_year.csv")

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

write_csv(premises_and_employee_count,
          "output/raw_data/premises_and_employee_count.csv")

# How many consecutive years of operation for each location?
consecutive_years_of_operation <-
  coffee_chains %>%
  group_by(house, street, formatted_tradename) %>%
  distinct(year) %>%
  tally(name = "consecutive_years_of_operation") %>%
  arrange(formatted_tradename, consecutive_years_of_operation)

write_csv(
  consecutive_years_of_operation,
  "output/raw_data/consecutive_years_of_operation.csv"
)

# Filter only open business
open_coffee_chains <- coffee_chains %>%
  filter(expireddate > format(Sys.time(), '%Y-%m-%d'))

write_csv(open_coffee_chains,
          "output/raw_data/open_coffee_chains.csv")

# How many unique owners are there?
unique_business_owners <- open_coffee_chains %>%
  group_by(businessname, formatted_tradename) %>%
  tally(name = "numer_of_locations")  %>%
  arrange(formatted_tradename, numer_of_locations)

write_csv(unique_business_owners,
          "output/raw_data/unique_business_owners.csv")

### End data wrangling/csv generation ###

### Start visualization ###
coffee_brand_colors <-
  c(
    blenz = "#5d4530",
    jj_bean = "#fa002d",
    mcdonalds = "#FFCC00",
    starbucks = "#006241",
    tim_hortons = "#C8102F"
  )

coffee_brand_labels <-
  c("Blenz", "JJ Bean", "McDonald's", "Starbucks", "Tim Hortons")

###### Start line plot generation ######
ggplot(
  license_count_by_year,
  aes(
    x = year,
    y = number_of_locations,
    color = formatted_tradename,
    group = formatted_tradename
  )
) +
  geom_line(linewidth = 1) +
  scale_color_manual(name = "Store Name",
                     values = coffee_brand_colors,
                     labels = coffee_brand_labels) +
  scale_y_continuous(breaks = pretty(license_count_by_year$number_of_locations, n = 20)) +
  labs(title = "City of Vancouver Coffee Chains",
       y = "Store Count",
       x = "Year")

ggsave("output/line_plots/coffee_chains_city_of_vancouver.png")

ggplot(
  license_count_by_year,
  aes(
    x = year,
    y = number_of_locations_downtown,
    color = formatted_tradename,
    group = formatted_tradename
  )
) +
  geom_line(linewidth = 1) +
  scale_color_manual(name = "Store Name",
                     values = coffee_brand_colors,
                     labels = coffee_brand_labels) +
  scale_y_continuous(breaks = pretty(license_count_by_year$number_of_locations_downtown, n = 15)) +
  labs(title = "Downtown Vancouver Coffee Chains",
       y = "Store Count",
       x = "Year")

ggsave("output/line_plots/coffee_chains_downtown_vancouver.png")
###### End line plot generation ######


### End visualization ###
