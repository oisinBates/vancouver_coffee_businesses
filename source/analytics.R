library(dplyr)
library(geosphere)
library(ggplot2)
library(readr)
library(stringr)
library(tidyr)
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
# Since these business types are not customer facing, they are filtered out.
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

# When did each location open, and is it still open?
# This data is mostly for the sake of the Gantt charts
min_and_max_years_of_operation <-
  coffee_chains %>%
  filter(localarea == "Downtown") %>%
  unite(full_address, c("house", "street"), sep = " ") %>%
  mutate(full_address = toupper(full_address)) %>%
  group_by(full_address, formatted_tradename) %>%
  summarise(min = min(year),
            max = max(year)) %>%
  arrange(formatted_tradename)

write_csv(unique_business_owners,
          "output/raw_data/min_and_max_years_of_operation.csv")

# What is the Haversine distance of each Downtown store to the City Center (Granville & West Georgia Street)
# I thought that a circular barplot could be an interesting way to visualize relative distance
# Calculating haversine distance with the geosphere library and the solution at https://stackoverflow.com/a/53559542
haversince_distance_to_city_center <- coffee_chains %>%
  filter(localarea == "Downtown") %>%
  mutate(Long = as.numeric(sub(".*\\[(.*?),.*", "\\1", geom)), Lat = as.numeric(sub(".* (.*)\\].*", "\\1", geom))) %>%
  unite(full_address, c("house", "street"), sep = " ") %>%
  mutate(full_address = toupper(full_address)) %>%
  rowwise() %>%
  # Granville & West Georgia = c(-123.11817, 49.28249)
  mutate(meter_distance_to_city_centre = distHaversine(c(Long, Lat), c(-123.11817, 49.28249))) %>%
  arrange(formatted_tradename, meter_distance_to_city_centre) %>%
  mutate(meter_distance_string = paste0("(", round(meter_distance_to_city_centre), " m)")) %>%
  unite(
    full_address_and_distance,
    c("full_address", "meter_distance_string"),
    remove = FALSE,
    sep = " "
  )

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

###### Start Gantt chart generation ######

# Opting to generate individual charts for each location, for ease of readability
generate_gantt_chart <-
  function(store_name,
           trade_name,
           min_and_max_years_of_operation) {
    filtered_data <- min_and_max_years_of_operation %>%
      filter(formatted_tradename == trade_name)

    # As some store locations were only operational for a single year, these don't show with geom_segment()
    # Adding geom_point() so stores, only in operation for a single year, are visible on their plot
    ggplot(
      filtered_data,
      aes(
        x = min,
        xend = max,
        y = full_address,
        yend = full_address,
        color = formatted_tradename
      )
    ) +
      geom_segment(linewidth = 4.2, show.legend = FALSE) +
      geom_point(aes(x = min),
                 shape = 'square',
                 size = 3.9,
                 show.legend = FALSE) +
      scale_color_manual(values = coffee_brand_colors) +
      labs(
        title = paste("Downtown Vancouver", store_name, "Locations"),
        y = "Store Location",
        x = "Year"
      )

    save_path <- paste0("output/gantt_charts/", trade_name, ".png")
    ggsave(save_path)
  }
generate_gantt_chart("Blenz", "blenz", min_and_max_years_of_operation)
generate_gantt_chart("JJ Bean", "jj_bean", min_and_max_years_of_operation)
generate_gantt_chart("McDonald's", "mcdonalds", min_and_max_years_of_operation)
generate_gantt_chart("Starbucks", "starbucks", min_and_max_years_of_operation)
generate_gantt_chart("Tim Hortons", "tim_hortons", min_and_max_years_of_operation)

###### End Gantt chart generation ######

###### Start circular barplot generation ######
# This solution is adapted from the example at https://r-graph-gallery.com/296-add-labels-to-circular-barplot.html
haversince_distance_to_city_center$x_axis_id = seq(1, nrow(haversince_distance_to_city_center))
row_count <- nrow(haversince_distance_to_city_center)
angle <-
  90 - 360 * (haversince_distance_to_city_center$x_axis_id - 0.5) / row_count
haversince_distance_to_city_center$hjust <-
  ifelse(angle < -90, 1, 0)
haversince_distance_to_city_center$angle <-
  ifelse(angle < -90, angle + 180, angle)


ggplot(
  active_businesses_with_haversine,
  aes(
    x = factor(x_axis_id),
    y = meter_distance_to_city_centre,
    fill =
      formatted_tradename
  )
) +
  geom_bar(stat = "identity") +
  ylim(-1300, 1800) +
  scale_x_discrete(labels = active_businesses_with_haversine$full_address_and_distance) +
  scale_fill_manual(name = "Store Name",
                    values = coffee_brand_colors,
                    labels = coffee_brand_labels) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_polar(clip = "off") +
  geom_text(
    aes(
      x = x_axis_id,
      y = meter_distance_to_city_centre + 40,
      label = full_address_and_distance,
      hjust = hjust
    ),
    color = "black",
    fontface = "bold",
    alpha = 0.6,
    size = 2.5,
    angle = active_businesses_with_haversine$angle,
    inherit.aes = FALSE
  ) +
  labs(title = "Meter distance to City Center",
       subtitle = "(Granville & West Georgia Street)")

# Programmatic saving via ggsave() was proving problematic, given the level of customization in this plot. As this is a one-off custom plot, I opted to export its png file manually
# 800 width x 750 height

###### End circular barplot generation ######


### End visualization ###
