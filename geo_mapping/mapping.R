library(data.table)
library(dplyr)
library(htmlwidgets)
library(jsonlite)
library(leaflet)
library(purrr)
library(tibble)
library(tidyr)
library(VancouvR)
library(withr)

# Set API Option from .Renviron
VancouverOpenDataApiKey <- Sys.getenv("vancouver_open_data_api_key")

# Get business with active licenses
active_businesses <- get_cov_data(dataset_id = "business-licences", where = "status='Issued'")


coffee_franchises <- active_businesses %>%
  filter(grepl('McDonalds|Coffee|Tim Horton', businesstradename)) %>%
  mutate(
    businesstradename = case_when(
      grepl("Blenz Coffee", businesstradename) ~ "Blenz Coffee",
      grepl("JJ Bean", businesstradename) ~ "JJ Bean",
      grepl("McDonalds", businesstradename) ~ "McDonalds",
      grepl("Rocanini", businesstradename) ~ "Rocanini Coffee Roasters",
      grepl("Starbucks", businesstradename) ~ "Starbucks",
      grepl("Tim Horton", businesstradename) ~ "Tim Horton\'s",
      grepl("Trees", businesstradename) ~ "Trees Organic Coffee",
      TRUE ~ businesstradename
    )
  ) %>%
  mutate_at("numberofemployees", as.numeric) %>%
  filter(
    businesstradename %in% c(
      "Blenz Coffee",
      "JJ Bean",
      "McDonalds",
      "Starbucks",
      "Tim Horton\'s"
    )
  ) %>%
  mutate(
    formatted_tradename = case_when(
      grepl("Blenz Coffee", businesstradename) ~ "blenz",
      grepl("JJ Bean", businesstradename) ~ "jj_bean",
      grepl("McDonalds", businesstradename) ~ "mcdonalds",
      grepl("Starbucks", businesstradename) ~ "starbucks",
      grepl("Tim Horton", businesstradename) ~ "tim_hortons",
      TRUE ~ businesstradename
    )
  )


formatted_coffee_franchises <- coffee_franchises %>%
  # filter( businesstype == "Casino", !is.na(geom) ) %>%
  mutate(Long = as.numeric( sub(".*\\[(.*),.*","\\1",geom) ), Lat = as.numeric ( sub(".* (.*)\\].*","\\1",geom) ) ) %>%
  select(businessname, businesstradename, formatted_tradename, numberofemployees, Long, Lat)


coffee_icon_list <- iconList(
  blenz = makeIcon("geo_mapping/assets/blenz_logo.jpeg", "geo_mapping/assets/blenz_logo.jpeg", 32, 32, shadowWidth = 5),
  jj_bean = makeIcon("geo_mapping/assets/jj_bean_logo.png", "geo_mapping/assets/jj_bean_logo.png", 34, 28),
  mcdonalds = makeIcon("geo_mapping/assets/mcdonalds_canada_logo.png", "geo_mapping/assets/mcdonalds_canada_logo.png", 32, 32),
  starbucks = makeIcon("geo_mapping/assets/starbucks_logo.png", "geo_mapping/assets/starbucks_logo.png", 30, 30),
  tim_hortons = makeIcon("geo_mapping/assets/tim_hortons_logo.jpg", "geo_mapping/assets/tim_hortons_logo.jpg", 32, 26)
)

mapped_franchise_locations <- leaflet(formatted_coffee_franchises) %>%
  addTiles() %>%
  # Select from coffee_icon_list based on formatted_coffee_franchises$formatted_tradename
  addMarkers(icon = ~coffee_icon_list[formatted_tradename])

# Ouput leaflet widget as html
with_dir('visualizations', saveWidget(mapped_franchise_locations, file="franchise_locations.html"))



# Future anlalytics, separate from geo mapping
premises_and_employee_count <- coffee_franchises %>% group_by(businesstradename) %>%
  summarise(premises_count = n(), registered_employee_count = sum(numberofemployees))
